# Workstation set-up ------------------------------------------------------

library(aws.s3)
aws.signature::use_credentials()
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

library(tidyverse)
library(sf)
library(readxl)
library(xlsx)
library(stringr)
library(DescTools)
library(rnaturalearth)
library(ggpubr)
library(units)
library(scales)
library(kableExtra)
library(here)
library(readstata13)
library(sjmisc)
library(terra) # put it after {raster} such that it superceeds homonym functions. 
library(exactextractr)
library(stars)

dir.create("temp_data", "coopbs_10km_buffer") # not use currently
dir.create(here("temp_data", "terrain"))
dir.create(here("temp_data", "BNETD"))
dir.create(here("temp_data", "prepared_main_dataset"))

# Assets and functions -----------------------------------------------------
# use the projected CRS used by BNETD for their 2020 land use map. 
civ_crs <- 32630

# load in particular the function fn_trader_to_group_names, str_trans, ... 
source(here("code", "USEFUL_STUFF_supplyshedproj.R"))


coopbsy = read.csv(
  file = here("temp_data/private_IC2B/IC2B_v2_coop_bs_year.csv")) %>% 
  rename(BUYER_LONGITUDE = LONGITUDE, 
         BUYER_LATITUDE = LATITUDE, 
         LINK_YEAR = YEAR)

licens19 = read.csv2(here("input_data", "CCC", "ACHATEURS_AGREES_2019_GEOCODED.csv"))
licens20 = read.csv2(here("input_data", "CCC", "ACHATEURS_AGREES_2020_GEOCODED.csv"))
licens21 = read.csv2(here("input_data", "CCC", "ACHATEURS_AGREES_2021_GEOCODED.csv"))

# Cargill link data
carg_links = read.csv(here("temp_data", "preprocessed_cargill", "cargill_links_standardized.csv")) %>% 
  # can remove this once cargill rerun
  rename(LINK_YEAR = YEAR) %>% 
  mutate(LINK_VOLUME_KG = NA) 
  
# JRC link data
jrc_links = read.csv(here("temp_data", "preprocessed_jrc_data", "jrc_links_standardized.csv"))%>% 
  rename(LINK_YEAR = YEAR)

# jrc_links_coops = 
#   jrc_links %>% 
#   filter(BUYER_IS_COOP) %>% 
#   select(-BUYER_IS_COOP) 
# 
# jrc_links_other = 
#   jrc_links %>% 
#   filter(!BUYER_IS_COOP) %>% 
#   select(-BUYER_IS_COOP) 


# Sustain-cocoa link data
# sc_links_vil = read.csv(here("temp_data", "preprocessed_sustain_cocoa", "sustain_cocoa_links_standardized.csv"))
sc_links = read.csv(here("temp_data", "preprocessed_sustain_cocoa", "sustain_cocoa_hh_links_standardized.csv")) %>% 
  rename(LINK_YEAR = YEAR)

# In SC data, in addition to just coop/other buyers, we have coops that didn't match IC2B
# sc_links_coops =
#   sc_links %>% 
#   filter(BUYER_IS_COOP) %>% 
#   select(-BUYER_IS_COOP) 
#   
sc_links_other =
  sc_links %>%
  filter(is.na(COOP_BS_ID)) 

sc_links_other$BUYER_IS_COOP %>% summary()

# # # # #
departements <- read_sf("input_data/s3/CIV_DEPARTEMENTS.geojson")
init_crs = st_crs(departements)
departements = 
  st_transform(departements, crs = civ_crs)
end_crs = st_crs(departements)


# Production per department estimated by Trase
production_dpt =
  read_csv(here("input_data/s3/indicators/in/q4_2023/cocoa_production_2019_2022_q4_2023.csv")) %>%
  select(LVL_4_CODE, LVL_4_NAME,
         production_2019, production_2020, production_2021, production_2022) %>%
  pivot_longer(cols = starts_with("production"),
               values_to = "COCOA_PRODUCTION_TONNES",
               names_to = "LINK_YEAR",
               names_prefix = "production_") %>%
  mutate(LINK_YEAR = as.numeric(LINK_YEAR)) %>% 
  summarise(.by = LVL_4_CODE, 
            AVG_COCOA_PRODUCTION_TONNES = mean(COCOA_PRODUCTION_TONNES))

is.grouped_df(production_dpt)
summary(production_dpt)


# TERRAIN
tri = rast(here("input_data/terrain/tri/tri.txt"))
tri_area = rast(here("input_data/terrain/cellarea/cellarea.txt"))

# BNETD LAND USE MAP FOR 2020
# aggregated in GEE to 1km, for binarized cocoa and settlement classes. 
bnetd = rast(here("input_data/GEE/BNETD_binary_cocoa_settlements_3km.tif"))


# coopbs_tmplt = coopbsy[235:240,]
# example = head(carg_links)
# example = 
#   example %>% 
#   mutate(FARM_ID = paste0("SUCDEN_FARMER_", 1:6), 
#          FARM_LONGITUDE = PRO_LONGITUDE,
#          FARM_LATITUDE = PRO_LATITUDE,
#          COOP_ACRONYM = head(coopbs_tmplt$SIMPLIF_ABRVNAME),
#          COOP_FULLNAME = head(coopbs_tmplt$SUPPLIER_FULLNAME),
#          COOP_LONGITUDE = head(coopbs_tmplt$BUYER_LONGITUDE), 
#          COOP_LATITUDE = head(coopbs_tmplt$BUYER_LATITUDE), 
#          FARM_GEOMETRY = head(departements$geometry)) %>% 
#   select(LINK_YEAR, starts_with("COOP_"), starts_with("FARM_"), -COOP_BS_ID)
# example
# 
# write.xlsx(example, here("temp_data", "link_data_example.xlsx"))

# CONSOLIDATE LINK DATA ---------------------

# Consolidate all actual links, to both coops and other buyers (split later) 

# Make template for the data frame consol, to frame the consolidation of every specific data set. 
consol <- data.frame(
  "LINK_YEAR" = NA, 
  "PRO_ID" = NA, 
  "COOP_BS_ID" = NA, 
  "LINK_ID_ONLYACTUAL" = NA,
  "BUYER_IS_COOP" = NA,
  "BUYER_LONGITUDE" = NA,
  "BUYER_LATITUDE" = NA,
  "LINK_ACTUALONLY_DISTANCE_METERS" = NA, 
  "LINK_VOLUME_KG" = NA, 
  # "PRO_DEPARTMENT_GEOCODE" = NA, 
  # "PRO_DEPARTMENT_NAME" = NA,
  "PRO_LONGITUDE" = NA, 
  "PRO_LATITUDE" = NA
)

# add Cargill
initcoln <- ncol(consol)
initrown <- nrow(consol)
consol <- full_join(consol, carg_links, 
                    by = intersect(colnames(consol), colnames(carg_links)), multiple = "all") 

if(ncol(consol) != initcoln){stop("something went wrong in consolidating disclosure data.")}

# Add JRC 
initcoln <- ncol(consol)
initrown <- nrow(consol)
consol <- full_join(consol, jrc_links, 
                    by = intersect(colnames(consol), colnames(jrc_links)), multiple = "all") 

if(ncol(consol) != initcoln){stop("something went wrong in consolidating disclosure data.")}

# Add Sustain-cocoa
initcoln <- ncol(consol)
initrown <- nrow(consol)
consol <- full_join(consol, sc_links, 
                    by = intersect(colnames(consol), colnames(sc_links)), multiple = "all") 

if(ncol(consol) != initcoln){stop("something went wrong in consolidating disclosure data.")}

# remove first rows that were just here for left_join-ing
consol <- filter(consol, !is.na(LINK_YEAR))
# for now remove bc not useful
consol <- select(consol, -LINK_ID_ONLYACTUAL)


## Split coops/other links ---------------
# BUYER_IS_COOP is true including for links with buyers we believe are coops but are not matched with IC2B (in SC data).
# Here, we want only those matched with IC2B.
consol_IC2Bcoops = 
  consol %>% 
  filter(!is.na(COOP_BS_ID))

consol_other = 
  consol %>% 
  filter(is.na(COOP_BS_ID)) 

consol_other$BUYER_IS_COOP %>% summary()

consol %>% filter(!is.na(COOP_BS_ID) & !BUYER_IS_COOP) %>% View()
# Actual coop link stats ----------------

# Make ID for actual links WITH COOPS 
# (actual links including other buyers exist already)
# allows in particular to identify actual links more easily if in some input data PRO_ID is duplicated in consol_IC2Bcoops
length(unique(consol_IC2Bcoops$PRO_ID))
nrow(consol_IC2Bcoops)
consol_IC2Bcoops = 
  consol_IC2Bcoops %>% 
  group_by(PRO_ID, COOP_BS_ID) %>% 
  mutate(LINK_ID_COOPS = cur_group_id()) %>% 
  ungroup()
if(consol_IC2Bcoops$LINK_ID_COOPS %>% unique() %>% length() != nrow(consol_IC2Bcoops)){stop()}

# to investigate potential prb
# consol_IC2Bcoops = 
#   consol_IC2Bcoops %>% 
#   mutate(DUPL_PRO = duplicated(PRO_ID)) %>% 
#   group_by(PRO_ID) %>% 
#   mutate(ANY_DUPL = any(DUPL_PRO)) %>% 
#   ungroup()
# consol_IC2Bcoops %>% 
#   filter(ANY_DUPL) %>% 
#   View()


# Latest surveyed year, to take the panel of coops at this time.  
latest_survey_year = max(consol_IC2Bcoops$LINK_YEAR, na.rm = TRUE)
latest_survey_year

# Distance producer - intermediary 
summary(consol_IC2Bcoops$LINK_ACTUALONLY_DISTANCE_METERS)

dist_outliers = boxplot.stats(consol_IC2Bcoops$LINK_ACTUALONLY_DISTANCE_METERS, coef = 2)$out %>% sort()
consol_IC2Bcoops = 
  consol_IC2Bcoops %>% 
  mutate(LINK_ACTUALONLY_DISTANCE_METERS = case_when(
    LINK_ACTUALONLY_DISTANCE_METERS %in% dist_outliers ~ NA,
    TRUE ~ LINK_ACTUALONLY_DISTANCE_METERS
  ))

(dist_meters_90pctl = quantile(consol_IC2Bcoops$LINK_ACTUALONLY_DISTANCE_METERS, 0.9, na.rm = T) %>% round(-3))
(dist_meters_95pctl = quantile(consol_IC2Bcoops$LINK_ACTUALONLY_DISTANCE_METERS, 0.95, na.rm = T) %>% round(-3))
(dist_meters_max = quantile(consol_IC2Bcoops$LINK_ACTUALONLY_DISTANCE_METERS, 1, na.rm = T) %>% round(-3))

(dist_meters_threshold <- dist_meters_95pctl)

summary(consol_IC2Bcoops$LINK_ACTUALONLY_DISTANCE_METERS)
sd(consol_IC2Bcoops$LINK_ACTUALONLY_DISTANCE_METERS[consol_IC2Bcoops$LINK_ACTUALONLY_DISTANCE_METERS], na.rm = TRUE)



# Prepare coop location/buffer ---------------
coopbsy$LINK_YEAR %>% summary()

coopbs = 
  coopbsy %>% 
  filter(LINK_YEAR == latest_survey_year)
#   distinct(COOP_BS_ID, .keep_all = TRUE)

coopbs_sf = 
  coopbs %>% 
  filter(!is.na(BUYER_LONGITUDE)) %>% 
  st_as_sf(coords = c("BUYER_LONGITUDE", "BUYER_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(crs = civ_crs) 

# coopbs_buffered_sf =
#   coopbs_sf %>%
#   st_buffer(dist = dist_meters_threshold)

# Extract terrain/LU in smaller buffers than the 90%+ threshold, to have meaningful variation
coopbs_10km_buffer =
  coopbs_sf %>%
  st_buffer(dist = 10*1e3) %>% 
  select(COOP_BS_ID) %>% 
  st_transform(crs = crs(tri))

# export to GEE and re-import here - not anymore, we do it in R only now.   
# write_sf(coopbs_10km_buffer, "temp_data/coopbs_10km_buffer/coopbs_10km_buffer.shp")


# DEPLOY MODEL STRUCTURE -----------------

## Set spatial parameters ------------
# The size of the grid is an important parameter. 
# 12km is because 75% of cocoa producers surveyed by the JRC have their cocoa plots less than 6km away from their house.
# The average location of the producer in a grid cell is in it's centroid. Taking 6km away in any direction from the center implies 12km. 
# At the same time, in Cargill data, 95% of the plots of the same farmer fit in bounding boxes of 25 hectares (0.5 x 0.5 km) or less.
grid_size_m = 30000

## Limit to cocoa growing region 
cocoa_departements = 
  departements %>% 
  left_join(production_dpt, 
            by = "LVL_4_CODE") %>% 
  filter(AVG_COCOA_PRODUCTION_TONNES > 500)

plot(cocoa_departements[,"AVG_COCOA_PRODUCTION_TONNES"])

grid_extent = ext(cocoa_departements)
grid_extent_geod = project(grid_extent, from = paste0("epsg:",civ_crs), to = "epsg:4326")
grid_crs = crs(cocoa_departements)


## Grid base -----------

# Make a terra template, just because I found how to do it before I found with stars.  
(grid_sr = rast(extent = grid_extent, 
                resolution = grid_size_m,
                crs = grid_crs)
)

## Cell-level variables -------------------
 # (do here so these vars are more clearly embedded in the grid, without dependence on cell ID attribution)

### Terrain ------------
# Do only Terrain Ruggedness Index for now

grid_extent_tricrs = project(grid_extent, from = paste0("epsg:",civ_crs), to = crs(tri))

tri_civ = 
  tri %>% 
  crop(grid_extent_tricrs)

tri_project_filename = paste0("tri_modelgrid_", grid_size_m*1e-3,"km.tif")

if(!file.exists(here("temp_data", "terrain", tri_project_filename))){
  project(x = tri_civ, 
          y = grid_sr,
          align = FALSE, # (the default) not necessary if cropped prior
          method = "average",
          threads = TRUE,
          filename = here("temp_data", "terrain", tri_project_filename), 
          overwrite = TRUE)
}

grid_tri = rast(here("temp_data", "terrain", tri_project_filename))

plot(grid_tri)
plot(st_geometry(departements), add = TRUE)

### Land use --------------
# we want to align exactly the GEE aggregate of BNETD raw data (10m) into 1km, to the grid base set here.   
bnetd
grid_sr

bnetd_project_filename = paste0("bnetd_modelgrid_", grid_size_m*1e-3,"km.tif")

if(!file.exists(here("temp_data", "BNETD", bnetd_project_filename))){
  terra::resample(x = bnetd, 
                  y = grid_sr,
                  method = "sum", # sum bc values in ha
                  threads = TRUE,
                  filename = here("temp_data", "BNETD", bnetd_project_filename), 
                  overwrite = TRUE)
}

grid_lu = rast(here("temp_data", "BNETD", bnetd_project_filename))

plot(grid_lu$cocoa)
plot(st_geometry(departements), add = TRUE)


### Suitability --------------



# Convert to stars

# grid_st = st_as_stars(grid_sr)
grid_st = 
  c(grid_lu, grid_tri) %>% 
  st_as_stars(ignore_file = TRUE, 
              as_attributes = TRUE) %>% 
  rename(CELL_COCOA_HA = cocoa, 
         CELL_SETTLEMENT_HA = settlements,
         CELL_TRI_MM = tri) # tri is in millimeters in Nunn & Puga data. 


## Add actual coop links -------------

# Spatialize actual links 
consol_IC2Bcoops_sf = 
  consol_IC2Bcoops %>% 
  st_as_sf(coords = c("PRO_LONGITUDE", "PRO_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs)

# convert raster to sf with polygons
(grid_poly = 
  grid_st %>% 
  st_xy2sfc(as_points = FALSE, na.rm = FALSE) %>% # as_points = F outputs grid cells as polygons
  st_as_sf() %>% 
  mutate(CELL_ID = row_number()) %>% 
   select(CELL_ID, CELL_COCOA_HA, CELL_SETTLEMENT_HA, CELL_TRI_MM) #,-lyr.1
  )

# and the same object, but with cell centroid geometry
grid_ctoid = 
  grid_poly %>% 
  mutate(geometry = st_centroid(geometry))

# This multiplies the number of rows (equal to number of cells in grid_poly) 
# by the number of actual links in every grid cell.  
grid_actual = 
  grid_poly %>% 
  st_join(consol_IC2Bcoops_sf, 
          join = st_intersects,
          left = TRUE)

if(
  consol_IC2Bcoops$COOP_BS_ID %>% unique() %>% na.omit() %>%length() != 
    grid_actual$COOP_BS_ID %>% na.omit() %>% unique() %>% length()
){stop("info on linked coops has been lost in the process")}

grid_actual = 
  grid_actual %>% 
  # mutate(TRAINING_SET_CELL = if_else(!is.na(PRO_ID), TRUE, FALSE)) %>% 
  rename(LINK_ACTUAL_COOP_BS_ID = COOP_BS_ID)

# The number of NAs in LINK_ID_COOPS is the number of grid cells with no actual link
grid_actual$LINK_ID_COOPS %>% summary()

# and this is the distribution of unique actual links by grid cell
grid_actual %>% 
  group_by(CELL_ID) %>% 
  mutate(N_UNIQUE_ACTUAL_LINKS_BYGRID = length(unique(na.omit(LINK_ID_COOPS)))) %>% 
  ungroup() %>% 
  pull(N_UNIQUE_ACTUAL_LINKS_BYGRID) %>% summary()


# Dataframe of actual links, without geometry for further join, and with potential link id column to identify duplicates 
actual = 
  grid_actual %>% 
  filter(!is.na(LINK_ACTUAL_COOP_BS_ID)) %>% 
  st_drop_geometry() %>% 
  mutate(LINK_POTENTIAL_COOP_BS_ID = LINK_ACTUAL_COOP_BS_ID) # every actual coop is also a potential one in the model's terminology.  

nrow(grid_poly) # out of the 37975 3km grid cells, 
actual$CELL_ID %>% unique() %>% length() # 510 3km cells have at least one actual link.  


## Add virtual coop links ---------------

# For every grid cell, this adds as many rows as there are potential links (i.e. coops within distance)
# -- including the actual ones -- which we remove below
potential = 
  grid_ctoid %>% 
  st_join(
    coopbs_sf %>% 
      select(COOP_BS_ID, BUYER_LONGITUDE, BUYER_LATITUDE), #
    join = st_is_within_distance, 
    dist = as_units(dist_meters_threshold, "m"), 
    left = TRUE
  ) %>% 
  st_drop_geometry() %>% 
  rename( # use names that make sense in the dimensions post join
    LINK_POTENTIAL_COOP_BS_ID = COOP_BS_ID) %>% 
  # make colnames be identical with actual and thus with consol_IC2Bcoops.   
  mutate(
    LINK_YEAR = NA, 
    PRO_ID = NA, 
    # LINK_ID_ONLYACTUAL = NA,
    LINK_ACTUAL_COOP_BS_ID = NA, 
    PRO_LONGITUDE = NA, 
    PRO_LATITUDE = NA, 
    LINK_ID_COOPS = NA, 
    LINK_ACTUALONLY_DISTANCE_METERS = NA, 
    LINK_VOLUME_KG = NA, 
    BUYER_IS_COOP = NA
  ) %>% 
  select(names(actual)) %>% 

  # Stack potential & actual and remove actual links counted twice
  rbind(actual) %>% 
  # identify origins 
  mutate(IS_FROM_STJOIN = is.na(LINK_ID_COOPS)) %>% 
  # identify duplicated coop ids within a grid cell
  # make sure that ALL duplicates are found, regardless of order. 
  group_by(CELL_ID) %>% 
  mutate(DUP_LINK = duplicated(LINK_POTENTIAL_COOP_BS_ID) | duplicated(LINK_POTENTIAL_COOP_BS_ID, fromLast = TRUE)) %>% 
  ungroup() %>% 
  # this identified duplicates including due to several actual links from this cell,
  # so remove rows that are duplicated coop ids, AND are from the spatial join, i.e. are not several actual links. 
  filter(!(DUP_LINK & IS_FROM_STJOIN)) %>% 
  # Number of reachable coop BS within a grid cell, regardless through how many links  
  group_by(CELL_ID) %>% 
  mutate(CELL_N_BS_WITHIN_DIST = length(na.omit(unique(LINK_POTENTIAL_COOP_BS_ID)))) %>%  # use this, and not n(), for the variable to take value 0 when there's is no potential coop matched (i.e. potential link), rather than one row that has NA value 
  ungroup() %>% 
  # identify grid cells with no actual link 
  group_by(CELL_ID) %>% 
  mutate(CELL_NO_ACTUAL_LINK = all(IS_FROM_STJOIN)) %>% 
  ungroup() %>% 
  # OUTCOME VARIABLE - is the link actual or virtual??? 
  mutate(LINK_IS_ACTUAL_COOP = !is.na(LINK_ID_COOPS))

# potential %>% 
#   filter((DUP_LINK & IS_FROM_STJOIN)) %>% View()

potential$LINK_IS_ACTUAL_COOP %>% summary()
potential$IS_FROM_STJOIN %>% summary()
potential$DUP_LINK %>% summary()

# The choice of the threshold, 90% vs. 100% makes a huge difference, since it's implies a reach of 41 vs. 75km resp. 
# not all actual links are retrieved in the spatial join, for 2 reasons probably: 
# - the threshold is not the max distance, but there's still a diff when using the max   
# - the distance is to grid cell centroids here. 
# but it's surprising there is such a difference... 

# - dans les grids avec des actual links, il y a le nombre d'actual links + le nombre d'AUTRES coops within distance du centroid. 
# - dans les mailles sans actual link, il y a le nombre de coops within distance du centroid. 

# But currently, there are always more different potential coops in potential than in stjoin, suggesting we haven't removed enough 
# TEST second proposition. The first one is not testable, because some actual links are not reproduced by the spatial join. 
if(
grid_ctoid %>% 
  st_join(
    coopbs_sf %>% 
      select(COOP_BS_ID), #, BUYER_LONGITUDE, BUYER_LATITUDE
    join = st_is_within_distance, 
    dist = as_units(dist_meters_threshold, "m"), 
    left = TRUE
  ) %>%
  st_drop_geometry() %>% 
  rename(LINK_POTENTIAL_COOP_BS_ID = COOP_BS_ID) %>% 
  summarise(.by = CELL_ID, 
            CELL_N_BS_WITHIN_DIST = length(unique(LINK_POTENTIAL_COOP_BS_ID))) %>% 
  inner_join(potential %>% 
              filter(CELL_NO_ACTUAL_LINK) %>% 
              select(CELL_ID, CELL_N_BS_WITHIN_DIST), 
            by = "CELL_ID", multiple = "all") %>% 
  mutate(PROBLEM = CELL_N_BS_WITHIN_DIST.x < CELL_N_BS_WITHIN_DIST.y) %>% 
  filter(PROBLEM) %>% 
  nrow() > 0
){stop("stacking method producing a different number of potential links than the spatial join in cells with no actual link, which is unexpected.")} 

# So there should be as many none actual links , i.e. virtual links, as there are links from stjoin now 
if(
  potential %>% 
  filter(!LINK_IS_ACTUAL_COOP != IS_FROM_STJOIN) %>% 
  nrow() != 0
){stop("something is misunderstood")}

# and there should not be any duplicated potential link within a grid and an actual link
if(
potential %>% 
  group_by(CELL_ID, LINK_ID_COOPS) %>% 
  mutate(ANY_DUPL_COOP = any(duplicated(LINK_POTENTIAL_COOP_BS_ID))) %>% 
  ungroup() %>% 
  pull(ANY_DUPL_COOP) %>% any()
){stop("something unexpected in the row deployment by the spatial join")}

# Another check: that LINK_POTENTIAL_COOP_BS_ID is NA only in grid cells with no link at all. 
if(
potential %>% 
  group_by(CELL_ID) %>% 
  mutate(ANY_NA = any(is.na(LINK_POTENTIAL_COOP_BS_ID)), 
         ALL_NA = all(is.na(LINK_POTENTIAL_COOP_BS_ID))) %>% 
  ungroup() %>% 
  filter(ANY_NA != ALL_NA) %>% nrow() != 0
){stop("unexpected")}

if(
  potential %>%
  filter(LINK_ACTUAL_COOP_BS_ID == LINK_POTENTIAL_COOP_BS_ID) %>% 
  nrow() != nrow(consol_IC2Bcoops_sf)
){stop("actual links were missed or duplicated")}
# potential %>%
#   filter(!is.na(LINK_ACTUAL_COOP_BS_ID)) %>%
#   nrow()

# With this method, it's not exactly the same number, but very marginal. 
# grid_actual_ctoid = 
#   grid_actual %>% 
#   st_centroid() %>% 
#   st_join(
#     coopbs_buffered_sf %>% select(COOP_BS_ID, BUYER_LONGITUDE, BUYER_LATITUDE),
#     join = st_intersects, 
#     left = TRUE
#   )

# The number of distinct actual links within a grid cell. 
# potential %>% 
#   group_by(CELL_ID) %>% 
#   mutate(IS_EMPTY_GRID = length(unique(na.omit(POTENTIAL_LINKS_IN_GRID_SET_ID)))) %>% 
#   ungroup() %>% 
#   pull(IS_EMPTY_GRID) %>% summary()
           

## Add links to other intermediaries --------------
# Do it here because given our data inputs, links to other intermediaries than coops is the exception and not the rule. 
# (It's from JRC & SC only.)

# Cell-level variables prepared above
cell_vars = 
  grid_poly %>% # the polygons (we need)
  select(CELL_ID) %>% 
  left_join(potential %>% 
              select(starts_with("CELL_")) %>% # all cell-level variables
              distinct(CELL_ID, .keep_all = TRUE),
            by = "CELL_ID")
  
consol_other = 
  consol_other %>% 
  st_as_sf(coords = c("PRO_LONGITUDE", "PRO_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) %>% 
  # Spatially join cell-level variables to the location of producers linked with other intermediaries than coops. 
  st_join(cell_vars, 
          join = st_intersects,
          left = TRUE) %>% 
  mutate(LINK_ID_OTHERS = row_number()) %>% 
  st_drop_geometry()

stopifnot(length(unique(consol_other$LINK_ID_OTHERS)) == nrow(consol_other))
if(anyNA(potential$LINK_IS_ACTUAL_COOP)){stop("below operations needs that there's no NA in this indicator")}

potential_all = 
  potential %>% 
  full_join(
    consol_other %>% select(-COOP_BS_ID), # as this is only NAs
    by = intersect(colnames(potential), colnames(consol_other)), multiple = "all") %>% 
  # create indicator for these links
  mutate(LINK_IS_ACTUAL_OTHER = !is.na(LINK_ID_OTHERS),
         # and correct LINK_IS_ACTUAL_COOP which otherwise has now NAs because of the merge
         LINK_IS_ACTUAL_COOP = if_else(is.na(LINK_IS_ACTUAL_COOP), FALSE, LINK_IS_ACTUAL_COOP)) 

names(potential_all)
if(nrow(potential_all) != nrow(potential) + nrow(filter(consol, is.na(COOP_BS_ID)))){stop("some rows were unexpectedly matched")}
stopifnot(sum(potential_all$LINK_IS_ACTUAL_OTHER) == nrow(consol_other))

# This adds 517 rows from JRC and ~270 from SC (one row per actual link with another buyer than a coop). 
# plus 143 rows from SC which are with a coop not matched with IC2B. 


# STRUCTURE IDENTIFIERS ----------------
# In potential_all, there can be: 
# - cells with no potential link (no actual nor virtual) - i.e. cells very remote
# - cells with potential links, but no actual link (only virtual links)
# - cells with actual link(s) only with coop(s)
# - cells with actual link(s) only with other intermediary(s)
# - cells with both actual links with coop(s) and intermediary(s)

# * where, currently, coop refers to IC2B coops only, and other includes unmatched SC coop *

# Make identifiers to ease manipulation of these groups  
potential_all = 
  potential_all %>% 
  mutate(
    # an actual link is either with a coop or with an other intermediary
    LINK_IS_ACTUAL = LINK_IS_ACTUAL_COOP | LINK_IS_ACTUAL_OTHER, 
    # a virtual link is a row that is not an actual link, but still matched a coopbs  
    LINK_IS_VIRTUAL = !LINK_IS_ACTUAL & !is.na(LINK_POTENTIAL_COOP_BS_ID), 
    # a potential link is either an actual or a virtual link
    LINK_IS_POTENTIAL = LINK_IS_ACTUAL | LINK_IS_VIRTUAL, 
    # Adjust BUYER_IS_COOP to be true for all potential links with coops
    BUYER_IS_COOP = case_when(
      is.na(BUYER_IS_COOP) & LINK_IS_ACTUAL_COOP | LINK_IS_VIRTUAL ~ TRUE, 
      TRUE ~ BUYER_IS_COOP
    )) %>% 
    # based on this, characterize cells
  group_by(CELL_ID) %>% 
  mutate(
    CELL_NO_POTENTIAL_LINK = !any(LINK_IS_POTENTIAL),
    CELL_ONLY_VIRTUAL_LINK = any(LINK_IS_POTENTIAL) & !any(LINK_IS_ACTUAL),
    CELL_ACTUAL_ONLYCOOP_LINK  = any(LINK_IS_ACTUAL_COOP)  & !any(LINK_IS_ACTUAL_OTHER),
    CELL_ACTUAL_ONLYOTHER_LINK = any(LINK_IS_ACTUAL_OTHER) & !any(LINK_IS_ACTUAL_COOP),
    CELL_ACTUAL_BOTH_LINK = any(LINK_IS_ACTUAL_OTHER) & any(LINK_IS_ACTUAL_COOP),
    CELL_ACTUAL_LINK = any(LINK_IS_ACTUAL_OTHER) | any(LINK_IS_ACTUAL_COOP)
  ) %>% 
  ungroup() %>% 
  select(CELL_ID, starts_with("CELL_"), starts_with("LINK_"), starts_with("COOP_"), 
         everything()) 
  
# Check that the cell identifiers cover all cells
if(
  potential_all %>% 
  rowwise() %>% 
  mutate(CELL_ANY_CAT = CELL_NO_POTENTIAL_LINK + CELL_ONLY_VIRTUAL_LINK + CELL_ACTUAL_ONLYCOOP_LINK + CELL_ACTUAL_ONLYOTHER_LINK + CELL_ACTUAL_BOTH_LINK) %>% 
  filter(CELL_ANY_CAT != 1) %>% nrow() > 0  
){stop("Categories don't cover all cells")}

# check that actual links with coops are 
if(!all.equal(potential_all %>% filter(LINK_IS_ACTUAL_COOP), 
              potential_all %>% filter(LINK_POTENTIAL_COOP_BS_ID == LINK_ACTUAL_COOP_BS_ID))
){stop("unexpected")}

if(anyNA(potential_all %>% select(LINK_IS_ACTUAL, 
                                  LINK_IS_VIRTUAL, 
                                  LINK_IS_POTENTIAL,
                                  CELL_NO_POTENTIAL_LINK, 
                                  CELL_ONLY_VIRTUAL_LINK, 
                                  CELL_ACTUAL_ONLYCOOP_LINK, 
                                  CELL_ACTUAL_ONLYOTHER_LINK, 
                                  CELL_ACTUAL_BOTH_LINK,
                                  CELL_ACTUAL_LINK))){stop("NAs not expected")}
potential_all %>% 
  select(LINK_IS_ACTUAL, 
         LINK_IS_VIRTUAL, 
         LINK_IS_POTENTIAL,
         CELL_NO_POTENTIAL_LINK, 
         CELL_ONLY_VIRTUAL_LINK, 
         CELL_ACTUAL_ONLYCOOP_LINK, 
         CELL_ACTUAL_ONLYOTHER_LINK, 
         CELL_ACTUAL_BOTH_LINK,
         CELL_ACTUAL_LINK) %>% 
  summary()
  
# group_by(CELL_ID) %>% 
# mutate(CELL_POTENTIAL_LINK_ID = cur_group_rows()) %>% 
# ungroup() %>% 
# mutate(CELL_POTENTIAL_LINK_ID = paste0(CELL_POTENTIAL_LINK_ID, "")) %>% 

potential_all$LINK_ID_COOPS %>% unique() %>% length() 
anyNA(potential_all$LINK_ID_COOPS)


# ADD VARIABLES ----------------

potential_all_save = potential_all

init_nrow_pa = nrow(potential_all) # # just to check that this section does not add any row

## Cell-level variables ------------------

### Nb of potential coops -------- 
# was computed above (CELL_N_BS_WITHIN_DIST), because useful in a test. 
potential_all$CELL_N_BS_WITHIN_DIST %>% summary()


### Cell district -----------
# Just easier to do that here rather than in grid-level above. 
# Use the full shapefile of departements, and not only cocoa departements here, 
# because there are grid cells in no-cocoa departements. 
grid_distr =
  grid_ctoid %>% select(CELL_ID) %>%  
  st_join(departements %>% select(CELL_DISTRICT_GEOCODE = LVL_4_CODE, CELL_DISTRICT_NAME = LVL_4_NAME),
          join = st_intersects) %>% 
  st_drop_geometry() 

if(nrow(grid_distr) != nrow(grid_ctoid)){stop("the spatial join matches several districts with one grid cell")}

potential_all = 
  potential_all %>% 
  left_join(grid_distr, 
            by = "CELL_ID")


### Nb of coops in cell's district -------------

(n_coops_dpt = 
  coopbs %>% 
  filter(!is.na(DISTRICT_GEOCODE)) %>% 
  summarise(.by = c(DISTRICT_GEOCODE, DISTRICT_NAME), 
            N_COOP_IN_DPT = length(na.omit(unique(COOP_ID)))) %>% 
  arrange(desc(N_COOP_IN_DPT)))

(sum(n_coops_dpt$N_COOP_IN_DPT)) 

potential_all = 
  potential_all %>% 
  left_join(n_coops_dpt %>% select(CELL_N_COOP_IN_DPT = N_COOP_IN_DPT, # this is a cell level var
                                   DISTRICT_GEOCODE), 
            by = join_by(CELL_DISTRICT_GEOCODE == DISTRICT_GEOCODE))



### Nb other licensed buyers in cell's district -----------
licens_panel = 
  rbind(licens19 %>% select(NOM, DENOMINATION, LVL_4_CODE) %>% mutate(LINK_YEAR = 2019),
        licens20 %>% select(NOM, DENOMINATION, LVL_4_CODE) %>% mutate(LINK_YEAR = 2020),
        licens21 %>% select(NOM, DENOMINATION, LVL_4_CODE) %>% mutate(LINK_YEAR = 2021)) %>% 
  arrange(NOM, DENOMINATION, LVL_4_CODE) %>% 
  group_by(NOM, DENOMINATION, LVL_4_CODE) %>% 
  # code from Trase work, don't bother, it's just an ID
  mutate(LICENSED_BUYER_ID = paste0("CI-COFFEE-COCOA-APPROVED-BUYER-", str_pad(cur_group_id(), width=3, pad = "0"))) %>% 
  ungroup() 

(n_licbuy_dpt = 
    licens_panel %>% 
    filter(!is.na(LVL_4_CODE)) %>% 
    summarise(.by = c(LVL_4_CODE, LINK_YEAR), 
              N_LICBUY_IN_DPT_YEAR = length(na.omit(unique(LICENSED_BUYER_ID)))) %>% 
    summarise(.by = LVL_4_CODE, 
              AVG_N_LICBUY_IN_DPT = mean(N_LICBUY_IN_DPT_YEAR)) %>% 
    arrange(desc(AVG_N_LICBUY_IN_DPT)))

(sum(n_licbuy_dpt$AVG_N_LICBUY_IN_DPT)) 

potential_all = 
  potential_all %>% 
  left_join(n_licbuy_dpt %>% select(CELL_AVG_N_LICBUY_IN_DPT = AVG_N_LICBUY_IN_DPT, # this is a cell level var
                                   LVL_4_CODE), 
            by = join_by(CELL_DISTRICT_GEOCODE == LVL_4_CODE))


## Coop/BS-level variables ---------------------------

### IC2B variables -------------

# We match these variabes at buying station level, but they vary at coop level anyway. 

# Make one-hot-encoded variables 
coopbs_ohe = 
  coopbs  %>% 
  rowwise() %>% 
  mutate(
    # for RFA, UTZ & FT, they are already produced in private_IC2B.R post-prod section.
    COOP_SSI_CARGILL = grepl("CARGILL", CERTIFICATIONS),
    COOP_SSI_BARRY = grepl("BARRY CALLEBAUT", CERTIFICATIONS),
    COOP_SSI_OLAM = grepl("OLAM", CERTIFICATIONS),
    COOP_SSI_ECOM = grepl("ECOM", CERTIFICATIONS),
    COOP_SSI_NESTLE = grepl("NESTLE", CERTIFICATIONS),
    COOP_SSI_MONDELEZ = grepl("MONDELEZ", CERTIFICATIONS),
    COOP_SSI_BLOMMER = grepl("BLOMMER", CERTIFICATIONS),
    COOP_SSI_CEMOI = grepl("CEMOI", CERTIFICATIONS),
    COOP_SSI_HERSHEY = grepl("HERSHEY", CERTIFICATIONS),
    COOP_SSI_MARS = grepl("MARS", CERTIFICATIONS),
    COOP_SSI_SUCDEN = grepl("SUCDEN", CERTIFICATIONS),
    COOP_SSI_PURATOS = grepl("PURATOS", CERTIFICATIONS),
    COOP_SSI_OTHER = grepl("OTHER", CERTIFICATIONS)
  ) %>% 
  mutate(tmp_value = TRUE) %>% 
  pivot_wider(names_from = DISTRICT_NAME, 
              names_prefix = "COOP_DISTRICT_",
              values_from = tmp_value,  
              values_fill = FALSE) %>% 
  mutate(tmp_value = TRUE) %>% 
  pivot_wider(names_from = COOP_STATUS, 
              names_prefix = "COOP_STATUS_",
              values_from = tmp_value,  
              values_fill = FALSE) %>% 
  select(-COOP_STATUS_NA) %>% 
  rename(COOP_FARMERS = TOTAL_FARMERS,
         COOP_ABRVNAME = SUPPLIER_ABRVNAME, 
         COOP_FULLNAME = SUPPLIER_FULLNAME, 
         COOP_BUYERS = TRADER_NAMES, 
         COOP_CERTIFICATIONS = CERTIFICATIONS) %>% 
  select(starts_with("COOP_"))
  
stopifnot(nrow(coopbs)==nrow(coopbs_ohe))

# names(coopbs_ohe)
# coopbs %>% 
#   filter(!is.na(BUYER_LONGITUDE)) %>% 
#   pull(TOTAL_FARMERS) %>% summary()

potential_all = 
  potential_all %>% 
  left_join(coopbs_ohe, 
            by = join_by(LINK_POTENTIAL_COOP_BS_ID == COOP_BS_ID))


### Terrain in BS buffers -------------
# Extract directly here in R
dir_tri = here("temp_data", "terrain", "tri_coopbs_10km_buffer.geojson")

if(!file.exists(dir_tri)){
  # This takes ~15min
  tri_coopbs_10km_buffer = 
    exact_extract(x = tri, 
                  y = coopbs_10km_buffer, 
                  append_cols = c("COOP_BS_ID"),
                  fun = "weighted_mean", 
                  weights = tri_area, 
                  progress = TRUE)
  
  st_write(tri_coopbs_10km_buffer, dir_tri)
  
}
tri_coopbs_10km_buffer = st_read(dir_tri)

tri_coopbs_10km_buffer = 
  tri_coopbs_10km_buffer %>% 
  rename(COOP_BS_10KM_TRI = weighted_mean) %>% 
  st_drop_geometry()

potential_all = 
  potential_all %>% 
  left_join(tri_coopbs_10km_buffer, 
            by = join_by(LINK_POTENTIAL_COOP_BS_ID == COOP_BS_ID))

# This would have been the workflow with GEE
# tri = rast(here("input_data/terrain/tri/tri.txt"))
# writeRaster(tri, here("temp_data", "terrain", "tri.tif"))
# tri_coopbs_10km_buffer = 
#   read.csv(here("input_data/GEE/tri_coopbs_10km_buffer.csv")) %>% 
#   as_tibble() %>%   
#   select(COOP_BS_ID, 
#          COOP_BS_10KM_TRI = tri)


### Land use in BS buffers -------------
# For land use, it was easier to do it in GEE.  
coopbs_10km_buffer_lu = 
  read.csv(here("input_data/GEE/bnetd_coopbs_10km_buffer.csv")) %>% 
  as_tibble() %>%   
  select(COOP_BS_ID, 
         COOP_BS_10KM_COCOA_HA = cocoa, 
         COOP_BS_10KM_SETTLEMENT_HA = settlements)

potential_all = 
  potential_all %>% 
  left_join(coopbs_10km_buffer_lu, 
            by = join_by(LINK_POTENTIAL_COOP_BS_ID == COOP_BS_ID))

names(potential_all)

if(init_nrow_pa != nrow(potential_all)){stop("adding variables also added rows")}


## Link-level variables ----------------

### Compute distances -------------
# Between every buyer location and either the producer location when available, or the cell centroid otherwise.

# Prepare the two kinds:

# Those with exact producer location
potential_obsed =
  potential_all %>% 
  filter(!is.na(PRO_LONGITUDE)) %>% 
  mutate(PROEXT_LONGITUDE = PRO_LONGITUDE,
         PROEXT_LATITUDE  = PRO_LATITUDE)

# Those without
grid_ctoid_coords = 
  grid_ctoid %>% 
  st_transform(crs = 4326) 

grid_ctoid_coords$PROEXT_LONGITUDE = st_coordinates(grid_ctoid_coords)[,1]
grid_ctoid_coords$PROEXT_LATITUDE = st_coordinates(grid_ctoid_coords)[,2]
grid_ctoid_coords = grid_ctoid_coords %>% select(CELL_ID, PROEXT_LONGITUDE, PROEXT_LATITUDE) %>% st_drop_geometry()
grid_ctoid_coords$PROEXT_LATITUDE %>% summary()

potential_notobsed =
  potential_all %>% 
  filter(is.na(PRO_LONGITUDE)) %>% 
  left_join(
    grid_ctoid_coords, 
    by = "CELL_ID", 
  ) %>% 
  select(names(potential_obsed))

# bind them 
only_potential_ctoid = 
  rbind(
    potential_obsed, 
    potential_notobsed
  ) %>% 
  st_as_sf(coords = c("PROEXT_LONGITUDE", "PROEXT_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) %>% 
  # bc of that filter, it's not the full grid anymore, but only grid cells with at least one potential link with a coop
  filter(!CELL_NO_POTENTIAL_LINK) %>% # this includes actual links with other buyers than coops 
  
  # In addoition, remove the SC links with other buyers or unmatched coops bc they miss coordinates. 
  # but leave links between JRC producers and their geolocated other buyers. 
  filter(!(grepl("SUSTAINCOCOA", PRO_ID) & is.na(LINK_ACTUAL_COOP_BS_ID))) 

# plot(st_geometry(only_potential_ctoid)) don't plot, its too heavy at 3km cells

if(only_potential_ctoid %>% filter(st_is_empty(geometry)) %>% nrow() > 0 | 
   anyNA(only_potential_ctoid$BUYER_LONGITUDE)){stop("only_potential_ctoid does not have the expected spatial attributes at this stage")}

only_potential_ctoid %>% filter(is.na(BUYER_LONGITUDE)) %>% View()
# these are the 415 other/non-IC2B from SC.

rm(potential_obsed, potential_notobsed)
# st_distance needs to work on same size df. 
potential_itmpt = 
  only_potential_ctoid %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("BUYER_LONGITUDE", "BUYER_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 

only_potential_ctoid$LINK_DISTANCE_METERS <- 
  st_distance(only_potential_ctoid, potential_itmpt, by_element = TRUE) %>% as.numeric()

rm(potential_itmpt)

# merge back to the full grid
(init_nrow <- nrow(potential_all))
potential_all = 
  potential_all %>% 
  left_join(only_potential_ctoid %>% 
              select(CELL_ID, LINK_ID_COOPS, LINK_ID_OTHERS, LINK_POTENTIAL_COOP_BS_ID, LINK_DISTANCE_METERS) %>% 
              st_drop_geometry(), 
            # (join by LINK_ID_OTHERS too, bc these links are also in only_potential_ctoid)
            by = c("CELL_ID", "LINK_ID_COOPS", "LINK_ID_OTHERS", "LINK_POTENTIAL_COOP_BS_ID"))

if(init_nrow != nrow(potential_all)){stop("rows added unexpectedly")}

rm(only_potential_ctoid)

# In actual links, gauge the error of computing distance from coop to cell center vs to farm center. 
# Should only be 0 now 
potential_all %>% 
  mutate(DIFF_DIST_TO_CELL_VS_FARM = case_when(
    LINK_IS_ACTUAL ~ LINK_DISTANCE_METERS - LINK_ACTUALONLY_DISTANCE_METERS,
    TRUE ~ NA
  )) %>% 
  pull(DIFF_DIST_TO_CELL_VS_FARM) %>% summary()


# EXPORT -------
# Export here the data for the second stage, to then work from a lighter object
potential_all = 
  potential_all %>% 
  select(starts_with("LINK_"),
         starts_with("CELL_"), 
         starts_with("PRO_"), 
         starts_with("COOP_"), 
         starts_with("BUYER_"))

names(potential_all)

saveRDS(potential_all, 
        here("temp_data", "prepared_main_dataset", paste0("prepared_main_dataset_", grid_size_m*1e-3, "km.Rdata")))


# 1ST STAGE DATA --------------

potential_all_fewer_vars = 
  potential_all %>% 
  select(starts_with("CELL_", ), 
         starts_with("LINK_", ), 
         starts_with("COOP_", ), 
         !starts_with("COOP_DISTRICT_")) # this cannot be aggregated - we could do it for cell however... 

## Aggregate to cell-level ------------

# Now that all that was possibly measured at link level has been measured, 
# for both actual links with coops and with other buyers, we aggregate these
# measurements at the level of one link with each buyer type. 

# * Hence, we upscale the observation to the level of the cell. *

# Re-adjust BUYER_IS_COOP to be true for all potential links
potential_all = 
  potential_all %>% 
  mutate(BUYER_IS_COOP = case_when(
    is.na(BUYER_IS_COOP) & LINK_IS_POTENTIAL ~ TRUE, 
    TRUE ~ BUYER_
  ))

# the only rows where BUYER_IS_COOP is NA are in cells with no potential link
potential_all$BUYER_IS_COOP %>% summary()
potential_all %>% filter(is.na(BUYER_IS_COOP)) %>% pull(CELL_NO_POTENTIAL_LINK) %>% summary()

cell_fstg =
  potential_all %>%
  mutate(LINK_VOLUME_KG_COOPS = LINK_VOLUME_KG*BUYER_IS_COOP)
#   # first, sum up individual link volumes within buyer type (coop, other or NA)
#   group_by(CELL_ID, BUYER_IS_COOP) %>% 
#   mutate(CELL_TYPE_OF_BUYER_VOLUME_KG = sum(LINK_VOLUME_KG, na.rm = TRUE) 
#   summarise(.by = c(CELL_ID),
#             # since there's volumes data for actual links only, we can just remove NAs in sums
#             CELL_TYPE_OF_BUYER_VOLUME_KG = sum()
#             
#             )
### Topological vars ------------


### Average IC2B vars





# PROPORTIONAL VOLUMES -------
# Make the outcome variable, i.e. the proportion of flows to coops rel to flows with other buyers
hhs_links_all_tmp = 
  hhs_links_all %>% 
  # first, sum up individual link volumes within buyer type (coop, other or NA)
  group_by(VILLAGE_SURVEY_NAME, BUYER_IS_COOP) %>% 
  mutate(VILLAGE_TYPE_OF_BUYER_VOLUME_KG = case_when(
    !is.na(BUYER_IS_COOP) ~ sum(BUYER_VOLUME_KG, na.rm = TRUE), 
    TRUE ~ NA)
  ) %>% 
  # then, sum up links to coops + links to other (but exclude NAs)
  group_by(VILLAGE_SURVEY_NAME) %>% 
  mutate(
    VILLAGE_COOP_OR_OTHER_BUYER_VOLUME_KG = sum(VILLAGE_TYPE_OF_BUYER_VOLUME_KG, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # finally, divide volumes to either coop or other by volumes to both types 
  mutate(
    LINK_VILLAGE_TYPE_OF_BUYER_REL_SIZE = VILLAGE_TYPE_OF_BUYER_VOLUME_KG / VILLAGE_COOP_OR_OTHER_BUYER_VOLUME_KG) 

hhs_links_all_tmp$BUYER_IS_COOP %>% summary()
hhs_links_all_tmp$VILLAGE_TYPE_OF_BUYER_VOLUME_KG %>% summary()
hhs_links_all_tmp$LINK_VILLAGE_TYPE_OF_BUYER_REL_SIZE %>% summary()



# OLD CODE BITS ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


# Switch geometry from the cell centroid to the polygon. - This takes ~15 minutes
# grid_potential_poly =
#   grid_actual %>%
#   arrange(CELL_ID) %>% # because this may speed up 
#   select(CELL_ID) %>%
#   full_join(
#     y = potential %>% 
#           st_drop_geometry() %>%
#           arrange(CELL_ID),
#     by = "CELL_ID")
# if(nrow(grid_potential_poly) != nrow(potential)){stop("the join did not work as expected")}


# Use terra (rather than stars), because apparently stars::st_rasterize does not 
# allow one to summarize the values of several points falling in the same grid cell. 
# https://gis.stackexchange.com/questions/432367/how-to-apply-mean-min-max-merge-functions-on-a-vector-layer-to-create-a-stars-ra

# the issue is that terra does not handle character values. 

# # Make spatial links
# consol_sr = 
#   consol_IC2Bcoops %>% 
#   vect(geom = c("PRO_LONGITUDE", "PRO_LATITUDE"), crs = "epsg:4326", keepgeom = TRUE) %>% 
#   project(paste0("epsg:", civ_crs))
# 
# allcel_tr = rasterize(consol_sr, tmplt_sr, field = c("PRO_ID", "COOP_BS_ID", "LINK_ACTUALONLY_DISTANCE_METERS"), fun = function(x){length(unique(x))}) # by = names(consol_sr), 
# # function(x){factor(paste0(na.omit(unique(x)), collapse = ";"))}
# rasterize(consol_sr, tmplt_sr, fun = "length") %>% values() %>% summary()
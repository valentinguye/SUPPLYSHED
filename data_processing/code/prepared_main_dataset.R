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

# Assets, parameters & ext. functions -----------------------------------------------------
# use the projected CRS used by BNETD for their 2020 land use map. 
civ_crs <- 32630

# Set this to 30000 to run a toy model, for tests etc.
grid_size_m = 4000

# load in particular the function fn_trader_to_group_names, str_trans, ... 
source(here("code", "USEFUL_STUFF_supplyshedproj.R"))


# Data set of the universe of cooperatives (private version of https://dataverse.uclouvain.be/dataset.xhtml?persistentId=doi:10.14428/DVN/YBWJOR) 
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
  mutate(PRO_VILLAGE_NAME = NA)
  

# JRC link data
jrc_links = read.csv(here("temp_data", "preprocessed_jrc_data", "jrc_links_standardized.csv")) %>% 
  select(!starts_with("VILLAGE_L"))

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
sc_links = read.csv(here("temp_data", "preprocessed_sustain_cocoa", "sustain_cocoa_hh_links_standardized.csv"))

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
departements <- read_sf(here("input_data/s3/CIV_DEPARTEMENTS.geojson"))
init_crs = st_crs(departements)
departements = 
  st_transform(departements, crs = civ_crs)
end_crs = st_crs(departements)

civ_boundary = ne_countries(scale = 10, country = "Ivory Coast", returnclass = "sf")
ggplot() +
  geom_sf(data = civ_boundary, fill = "transparent") +
  geom_sf(data = departements, fill = "transparent") 


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
# In hectares of 11 LU classes, aggregated in GEE to 1km
bnetd = rast(here("input_data/GEE/BNETD_binary_1km.tif"))
# bnetd %>% values() %>% summary()

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
  "PRO_VILLAGE_NAME" = NA,
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

## Export -------
# For descriptions in next scripts
saveRDS(consol, here("temp_data", "actual_links_consolidated.Rdata"))

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

consol %>% filter(!is.na(COOP_BS_ID) & !BUYER_IS_COOP) %>% nrow()

# Make an buyer ID for non IC2B buyers (others than coop or SC coop not matched to IC2B)
consol_other = 
  consol_other %>% 
  group_by(BUYER_IS_COOP, BUYER_LONGITUDE, BUYER_LATITUDE) %>% 
  mutate(BUYER_NONIC2B_ID = paste0("NONIC2B-", cur_group_id())) %>% 
  ungroup()

length(unique(consol_other$BUYER_NONIC2B_ID))

# Actual link stats ----------------

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

# USE THE MAX BECAUSE WE WILL UNDERSAMPLE VIRTUAL LINKS LATER ON, EXPLICITLY RANDOMLY, IN THE MACHINE LEARNING MODEL. 
dist_meters_threshold <- dist_meters_max

summary(consol_IC2Bcoops$LINK_ACTUALONLY_DISTANCE_METERS)
sd(consol_IC2Bcoops$LINK_ACTUALONLY_DISTANCE_METERS[consol_IC2Bcoops$LINK_ACTUALONLY_DISTANCE_METERS], na.rm = TRUE)



# Prepare coop location/buffer ---------------
coopbsy$LINK_YEAR %>% summary()

coopbs = 
  coopbsy %>% 
  filter(LINK_YEAR == latest_survey_year) %>% 
  # also remove that one isolated coop very north
  filter(BUYER_LATITUDE < 9 | is.na(BUYER_LATITUDE))
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
# Old remarks: 
  # 12km is because 75% of cocoa producers surveyed by the JRC have their cocoa plots less than 6km away from their house.
  # The average location of the producer in a grid cell is in it's centroid. Taking 6km away in any direction from the center implies 12km. 
  # At the same time, in Cargill data, 95% of the plots of the same farmer fit in bounding boxes of 25 hectares (0.5 x 0.5 km) or less.

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

# First aggregate from 1km to grid_size_m (4km in current default)
bnetd_aggr = 
  terra::aggregate(x = bnetd, 
                 fact = grid_size_m / res(bnetd), 
                 fun = "sum")

bnetd_resample_filename = paste0("bnetd_modelgrid_", grid_size_m*1e-3,"km.tif")

terra::resample(x = bnetd_aggr, 
                  y = grid_sr,
                  method = "sum", # sum bc values in ha
                  threads = FALSE, # not necessary
                  filename = here("temp_data", "BNETD", bnetd_resample_filename), 
                  overwrite = TRUE)

grid_lu = rast(here("temp_data", "BNETD", bnetd_resample_filename))

plot(grid_lu$cocoa)
plot(st_geometry(departements), add = TRUE)

plot(grid_lu$denseForest)
plot(st_geometry(departements), add = TRUE)

plot(grid_lu$impossible)
plot(st_geometry(departements), add = TRUE)


### Suitability --------------
# DON'T USE IT, BECAUSE IT SEEMS INACCURATE
# GAEZ seems to predict no suitability in places where there actually is cocoa (NE). See plot below. 

# gaez_cocoa_path = here("temp_data", "GAEZ_v4", "AES_index_value", "Rain-fed", "Low-input", "Cocoa")
# gaez_cocoa = rast(paste0(gaez_cocoa_path, "/cocoa.tif"))
# 
# grid_extent_gaezcrs = project(grid_extent, from = paste0("epsg:",civ_crs), to = crs(gaez_cocoa))
# 
# gaez_cocoa_civ = 
#   gaez_cocoa %>% 
#   crop(grid_extent_gaezcrs)
# 
# # Project 
# gaez_cocoa_project_path = paste0(gaez_cocoa_path, "/gaez_cocoa_modelgrid_", grid_size_m*1e-3,"km.tif")
# terra::project(gaez_cocoa_civ, 
#                 grid_sr,
#                 align = FALSE, 
#                 filename = gaez_cocoa_project_path, 
#                 overwrite = TRUE)
# 
# grid_gaez_cocoa = rast(gaez_cocoa_project_path)
# 
# plot(grid_gaez_cocoa)
# plot(st_geometry(departements), add = TRUE)



### Convert all LUs to stars --------

# grid_st = st_as_stars(grid_sr)
grid_st = 
  c(grid_lu, grid_tri) %>% #, grid_gaez_cocoa
  st_as_stars(ignore_file = TRUE, 
              as_attributes = TRUE) %>% 
  rename(CELL_DENSEFOREST_HA = denseForest,
         CELL_OTHERFORESTS_HA = otherForests,
         CELL_COCOA_HA = cocoa,
         CELL_COFFEE_HA = coffee,
         CELL_RUBBER_HA = rubber,
         CELL_PALM_HA = palm,
         CELL_COCONUT_HA = coconut,
         CELL_CASHEW_HA = cashew,
         CELL_OTHERAG_HA = otherAg,
         CELL_SETTLEMENT_HA = settlements,
         CELL_IMPOSSIBLE_HA = impossible,
         CELL_TRI_MM = tri) # tri is in millimeters in Nunn & Puga data. 
       # CELL_GAEZCOCOA_AESI = Cocoa

  

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
   select(CELL_ID, 
          CELL_DENSEFOREST_HA,
          CELL_OTHERFORESTS_HA,
          CELL_COCOA_HA,
          CELL_COFFEE_HA,
          CELL_RUBBER_HA,
          CELL_PALM_HA,
          CELL_COCONUT_HA,
          CELL_CASHEW_HA,
          CELL_OTHERAG_HA,
          CELL_SETTLEMENT_HA,
          CELL_IMPOSSIBLE_HA,
          CELL_TRI_MM)
        # CELL_GAEZCOCOA_AESI
  )

# and the same object, but with cell centroid geometry
grid_ctoid = 
  grid_poly %>% 
  mutate(geometry = st_centroid(geometry))

# add the geodesic coordinates of centroids, for later plots, to BOTH grid_poly and grid_ctoid (necessary)
grid_ctoid = 
  bind_cols(
    grid_ctoid, 
    grid_ctoid %>% 
      st_transform(crs = 4326) %>% 
      st_coordinates() %>% 
      as.data.frame() %>% 
      rename(CELL_LONGITUDE = X, 
             CELL_LATITUDE = Y)
  )
grid_poly = 
  bind_cols(
    grid_poly, 
    grid_ctoid %>% 
      st_transform(crs = 4326) %>% 
      st_coordinates() %>% 
      as.data.frame() %>% 
      rename(CELL_LONGITUDE = X, 
             CELL_LATITUDE = Y)
  )

# MATCH TO CONSOLIDATED ACTUAL LINKS
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

nrow(grid_poly) # out of the 21353 4km grid cells, 
actual$CELL_ID %>% unique() %>% length() # 407 4km cells have at least one actual link.  

# actual %>% filter(!is.na(LINK_VOLUME_KG)) %>% View()

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
    PRO_VILLAGE_NAME = NA,
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


## Remove links from cells outside CIV ----------------------- 
# Identify cells outside 
grid_ctoid$IS_TERRITORIAL =     
  grid_ctoid %>% 
  st_transform(crs = st_crs(civ_boundary)) %>% 
  st_intersects(
    y = civ_boundary,
  ) %>% 
  lengths()

# Remove links in those cells
potential_all = 
  potential_all %>% 
  left_join(
    y = grid_ctoid %>% select(CELL_ID, IS_TERRITORIAL), 
    by = "CELL_ID"
  ) %>% 
  filter(IS_TERRITORIAL == 1) %>% 
  select(-IS_TERRITORIAL)

potential_all$CELL_ID %>% unique() %>% length()
nrow(potential_all)


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
      is.na(BUYER_IS_COOP) & (LINK_IS_ACTUAL_COOP | LINK_IS_VIRTUAL) ~ TRUE, 
      TRUE ~ BUYER_IS_COOP
    )) %>% 
    # based on this, characterize cells
  group_by(CELL_ID) %>% 
  mutate(
    CELL_NO_POTENTIAL_LINK = !any(LINK_IS_POTENTIAL),
    CELL_ONLY_VIRTUAL_LINK = any(LINK_IS_POTENTIAL) & !any(LINK_IS_ACTUAL),
    # This is the variable to use to isolate train/test data
    CELL_ANY_ACTUAL_COOP_LINK = any(LINK_IS_ACTUAL_COOP),
    # below breakdown is just for descriptions
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
  # rowwise() %>%
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

## Buyer ID ---------------
# No obs. should have a non-IC2B buyer ID and a COOP BS ID.  
stopifnot(potential_all %>% filter(!is.na(LINK_POTENTIAL_COOP_BS_ID) & !is.na(BUYER_NONIC2B_ID)) %>% nrow() == 0)
# Those with NAs in both are in the part of the country with no potential link 
# potential_all %>% filter(is.na(LINK_POTENTIAL_COOP_BS_ID)) %>% pull(BUYER_NONIC2B_ID) %>% is.na() %>% any())

potential_all = 
  potential_all %>% 
  mutate(BUYER_ID = case_when(
    is.na(LINK_POTENTIAL_COOP_BS_ID) ~ BUYER_NONIC2B_ID, 
    TRUE ~ LINK_POTENTIAL_COOP_BS_ID
  ))
potential_all$BUYER_ID %>% unique() %>% length()

# ADD VARIABLES ----------------

potential_all_save = potential_all

init_nrow_pa = nrow(potential_all) # # just to check that this section does not add any row




## Link-level variables ----------------

### Link  ID ------------------
# Make a full link id for convenience
potential_all = 
  potential_all %>% 
  group_by(CELL_ID, LINK_ID_COOPS, LINK_ID_OTHERS, LINK_ACTUAL_COOP_BS_ID, LINK_POTENTIAL_COOP_BS_ID) %>% 
  mutate(LINK_ID = cur_group_id()) %>% 
  ungroup()
stopifnot(nrow(potential_all) == length(unique(potential_all$LINK_ID)))


### Euclidean distances -------------
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
only_potential_propt = 
  rbind(
    potential_obsed, 
    potential_notobsed
  ) %>% 
  st_as_sf(coords = c("PROEXT_LONGITUDE", "PROEXT_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) %>% 
  # bc of that filter, it's not the full grid anymore, but only grid cells with at least one potential link with a coop
  filter(!CELL_NO_POTENTIAL_LINK) %>% # this includes actual links with other buyers than coops 
  
  # In addition, remove the SC links with other buyers or unmatched coops bc they miss coordinates. 
  # but leave links between JRC producers and their geolocated other buyers. 
  filter(!(grepl("SUSTAINCOCOA", PRO_ID) & is.na(LINK_ACTUAL_COOP_BS_ID))) 

# plot(st_geometry(only_potential_propt)) don't plot, its too heavy at 3km cells

if(only_potential_propt %>% filter(st_is_empty(geometry)) %>% nrow() > 0 | 
   anyNA(only_potential_propt$BUYER_LONGITUDE)){stop("only_potential_propt does not have the expected spatial attributes at this stage")}

only_potential_propt %>% filter(is.na(BUYER_LONGITUDE)) %>% View()
# these are the 415 other/non-IC2B from SC.

rm(potential_obsed, potential_notobsed)
# st_distance needs to work on same size df. 
only_potential_itmpt = 
  only_potential_propt %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("BUYER_LONGITUDE", "BUYER_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 

only_potential_propt$LINK_DISTANCE_METERS <- 
  st_distance(only_potential_propt, only_potential_itmpt, by_element = TRUE) %>% as.numeric()

# merge back to the full grid
(init_nrow <- nrow(potential_all))
potential_all = 
  potential_all %>% 
  left_join(only_potential_propt %>% 
              select(CELL_ID, LINK_ID_COOPS, LINK_ID_OTHERS, LINK_POTENTIAL_COOP_BS_ID, LINK_DISTANCE_METERS) %>% 
              st_drop_geometry(), 
            # (join by LINK_ID_OTHERS too, bc these links are also in only_potential_propt)
            by = c("CELL_ID", "LINK_ID_COOPS", "LINK_ID_OTHERS", "LINK_POTENTIAL_COOP_BS_ID"))

if(init_nrow != nrow(potential_all)){stop("rows added unexpectedly")}

# In actual links, gauge the error of computing distance from coop to cell center vs to farm center. 
# Should only be 0 now 
stopifnot(
  potential_all %>% 
    mutate(DIFF_DIST_TO_CELL_VS_FARM = case_when(
      LINK_IS_ACTUAL ~ LINK_DISTANCE_METERS - LINK_ACTUALONLY_DISTANCE_METERS,
      TRUE ~ NA
    )) %>% 
    pull(DIFF_DIST_TO_CELL_VS_FARM) %>% round(1) %>% na.omit() %>% unique() == 0
)

# tmptoplot = only_potential_propt %>% filter(!duplicated(CELL_ID))
# tmptoplot_itm = only_potential_itmpt %>% filter(!duplicated(BUYER_LONGITUDE, BUYER_LATITUDE))
# 
# ggplot() +
#   theme_bw() +
#   geom_sf(data = tmptoplot, , size = 0.05, 
#              col = "red") +
#   geom_sf(data = tmptoplot_itm, , size = 1, 
#           col = "blue") +
#     # geom_sf(data = departements, fill = "transparent") +
#     geom_sf(data = civ_boundary, fill = "transparent") 

rm(only_potential_itmpt)


### Road travel duration and distance -------------
only_potential_coords = 
  only_potential_propt %>% 
  select(CELL_ID, LINK_ID, LINK_ID_COOPS, LINK_ID_OTHERS, LINK_POTENTIAL_COOP_BS_ID, LINK_DISTANCE_METERS,
         PROEXT_LONGITUDE, PROEXT_LATITUDE, BUYER_LONGITUDE, BUYER_LATITUDE,
  ) %>% 
  st_drop_geometry() 


# only_potential_coords %>% 
#   filter(duplicated(PROEXT_LATITUDE, PROEXT_LONGITUDE, BUYER_LONGITUDE, BUYER_LATITUDE))

# write.csv(only_potential_coords, here("temp_data", paste0("potential_links_",grid_size_m*1e-3,"km_coords.csv")))

rm(only_potential_propt, only_potential_coords)

travel_times = read.csv(here("input_data", "IIASA", "complete.csv")) %>% 
  # Turn distance from kilometer into meter
  mutate(distance = distance*1000)

potential_all = 
  potential_all %>% 
  left_join(travel_times %>% select(LINK_ID, LINK_TRAVEL_MINUTES = duration, LINK_TRAVEL_METERS = distance), 
            by = "LINK_ID")

# The unmatched links are those with no buyer coordinates in SC data, and those with no potential link at all. 
potential_all %>% filter(is.na(LINK_TRAVEL_MINUTES)) %>% View()

rm(travel_times)

## Cell-level topological variables ------------------

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
            by = join_by(CELL_DISTRICT_GEOCODE == DISTRICT_GEOCODE)) %>% 
  # in departments where there are no IC2B coops, the left_join gives NAs, 
  # --> replace by 0. 
  mutate(CELL_N_COOP_IN_DPT = if_else(is.na(CELL_N_COOP_IN_DPT), 
                                            0, 
                                      CELL_N_COOP_IN_DPT))




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
(mean_n_dpt_licbuy = mean(n_licbuy_dpt$AVG_N_LICBUY_IN_DPT))
(median_n_dpt_licbuy = median(n_licbuy_dpt$AVG_N_LICBUY_IN_DPT))

potential_all = 
  potential_all %>% 
  left_join(n_licbuy_dpt %>% select(CELL_AVG_N_LICBUY_IN_DPT = AVG_N_LICBUY_IN_DPT, # this is a cell level var
                                   LVL_4_CODE), 
            by = join_by(CELL_DISTRICT_GEOCODE == LVL_4_CODE)) %>% 
  # in departments where no licensed buyers are known, the left_join gives NAs, 
  # but replace by 0. 
  mutate(CELL_AVG_N_LICBUY_IN_DPT = if_else(is.na(CELL_AVG_N_LICBUY_IN_DPT), 
                                            0, 
                                            CELL_AVG_N_LICBUY_IN_DPT))

### Nearest coop(s) -------------- 
# NOW WE CONSIDER NEAREST BY ROAD TRAVEL DISTANCE, NOT EUCLIDEAN. 
potential_all = 
  potential_all %>% 
  group_by(CELL_ID) %>% 
  arrange(LINK_TRAVEL_METERS) %>% # this matters
  mutate(
    # do avg distance of the 5 closest BS, rather than avg of the five smallest distances which may 
    # well be to the same coop in the many cases where several actual links would exist with it. 
    # LINK_DISTANCE_5TH_NEAREST_POTENTIAL_COOP = head(sort(LINK_TRAVEL_METERS))[5]) %>% 
    # na.omit because there may be NAs if in the smallest distances there is a JRC other buyer. 
    LINK_5_NEAREST_POTENTIAL_COOP_BS_ID = list(head(na.omit(unique(LINK_POTENTIAL_COOP_BS_ID)), 5)), 
    LINK_1_NEAREST_POTENTIAL_COOP_BS_ID = list(head(na.omit(unique(LINK_POTENTIAL_COOP_BS_ID)), 1))
    # CELL_MIN_DISTANCE_METERS = min(LINK_TRAVEL_METERS, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    #LINK_IS_WITH_5_NEAREST_COOPS = LINK_TRAVEL_METERS <= LINK_DISTANCE_5TH_NEAREST_POTENTIAL_COOP
    LINK_IS_WITH_5_NEAREST_COOPS = LINK_POTENTIAL_COOP_BS_ID %in% LINK_5_NEAREST_POTENTIAL_COOP_BS_ID, 
    LINK_IS_WITH_1_NEAREST_COOPS = LINK_POTENTIAL_COOP_BS_ID %in% LINK_1_NEAREST_POTENTIAL_COOP_BS_ID
    #LINK_IS_WITH_1_NEAREST_COOPS = LINK_TRAVEL_METERS == CELL_MIN_DISTANCE_METERS
  ) %>% 
  ungroup() %>% 
  select(-LINK_5_NEAREST_POTENTIAL_COOP_BS_ID, 
         -LINK_1_NEAREST_POTENTIAL_COOP_BS_ID)

stopifnot(
  potential_all %>% 
    filter(LINK_IS_WITH_5_NEAREST_COOPS) %>% 
    summarise(.by = "CELL_ID", 
              TEST = length(unique(LINK_POTENTIAL_COOP_BS_ID))) %>% 
    pull(TEST) %>% unique() <= 5 # sometimes there are fewer than 5 potential coops
)


## Coop/BS-level variables ---------------------------
# CELL_ID, LINK_ID_COOPS, LINK_ID_OTHERS, LINK_ACTUAL_COOP_BS_ID, 


### IC2B variables -------------

#### Impute missing coop size -----------

anyNA(coopbs$TOTAL_FARMERS)
anyNA(coopbs$COOP_FARMERS_RFA)
anyNA(coopbs$COOP_FARMERS_FT)

(avg_coop_farmers = mean(coopbs$TOTAL_FARMERS, na.rm = TRUE) %>% round(0))

coopbs = 
  coopbs %>% 
  mutate(TOTAL_FARMERS = if_else(is.na(TOTAL_FARMERS) | TOTAL_FARMERS==0, 
                                 avg_coop_farmers, TOTAL_FARMERS))

summary(coopbs$TOTAL_FARMERS)

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
  ungroup() %>% 
  mutate(tmp_value = TRUE, 
         COOP_DISTRICT_NAME = DISTRICT_NAME) %>% # to be able to keep it, bc these non-ohe vars can be useful for summaries
  pivot_wider(names_from = DISTRICT_NAME, 
              names_prefix = "COOP_DISTRICT_",
              values_from = tmp_value,  
              values_fill = FALSE) %>% 
  select(-COOP_DISTRICT_NA) %>% 
  mutate(tmp_value = TRUE, 
         STATUS = COOP_STATUS) %>% 
  pivot_wider(names_from = STATUS, 
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

tri_coopbs_10km_buffer$COOP_BS_10KM_TRI %>% summary()

potential_all = 
  potential_all %>% 
  left_join(tri_coopbs_10km_buffer, 
            by = join_by(LINK_POTENTIAL_COOP_BS_ID == COOP_BS_ID))

## Impute missing TRI 
potential_all = 
  potential_all %>% 
  mutate(COOP_BS_10KM_TRI = if_else(is.na(COOP_BS_10KM_TRI), 
                                 mean(tri_coopbs_10km_buffer$COOP_BS_10KM_TRI), 
                                 COOP_BS_10KM_TRI))
stopifnot(!anyNA(potential_all$COOP_BS_10KM_TRI))

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
  read.csv(here("input_data/GEE/bnetd_allclasses_coopbs_10km_buffer.csv")) %>% 
  as_tibble() %>%   
  select(COOP_BS_ID, 
         COOP_BS_10KM_DENSEFOREST_HA = denseForest,
         COOP_BS_10KM_OTHERFORESTS_HA = otherForests,
         COOP_BS_10KM_COCOA_HA = cocoa,
         COOP_BS_10KM_COFFEE_HA = coffee,
         COOP_BS_10KM_RUBBER_HA = rubber,
         COOP_BS_10KM_PALM_HA = palm,
         COOP_BS_10KM_COCONUT_HA = coconut,
         COOP_BS_10KM_CASHEW_HA = cashew,
         COOP_BS_10KM_OTHERAG_HA = otherAg,
         COOP_BS_10KM_SETTLEMENT_HA = settlements,
         COOP_BS_10KM_IMPOSSIBLE_HA = impossible)

potential_all = 
  potential_all %>% 
  left_join(coopbs_10km_buffer_lu, 
            by = join_by(LINK_POTENTIAL_COOP_BS_ID == COOP_BS_ID))

names(potential_all)

if(init_nrow_pa != nrow(potential_all)){stop("adding variables also added rows")}





# SUB-SAMPLING ----------
set.seed(8888)

## SC coop links ------------------------

# As checked below if run without sub-sampling, and in SC pre-processing script, 
# the cell's share of cooperative outlet is over-represented in the data. 
# This is due to sampling biases towards farmers linked with cooperatives in SC data. 
# These sampling biases can be at village and at household levels. 
# We correct by sub-sampling at household link level. 

# So concretely, we want to remove some actual links between SC farmers and coops. 

sc_coop_links = 
  potential_all %>% 
  filter(grepl("SUSTAINCOCOA_", PRO_ID) & BUYER_IS_COOP) %>% 
  pull(LINK_ID) 

sc_coop_share = 
  sum(filter(sc_links, BUYER_IS_COOP)$LINK_VOLUME_KG, na.rm = T) /
  (sum(filter(sc_links, BUYER_IS_COOP)$LINK_VOLUME_KG, na.rm = T) +
     sum(filter(sc_links, !BUYER_IS_COOP)$LINK_VOLUME_KG, na.rm = T) ) 

sc_coop_share_inrows = 
  nrow(filter(sc_links, BUYER_IS_COOP)) / nrow(sc_links) 

seipcs_direct_share = 0.45
# this ratio is equal to 1 if coop sourcing = 50%
target_coop_to_other_ratio = seipcs_direct_share / (1-seipcs_direct_share) # based on SEI-PCS v1.0 (2019) which equates direct sourcing (45%) to coop sourcing.  
# but we use the share directly
target_share = seipcs_direct_share

# The sub-sample share is the share of obs. in a category we want to sample - i.e. to *KEEP* - to have a more balanced sample.  
# Here: the pct of sc-coop links  
# the lower the 'true' (target) coop sourcing share relative to the share in SC, the lower the sub-sample share, 
# i.e. the fewer sc-coop links we want to sample.   
(subsample_share = (target_share / sc_coop_share_inrows) * ((1 - sc_coop_share_inrows) / (1 - target_share)))
# Derived from Equation: SHARE_ss * N_sc / T'_sc = SHARE_target 
# where N_sc is the # of sc-coop links;
# T_sc and T'_sc are the total # of sc links resp. before and after sub-sampling; 
# with T'_sc = T_sc - N_sc(1-S_ss) (the correction implies the factor in the 2nd parenthesis)
# and N_sc / T_sc = SHARE_sc = sc_coop_share_inrows
# so the Equation means: 
# apply the sub-sample share S_ss to N_sc, such that, reported to the total after sub-sampling, we obtain the target share. 

sc_coop_links_tokeep = sample(sc_coop_links, size = round(subsample_share*length(sc_coop_links)))
length(sc_coop_links_tokeep)

# We can just FLAG, and not remove them, because sc-coop links are not going to cause 
# target imbalance in 2nd stage and they make valuable information, so we want to keep them. 
# US_SC = UnderSample SustainCocoa 
potential_all = 
  potential_all %>% 
  # be a link outside the category of interest, or in the list of links under-sampled. 
  mutate(LINK_TO_KEEP_TO_US_SC = (!LINK_ID %in% sc_coop_links) | 
                                   LINK_ID %in% sc_coop_links_tokeep)

stopifnot(nrow(potential_all) == nrow(filter(potential_all, LINK_TO_KEEP_TO_US_SC)) + round((1-subsample_share)*length(sc_coop_links)))

rm(subsample_share, sc_coop_links, sc_coop_links_tokeep, 
   target_share, seipcs_direct_share, target_coop_to_other_ratio, 
   sc_coop_share_inrows, sc_coop_share)




## Virtual links ------------------

### Remove Cargill false negatives -------------------

potential_all =
  potential_all %>% 
  group_by(CELL_ID) %>% 
  mutate(
    CELL_HAS_FALSENEG = any(grepl("CARGILL_", PRO_ID)) & !(any(grepl("JRC_|SUSTAINCOCOA_", PRO_ID)))) %>% 
  ungroup() %>% 
  mutate(LINK_POSSIBLE_FALSENEG = CELL_HAS_FALSENEG & LINK_IS_VIRTUAL)

if(potential_all %>% filter(CELL_HAS_FALSENEG) %>% pull(PRO_ID) %>% grepl(pattern = "JRC|SUSTAIN") %>% any()
){stop()}

# keep working on the same object actually, because when aggregating to cells for 1st stage, 
# we want to have all the virtual links from Cargill cells. The fact that they can 
# be false negatives is no problem in 1st stage, because these cells are in the predict set then. 
# potential_nofalneg = 
#   potential_all %>% 
#   filter(!LINK_POSSIBLE_FALSENEG) 

# Removes ~200-300 cells
potential_all %>% filter(CELL_HAS_FALSENEG) %>% pull(CELL_ID) %>% unique() %>% length()

# ~2-3% of the data is removed 
(nrow(filter(potential_all, !LINK_POSSIBLE_FALSENEG)) - nrow(potential_all))/nrow(potential_all)

# potential_all %>% filter(CELL_HAS_FALSENEG) %>% select(LINK_POSSIBLE_FALSENEG, everything()) %>% View()
# potential_all %>% filter(CELL_ID == 11657) %>% select(LINK_POSSIBLE_FALSENEG, everything()) %>% View()

# check that this does not cover whole cells (since it should only cover virtual links)
stopifnot(filter(potential_all, !LINK_POSSIBLE_FALSENEG) %>% 
            pull(CELL_ID) %>% unique() %>% length() == sum(grid_ctoid$IS_TERRITORIAL)
          )
potential_all$CELL_ID %>% unique() %>% length()

# This is virtual links removed from 264 cells: 
(cells_false_negs_1 = 
  potential_all %>% 
  filter(LINK_POSSIBLE_FALSENEG) %>% 
  pull(CELL_ID) %>% unique() %>% length())

### Remove JRC false negatives --------------
# These are in cells with isolated farmers that get separated from their village by the cell match. 
# + the few cases of villages with very few surveyed farmers 
# (not addressed in JRC script to be addressed here.)
# (Note that in the first stage, they get weights proportional to representativity, 
# so we only address virtual links being false negatives in the second stage here.)
potential_all = 
  potential_all %>% 
  group_by(CELL_ID) %>% 
  mutate(
    CELL_N_JRC_FARMERS = length(na.omit(unique(grep(pattern = "JRC_", x = PRO_ID, value = TRUE))))
  ) %>% 
  ungroup()
potential_all %>% 
  summarise(.by = CELL_N_JRC_FARMERS, 
            N_CELLS = length(unique(CELL_ID))) %>% 
  arrange(CELL_N_JRC_FARMERS)

# Discard virtual links in cells with less than 4 farmers 
# (but not in cells with 0 JRC farmers, these are all the others, 
#  nor in cells where there could be a sustaincocoa village)
potential_all =
  potential_all %>% 
  group_by(CELL_ID) %>% 
  mutate(
    LINK_JRC_POSSIBLE_FALSENEG = 
      CELL_N_JRC_FARMERS %in% c(1:3) & LINK_IS_VIRTUAL & !(any(grepl("SUSTAINCOCOA_", PRO_ID))), 
  ) %>% 
  ungroup() %>% 
  # update the existing variable LINK_POSSIBLE_FALSENEG
  mutate(
    LINK_POSSIBLE_FALSENEG = case_when(
      LINK_JRC_POSSIBLE_FALSENEG ~ TRUE, 
      TRUE ~ LINK_POSSIBLE_FALSENEG
    )
  )

potential_all %>% filter(LINK_POSSIBLE_FALSENEG) %>% View()
potential_all %>% filter(CELL_ID == 385) %>% View()
# number of JRC villages in the data 
# potential_all %>% filter(grepl("JRC_", x = PRO_ID)) %>% pull(PRO_VILLAGE_NAME) %>% unique() %>% length()
# potential_all %>% filter(grepl("JRC_", x = PRO_ID) & LINK_POSSIBLE_FALSENEG) %>% pull(PRO_VILLAGE_NAME) %>% unique() %>% length()
# but that's not what matters here. We'd rather know how many cells get their virtual links removed
potential_all %>% filter(CELL_N_JRC_FARMERS %in% 1:3 & LINK_POSSIBLE_FALSENEG) %>%
  pull(CELL_ID) %>% unique() %>% length()

# check that no cell is completely removed (it shouldn't)
stopifnot(filter(potential_all, !LINK_POSSIBLE_FALSENEG) %>% 
            pull(CELL_ID) %>% unique() %>% length() == sum(grid_ctoid$IS_TERRITORIAL)
)

potential_all %>% 
  filter(
    LINK_JRC_POSSIBLE_FALSENEG
  ) %>% nrow()
potential_all %>% 
  filter(LINK_JRC_POSSIBLE_FALSENEG) %>% 
  pull(CELL_ID) %>% unique() %>% length()

potential_all = potential_all %>% select(-LINK_JRC_POSSIBLE_FALSENEG)

### Random sub-sampling --------------------

# Make separate object for convenience 
traintest_2dstg = 
  potential_all %>% 
  # Imbalance should be computed in the train/test set only. 
  filter(CELL_ACTUAL_LINK & !CELL_ACTUAL_ONLYOTHER_LINK) %>% 
  # Moreover, imbalance should be computed with all the possible false negatives removed. 
  filter(!LINK_POSSIBLE_FALSENEG)
# order in the above filters is inconsequential

# Compute the imbalance ratio in the data (N(N-1) - E)/E as in Mungo et al. 2023.
# "the number of pairs that do not have a link to the number of pairs that do have a link."
# but here, be N the number of cells, and replace 'N-1' by M, the number of cooperatives. 
(N_cells = traintest_2dstg$CELL_ID %>% unique() %>% length())
(M_coops = coopbs$COOP_BS_ID %>% unique() %>% length())
(E_links = traintest_2dstg$LINK_IS_ACTUAL_COOP %>% sum())
# this is the same as 
E_links == consol_IC2Bcoops %>% nrow()

(initial_imbalance = (N_cells*M_coops - E_links)/E_links)

# And the actual imbalance, i.e. now that we limited the number of virtual links to within 72km (the max observed distance in actual links). 
# i.e. the ratio of virtual to actual links
(N_actual = traintest_2dstg$LINK_IS_ACTUAL_COOP %>% sum())
(N_virtual = traintest_2dstg$LINK_IS_VIRTUAL %>% sum())
(current_imbalance = N_virtual / N_actual)

# This is a reduction by a factor of 
(round(initial_imbalance/current_imbalance))

# Compute it as a share 
(current_virtual_share = N_virtual / (N_actual + N_virtual))
# note it can be computed as 
current_imbalance/(current_imbalance + 1)

# and 
(current_actual_share = N_actual / (N_actual + N_virtual))
# (or)
1/(current_imbalance + 1)

# Now, there are three possible targets: 
  # - 1 do nothing because the imbalance is already limited by previous informed sub-sampling
if(initial_imbalance/4 > current_imbalance){
  print("It is not needed to further correct for class imbalance by random sub-sampling.")
  # just make the variable exist to avoid bugs.
  potential_all = 
    potential_all %>% 
    mutate(LINK_TO_KEEP_TO_US_VIRTUAL = TRUE)
}else{
  # - 2 the target of perfect balance 
  target_no_imbalance_share = .5
  # - 3 the target of a 4 times lower imbalance, as in Mungo et al. 2023 who choose an sub-sampling ratio 4 times lower than the data imbalance 
  (target_mungo_imbalance = current_imbalance/4)
  target_mungo_share = 1 / (target_mungo_imbalance + 1) # following above equivalence
  
  TARGET_SHARE_TO_USE = target_mungo_share
  
  # Apply the sub-sample share formula (see sc coop link sub-sampling above  for explanations)  
  (subsample_share = (TARGET_SHARE_TO_USE / current_virtual_share) * ((1 - current_virtual_share) / (1 - TARGET_SHARE_TO_USE)))
  
  # Sample in virtual links 
  virtual_links = 
    # important to sample only in virtual links of the train-test set 
    traintest_2dstg %>% 
    filter(LINK_IS_VIRTUAL) %>% 
    pull(LINK_ID) 
  
  virtual_links_tokeep = sample(virtual_links, size = round(subsample_share*length(virtual_links)))
  length(virtual_links_tokeep)
  
  potential_all = 
    potential_all %>% 
    # be a link outside the category of interest, or in the list of links under-sampled. 
    mutate(LINK_TO_KEEP_TO_US_VIRTUAL = (!LINK_ID %in% virtual_links) | 
                                          LINK_ID %in% virtual_links_tokeep)
  
  stopifnot(nrow(potential_all) == nrow(filter(potential_all, LINK_TO_KEEP_TO_US_VIRTUAL)) + 
              round((1-subsample_share)*length(virtual_links)))
}

rm(traintest_2dstg)

# EXPORT -------
# Export here the data for the second stage, to then work from a lighter object
potential_all = 
  potential_all %>% 
  select(starts_with("PRO_"),
         starts_with("LINK_"),
         starts_with("CELL_"), 
         starts_with("COOP_"), 
         starts_with("BUYER_"))

names(potential_all)

saveRDS(potential_all, 
        here("temp_data", "prepared_main_dataset", paste0("cell_links_", grid_size_m*1e-3, "km.Rdata")))


rm(potential_all_save, potential)

# 1ST STAGE DATA --------------
potential_all = readRDS(here("temp_data", "prepared_main_dataset", paste0("cell_links_", grid_size_m*1e-3, "km.Rdata")))

## Apply sub-sampling -------------
# We apply only 1st stage sub-sampling. 
potential_1st = 
  potential_all %>% 
  filter(LINK_TO_KEEP_TO_US_SC)


# Prepare 
# there are cells where there are potential links, but they are with buyers that are not spatially explicit and thus distance is NA.
# it is important to remove these NAs so they don't make cell distance summaries NAs. 
# Hence, the warning thrown by min() that Inf is returned when no argument is found is expected, for all cells with only NAs. 
# but now we don't use min anymore anyway. 
potential_1st = 
  potential_1st %>% 
  select(!starts_with("COOP_DISTRICT_"), -COOP_STATUS)  # this cannot be aggregated - we could do it for cell however... 
  

## Dependent variable ------------

# Now that all that was possibly measured at link level has been measured, 
# for both actual links with coops and with other buyers, we aggregate these
# measurements at the level of one link with each buyer type. 

# * Hence, we upscale the observation to the level of the cell. *

# About link volumes: 
potential_1st %>% filter(!is.na(LINK_VOLUME_KG)) %>% nrow()
potential_1st$LINK_VOLUME_KG %>% summary()
potential_1st %>% filter(!is.na(LINK_VOLUME_KG)) %>% View()

potential_1st$BUYER_IS_COOP %>% summary()
potential_1st %>% filter(is.na(BUYER_IS_COOP)) %>% pull(CELL_NO_POTENTIAL_LINK) %>% summary()

# The if_else is because sum(., na.rm = T) returns 0 if . has only NAs. 
# This is the case of cells with no actual link from JRC or SC. 
# potential_1st %>% group_by(CELL_ID) %>% filter(all(is.na(LINK_VOLUME_KG))) %>% ungroup() %>% filter(!CELL_NO_ACTUAL_LINK) %>% pull(PRO_ID) %>% grepl(pattern = "SUSTAIN|JRC") %>% any()
# So if_else makes sure that in these cases, cell-level volumes are NAs and not 0
cell_depvars =
  potential_1st %>%
  summarise(.by = "CELL_ID",
            # This is the sum of the volumes of all actual links from a cell. 
            CELL_VOLUME_KG = if_else(all(is.na(LINK_VOLUME_KG)), NA, sum(LINK_VOLUME_KG, na.rm = TRUE)),
            
            # Now make the sum of the volumes to coops and to others specifically. 
            # The only rows where BUYER_IS_COOP is NA are in cells with no potential link 
            # (since adjustment in "Add virtual coop links" section)
            # so multiplying by BUYER_IS_COOP makes NAs in these cells, and 0 for actual links with other buyers than coops
            CELL_VOLUME_KG_COOPS  = if_else(all(is.na(LINK_VOLUME_KG)), NA, sum(LINK_VOLUME_KG*BUYER_IS_COOP, na.rm = TRUE)), 
            CELL_VOLUME_KG_OTHERS = if_else(all(is.na(LINK_VOLUME_KG)), NA, sum(LINK_VOLUME_KG*!BUYER_IS_COOP, na.rm = TRUE)) 
  ) %>% 
  mutate(
    # Split indicator, between train/test and predict sets in 1st stage: 
    CELL_VOLUME_OBSERVED = !is.na(CELL_VOLUME_KG),
    
    CELL_PROP_VOLUME_COOPS = case_when(
      CELL_VOLUME_KG != 0 & !is.na(CELL_VOLUME_KG) ~ CELL_VOLUME_KG_COOPS / CELL_VOLUME_KG, 
      TRUE ~ NA), 
    CELL_PROP_VOLUME_OTHERS = case_when(
      CELL_VOLUME_KG != 0 & !is.na(CELL_VOLUME_KG) ~ CELL_VOLUME_KG_OTHERS / CELL_VOLUME_KG, 
      TRUE ~ NA)
    )
# (multiplying by BUYER_IS_COOP in the sum of summarise is rowwise, i.e. identical to doing it in a prior mutate)

# check that props sum to 1
stopifnot(
  cell_depvars %>% 
    group_by(CELL_ID) %>% 
    mutate(test = CELL_PROP_VOLUME_COOPS + CELL_PROP_VOLUME_OTHERS) %>% 
    ungroup() %>% 
    pull(test) %>% unique() %>% na.omit() == 1
)

cell_depvars$CELL_PROP_VOLUME_COOPS %>% summary()

cell_depvars %>% filter(!is.na(CELL_VOLUME_KG_COOPS)) %>% nrow()

potential_1st %>% filter(LINK_VOLUME_KG == 0) %>% View()
cell_depvars %>% filter(CELL_VOLUME_KG == 0) %>% nrow()

potential_1st %>% filter(grepl(pattern = "SUSTAIN|JRC", PRO_ID)) %>% 
  group_by(CELL_ID) %>% filter(n()>5) %>%
  arrange(CELL_ID) %>% 
  select(!starts_with("COOP_")) %>% 
  View()

# potential_1st$LINK_TRAVEL_METERS %>% summary()
# potential_1st %>% filter(is.na(LINK_TRAVEL_METERS)) %>% pull(CELL_ACTUAL_ONLYCOOP_LINK) %>% summary()
# potential_1st %>% filter(is.na(LINK_TRAVEL_METERS) & CELL_NO_POTENTIAL_LINK) %>% View() 


## Cell level Xs -----------

# We need to do differently, because many links with the same coops can be TRUE on LINK_IS_WITH_1_NEAREST_COOPS 
# (and even more on 5 nearest). Thus, cells with many actual links (cargill farms typically) will average, and, 
# more problematically, sum, not across 1 or 5 coops, but across many repeats of these coops. 
# LINK_IS_WITH_X_NEAREST_COOPS variables are correct, they do reflect what they mean to, 
# it is how we use them here that needs to be corrected. 

# Thus, we make a coop selector variable to average/sum over 1 or 5 values corresponding 
# to the 1 or 5 nearest coops, not many values of the 1 or 5 nearest coops.  
potential_1st = 
  potential_1st %>% 
  group_by(CELL_ID) %>% 
  mutate(
    COOP_SELECTOR = !duplicated(COOP_ID) 
  ) %>% 
  ungroup() 

# there are cells where there are potential links, but they are with buyers that are not spatially explicit and thus distance is NA.
# it is important to remove these NAs so they don't make cell distance summaries NAs. 
# hence na.rm in the mean
cell_cellvars = 
  potential_1st %>% 
  summarise(.by = "CELL_ID",
            # For plotting purposes 
            CELL_ANY_ACTUAL_COOP_LINK = unique(CELL_ANY_ACTUAL_COOP_LINK), # do not recompute it, because some actual links have been under-sampled.  
            # CELL_TRAINTEST_2ND_STAGE = 
            CELL_LONGITUDE = unique(CELL_LONGITUDE),
            CELL_LATITUDE = unique(CELL_LATITUDE),
            
            # Topological 
            # unique() to put equal weights on distances to the five distinct coop BS (not those with more actual links having more weights)
            CELL_AVG_DISTANCE_METERS_5_NEAREST_COOPS  = mean(unique(COOP_SELECTOR*LINK_IS_WITH_5_NEAREST_COOPS*LINK_DISTANCE_METERS), na.rm = TRUE),
            CELL_MIN_DISTANCE_METERS                  = mean(unique(COOP_SELECTOR*LINK_IS_WITH_1_NEAREST_COOPS*LINK_DISTANCE_METERS), na.rm = TRUE),
            
            CELL_AVG_TRAVEL_METERS_5_NEAREST_COOPS  = mean(unique(COOP_SELECTOR*LINK_IS_WITH_5_NEAREST_COOPS*LINK_TRAVEL_METERS), na.rm = TRUE),
            CELL_MIN_TRAVEL_METERS                  = mean(unique(COOP_SELECTOR*LINK_IS_WITH_1_NEAREST_COOPS*LINK_TRAVEL_METERS), na.rm = TRUE),
            
            CELL_N_BS_WITHIN_DIST               = unique(CELL_N_BS_WITHIN_DIST),
            CELL_N_COOP_IN_DPT                  = unique(CELL_N_COOP_IN_DPT),
            CELL_AVG_N_LICBUY_IN_DPT            = unique(CELL_AVG_N_LICBUY_IN_DPT),
            
            # Producer end
            CELL_DENSEFOREST_HA = unique(CELL_DENSEFOREST_HA),
            CELL_OTHERFORESTS_HA = unique(CELL_OTHERFORESTS_HA),
            CELL_COCOA_HA = unique(CELL_COCOA_HA),
            CELL_COFFEE_HA = unique(CELL_COFFEE_HA),
            CELL_RUBBER_HA = unique(CELL_RUBBER_HA),
            CELL_PALM_HA = unique(CELL_PALM_HA),
            CELL_COCONUT_HA = unique(CELL_COCONUT_HA),
            CELL_CASHEW_HA = unique(CELL_CASHEW_HA),
            CELL_OTHERAG_HA = unique(CELL_OTHERAG_HA),
            CELL_SETTLEMENT_HA = unique(CELL_SETTLEMENT_HA),
            CELL_IMPOSSIBLE_HA = unique(CELL_IMPOSSIBLE_HA),
            CELL_TRI_MM = unique(CELL_TRI_MM), 
            # CELL_GAEZCOCOA_AESI = unique(CELL_GAEZCOCOA_AESI),
            CELL_DISTRICT_GEOCODE = unique(CELL_DISTRICT_GEOCODE),
            CELL_DISTRICT_NAME    = unique(CELL_DISTRICT_NAME),
            
            # Indicators
            CELL_NO_POTENTIAL_LINK     = unique(CELL_NO_POTENTIAL_LINK),
            # CELL_NO_ACTUAL_LINK        = unique(CELL_NO_ACTUAL_LINK), # this isn't a correct structure var at this stage.  
            CELL_ONLY_VIRTUAL_LINK     = unique(CELL_ONLY_VIRTUAL_LINK), 
            CELL_ACTUAL_ONLYCOOP_LINK  = unique(CELL_ACTUAL_ONLYCOOP_LINK), 
            CELL_ACTUAL_ONLYOTHER_LINK = unique(CELL_ACTUAL_ONLYOTHER_LINK), 
            CELL_ACTUAL_BOTH_LINK      = unique(CELL_ACTUAL_BOTH_LINK), 
            CELL_ACTUAL_LINK           = unique(CELL_ACTUAL_LINK),

)
cell_cellvars$CELL_MIN_DISTANCE_METERS %>% summary()
cell_cellvars$CELL_AVG_DISTANCE_METERS_5_NEAREST_COOPS %>% summary()
cell_cellvars$CELL_MIN_TRAVEL_METERS %>% summary()
cell_cellvars$CELL_AVG_TRAVEL_METERS_5_NEAREST_COOPS %>% summary()
potential_1st$LINK_DISTANCE_METERS %>% summary() # (This has a very high max, coming from a link with another kind of buyer, thus not constrained by the 72km condition.)
potential_1st$LINK_TRAVEL_METERS %>% summary()

## Coop level Xs -----------
# Do every average or sum over 1/ all potential coops; 2/ only the actual ones; 3/ the five closest ones. 

### Mean binary ------------------------
# (averages of binary vars make proportions)
fn_coop_prop_summary = function(COOP_VAR){
  potential_1st %>% 
    summarise(.by = "CELL_ID", 
              !!as.symbol(paste0("CELL_PROP_", COOP_VAR))           := mean(!!as.symbol(COOP_VAR)*COOP_SELECTOR, na.rm = TRUE),
              !!as.symbol(paste0("CELL_PROP_1_NEAREST_", COOP_VAR))    := mean(!!as.symbol(COOP_VAR)*COOP_SELECTOR*LINK_IS_WITH_1_NEAREST_COOPS, na.rm = TRUE),
              !!as.symbol(paste0("CELL_PROP_5_NEAREST_", COOP_VAR)) := mean(!!as.symbol(COOP_VAR)*COOP_SELECTOR*LINK_IS_WITH_5_NEAREST_COOPS, na.rm = TRUE)
    )
}

coop_vars_toavg = c(
  # Coop certification
  "COOP_CERTIFIED",
  "COOP_FT",
  "COOP_RFA",
  "COOP_UTZ",
  # Coop SSIs
  "COOP_HAS_SSI",
  "COOP_SSI_CARGILL",
  "COOP_SSI_BARRY",
  "COOP_SSI_OLAM",
  "COOP_SSI_ECOM",
  "COOP_SSI_NESTLE",
  "COOP_SSI_MONDELEZ",
  "COOP_SSI_BLOMMER",
  "COOP_SSI_CEMOI",
  "COOP_SSI_HERSHEY",
  "COOP_SSI_MARS",
  "COOP_SSI_SUCDEN",
  "COOP_SSI_PURATOS",
  "COOP_SSI_OTHER",
  # Coop status
  "COOP_STATUS_SCOOPS",
  "COOP_STATUS_COOP-CA" # repeat for COOP-CA, bc we don't always know (so it's not going to be perfectly colinear)
  )


list_binary_summaries = list()
for(coop_var in coop_vars_toavg){
  list_binary_summaries[[coop_var]] = fn_coop_prop_summary(COOP_VAR = coop_var)
}
cell_binaryvars = 
  list_binary_summaries %>% 
  bind_cols() %>%
  mutate(CELL_ID = CELL_ID...1) %>% 
  select(!starts_with("CELL_ID..."))
  
  
### Mean others ------------
fn_coop_mean_summary = function(COOP_VAR){
  potential_1st %>% 
    summarise(.by = "CELL_ID", 
              !!as.symbol(paste0("CELL_AVG_", COOP_VAR))           := mean(!!as.symbol(COOP_VAR)*COOP_SELECTOR, na.rm = TRUE),
              !!as.symbol(paste0("CELL_AVG_1_NEAREST_", COOP_VAR))    := mean(!!as.symbol(COOP_VAR)*COOP_SELECTOR*LINK_IS_WITH_1_NEAREST_COOPS, na.rm = TRUE),
              !!as.symbol(paste0("CELL_AVG_5_NEAREST_", COOP_VAR)) := mean(!!as.symbol(COOP_VAR)*COOP_SELECTOR*LINK_IS_WITH_5_NEAREST_COOPS, na.rm = TRUE)
    )
}
coop_othervars_toavg = c(
  # and these are not binary, but we want to average them still (rather than sum, bc their values may overlap across coops)
  "COOP_N_KNOWN_BUYERS",  
  # Coop surroundings
  "COOP_BS_10KM_TRI",
  # In the coop side all LU variables are relevant predictors. 
  "COOP_BS_10KM_DENSEFOREST_HA",
  "COOP_BS_10KM_OTHERFORESTS_HA",
  "COOP_BS_10KM_COCOA_HA",
  "COOP_BS_10KM_COFFEE_HA",
  "COOP_BS_10KM_RUBBER_HA",
  "COOP_BS_10KM_PALM_HA",
  "COOP_BS_10KM_COCONUT_HA",
  "COOP_BS_10KM_CASHEW_HA",
  "COOP_BS_10KM_OTHERAG_HA",
  "COOP_BS_10KM_SETTLEMENT_HA",
  "COOP_BS_10KM_IMPOSSIBLE_HA"
)

list_othermean_summaries = list()
for(coop_var in coop_othervars_toavg){
  list_othermean_summaries[[coop_var]] = fn_coop_mean_summary(COOP_VAR = coop_var)
}
cell_othermeanvars = 
  list_othermean_summaries %>% 
  bind_cols() %>%
  mutate(CELL_ID = CELL_ID...1) %>% 
  select(!starts_with("CELL_ID..."))

### Sum --------------------------
fn_coop_sum_summary = function(COOP_VAR){
  potential_1st %>% 
    summarise(.by = "CELL_ID", 
            !!as.symbol(paste0("CELL_COUNT_", COOP_VAR))           := sum(!!as.symbol(COOP_VAR)*COOP_SELECTOR, na.rm = TRUE),
            !!as.symbol(paste0("CELL_COUNT_1_NEAREST_", COOP_VAR))    := sum(!!as.symbol(COOP_VAR)*COOP_SELECTOR*LINK_IS_WITH_1_NEAREST_COOPS, na.rm = TRUE),
            !!as.symbol(paste0("CELL_COUNT_5_NEAREST_", COOP_VAR)) := sum(!!as.symbol(COOP_VAR)*COOP_SELECTOR*LINK_IS_WITH_5_NEAREST_COOPS, na.rm = TRUE)
  )
}

coop_vars_tosum = c(
  # Coop size
  "COOP_FARMERS",
  "COOP_FARMERS_FT",
  "COOP_FARMERS_RFA",
  "COOP_N_KNOWN_BS"
)

list_count_summaries = list()
for(coop_var in coop_vars_tosum){
  list_count_summaries[[coop_var]] = fn_coop_sum_summary(COOP_VAR = coop_var)
}
cell_countvars = 
  list_count_summaries %>% 
  bind_cols() %>%
  mutate(CELL_ID = CELL_ID...1) %>% 
  select(!starts_with("CELL_ID..."))

# These should match  
cell_countvars$CELL_COUNT_1_NEAREST_COOP_FARMERS %>% summary()
coopbs$TOTAL_FARMERS %>% summary()

stopifnot(max(coopbs$TOTAL_FARMERS) == max(cell_countvars$CELL_COUNT_1_NEAREST_COOP_FARMERS))

## Representativity weights ---------------------
cell_weights = 
  potential_1st %>% 
  summarise(.by = "CELL_ID", 
            CELL_N_JRC_FARMERS = length(na.omit(unique(grep(pattern = "JRC_", x = PRO_ID, value = TRUE)))),
            CELL_COCOA_HA = unique(CELL_COCOA_HA)) %>% 
  mutate(
    CELL_REPRESENTATIVITY_WEIGHT = case_when(
      CELL_COCOA_HA > 0 ~ CELL_N_JRC_FARMERS/CELL_COCOA_HA, 
      TRUE ~ NA
    ),
    STANDARDIZER = sum(CELL_REPRESENTATIVITY_WEIGHT, na.rm = TRUE), 
    CELL_REPRESENTATIVITY_STD_WEIGHT = CELL_REPRESENTATIVITY_WEIGHT/STANDARDIZER) %>% 
  select(CELL_ID, CELL_REPRESENTATIVITY_STD_WEIGHT)
stopifnot(cell_weights$CELL_REPRESENTATIVITY_STD_WEIGHT %>% sum(na.rm = TRUE) %>% round(10) == 1)

## Merge all cell variables -------------
cell_all = 
  cell_depvars %>%  
  inner_join(cell_cellvars, 
             by = "CELL_ID") %>% 
  inner_join(cell_binaryvars, 
             by = "CELL_ID") %>% 
  inner_join(cell_othermeanvars, 
             by = "CELL_ID") %>% 
  inner_join(cell_countvars, 
             by = "CELL_ID") %>% 
  inner_join(cell_weights, 
             by = "CELL_ID")
  
stopifnot(potential_1st$CELL_ID %>% unique() %>% length() == nrow(cell_all))

# other checks
prop_var_names = 
  cell_all %>% 
  select(starts_with("CELL_PROP_")) %>% 
  names()
checks_list = list()
for(var in prop_var_names){
  checks_list[[var]] = any(na.omit(cell_all[,var]) > 1)
}
if(
  checks_list %>% unlist() %>% any()
){stop("some variables are not proportions while they are expected so")}




## EXPORT ----------------
saveRDS(cell_all, 
        here("temp_data", "prepared_main_dataset", paste0("cell_", grid_size_m*1e-3, "km.Rdata")))


stop()
# OPTIMAL CELL SIZE SEARCH -------------- 
# check number of villages by cell, when village is known
# first check that village names are not common in JRC and SC... 
length(na.omit(unique(sc_links$PRO_VILLAGE_NAME))) + 
  length(na.omit(unique(jrc_links$PRO_VILLAGE_NAME))) == 
  length(na.omit(unique(consol$PRO_VILLAGE_NAME)))
# it cannot anyway, because for JRC, the variable is coded in numbers. 

candd_potential_all = potential_all
# For JRC

cell_n_jrc_vlg = list()
for(grid_pot_size_km in c(3, 4, 5, 6, 7, 8, 9)){ #  
  
  print(paste0(grid_pot_size_km, "km: "))
        
  candd_potential_all = 
    readRDS(here("temp_data", "prepared_main_dataset", paste0("cell_links_", grid_pot_size_km, "km.Rdata")))
  
  potential_jrc = 
    candd_potential_all %>% 
    filter(grepl("JRC_", PRO_ID))
  nrow(potential_jrc)

  # number of cells per JRC village (the same cell can count for several villages)
  jrc_vlg_n_cells = 
    potential_jrc %>% 
    summarise(.by = PRO_VILLAGE_NAME, 
              # the number of cells over which every village spans (with potential double counting of cells)
              VILLAGE_N_CELLS = length(na.omit(unique(CELL_ID)))) %>% 
    arrange(desc(VILLAGE_N_CELLS))
  # total - i.e. number of cells with some JRC info (does not reflect number of villages, bc of double counting)
  # where 1 cell can have info from several villages and several cells can have info from the same village 
  # print(
  #   paste0(" : ", sum(jrc_vlg_n_cells$VILLAGE_N_CELLS)
  #   ))

  print(
    paste0("# cells with JRC info: ", 
           potential_jrc %>%  
             distinct(CELL_ID) %>% nrow()
    ))
  print(
    paste0("# villages contained in 1 single cell: ", jrc_vlg_n_cells %>% filter(VILLAGE_N_CELLS==1) %>% nrow()
    ))
  print(
    paste0("# villages spanning over more than 1 cells: ", jrc_vlg_n_cells %>% filter(VILLAGE_N_CELLS>1) %>% nrow()
    ))
  print(
    paste0("# villages spanning over more than 2 cells: ", jrc_vlg_n_cells %>% filter(VILLAGE_N_CELLS>2) %>% nrow()
    ))
  print(
    paste0("# villages spanning over more than 3 cells: ", jrc_vlg_n_cells %>% filter(VILLAGE_N_CELLS>3) %>% nrow()
    ))
  
  # Now consider the number of CELLS with few farmers in them, 
  # which is the problem with village split across cells, rather than the split it self
  # (a village split in two halves, both representative, is fine for instance, 
  # but an isolated farmer just beyond the cell cut off and alone and not representative in the next cell, is not fine.)
  cell_n_jrc_vlg[[grid_pot_size_km]] = 
  potential_jrc %>% 
    summarise(.by = CELL_ID, 
              CELL_N_JRC_FARMERS = length(unique(PRO_ID))) %>% 
    arrange(CELL_N_JRC_FARMERS)
  
  
  
  # # number of JRC villages that a single cell encompasses 
  # # No, rather the number of cells that have different villages in them, 
  # # sorted by the number of these different villages
  # cell_n_jrc_vlg[[grid_pot_size_km]] = 
  #   potential_jrc %>% 
  #   group_by(CELL_ID) %>% 
  #   mutate(CELL_N_JRC_VILLAGES = length(unique(PRO_VILLAGE_NAME))) %>% 
  #   ungroup() %>% 
  #   # the number of cells that encompass one or more villages, for each nb of village encompassed
  #   summarise(.by = CELL_N_JRC_VILLAGES, 
  #             N_CELLS = length(unique(CELL_ID))) %>% 
  #   arrange(CELL_N_JRC_VILLAGES)
  # 
  # # number of villages (also having the two kinds of double counting)... 
  # # print(
  # #   cell_n_jrc_vlg[[grid_pot_size_km]] %>% 
  # #     mutate(N_DIST_VILLAGES = CELL_N_JRC_VILLAGES * N_CELLS) %>% 
  # #     pull(N_DIST_VILLAGES) %>% sum()
  # # )
  # 
  # print(
  #   paste0("# wasted villages: ", 
  #          cell_n_jrc_vlg[[grid_pot_size_km]] %>% 
  #                mutate(N_WASTED_VILLAGES = (CELL_N_JRC_VILLAGES-1) * N_CELLS) %>%
  #                pull(N_WASTED_VILLAGES) %>% sum()
  #   ))
  # 
  print(" ")

}
cell_n_jrc_vlg

cell_n_jrc_vlg[[4]] %>% 
  filter(CELL_N_JRC_FARMERS <= 2)
cell_n_jrc_vlg[[4]] %>% nrow()


# Flawed approach 
cell_n_jrc_store_flawed = list()
for(grid_pot_size_km in c(8, 9, 10)){ # 5, 6, 7, 
  
  candd_potential_all = 
    readRDS(here("temp_data", "prepared_main_dataset", paste0("cell_links_", grid_pot_size_km, "km.Rdata")))
  
  candd_potential_all = 
    candd_potential_all %>% 
    group_by(CELL_ID) %>% 
    mutate(CELL_N_JRC_VILLAGES = case_when(
      grepl("JRC_", PRO_ID) ~ length(na.omit(unique(PRO_VILLAGE_NAME))), 
      TRUE ~ 0
    )) %>% 
    ungroup()
  
  cell_n_jrc_store_flawed[[grid_pot_size_km]] = 
    candd_potential_all %>% 
    summarise(.by = CELL_N_JRC_VILLAGES, 
              N_CELLS = length(unique(CELL_ID))) 
  
  print(
    cell_n_jrc_store_flawed[[grid_pot_size_km]] %>% 
      filter(CELL_N_JRC_VILLAGES>0) %>%
      mutate(N_DIST_VILLAGES = CELL_N_JRC_VILLAGES * N_CELLS) %>% 
      pull(N_DIST_VILLAGES) %>% sum()
  )
}
cell_n_jrc_store_flawed

print(paste0("There are ", 
             length(na.omit(unique(potential_all$PRO_VILLAGE_NAME))),
             " different sampled villages in JRC and SC surveys"))

potential_all = 
  potential_all %>% 
  group_by(CELL_ID) %>% 
  mutate(CELL_N_VILLAGES = length(na.omit(unique(PRO_VILLAGE_NAME)))) %>% 
  ungroup()
potential_all %>% 
  summarise(.by = CELL_N_VILLAGES, 
            N_CELLS = length(unique(CELL_ID))) 

# For SC 
print(paste0("Just in the SUSTAINCOCOA survey, there are ", 
             length(na.omit(unique(filter(potential_all, grepl("SUSTAINCOCOA_", PRO_ID))$PRO_VILLAGE_NAME))), 
             " different sampled villages."
))

potential_all = 
  potential_all %>% 
  group_by(CELL_ID) %>% 
  mutate(CELL_N_SUSTAINCOCOA_VILLAGES = case_when(
    str_contains(PRO_ID, "SUSTAINCOCOA_") ~ length(na.omit(unique(PRO_VILLAGE_NAME))), 
    TRUE ~ 0
  )) %>% 
  ungroup()

potential_all %>% 
  summarise(.by = CELL_N_SUSTAINCOCOA_VILLAGES, 
            N_CELLS = length(unique(CELL_ID))) 



print(paste0("Just in the JRC survey, there are ", 
             length(na.omit(unique(filter(potential_all, grepl("JRC_", PRO_ID))$PRO_VILLAGE_NAME))), 
             " different sampled villages."
))










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
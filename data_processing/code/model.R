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
library(terra)
library(stars)


# Assets and functions -----------------------------------------------------
# use the projected CRS used by BNETD for their 2020 land use map. 
civ_crs <- 32630


# load in particular the function fn_trader_to_group_names, str_trans, ... 
source(here("code", "USEFUL_STUFF_manually_copy_pasted.R"))


coopbsy = read.csv(
  file = here("temp_data/private_IC2B/IC2B_v2_coop_bs_year.csv")) %>% 
  rename(BS_LONGITUDE = LONGITUDE, 
         BS_LATITUDE = LATITUDE)

# Cargill link data
carg_links = read.csv(here("temp_data", "preprocessed_cargill", "cargill_links_standardized.csv"))
  
# JRC link data
jrc_links = read.csv(here("temp_data", "preprocessed_jrc_data", "jrc_links_standardized.csv"))

# Sustain-cocoa link data
sc_links = read.csv(here("temp_data", "preprocessed_sustain_cocoa", "sustain_cocoa_links_standardized.csv"))

departements <- read_sf("input_data/s3/CIV_DEPARTEMENTS.geojson")

departements = 
  st_transform(departements, crs = civ_crs)

# CONSOLIDATE LINK DATA ---------------------

# Make template for the data frame consol, to frame the consolidation of every specific data set. 
consol <- data.frame(
  "YEAR" = NA, 
  "PRO_ID" = NA, 
  "COOP_BS_ID" = NA, 
  "LINK_DISTANCE_METERS" = NA, 
  "PRO_DEPARTMENT_GEOCODE" = NA, 
  "PRO_DEPARTMENT_NAME" = NA,
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
consol <- filter(consol, !is.na(YEAR))
# for now remove for convenience
consol <- select(consol, -PRO_DEPARTMENT_NAME, -PRO_DEPARTMENT_GEOCODE)

# Make ID for actual links
# allows in particular to identify actual links more easily if in some input data PRO_ID is duplicated in consol
length(unique(consol$PRO_ID))
nrow(consol)
consol = 
  consol %>% 
  group_by(PRO_ID, COOP_BS_ID) %>% 
  mutate(ACTUAL_LINK_ID = cur_group_id()) %>% 
  ungroup()
if(consol$ACTUAL_LINK_ID %>% unique() %>% length() != nrow(consol)){stop()}

# to investigate potential prb
# consol = 
#   consol %>% 
#   mutate(DUPL_PRO = duplicated(PRO_ID)) %>% 
#   group_by(PRO_ID) %>% 
#   mutate(ANY_DUPL = any(DUPL_PRO)) %>% 
#   ungroup()
# consol %>% 
#   filter(ANY_DUPL) %>% 
#   View()


# LINK STATS ----------------

# Latest surveyed year, to take the panel of coops at this time.  
latest_survey_year = max(consol$YEAR, na.rm = TRUE)
latest_survey_year

# Distance producer - intermediary 
summary(consol$LINK_DISTANCE_METERS)

dist_outliers = boxplot.stats(consol$LINK_DISTANCE_METERS, coef = 2)$out %>% sort()
consol = 
  consol %>% 
  mutate(LINK_DISTANCE_METERS = case_when(
    LINK_DISTANCE_METERS %in% dist_outliers ~ NA,
    TRUE ~ LINK_DISTANCE_METERS
  ))

(dist_meters_90pctl = quantile(consol$LINK_DISTANCE_METERS, 0.9, na.rm = T) %>% round(-3))
(dist_meters_95pctl = quantile(consol$LINK_DISTANCE_METERS, 0.95, na.rm = T) %>% round(-3))
(dist_meters_max = quantile(consol$LINK_DISTANCE_METERS, 1, na.rm = T) %>% round(-3))

(dist_meters_threshold <- dist_meters_95pctl)

summary(consol$LINK_DISTANCE_METERS)
sd(consol$LINK_DISTANCE_METERS[consol$LINK_DISTANCE_METERS], na.rm = TRUE)

# COOP BUFFERS ---------------
coopbsy$YEAR %>% summary()

coopbs = 
  coopbsy %>% 
  filter(YEAR == latest_survey_year)
#   distinct(COOP_BS_ID, .keep_all = TRUE)

coopbs_sf = 
  coopbs %>% 
  filter(!is.na(BS_LONGITUDE)) %>% 
  st_as_sf(coords = c("BS_LONGITUDE", "BS_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(crs = civ_crs) 

# coopbs_buffered_sf = 
#   coopbs %>% 
#   filter(!is.na(BS_LONGITUDE)) %>% 
#   st_as_sf(coords = c("BS_LONGITUDE", "BS_LATITUDE"), crs = 4326, remove = FALSE) %>% 
#   st_transform(crs = civ_crs) %>% 
#   st_buffer(dist = dist_meters_threshold)

  
# EXTRACT IN COOP BUFFERS ------------
  
# export to GEE and re-import here.  
  
  
# DEPLOY MODEL STRUCTURE -----------------

## Grid base -----------
# The size of the grid is an important parameter. 
# 12km is because 75% of cocoa producers surveyed by the JRC have their cocoa plots less than 6km away from their house.
# The average location of the producer in a grid cell is in it's centroid. Taking 6km away in any direction from the center implies 12km. 
# At the same time, in Cargill data, 95% of the plots of the same farmer fit in bounding boxes of 25 hectares (0.25 square km) or less.
grid_size_m = 30000

# # Limit to cocoa growing region 
# production_geocode_panel <- s3read_using(
#   object = "cote_divoire/cocoa/indicators/in/q4_2023/cocoa_production_2019_2022_q4_2023.csv",
#   bucket = "trase-storage",
#   opts = c("check_region" = T),
#   FUN = read_delim,
#   delim = ",") %>%
#   select(LVL_4_CODE, LVL_4_NAME, 
#          production_2019, production_2020, production_2021, production_2022) %>%
#   pivot_longer(cols = starts_with("production"), 
#                values_to = "COCOA_PRODUCTION_TONNES",
#                names_to = "YEAR",
#                names_prefix = "production_") %>% 
#   mutate(YEAR = as.numeric(YEAR)) %>% 
#   rowwise() %>% 
#   mutate(LVL4_TRASE_ID_PROD = geocode_to_trase_id(LVL_4_CODE)) %>% 
#   ungroup() # REMOVES THE ROWWISE 



# Make a terra template, just because I found how to do it before I found with stars.  
(grid_sr = rast(extent = ext(departements), 
                resolution = grid_size_m,
                crs = crs(departements))
)

# Convert to stars
grid_st = st_as_stars(grid_sr)

## Add actual links -------------

# Spatialize actual links 
consol_sf = 
  consol %>% 
  st_as_sf(coords = c("PRO_LONGITUDE", "PRO_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs)

# convert raster to sf
(grid_sf = 
  grid_st %>% 
  st_xy2sfc(as_points = FALSE, na.rm = FALSE) %>% # as_points = F outputs grid cells as polygons
  st_as_sf() %>% 
  mutate(GRID_ID = row_number()) %>% 
  select(-lyr.1))

# This extends the rows of the grids to the extent that there are actual links in every grid. 
grid_actual = 
  grid_sf %>% 
  st_join(consol_sf, 
          join = st_intersects,
          left = TRUE)

if(
  consol$COOP_BS_ID %>% unique() %>% na.omit() %>%length() != 
    grid_actual$COOP_BS_ID %>% na.omit() %>% unique() %>% length()
){stop("info on linked coops has been lost in the process")}

### Mark actual links ----------------
grid_actual = 
  grid_actual %>% 
  # mutate(TRAINING_SET_CELL = if_else(!is.na(PRO_ID), TRUE, FALSE)) %>% 
  rename(ACTUAL_COOP_BS_ID = COOP_BS_ID)

# The number of NAs in ACTUAL_LINK_ID is the number of grid cells with no actual link
grid_actual$ACTUAL_LINK_ID %>% summary()

# and this is the distribution of unique actual links by grid cell
grid_actual %>% 
  group_by(GRID_ID) %>% 
  mutate(N_UNIQUE_ACTUAL_LINKS_BYGRID = length(unique(na.omit(ACTUAL_LINK_ID)))) %>% 
  ungroup() %>% 
  pull(N_UNIQUE_ACTUAL_LINKS_BYGRID) %>% summary()


# Dataframe of actual links, without geometry for further join, and with potential link id column to identify duplicates 
actual = 
  grid_actual %>% 
  filter(!is.na(ACTUAL_COOP_BS_ID)) %>% 
  st_drop_geometry() %>% 
  mutate(POTENTIAL_COOP_BS_ID = ACTUAL_COOP_BS_ID) 

## Add virtual links ---------------

### Identify coops within distance -------------

# For every grid cell, this adds as many rows as there are potential links, including the actual ones. 
potential = 
  grid_sf %>% 
  mutate(geometry = st_centroid(geometry)) %>%
  st_join(
    coopbs_sf %>% 
      select(COOP_BS_ID), #, BS_LONGITUDE, BS_LATITUDE
    join = st_is_within_distance, 
    dist = as_units(dist_meters_threshold, "m"), 
    left = TRUE
  ) %>% 
  st_drop_geometry() %>% 
  rename( # use names that make sense in the dimensions post join
    POTENTIAL_COOP_BS_ID = COOP_BS_ID) %>% 
    # POTENTIAL_LINKS_IN_GRID_SET_ID = ACTUAL_LINK_ID
  mutate(
    YEAR = NA, 
    PRO_ID = NA, 
    ACTUAL_COOP_BS_ID = NA, 
    LINK_DISTANCE_METERS = NA, 
    PRO_LONGITUDE = NA, 
    PRO_LATITUDE = NA, 
    ACTUAL_LINK_ID = NA
  ) %>% 
  select(names(actual)) %>% 

  # Stack potential & actual and remove actual links counted twice
  rbind(actual) %>% 
  # identify origins 
  mutate(IS_FROM_STJOIN = is.na(ACTUAL_LINK_ID)) %>% 
  # identify duplicated coop ids within a grid cell
  # make sure that ALL duplicates are found, regardless of order. 
  group_by(GRID_ID) %>% 
  mutate(DUP_LINK = duplicated(POTENTIAL_COOP_BS_ID) | duplicated(POTENTIAL_COOP_BS_ID, fromLast = TRUE)) %>% 
  ungroup() %>% 
  # this identified duplicates including due to several actual links from this cell,
  # so remove rows that are duplicated coop ids, AND are from the spatial join, i.e. are not several actual links. 
  filter(!(DUP_LINK & IS_FROM_STJOIN)) %>% 
  # Number of reachable coops within a grid cell, regardless through how many links  
  group_by(GRID_ID) %>% 
  mutate(N_POTENTIAL_COOPS = length(unique(POTENTIAL_COOP_BS_ID))) %>% 
  ungroup() %>% 
  # identify grid cells with no actual link 
  group_by(GRID_ID) %>% 
  mutate(NO_ACTUAL_LINK = all(IS_FROM_STJOIN)) %>% 
  ungroup()

# potential %>% 
#   filter((DUP_LINK & IS_FROM_STJOIN)) %>% View()

potential$IS_FROM_STJOIN %>% summary()
potential$DUP_LINK %>% summary()

# The choice of the threshold, 90% vs. 100% makes a huge difference, since it's implies a reach of 41 vs. 75km resp. 

# - dans les grids avec des actual links, il y a le nombre d'actual links + le nombre d'AUTRES coops within distance du centroid. 
# - dans les mailles sans actual link, il y a le nombre de coops within distance du centroid. 

# But currently, there are always more different potential coops in potential than in stjoin, suggesting we haven't removed enough 
# TEST second proposition. The first one is not testable, because some actual links are not reproduced by the spatial join. 
grid_sf %>% 
  mutate(geometry = st_centroid(geometry)) %>%
  st_join(
    coopbs_sf %>% 
      select(COOP_BS_ID), #, BS_LONGITUDE, BS_LATITUDE
    join = st_is_within_distance, 
    dist = as_units(dist_meters_threshold, "m"), 
    left = TRUE
  ) %>%
  st_drop_geometry() %>% 
  rename(POTENTIAL_COOP_BS_ID = COOP_BS_ID) %>% 
  summarise(.by = GRID_ID, 
            N_POTENTIAL_COOPS = length(unique(POTENTIAL_COOP_BS_ID))) %>% 
  inner_join(potential %>% 
              filter(NO_ACTUAL_LINK) %>% 
              select(GRID_ID, N_POTENTIAL_COOPS), 
            by = "GRID_ID") %>% 
  mutate(PROBLEM = N_POTENTIAL_COOPS.x < N_POTENTIAL_COOPS.y) %>% 
  filter(PROBLEM) %>% 
  nrow() > 0
  

# So there should not be any duplicated potential link within a grid and an actual link
if(
potential %>% 
  group_by(GRID_ID, ACTUAL_LINK_ID) %>% 
  mutate(ANY_DUPL_COOP = any(duplicated(POTENTIAL_COOP_BS_ID))) %>% 
  ungroup() %>% 
  pull(ANY_DUPL_COOP) %>% any()
){stop("something unexpected in the row deployment by the spatial join")}

# Another check: that POTENTIAL_COOP_BS_ID is NA only in grid cells with no link at all. 
if(
potential %>% 
  group_by(GRID_ID) %>% 
  mutate(ANY_NA = any(is.na(POTENTIAL_COOP_BS_ID)), 
         ALL_NA = all(is.na(POTENTIAL_COOP_BS_ID))) %>% 
  ungroup() %>% 
  filter(ANY_NA != ALL_NA) %>% nrow() != 0
){stop("unexpected")}

# With this method, it's not exactly the same number, but very marginal. 
# grid_actual_ctoid = 
#   grid_actual %>% 
#   st_centroid() %>% 
#   st_join(
#     coopbs_buffered_sf %>% select(COOP_BS_ID, BS_LONGITUDE, BS_LATITUDE),
#     join = st_intersects, 
#     left = TRUE
#   )

###  --------------------

# potential %>% 
#   filter(ACTUAL_COOP_BS_ID == POTENTIAL_COOP_BS_ID) %>% View()
# nrow(filter(potential, TRAINING_SET_CELL))

# The number of distinct actual links within a grid cell. 
# potential %>% 
#   group_by(GRID_ID) %>% 
#   mutate(IS_EMPTY_GRID = length(unique(na.omit(POTENTIAL_LINKS_IN_GRID_SET_ID)))) %>% 
#   ungroup() %>% 
#   pull(IS_EMPTY_GRID) %>% summary()
           

### Mark virtual links --------------
potential 

# not all actual links are retrieved in the spatial join, for 2 reasons probably: 
# - the threshold is not the max distance, but there's still a diff when using the max   
# - the distance is to grid cell centroids here. 
# but it's surprising there is such a difference... 
# it's not possible to identify actual links after the join, other than by the equality below which fails to identify them all... 
potential %>%
  filter(ACTUAL_COOP_BS_ID == POTENTIAL_COOP_BS_ID) %>% nrow() < nrow(consol)


potential %>% 
  filter(!is.na(ACTUAL_COOP_BS_ID)) %>% 
  nrow()

# MAKE VARIABLES ----------------


## Count # reachable   -------------

potential = 
  potential %>% 
  group_by(GRID_ID, POTENTIAL_LINKS_IN_GRID_SET_ID) %>%  
  mutate(N_BS_WITHIN_DIST = length(na.omit(unique(POTENTIAL_COOP_BS_ID))) # use this, and not n(), for the variable to take value 0 when there's is no match, rather than one row that has NA value 
         # N_BS_WITHIN_DIST2 = n(), this would be equal to 1, and not 0, for grid cells that reach no buying station. 
         ) %>% 
  ungroup()

# potential %>% 
#   filter(N_BS_WITHIN_DIST != N_BS_WITHIN_DIST2) %>% View()

potential$N_BS_WITHIN_DIST %>% summary()


## Compute distances -------------
# st_distance needs to work on same size df. 

# this is not the full grid anymore, but only grid cells with at least one potential link 
potential = 
  potential %>% 
  filter(N_BS_WITHIN_DIST > 0)
nrow(potential) != nrow(potential)

if(potential %>% filter(st_is_empty(geometry)) %>% nrow() > 0 | 
   anyNA(potential$BS_LONGITUDE)){stop("potential does not have the expected spatial attributes at this stage")}

potential_bspt = 
  potential %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("BS_LONGITUDE", "BS_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 

potential$POTENTIAL_LINK_DISTANCE_METERS <- 
  st_distance(potential, potential_bspt, by_element = TRUE) %>% as.numeric()

# merge back to the full grid
(init_nrow <- nrow(potential))
potential = 
  potential %>% 
  left_join(potential %>% 
              select(GRID_ID, POTENTIAL_LINKS_IN_GRID_SET_ID, POTENTIAL_COOP_BS_ID, POTENTIAL_LINK_DISTANCE_METERS) %>% 
              st_drop_geometry(), 
            by = c("GRID_ID", "POTENTIAL_LINKS_IN_GRID_SET_ID", "POTENTIAL_COOP_BS_ID"))
if(init_nrow != nrow(potential)){stop("rows added unexpectedly")}

# In actual links, gauge the error of computing distance from coop to cell center vs to farm center. 
potential %>% 
  mutate(DIFF_DIST_TO_CELL_VS_FARM = case_when(
    POTENTIAL_COOP_BS_ID == ACTUAL_COOP_BS_ID ~ POTENTIAL_LINK_DISTANCE_METERS - LINK_DISTANCE_METERS,
    TRUE ~ NA
    )) %>% 
  pull(DIFF_DIST_TO_CELL_VS_FARM) %>% summary()


## Merge LU variables in grid cells -------------

# These were computed in the grid, preserving the GRID_ID. 
# au pire, on peut juste extraire dans la grid totale, et ne merger que les variables extraites, pas forcément la géométrie...


# Switch geometry from the cell centroid to the polygon. - This takes ~15 minutes
# grid_potential_poly =
#   grid_actual %>%
#   arrange(GRID_ID) %>% # because this may speed up 
#   select(GRID_ID) %>%
#   full_join(
#     y = potential %>% 
#           st_drop_geometry() %>%
#           arrange(GRID_ID),
#     by = "GRID_ID")
# if(nrow(grid_potential_poly) != nrow(potential)){stop("the join did not work as expected")}






# *  ---------------------------------
# Use terra (rather than stars), because apparently stars::st_rasterize does not 
# allow one to summarize the values of several points falling in the same grid cell. 
# https://gis.stackexchange.com/questions/432367/how-to-apply-mean-min-max-merge-functions-on-a-vector-layer-to-create-a-stars-ra

# the issue is that terra does not handle character values. 

# # Make spatial links
# consol_sr = 
#   consol %>% 
#   vect(geom = c("PRO_LONGITUDE", "PRO_LATITUDE"), crs = "epsg:4326", keepgeom = TRUE) %>% 
#   project(paste0("epsg:", civ_crs))
# 
# allcel_tr = rasterize(consol_sr, tmplt_sr, field = c("PRO_ID", "COOP_BS_ID", "LINK_DISTANCE_METERS"), fun = function(x){length(unique(x))}) # by = names(consol_sr), 
# # function(x){factor(paste0(na.omit(unique(x)), collapse = ";"))}
# rasterize(consol_sr, tmplt_sr, fun = "length") %>% values() %>% summary()
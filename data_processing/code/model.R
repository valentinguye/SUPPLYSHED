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

departements <- s3read_using(
  object = "cote_divoire/spatial/BOUNDARIES/DEPARTEMENT/OUT/CIV_DEPARTEMENTS.geojson",#"cote_divoire/spatial/BOUNDARIES/DEPARTEMENT/OUT/ci_departments_wgs84_level4.geojson",
  bucket = "trase-storage",
  FUN = read_sf,
  #sheet = "Cacao",
  #skip = 3,
  opts = c("check_region" = T)
)
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

dist_meters_threshold <- dist_meters_95pctl

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
  mutate(GRID_ID = as.character(row_number())) %>% 
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
  mutate(TRAINING_SET_CELL = if_else(!is.na(PRO_ID), TRUE, FALSE)) %>% 
  rename(ACTUAL_COOP_BS_ID = COOP_BS_ID)

# The number of NAs in ACTUAL_LINK_ID is the number of grid cells with no actual link
grid_actual$ACTUAL_LINK_ID %>% summary()


# and this is the distribution of unique actual links by grid cell
grid_actual %>% 
  group_by(GRID_ID) %>% 
  mutate(N_UNIQUE_ACTUAL_LINKS_BYGRID = length(unique(ACTUAL_LINK_ID))) %>% 
  ungroup() %>% 
  pull(N_UNIQUE_ACTUAL_LINKS_BYGRID) %>% summary()


## Add virtual links ---------------

### Identify coops within distance -------------

# This adds one row for every potential link, including the actual ones which were already there.  
grid_potential_ctoid = 
  grid_actual %>% 
  mutate(geometry = st_centroid(geometry)) %>% 
  st_join(
    coopbs_sf %>% select(COOP_BS_ID, BS_LONGITUDE, BS_LATITUDE),
    join = st_is_within_distance, 
    dist = as_units(dist_meters_threshold, "m"), 
    left = TRUE
  ) %>% 
  rename(POTENTIAL_COOP_BS_ID = COOP_BS_ID)
# The choice of the threshold, 90% vs. 100% makes a huge difference, since it's implies a reach of 41 vs. 75km resp. 

# With this method, it's not exactly the same number, but very marginal. 
# grid_actual_ctoid = 
#   grid_actual %>% 
#   st_centroid() %>% 
#   st_join(
#     coopbs_buffered_sf %>% select(COOP_BS_ID, BS_LONGITUDE, BS_LATITUDE),
#     join = st_intersects, 
#     left = TRUE
#   )

# So we need to remove the actual links added a second time.
grid_potential_ctoid %>% 
  filter(ACTUAL_COOP_BS_ID == POTENTIAL_COOP_BS_ID) %>% View()

nrow(filter(grid_potential_ctoid, TRAINING_SET))

# The number of distinct actual links within a grid cell. 
grid_potential_ctoid %>% 
  group_by(GRID_ID) %>% 
  mutate(IS_EMPTY_GRID = length(unique(ACTUAL_LINK_ID))) %>% 
  ungroup() %>% 
  pull(IS_EMPTY_GRID) %>% summary()
           
tmp = 
  grid_potential_ctoid %>% 
  group_by(GRID_ID, ACTUAL_LINK_ID) %>% 
  mutate(IS_DUPL = duplicated(POTENTIAL_COOP_BS_ID)) %>% 
  ungroup()
tmp %>% filter(ACTUAL_COOP_BS_ID == POTENTIAL_COOP_BS_ID) %>% View()
tmp %>% 
  filter(TRAINING_SET) %>% 
  arrange(GRID_ID, PRO_ID, POTENTIAL_COOP_BS_ID) %>% View()


### Mark virtual links --------------
grid_actual_ctoid


# MAKE VARIABLES ----------------

## Compute distances -------------



## Count # reachable   -------------
grid_actual$GRID_ID%>% unique() %>% length() 
grid_actual_ctoid$GRID_ID%>% unique() %>% length() 
# tmpsave <- pot_cells_ctoid

grid_actual_ctoid = 
  grid_actual_ctoid %>% 
  group_by(GRID_ID) %>%  
  mutate(N_BS_WITHIN_DIST = length(na.omit(unique(COOP_BS_ID))),# n(), POURQUOI EST_CE QUE CA FAIT PAS PAREIL AVEC n() ? DIT QQCHOSE DE LA STRUCTURE DES DONN2ES QUI N4EST PAS COMME ATTENDUE ? 
         ANY_ACTUAL_LINK = any(TRAINING_SET)) %>% 
  ungroup()

# length(na.omit(unique(COOP_)
grid_actual_ctoid$N_BS_WITHIN_DIST %>% summary()


## Extract LU in grid cells -------------


allcel_st = 
  st_rasterize(sf = consol_sf, template = tmplt_st, align=FALSE, options= "")

allcel_sf = st_xy2sfc(allcel_st, as_points = TRUE, na.rm = FALSE)
class(allcel_sf)

plot(allcel_st)



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
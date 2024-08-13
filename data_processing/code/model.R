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

# Assets and functions -----------------------------------------------------
# use the projected CRS used by BNETD for their 2020 land use map. 
civ_crs <- 32630

coopbsy = read.csv(
  file = here("temp_data/private_IC2B/IC2B_v2_coop_bs_year.csv"))

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

# temporary necessary 
# consol = 
#   consol %>% 
#   rename(LINK_DISTANCE_METERS = DISTANCE_PRO_ITM)

# LINK STATS ----------------

# Latest surveyed year, to take the panel of coops at this time.  
latest_survey_year = max(consol$YEAR, na.rm = TRUE)
latest_survey_year

# Distance producer - intermediary 
summary(consol$LINK_DISTANCE_METERS)

dist_meters_threshold = quantile(consol$LINK_DISTANCE_METERS, 0.9) %>% round(-3)

summary(consol$LINK_DISTANCE_METERS)
sd(consol$LINK_DISTANCE_METERS[consol$LINK_DISTANCE_METERS<50*1e3])

# COOPS' BUFFERS ---------------
coopbsy$YEAR %>% summary()

coopbs = 
  coopbsy %>% 
  filter(YEAR == latest_survey_year)
#   distinct(COOP_BS_ID, .keep_all = TRUE)

wkcoop = 
  coopbs %>% 
  filter(!is.na(LONGITUDE)) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>% 
  st_transform(crs = civ_crs) %>% 
  st_buffer(dist = dist_meters_threshold)
  
  
# EXTRACT IN COOPS' BUFFERS ------------
  
# export to GEE and re-import here.  
  
  
# RASTERIZE -----------------
# Make raster template 
# The size of the grid is an important parameter. 
# 12km is because 75% of cocoa producers surveyed by the JRC have their cocoa plots less than 6km away from their house.
# The average location of the producer in a grid cell is in it's centroid. Taking 6km away in any direction from the center implies 12km. 
# Hence, there is 
grid_size_m = 3000

tmplt = rast(extent = ext(departements), 
             resolution = grid_size_m,
             crs = crs(departements))
tmplt

terra::plot(tmplt, grid = TRUE)




  
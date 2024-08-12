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

# Assets and functions -----------------------------------------------------
civ_crs <- 4226

coopbsy = read.csv(
  file = here("temp_data/private_IC2B/IC2B_v2_coop_bs_year.csv"))

coopbs = 
  coopbsy %>% 
  distinct(COOP_BS_ID, .keep_all = TRUE)

# Cargill link data
carg_links = read.csv(here("temp_data", "preprocessed_cargill", "cargill_links_standardized.csv"))
  
# JRC link data
jrc_links = read.csv(here("temp_data", "preprocessed_jrc_data", "jrc_links_standardized.csv"))

# Sustain-cocoa link data
sc_links = read.csv(here("temp_data", "preprocessed_sustain_cocoa", "sustain_cocoa_links_standardized.csv"))


# CONSOLIDATE LINK DATA ---------------------

# Make template for the data frame master, to frame the consolidation of every specific data set. 
master <- data.frame(
  "YEAR" = NA, 
  "PRO_ID" = NA, 
  "COOP_BS_ID" = NA, 
  "DISTANCE_PRO_ITM" = NA, 
  "PRO_DEPARTMENT_GEOCODE" = NA, 
  "PRO_DEPARTMENT_NAME" = NA,
  "PRO_LONGITUDE" = NA, 
  "PRO_LATITUDE" = NA
)

# LINK STATS ----------------

# Latest surveyed year, to take the panel of coops at this time.  
latest_survey_year = max(consol$YEAR)

# 
dist_meters_threshold = quantile(consol$LINK_DISTANCE_METERS, 0.9)


# BUFFER COOPS 

wkcoop = 
  coopbs %>% 
  filter(YEAR == latest_survey_year)
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = civ_crs) %>% 
  st_buffer(dist = dist_meters_threshold)
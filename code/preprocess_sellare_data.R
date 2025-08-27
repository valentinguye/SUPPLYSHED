# Purpose of this script: pre-process JRC survey data provided for the supply shed project
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
library(here)
library(readstata13)
library(sjmisc)
library(units)

# Will write preprocessed data there
dir.create(here("temp_data", "preprocessed_jrc_data"))

# load in particular the function fn_trader_to_group_names, str_trans, ... 
source(here("code", "USEFUL_STUFF_manually_copy_pasted.R"))

# READ ------------ 

jor = read.csv(here("input_data", "jorge_sellare", "coop_clean full data.csv"))



# Departements (districts)
departements <- s3read_using(
  object = "cote_divoire/spatial/BOUNDARIES/DEPARTEMENT/OUT/CIV_DEPARTEMENTS.geojson",#"cote_divoire/spatial/BOUNDARIES/DEPARTEMENT/OUT/ci_departments_wgs84_level4.geojson", 
  bucket = "trase-storage",
  FUN = read_sf,
  #sheet = "Cacao", 
  #skip = 3,
  opts = c("check_region" = T)
)

# Purpose of this script: pre-process CARGILL scraped data 
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

# Functions 
fn_clean_abrvname3 <- function(col_name){
  gsub(pattern = "COOP CA | COOP CA$|COOP-CA | COOP-CA$|COOPCA | COOPCA$|COOPCA-|-COOPCA$|COOP-CA-|-COOP-CA$|COOP | COOP$|COOP-|-COOP$|SCOOP | SCOOP$|SCOOP-|-SCOOP$|SCOOPS | SCOOPS$|SCOOPS-|-SCOOPS$", 
       replacement = "", 
       x = col_name)
}

grep("COOPCA", ic2b$DISCL_SUPPLIER_ABRVNAME, value = T) %>% unique()


# READ ------------ 

carg <- read_sf(here("input_data", "s3", "logistics", "originals", "CARGILL", "2023-03-20-cargill_farms_CIV_2019-2020.geojson"))

carg <- 
  carg %>% 
  st_transform(crs = 4226) # https://epsg.io/4226

sf_use_s2(FALSE)
carg_pt <- 
  carg %>% 
  # st_simplify() %>%
  st_centroid()

ggplot()+
  geom_sf(data = carg_pt, fill = "black", alpha = 0.2) +
  geom_sf(data = carg, col = "red") 


## match with IC2B to get coop coordinates -----

ic2b <- 
  read.csv(
    file = here("temp_data/private_IC2B/IC2B_v2_coop_bs_year.csv"))

carg = 
  carg %>% 
  mutate(SIMPLIF_COOPERATIV = fn_clean_abrvname3(COOPERATIV)) # This function removes generic terms like CA COOP and SCOOPS

intersect(unique(carg$SIMPLIF_COOPERATIV), unique(ic2b$SUPPLIER_ABRVNAME))

# this one needs to be adjusted manually
grep("EDIFIE|DOUKOUYA", ic2b$SUPPLIER_ABRVNAME, value = T) %>% unique()
unique(carg$SIMPLIF_COOPERATIV)

carg = 
  carg %>% 
  mutate(SIMPLIF_COOPERATIV = case_when(
    SIMPLIF_COOPERATIV == "EDIFIE-DOUKOUYA" ~ "EDIFIE DOUKOUYA", 
    TRUE ~ SIMPLIF_COOPERATIV
  ))
intersect(unique(carg$SIMPLIF_COOPERATIV), unique(ic2b$SUPPLIER_ABRVNAME))

# join 
# limit ic2b to unique rows
# 2408 and 2409 I don't understand why they get diff coop ids, they have same abrv name, (homogenized) full name and district... 
# 2581 it's a matter of deciding which of the 2 buying stations with coordinates we choose... 

carg_coop_pt =
  carg %>% 
  st_drop_geometry() %>% 
  left_join(ic2b %>% select(SUPPLIER_ABRVNAME, LONGITUDE, LATITUDE), 
            by = join_by("SIMPLIF_COOPERATIV" == "SUPPLIER_ABRVNAME"))

# ic2b %>% filter(SUPPLIER_ABRVNAME != SIMPLIF_ABRVNAME)
# 
# cam <-
# read.csv2(
#   file = here("input_data/s3/logistics/out/CAM_V4/CAM_coopyear.csv"))
# cam %>% filter(SUPPLIER_ABRVNAME != SIMPLIF_ABRVNAME)



# Departements (districts)
departements <- s3read_using(
  object = "cote_divoire/spatial/BOUNDARIES/DEPARTEMENT/OUT/CIV_DEPARTEMENTS.geojson",#"cote_divoire/spatial/BOUNDARIES/DEPARTEMENT/OUT/ci_departments_wgs84_level4.geojson", 
  bucket = "trase-storage",
  FUN = read_sf,
  #sheet = "Cacao", 
  #skip = 3,
  opts = c("check_region" = T)
)

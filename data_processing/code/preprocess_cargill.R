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
dir.create(here("temp_data", "preprocessed_cargill"))

# Assets and functions --------------
# this is a geodetic crs, i.e. not projecting to a plan. But I haven't found a proj one. for Côte d'Ivoire for now. 
civ_crs = 4226 # https://epsg.io/4226 

# load in particular the function fn_trader_to_group_names, str_trans, ... 
source(here("code", "USEFUL_STUFF_manually_copy_pasted.R"))

# Functions 
fn_clean_abrvname3 <- function(col_name){
  gsub(pattern = "COOP CA | COOP CA$|COOP-CA | COOP-CA$|COOPCA | COOPCA$|COOPCA-|-COOPCA$|COOP-CA-|-COOP-CA$|COOP | COOP$|COOP-|-COOP$|SCOOP | SCOOP$|SCOOP-|-SCOOP$|SCOOPS | SCOOPS$|SCOOPS-|-SCOOPS$", 
       replacement = "", 
       x = col_name)
}

# grep("COOPCA", coopbsy$DISCL_SUPPLIER_ABRVNAME, value = T) %>% unique()

# Departements (districts)
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


# CARGILL data (scraped)
carg <- read_sf(here("input_data", "s3", "logistics", "originals", "CARGILL", "2023-03-20-cargill_farms_CIV_2019-2020.geojson"))
carg <- 
  carg %>% 
  st_transform(crs = civ_crs) 

# IC2B (private) 
coopbsy <- 
  read.csv(
    file = here("temp_data/private_IC2B/IC2B_v2_coop_bs_year.csv"))


# Farm centroids -------

sf_use_s2(FALSE)
carg_pt <- 
  carg %>% 
  st_simplify(dTolerance = 0.0001) %>% # 1 decimal degree = 111.1 km at equator
  summarise(
    .by = FARMER_COD,
    COOPERATIV = unique(COOPERATIV),  
    geometry = st_union(geometry)
    ) %>% 
  st_centroid()

carg_pt %>% nrow() == carg$FARMER_COD %>% unique() %>% length()
carg_pt$COOPERATIV %>% unique()
all.equal(st_crs(carg), st_crs(carg_pt))

ggplot()+
  geom_sf(data = carg_pt, fill = "black", alpha = 0.2) +
  geom_sf(data = carg, col = "red")  +
  geom_sf(data = departements, fill = "transparent")

# Attribute a department name to cargill FARM POINT data (not coops)
# Join by st_within
sf_use_s2(FALSE)
carg_pt <- st_join(carg_pt,
                   departements[,c("LVL_4_CODE", "LVL_4_NAME")],
                   join = st_intersects) 

if(nrow(carg_pt) != length(unique(carg_pt$FARMER_COD))){stop("the join with departments probably adds rows")}

carg_department_geocodes = carg_pt$LVL_4_CODE %>% unique()

st_is_longlat(carg_pt$geometry)
st_is_longlat(departements$geometry)
st_is_longlat(carg$geometry)


# Remove geometry to be able to dplyr join, but keep coordinates
carg_pt = 
  carg_pt %>% 
  rowwise() %>% 
  mutate(FARM_LONGITUDE = unlist(geometry)[1],
         FARM_LATITUDE = unlist(geometry)[2]) %>% 
  filter(!st_is_empty(geometry)) %>% 
  st_drop_geometry() %>% 
  # also remove the few farms in Yamoussoukro which otherwise get matched to Socaan which is several departments away
  filter(!LVL_4_NAME%in%c("YAMOUSSOUKRO", "AGNIBILEKRO"))

# carg_pt$geometry[[1]][1]

carg_pt$FARM_LATITUDE %>% summary()
carg_pt$FARM_LONGITUDE %>% summary() # this is in civ_crs 


# Join with IC2B to get coop coordinates -----

## Prepare joining keys ----------
carg_pt = 
  carg_pt %>% 
  mutate(SIMPLIF_COOPERATIV = fn_clean_abrvname3(COOPERATIV)) # This function removes generic terms like CA COOP and SCOOPS

unique(carg_pt$SIMPLIF_COOPERATIV)
intersect(unique(carg_pt$SIMPLIF_COOPERATIV), unique(coopbsy$SIMPLIF_ABRVNAME))

# this one needs to be adjusted manually
grep("EDIFIE|DOUKOUYA", coopbsy$SUPPLIER_ABRVNAME, value = T) %>% unique()
unique(carg_pt$SIMPLIF_COOPERATIV)

carg_pt = 
  carg_pt %>% 
  mutate(SIMPLIF_COOPERATIV = case_when(
    SIMPLIF_COOPERATIV == "EDIFIE-DOUKOUYA" ~ "EDIFIE DOUKOUYA", 
    TRUE ~ SIMPLIF_COOPERATIV
  ))
intersect(unique(carg_pt$SIMPLIF_COOPERATIV), unique(coopbsy$SIMPLIF_ABRVNAME))


# join 

# limit coopbsy to unique rows and buying stations which it makes sense to match. 
bs_carg =
  coopbsy %>% 
  distinct(COOP_BS_ID, .keep_all = TRUE) %>% 
  # and useful rows, i.e. those that have coordinates
  filter(!is.na(LONGITUDE)) %>% 
  filter(grepl("CARGILL", DISCLOSURE_SOURCES)) %>% 
  filter(SIMPLIF_ABRVNAME %in% carg_pt$SIMPLIF_COOPERATIV & 
         DISTRICT_GEOCODE %in% carg_department_geocodes) 

# Some coops have the same acronym, but are clearly different, e.g. not in the same department. 
# e.g. BENKADI
bs_toview = 
  bs_carg %>% 
  # just reproduce rounded coords
  mutate(ROUND_LONGITUDE = round(LONGITUDE, 2), 
         ROUND_LATITUDE = round(LATITUDE, 2)) %>% 
  select(COOP_ID, COOP_BS_ID, SIMPLIF_ABRVNAME, SUPPLIER_FULLNAME, ROUND_LATITUDE, ROUND_LONGITUDE, DISTRICT_NAME, everything()) %>% 
  arrange(COOP_ID)
View(bs_toview)

carg_pt %>% filter(SIMPLIF_COOPERATIV=="BENKADI") %>% pull(LVL_4_NAME) %>% unique()
carg_pt$LVL_4_NAME %>% unique()

# EDIFIE-DOUKOUYA we need to choose which of the two BS it is.  
# COOPERATIVE DE DEVELOPPEMENT AGRICOLE DE YAKASSE- ME is apparently a different coop than COOPERATIVE AGRICOLE DE YAKASE ATTOBROU, although they are both CAYAT. 
# only one is disclosed by cargill anyway. 
if(bs_carg$COOP_ID %>% unique() %>% length() != length(unique(carg_pt$COOPERATIV))){stop("the merge wont be correct")}

# For 3 coops, there are more than one buying station. 

# multiple matches are expected, because 3 of the 6 coops have several buying stations in IC2B.
carg_pt_bs =
  carg_pt %>% 
  left_join(bs_carg %>% select(SIMPLIF_ABRVNAME, BS_LONGITUDE = LONGITUDE, BS_LATITUDE = LATITUDE, 
                               COOP_BS_ID), # to be able to match back to IC2B. 
            by = join_by("SIMPLIF_COOPERATIV" == "SIMPLIF_ABRVNAME"), 
            multiple = "all") 

carg_pt_sfbs = 
  carg_pt_bs %>% 
  st_as_sf(coords = c("BS_LONGITUDE", "BS_LATITUDE"), crs = 4326) %>% 
  st_transform(civ_crs) # change rien. 
# bs_carg %>% filter(SUPPLIER_ABRVNAME != SIMPLIF_ABRVNAME)

# start from the merger, for st_distance to work on same size df. 
carg_sfpt_bs = 
  carg_pt_bs %>% 
  st_as_sf(coords = c("FARM_LONGITUDE", "FARM_LATITUDE"), crs = civ_crs) 


ggplot()+
  geom_sf(data = carg_sfpt_bs, col = "grey")  +
  geom_sf(data = carg_pt_sfbs, aes(col=SIMPLIF_COOPERATIV))  +
  geom_sf(data = departements, fill = "transparent")

ggplot()+
    geom_sf(data = carg_sfpt_bs, aes(col = SIMPLIF_COOPERATIV))  +
              geom_sf(data = carg_pt_sfbs, col="black")  +
              geom_sf(data = departements, fill = "transparent")

carg_pt_bs$DISTANCE_PRO_ITM <- 
  st_distance(carg_sfpt_bs, carg_pt_sfbs, by_element = TRUE)

# Keep only the closest buying station of the linked coop, from a given farm, i.e. across matched buying stations, which are all of the same coop.  
carg_pt_closestbs =
  carg_pt_bs %>% 
  group_by(FARMER_COD) %>% 
  mutate(SMALLEST_DIST = min(DISTANCE_PRO_ITM)) %>% 
  ungroup() %>% 
  filter(DISTANCE_PRO_ITM == SMALLEST_DIST)

# check that this goes back to the number of plots before we merged with multiple BS
if(nrow(carg_pt_closestbs) != nrow(carg_pt)){stop('going back not correct')}

# Export ----
carg %>% 
  filter(grepl("DATE:2020", LOCATION_N)) %>% 
  nrow()
# the latest year seems to be 2019

toexport = 
  carg_pt_closestbs %>% 
  mutate(YEAR = 2019, 
         FARM_ID = paste0("CARGILL_",FARMER_COD)) %>% 
  select(YEAR, FARM_ID, COOP_BS_ID, DISTANCE_PRO_ITM, 
         FARM_DEPARTMENT_GEOCODE = LVL_4_CODE, 
         FARM_DEPARTMENT_NAME = LVL_4_NAME,
         FARM_LONGITUDE, FARM_LATITUDE)
  

write_csv(toexport,
          file = here("temp_data", "preprocessed_cargill", "cargill_links_standardized.csv"),
          na = "NA", 
          append = FALSE, 
          col_names = TRUE)

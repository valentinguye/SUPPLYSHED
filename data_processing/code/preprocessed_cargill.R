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
# use the projected CRS used by BNETD for their 2020 land use map. 
civ_crs <- 32630


# load in particular the function fn_trader_to_group_names, str_trans, ... 
source(here("code", "USEFUL_STUFF_supplyshedproj.R"))


# Departements (districts)
departements <- read_sf("input_data/s3/CIV_DEPARTEMENTS.geojson")

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
carg_pt =
  carg %>% 
  # Remove the few farms in Yamoussoukro which otherwise get matched to Socaan which is several departments away
  # (do it before centroids grouped by farmers)
  filter(!FID%in%c(1156, 1176, 1181, 3076, 5053, 1173, 1160)) %>% 
  filter(!st_is_empty(geometry))
# those in Yamoussoukro get also filtered with a st_simplify 
# carg_pt %>%
#   filter(LVL_4_NAME%in%c("YAMOUSSOUKRO", "AGNIBILEKRO")) %>%
#   pull(FID)

# Descriptive stats on farm bbox
sf_use_s2(TRUE)
carg_farm = 
  carg_pt %>% 
  # here it is projected so we can give it in meters. in lon/lat, this needs to be expressed in decimal degree and 1 decimal degree = 111.1 km at equator, so 0.0001 is good. 
  st_simplify(dTolerance = 0.001) %>%
  summarise(
    .by = FARMER_COD,
    geometry = st_union(geometry)
  )
carg_farm %>% filter(st_is_empty(geometry))

carg_bbx = 
  carg_farm %>% 
  rowwise() %>% 
  mutate(FARM_BBOX_HA = set_units(st_area(st_as_sfc(st_bbox(geometry))), "ha", na.rm = T))

carg_bbx$FARM_BBOX_HA %>% summary()
quantile(carg_bbx$FARM_BBOX_HA, 0.95)

carg_pt$geometry
sf_use_s2(TRUE)
carg_pt = 
  carg_pt %>% 
  # here it is projected so we can give it in meters. in lon/lat, this needs to be expressed in decimal degree and 1 decimal degree = 111.1 km at equator, so 0.0001 is good. 
  # I simplify to the minimum (smallest possible tolerance value), for the operation to run but without 
  # returning any empty geometry (which would be the case for instance with a 1m tolerance) 
  st_simplify(dTolerance = 0.001) %>%
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
  st_transform(crs = 4326) %>% 
  rowwise() %>% 
  mutate(PRO_LONGITUDE = unlist(geometry)[1],
         PRO_LATITUDE = unlist(geometry)[2]) %>% 
  filter(!st_is_empty(geometry)) %>% 
  st_drop_geometry()

# carg_pt$geometry[[1]][1]

carg_pt$PRO_LATITUDE %>% summary()
carg_pt$PRO_LONGITUDE %>% summary() # this is in lon/lat


# Join with IC2B -----

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


## Prepare IC2B to join --------

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


## Join links with IC2B -------

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
  st_as_sf(coords = c("BS_LONGITUDE", "BS_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 
# bs_carg %>% filter(SUPPLIER_ABRVNAME != SIMPLIF_ABRVNAME)

# start from the merger, for st_distance to work on same size df. 
carg_sfpt_bs = 
  carg_pt_bs %>% 
  st_as_sf(coords = c("PRO_LONGITUDE", "PRO_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 


ggplot()+
  geom_sf(data = carg_sfpt_bs, col = "grey")  +
  geom_sf(data = carg_pt_sfbs, aes(col=SIMPLIF_COOPERATIV))  +
  geom_sf(data = departements, fill = "transparent")

ggplot()+
    geom_sf(data = carg_sfpt_bs, aes(col = SIMPLIF_COOPERATIV))  +
    geom_sf(data = carg_pt_sfbs, col="black")  +
    geom_sf(data = departements, fill = "transparent")

carg_pt_bs$LINK_DISTANCE_METERS <- 
  st_distance(carg_sfpt_bs, carg_pt_sfbs, by_element = TRUE)

# Keep only the closest buying station of the linked coop, from a given farm, i.e. across matched buying stations, which are all of the same coop.  
# (this is ok to group just by farmer, because the info Cargill gives is a single coop that every farmer supplies.)
carg_pt_closestbs =
  carg_pt_bs %>% 
  group_by(FARMER_COD) %>% 
  mutate(SMALLEST_DIST = min(LINK_DISTANCE_METERS)) %>% 
  ungroup() %>% 
  filter(LINK_DISTANCE_METERS == SMALLEST_DIST)

# check that this goes back to the number of plots before we merged with multiple BS
if(nrow(carg_pt_closestbs) != nrow(carg_pt)){stop('going back not correct')}

# Link ID ------------

# Create a link ID that uniquely identifies them
carg_pt_closestbs = 
  carg_pt_closestbs %>% 
  # group by buyer simplif name and coop bs id necessary because the latter is many times NAs for HH not matched with IC2B.
  group_by(FARMER_COD, COOP_BS_ID) %>% 
  mutate(LINK_ID = cur_group_id()) %>% 
  ungroup()

stopifnot(carg_pt_closestbs$LINK_ID %>% unique() %>% length() == nrow(carg_pt_closestbs))

# Export ----
carg %>% 
  filter(grepl("DATE:2020", LOCATION_N)) %>% 
  nrow()
# the latest year seems to be 2019

toexport = 
  carg_pt_closestbs %>% 
  mutate(YEAR = 2019, 
         DATA_SOURCE = "CARGILL",
         PRO_ID = paste0("CARGILL_FARMER_",FARMER_COD),
         ACTUAL_LINK_ID = paste0("CARGILL_FARMER_",LINK_ID), 
         BUYER_IS_COOP = TRUE) %>% 
  select(YEAR, PRO_ID, COOP_BS_ID,  
         ACTUAL_LINK_ID,
         BUYER_IS_COOP,
         BS_LONGITUDE, BS_LATITUDE,
         LINK_DISTANCE_METERS, 
         # PRO_DEPARTMENT_GEOCODE = LVL_4_CODE, 
         # PRO_DEPARTMENT_NAME = LVL_4_NAME,
         PRO_LONGITUDE, PRO_LATITUDE)
  

write_csv(toexport,
          file = here("temp_data", "preprocessed_cargill", "cargill_links_standardized.csv"),
          na = "NA", 
          append = FALSE, 
          col_names = TRUE)

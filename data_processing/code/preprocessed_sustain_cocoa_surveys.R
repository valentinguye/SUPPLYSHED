

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

# Will write preprocessed data there
dir.create(here("temp_data", "preprocessed_sustain_cocoa"))

# Assets and functions --------------
# use the projected CRS used by BNETD for their 2020 land use map. 
civ_crs <- 32630

# load in particular the function fn_trader_to_group_names, str_trans, ... 
source(here("code", "USEFUL_STUFF_manually_copy_pasted.R"))


vil = read_xlsx(here("input_data", "sustain_cocoa", "surveys_civ", "Village_level_survey_Cote_dIvoire_-_all_versions_-_False_-_2023-12-04-14-44-13.xlsx"), 
                    sheet = "Village level survey_Cote d'...", 
                    col_names = TRUE) %>% 
  rename(VILLAGE_SURVEY_ID = `_index`) 

surv_coop = read_xlsx(here("input_data", "sustain_cocoa", "surveys_civ", "Village_level_survey_Cote_dIvoire_-_all_versions_-_False_-_2023-12-04-14-44-13.xlsx"), 
                 sheet = "SSI_cooperative_overview", 
                 col_names = TRUE) %>% 
  rename(VILLAGE_SURVEY_ID = `_parent_index`, 
         COOP_SURVEY_ID = `_index`) %>% 
  mutate(BUYER_TYPE = "COOPERATIVE")

trait = read_xlsx(here("input_data", "sustain_cocoa", "surveys_civ", "Village_level_survey_Cote_dIvoire_-_all_versions_-_False_-_2023-12-04-14-44-13.xlsx"), 
                  sheet = "SSI_traitant_overview", 
                  col_names = TRUE) %>% 
  rename(VILLAGE_SURVEY_ID = `_parent_index`, 
         TRAITANT_SURVEY_ID = `_index`) %>% 
  mutate(BUYER_TYPE = "TRAITANT")

choices = read_xlsx(here("input_data", "sustain_cocoa", "surveys_civ", "ref_survey_041223.xlsx"), 
                  sheet = "choices", 
                  col_names = TRUE) 

# IC2B (private) 
# filter to 2021 because we need a cross-section and it's the year when survey data is valid for 
# and there is almost only 1-1 matches between surveyed and IC2B coops in 2021, while there are 
# many 1-multiple matches with IC2B 2022 or later. It is resolved anyway by a closest BS condition, 
# but it's probably just more correct to match before many homonym coops are "created". 

# ic2b = read.csv(here("input_data", "dataverse_files", "IC2B_coopyear.csv")) %>% 
coopbs <- 
  read.csv(
    file = here("temp_data/private_IC2B/IC2B_v2_coop_bs_year.csv")) %>% 
  filter(YEAR == 2021) 
  

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


# Join with IC2B -----

# Join on abbreviated name to add the coordinates to the survey data.

## Prepare joining keys ----------

allcoop = 
  choices %>% 
  filter(list_name == "cooperative") %>% 
  select(COOP_ABRV_NAME = `label::Francais (fr)`, 
         COOP_SURVEY_KEY = name) %>% 
  filter(!COOP_SURVEY_KEY %in% c("DK", "other"))
# allcoop %>% filter(`label::Francais (fr)` != `label::English (en)`)
# allcoop$label %>% unique()

surv_coop = 
  surv_coop %>% 
  # merged on surv_coop key in survey to add the surv_coop name 
  left_join(allcoop, 
            by = join_by("ssicooperative_name" == "COOP_SURVEY_KEY")) %>% 
  # When the key is missing, no name is matched. In this case, take the name responded in 'other'
  mutate(COOP_ABRV_NAME = case_when(
    is.na(COOP_ABRV_NAME) ~ ssicooperative_other, 
    TRUE ~ COOP_ABRV_NAME
  )) %>% 
  filter(!is.na(COOP_ABRV_NAME)) %>% 
  # clean these abbreviated names like they were cleaned for IC2B
  mutate(COOP_SIMPLIF_ABRV_NAME = fn_clean_abrvname3(fn_clean_abrvname2(fn_clean_abrvname1(str_trans(str_squish(COOP_ABRV_NAME))))))

surv_coop %>% 
  distinct(COOP_ABRV_NAME, .keep_all = T) %>% 
  select(COOP_ABRV_NAME, COOP_SIMPLIF_ABRV_NAME) %>% 
  View()

simplif_names = unique(surv_coop$COOP_SIMPLIF_ABRV_NAME)

## Prepare IC2B to join --------

# limit coopbs to unique rows and buying stations which it makes sense to match. 
coopbs_tomatch = 
  coopbs %>% 
  # and useful rows, i.e. those that have coordinates
  filter(!is.na(LONGITUDE)) %>% 
  filter(SIMPLIF_ABRVNAME %in% simplif_names) %>% 
  # just reproduce rounded coords
  mutate(ROUND_LONGITUDE = round(LONGITUDE, 2), 
         ROUND_LATITUDE = round(LATITUDE, 2)) %>% 
  select(COOP_ID, COOP_POINT_ID, COOP_BS_ID, SIMPLIF_ABRVNAME, SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, ROUND_LATITUDE, ROUND_LONGITUDE, DISTRICT_NAME, everything())  %>% 
  arrange(SIMPLIF_ABRVNAME)
  
# we want to avoid homonyms in simplif abrv names, but here it's not possible to arbitrate which is the actual surv_coop, 
# because we have no more info and there are many homonyms in some cases (more than 4). 
coopbs_tomatch %>% 
  group_by(SIMPLIF_ABRVNAME) %>% 
  mutate(N_DIFF_BS = length(unique(COOP_BS_ID))) %>% 
  ungroup() %>%   
  arrange(desc(N_DIFF_BS)) %>% 
  select(N_DIFF_BS, everything()) %>% 
  View()


## Join coop survey with IC2B -------

#  To give IC2B attributes (at bs-level) to survey coop data 
surv_coop_bs = 
  surv_coop %>% 
  # inner_join because we are not interested in keeping coops matched with nothing in IC2B here. 
  inner_join(coopbs_tomatch, 
            by = join_by("COOP_SIMPLIF_ABRV_NAME" == "SIMPLIF_ABRVNAME"), 
            multiple = "all") # multiple matches are expected, coops have several buying stations in IC2B.

# Options to handle multiple matches (homonyms in abbreviated names)
# - resolve manually
# - apply distance threshold
# - apply nearest match 
# Currently we do nearest match. 

coopbs_tomatch$COOP_BS_ID %>% unique() %>% length()
surv_coop_bs$COOP_BS_ID %>% unique() %>% length()
# names(surv_coop_bs)
surv_coop_bs %>% 
  select(COOP_ID, COOP_POINT_ID, COOP_BS_ID, COOP_SURVEY_ID, VILLAGE_SURVEY_ID, ssicooperative_name, ssicooperative_other, 
         COOP_ABRV_NAME, 
         SUPPLIER_ABRVNAME, COOP_SIMPLIF_ABRV_NAME, SUPPLIER_FULLNAME, 
         LONGITUDE, LATITUDE) %>% 
  View()
  

## Join coop survey with village survey ---------
# ... to make links

vil_bs =
  vil %>% 
  inner_join(surv_coop_bs %>% select(VILLAGE_SURVEY_ID, COOP_SIMPLIF_ABRV_NAME, BS_LONGITUDE = LONGITUDE, BS_LATITUDE = LATITUDE, 
                               COOP_BS_ID), # to be able to match back to IC2B. 
            by = "VILLAGE_SURVEY_ID", # "COOP_SIMPLIF_ABRV_NAME" == "SIMPLIF_ABRVNAME"
            multiple = "all") 


# Spatialize -----------
sfvil_bs = 
  vil_bs %>% 
  mutate(
    `_loc_location_x_y_longitude` = as.numeric(`_loc_location_x_y_longitude`),
    `_loc_location_x_y_latitude` = as.numeric(`_loc_location_x_y_latitude`),
    `loc_x_1` = as.numeric(`loc_x_1`),
    `loc_Y_1` = as.numeric(`loc_Y_1`),
    PRO_LONGITUDE = case_when(
      is.na(`_loc_location_x_y_longitude`) ~ loc_x_1, 
      TRUE ~ `_loc_location_x_y_longitude`
    ),
    PRO_LATITUDE = case_when(
      is.na(`_loc_location_x_y_latitude`) ~ loc_Y_1, 
      TRUE ~ `_loc_location_x_y_latitude`
    )
  ) %>% 
  filter(!is.na(PRO_LONGITUDE)) %>% 
  # remove outlier 
  filter(PRO_LATITUDE > 4 & PRO_LONGITUDE < 2) %>%
  st_as_sf(coords = c("PRO_LONGITUDE", "PRO_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 

nrow(vil_bs)
nrow(sfvil_bs)

### Add departments
sfvil_bs <- st_join(sfvil_bs,
                   departements[,c("LVL_4_CODE", "LVL_4_NAME")],
                   join = st_intersects) 

## Make distance var ------------

# start from the above, not the merger
vil_sfbs = 
  sfvil_bs %>% 
  st_drop_geometry() %>% 
  # filter(!is.na(BS_LONGITUDE)) %>% 
  st_as_sf(coords = c("BS_LONGITUDE", "BS_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 

nrow(vil_sfbs)

ggplot()+
  geom_sf(data = sfvil_bs, col = "grey")  +
  geom_sf(data = vil_sfbs, aes(col=COOP_SIMPLIF_ABRV_NAME))  +
  geom_sf(data = departements, fill = "transparent")

ggplot()+
  geom_sf(data = sfvil_bs, aes(col = COOP_SIMPLIF_ABRV_NAME))  +
  geom_sf(data = vil_sfbs, col="black")  +
  geom_sf(data = departements, fill = "transparent")

vil_sfbs$LINK_DISTANCE_METERS <- 
  st_distance(sfvil_bs, vil_sfbs, by_element = TRUE)

summary(vil_sfbs$LINK_DISTANCE_METERS)

vil_bs = 
  vil_sfbs %>% 
  st_drop_geometry()

vil$VILLAGE_SURVEY_ID %>% unique() %>% length() == nrow(vil)
vil_bs$VILLAGE_SURVEY_ID %>% unique() %>% length() == nrow(vil_bs)

# For a given village, keep only the closest buying station of the closest coop among the possibly several 
# buying stations and possibly several coops matched based on the abbreviated name, 
vil_bs_closestbs =
  vil_bs %>% 
  group_by(VILLAGE_SURVEY_ID) %>% 
  mutate(SMALLEST_DIST = min(LINK_DISTANCE_METERS)) %>% 
  ungroup() %>% 
  filter(LINK_DISTANCE_METERS == SMALLEST_DIST)

# check that this goes back to one row per village 
vil_bs_closestbs$VILLAGE_SURVEY_ID %>% unique() %>% length() 
nrow(vil_bs_closestbs) 
# vil_bs %>% 
#   filter(VILLAGE_SURVEY_ID==15) %>% 
#   distinct()
# there is one remaining duplicate on everything, I don't understand why. I move on. 
vil_bs_closestbs = 
  vil_bs_closestbs %>% 
  distinct(VILLAGE_SURVEY_ID, .keep_all = TRUE)

vil_bs_closestbs$VILLAGE_SURVEY_ID %>% unique() %>% length() 
nrow(vil_bs_closestbs) 


# Export ----

toexport = 
  vil_bs_closestbs %>% 
  mutate(YEAR = 2021, 
         DATA_SOURCE = "SUSTAINCOCOA",
         PRO_ID = paste0("SUSTAINCOCOA_VILLAGE_",VILLAGE_SURVEY_ID)) %>% 
  select(YEAR, PRO_ID, COOP_BS_ID, 
         BS_LONGITUDE, BS_LATITUDE,
         LINK_DISTANCE_METERS, 
         # PRO_DEPARTMENT_GEOCODE = LVL_4_CODE, 
         # PRO_DEPARTMENT_NAME = LVL_4_NAME,
         PRO_LONGITUDE, PRO_LATITUDE)


write_csv(toexport,
          file = here("temp_data", "preprocessed_sustain_cocoa", "sustain_cocoa_links_standardized.csv"),
          na = "NA", 
          append = FALSE, 
          col_names = TRUE)

## Export with more variables 

potential_coopbs = 
  coopbs %>% 
  mutate(COOP_FARMERS_FT = if_else(is.na(TOTAL_FARMERS_FT), 0, TOTAL_FARMERS_FT),
         COOP_FARMERS_RFA = if_else(is.na(TOTAL_FARMERS_RFA), 0, TOTAL_FARMERS_RFA), 
         
         TRADER_NAMES = if_else(TRADER_NAMES == "" | TRADER_NAMES == " ", NA, TRADER_NAMES),
         DISCLOSURE_SOURCES = if_else(DISCLOSURE_SOURCES == "" | DISCLOSURE_SOURCES == " ", NA, DISCLOSURE_SOURCES),
         CERTIFICATIONS = if_else(CERTIFICATIONS == "" | CERTIFICATIONS == " ", NA, CERTIFICATIONS),
         
         # characterize certifications
         COOP_CERTIFIED_OR_SSI = !is.na(CERTIFICATIONS),
         
         # this removes NAs because grepl("RA", NA) -> FALSE
         COOP_CERTIFIED = grepl("RAINFOREST ALLIANCE|UTZ|FAIRTRADE", CERTIFICATIONS), #|FAIR FOR LIFE|BIOLOGIQUE
         # detail certification
         RFA = grepl("RAINFOREST ALLIANCE", CERTIFICATIONS),
         UTZ = grepl("UTZ", CERTIFICATIONS),
         FT = grepl("FAIRTRADE", CERTIFICATIONS),
         COOP_ONLY_RFA = RFA & !UTZ & !FT,
         COOP_ONLY_UTZ = !RFA & UTZ & !FT,
         COOP_ONLY_FT  = !RFA & !UTZ & FT,
         COOP_RFA_AND_UTZ = RFA & UTZ & !FT,
         COOP_RFA_AND_FT  = RFA & !UTZ & FT,
         COOP_UTZ_AND_FT  = !RFA & UTZ & FT,
         COOP_RFA_AND_UTZ_AND_FT = RFA & UTZ & FT,
         
         COOP_HAS_SSI = grepl("[(]", CERTIFICATIONS), # see fn_standard_certification_names in private_IC2B.R
         
         # characterize buyers
         COOP_N_KNOWN_BUYERS = case_when(
           !is.na(TRADER_NAMES) ~ str_count(TRADER_NAMES, "[+]") + 1, 
           TRUE ~ 0
         )) %>% 
  
  rename(COOP_N_FARMERS = TOTAL_FARMERS,
         COOP_ABRVNAME = SUPPLIER_ABRVNAME, 
         COOP_FULLNAME = SUPPLIER_FULLNAME, 
         COOP_KNOWN_BUYERS = TRADER_NAMES, 
         COOP_DISCLOSURE_SOURCES = DISCLOSURE_SOURCES,
         COOP_KNOWN_CERTIFICATIONS = CERTIFICATIONS 
         # COOP_BS_LONGITUDE = LONGITUDE,
         # COOP_BS_LATITUDE = LATITUDE
         ) %>% 
  select(starts_with("COOP_"), -COOP_POINT_ID)

toexport_2 = 
  vil_bs_closestbs %>% 
  mutate(PRODUCER_BS_DISTANCE_METERS = as.numeric(LINK_DISTANCE_METERS)) %>% 
  left_join(potential_coopbs, 
            by = "COOP_BS_ID") %>% 
  select(starts_with("COOP_"),
         BS_LONGITUDE, BS_LATITUDE,
         PRODUCER_BS_DISTANCE_METERS, 
         PRODUCER_LONGITUDE = PRO_LONGITUDE,
         PRODUCER_LATITUDE = PRO_LATITUDE,
         everything())

toexport_2$COOP_DISCLOSURE_SOURCES %>% unique()

write_csv(toexport_2,
          file = here("temp_data", "preprocessed_sustain_cocoa", "sustaincocoa-privateIC2B_merge.csv"),
          na = "NA", 
          append = FALSE, 
          col_names = TRUE)

### Traitants --------
nrow(vil)

vil = 
  vil %>% 
  mutate(
    `_loc_location_x_y_longitude` = as.numeric(`_loc_location_x_y_longitude`),
    `_loc_location_x_y_latitude` = as.numeric(`_loc_location_x_y_latitude`),
    `loc_x_1` = as.numeric(`loc_x_1`),
    `loc_Y_1` = as.numeric(`loc_Y_1`),
    LONGITUDE = case_when(
      is.na(`_loc_location_x_y_longitude`) ~ loc_x_1, 
      TRUE ~ `_loc_location_x_y_longitude`
    ),
    LATITUDE = case_when(
      is.na(`_loc_location_x_y_latitude`) ~ loc_Y_1, 
      TRUE ~ `_loc_location_x_y_latitude`
    )
  ) %>% 
  filter(!is.na(LONGITUDE)) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

nrow(vil)


links =
  left_join(vil, 
            surv_coop, 
            by = "VILLAGE_SURVEY_ID") 


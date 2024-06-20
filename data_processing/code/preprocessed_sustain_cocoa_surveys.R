

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

# INPUTS ####

vil = read_xlsx(here("input_data", "sustain_cocoa", "surveys_civ", "Village_level_survey_Cote_dIvoire_-_all_versions_-_False_-_2023-12-04-14-44-13.xlsx"), 
                    sheet = "Village level survey_Cote d'...", 
                    col_names = TRUE) %>% 
  rename(VILLAGE_SURVEY_ID = `_index`) 

coop = read_xlsx(here("input_data", "sustain_cocoa", "surveys_civ", "Village_level_survey_Cote_dIvoire_-_all_versions_-_False_-_2023-12-04-14-44-13.xlsx"), 
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

# filter to 2023 because we need a cross-section and it's the year when survey data was collected
ic2b_2023 = read.csv(here("input_data", "dataverse_files", "IC2B_coopyear.csv")) %>% 
  filter(YEAR == 2023)
  

# Pre-process ####
allcoop = 
  choices %>% 
  filter(list_name == "cooperative") %>% 
  select(COOP_ABRV_NAME = `label::Francais (fr)`, 
         COOP_SURVEY_KEY = name) %>% 
  filter(!COOP_SURVEY_KEY %in% c("DK", "other"))
# allcoop %>% filter(`label::Francais (fr)` != `label::English (en)`)
# allcoop$label %>% unique()

coop_extd = 
  coop %>% 
  # merged on coop key in survey to add the coop name 
  left_join(allcoop, 
            by = join_by("ssicooperative_name" == "COOP_SURVEY_KEY")) %>% 
  # When the key is missing, no name is matched. In this case, take the name responded in 'other'
  mutate(COOP_ABRV_NAME = case_when(
    is.na(COOP_ABRV_NAME) ~ ssicooperative_other, 
    TRUE ~ COOP_ABRV_NAME
  )) %>% 
  filter(!is.na(COOP_ABRV_NAME)) %>% 
  # clean these abbreviated names like they were cleaned for IC2B
  # MANY GOOD MATCHES SANS RIEN FAIRE. ON PEUT RESOUDRE CEUX RESTANTS MANUELLEMENT 
  # merge on coop name to add the coordinates
  left_join(ic2b_2023, 
            by = join_by("COOP_ABRV_NAME" == "ABBREVIATED_NAME"))
## THE MULTIPLE MATCHES ARE DUE TO HOMONYMS IN ABBREVIATED DATA. SEE IF FEDE HAS SOMETHING TO SHARE TO HELP RESOLVE
# OTHERWISE, RESOLVE MANUALLY

coop_extd %>% 
  select(COOP_SURVEY_ID, VILLAGE_SURVEY_ID, ssicooperative_name, ssicooperative_other, 
         COOP_ABRV_NAME, FULL_NAME, LONGITUDE, LATITUDE, COOP_STABLE_ID) %>% 
  View()
  
## Spatialize ####
### Villages --------
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

### Cooperatives --------
nrow(coop)

coop = 
  coop %>% 
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

nrow(coop)

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
            coop, 
            by = "VILLAGE_SURVEY_ID") 


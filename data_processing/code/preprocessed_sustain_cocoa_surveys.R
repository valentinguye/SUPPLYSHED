

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
# this is a geodetic crs, i.e. not projecting to a plan. But I haven't found a proj one. for Côte d'Ivoire for now. 
civ_crs = 4226 # https://epsg.io/4226 

# load in particular the function fn_trader_to_group_names, str_trans, ... 
source(here("code", "USEFUL_STUFF_manually_copy_pasted.R"))

# This function handles cases where a full name and an abbreviated name are given in the abrv name variable.
fn_clean_abrvname1 <- function(col_name){
  case_when(
    # grepl("UNITE COOPERATIVE AGRICOLE DE DANANE", col_name) ~ "COOP UDAN",
    # grepl("COOPERATIVE AGRICOLE BACON ESPOIR", col_name) ~ "CABES",
    # grepl("COOPERATIVE AGRICOLE ABOTRE DE NIABLE", col_name) ~ "COAANI",
    # grepl("COOPERATIVE AGRICOLE BENKADI", col_name) ~ "COOPABENKADI",
    # grepl("COOPERATIVE AGRICOLE D'AGNANFOUTOU", col_name) ~ "COESAG",
    # grepl("COOPERATIVE AGRICOLE ZEMESS TAABA DE GBABAM", col_name) ~ "COOPAZEG",
    # grepl("COOPERATIVE DES PRODUCTEURS DE YAKASSE ATTOBROU", col_name) ~ "COOPROYA",
    # grepl("SOCIETE AGRICOLE BINKADI DE BROUDOUGOU PENDA CA", col_name) ~ "SOCABB",
    # grepl("SCOOPAO SOCIETE COOPERATIVE DES AGRICULTEURS DE PETIT OUAGA", col_name) ~ "SCOOPAO",
    # grepl("SOCIETE COOPERATIVE SIMPLIFIEE AGRICOLE KAMBONOU DE ARRAH", col_name) ~ "SCOAKA",
    # grepl("SOCIETE COOPERATIVE ESPERENCE DE KPELEKRO", col_name) ~ "SOCAEK",
    # grepl("SOCIETE COOPERATIVE AVEC CONSEIL D'ADMINISTRATION ENTENTE DE SEGUELA", col_name) ~ "COOP CA ES",
    # grepl("COOPERATIVE DES PRODUCTEURS AGRICOLES DE NIABLE", col_name) ~ "COOP CA PAN",
    # grepl("COOPERATIVE YEYONIAN DU CANTON ANIASSUE", col_name) ~ "COOPYCA",
    # grepl("SOCIETE COOPERATIVE AGRICOLE DE GNATO AVEC CONSEIL D'ADMINISTRATION", col_name) ~ "SOCAG",
    # grepl("SOCIETE COOPERATIVE AGRICOLE DE BAYOTA", col_name) ~ "SOCABA COOP CA",
    # grepl("SPAD GAGNOA", col_name) ~ "SPAD GAGNOA", # ETG adds the name of the manufacturer to some surv_coop names. 
    # grepl("SCAT I|SCAT 1", col_name) ~ "SCAT 1",
    grepl("\\(CA\\)$", col_name) ~ " CA ",
    grepl("\\(COOP\\)$", col_name) ~ " COOP ",
    grepl("\\(SCOOP\\)$", col_name) ~ " SCOOP ",
    TRUE ~ col_name
  )
}

# This function removes some common characters used in abreviated surv_coop names
fn_clean_abrvname2 <- function(col_name){
  cleaned_col <- str_trans(str_trim(col_name))
  cleaned_col <- gsub(pattern = "\\.|[(]|[)]| WAREHOUSE$", "", cleaned_col)
  cleaned_col <- gsub(pattern = "\n|\\_|\\/|-", " ", cleaned_col)
  cleaned_col <- gsub(pattern = "Ô", "O", cleaned_col)
  # cleaned_col <- gsub(pattern = "A N E K", "ANEK", cleaned_col) 
  
  cleaned_col <- str_squish(cleaned_col)
  
  return(cleaned_col)
}

fn_clean_abrvname3 <- function(col_name){
  gsub(pattern = "COOP CA | COOP CA$|COOP-CA | COOP-CA$|COOPCA | COOPCA$|COOPCA-|-COOPCA$|COOP-CA-|-COOP-CA$|COOP | COOP$|COOP-|-COOP$|SCOOP | SCOOP$|SCOOP-|-SCOOP$|SCOOPS | SCOOPS$|SCOOPS-|-SCOOPS$", 
       replacement = "", 
       x = col_name)
}

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
# filter to 2023 because we need a cross-section and it's the year when survey data was collected
# ic2b = read.csv(here("input_data", "dataverse_files", "IC2B_coopyear.csv")) %>% 
coopbs <- 
  read.csv(
    file = here("temp_data/private_IC2B/IC2B_v2_coop_bs_year.csv")) %>% 
  filter(YEAR == 2023) 
  

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
  # MANY GOOD MATCHES SANS RIEN FAIRE. ON PEUT RESOUDRE CEUX RESTANTS MANUELLEMENT 
  # THE MULTIPLE MATCHES ARE DUE TO HOMONYMS IN ABBREVIATED DATA. SEE IF FEDE HAS SOMETHING TO SHARE TO HELP RESOLVE
  # OTHERWISE, RESOLVE by applying a distance threshold below, hardcoding a percentile from the other data sources.  

coopbs_tomatch$COOP_BS_ID %>% unique() %>% length()
surv_coop_bs$COOP_BS_ID %>% unique() %>% length()
# names(surv_coop_bs)
surv_coop_bs %>% 
  select(COOP_ID, COOP_POINT_ID, COOP_BS_ID, COOP_SURVEY_ID, VILLAGE_SURVEY_ID, ssicooperative_name, ssicooperative_other, 
         COOP_ABRV_NAME, 
         SUPPLIER_ABRVNAME, COOP_SIMPLIF_ABRV_NAME, SUPPLIER_FULLNAME, 
         LONGITUDE, LATITUDE) %>% 
  View()
  

## Join village survey with the extended coop survey ---------
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

## Make distance var ------------
vil_sfbs$DISTANCE_PRO_ITM <- 
  st_distance(sfvil_bs, vil_sfbs, by_element = TRUE)

summary(vil_sfbs$DISTANCE_PRO_ITM)

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
  mutate(SMALLEST_DIST = min(DISTANCE_PRO_ITM)) %>% 
  ungroup() %>% 
  filter(DISTANCE_PRO_ITM == SMALLEST_DIST)

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
  mutate(YEAR = 2023, 
         PRO_ID = paste0("SUSTAINCOCOA_VILLAGE_",VILLAGE_SURVEY_ID)) %>% 
  select(YEAR, PRO_ID, COOP_BS_ID, DISTANCE_PRO_ITM, 
         PRO_DEPARTMENT_GEOCODE = LVL_4_CODE, 
         PRO_DEPARTMENT_NAME = LVL_4_NAME,
         PRO_LONGITUDE, PRO_LATITUDE)


write_csv(toexport,
          file = here("temp_data", "preprocessed_sustain_cocoa", "sustain_cocoa_links_standardized.csv"),
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




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
source(here("code", "USEFUL_STUFF_supplyshedproj.R"))


hhs = read.csv(here("input_data", "sustain_cocoa", "surveys_civ", "cocoa_sales_general.csv")) %>% 
  rename(HH_SURVEY_ID = SID) 

hhs_byb = read.csv(here("input_data", "sustain_cocoa", "surveys_civ", "cocoa_sales_bybuyer.csv"))  %>% 
  rename(HH_SURVEY_ID = SID) 

hhs_morevars = read.csv(here("input_data", "sustain_cocoa", "surveys_civ", "respondent_CE_interview_details_270224.csv"))  %>% 
  rename(HH_SURVEY_ID = SID) 


choices = read_xlsx(here("input_data", "sustain_cocoa", "surveys_civ", "ref_survey_041223.xlsx"), 
                    sheet = "choices", 
                    col_names = TRUE) 

# IC2B (private) 
# filter to 2021 because we need a cross-section and it's the year when survey data is valid for 

# ic2b = read.csv(here("input_data", "dataverse_files", "IC2B_coopyear.csv")) %>% 
coopbs <- 
  read.csv(
    file = here("temp_data/private_IC2B/IC2B_v2_coop_bs_year.csv")) %>% 
  filter(YEAR == 2021) 


# Departements (districts)
departements <- read_sf("input_data/s3/CIV_DEPARTEMENTS.geojson")

departements = 
  st_transform(departements, crs = civ_crs)


# Join with IC2B -----

# Join with HOUSEHOLD-level survey

# Join on abbreviated name to add the coordinates of the main buyers reported in the survey

## Prepare joining keys ----------
sc_coop_names = 
  choices %>% 
  filter(list_name == "cooperative") %>% 
  select(COOP_ABRV_NAME = `label::Francais (fr)`, 
         COOP_SURVEY_KEY = name) %>% 
  filter(!COOP_SURVEY_KEY %in% c("DK", "other"))
# sc_coop_names %>% filter(`label::Francais (fr)` != `label::English (en)`)
# sc_coop_names$label %>% unique()


## Prepare joining keys -----------
names(hhs)
glimpse(hhs)
# The coop IDs are in variables buyer_cocoa_name_1, buyer_cocoa_name_2, buyer_cocoa_name_3
# and when value is "other", the name is in buyer_cocoa_name_other_1, buyer_cocoa_name_other_2, buyer_cocoa_name_other_3
# As indicated in ref_quest_CDI_1.7_2024_05_10.xlsx, in tab 'survey': these names are responses to the question:
# "Je vais maintenant vous demander de me dire les noms des acheteurs de cacao à qui vous avez vendu votre cacao pendant la campagne cacaoyère 2022-2023 ?"


# Split rows (one by HH) by the number of buyers they are linked with
hhs_links = 
  hhs %>% 
  # remove unnecessary vars for now, incl. buyer_cocoa_name which is empty
  select(HH_SURVEY_ID, starts_with("buyer_cocoa_name_")) %>% 
  # When the name is "other", give the value reported in the corresponding column
  mutate(
    COOP_ABRV_NAME_1 = case_when(
      buyer_cocoa_name_1 == "other" ~ buyer_cocoa_name_other_1, 
      TRUE ~ buyer_cocoa_name_1
    ),
    COOP_ABRV_NAME_2 = case_when(
      buyer_cocoa_name_2 == "other" ~ buyer_cocoa_name_other_2, 
      TRUE ~ buyer_cocoa_name_2
    ),
    COOP_ABRV_NAME_3 = case_when(
      buyer_cocoa_name_3 == "other" ~ buyer_cocoa_name_other_3, 
      TRUE ~ buyer_cocoa_name_3
    ),
    COOP_ABRV_NAME_4 = case_when(
      buyer_cocoa_name_4 == "other" ~ buyer_cocoa_name_other_4, 
      TRUE ~ buyer_cocoa_name_4
    )
  ) %>% 
  # reshape to have one row per HH-buyer link
  pivot_longer(
    cols = starts_with("COOP_ABRV_NAME_"), 
    values_to = "COOP_ABRV_NAME"
  ) %>% 
  
  # convert the keys responded in buyer_cocoa_name into names
  left_join(sc_coop_names %>% rename(COOP_ABRV_NAME_EXTERN = COOP_ABRV_NAME), 
            by = join_by("COOP_ABRV_NAME" == "COOP_SURVEY_KEY")) %>% 
  mutate(COOP_ABRV_NAME = case_when(
    !is.na(COOP_ABRV_NAME_EXTERN) ~ COOP_ABRV_NAME_EXTERN, 
    TRUE ~ COOP_ABRV_NAME
  )) 
  hhs_links$COOP_ABRV_NAME %>% str_trans() %>% unique()
  
  # Now clean these strings
  hhs_links = 
    hhs_links %>% 
    mutate(COOP_ABRV_NAME = str_trans(COOP_ABRV_NAME), 
           COOP_ABRV_NAME = case_when(
             COOP_ABRV_NAME %in% c("", " ", "NA", "N A", "999", "1", "COOPERATIVE 1", "N[\\]A", "N[/]A", "N[//]A") | grepl("OUBLIE|CONNAIS PAS", COOP_ABRV_NAME) ~ NA, 
             TRUE ~ COOP_ABRV_NAME
           ), 
           COOP_ABRV_NAME = case_when(
             COOP_ABRV_NAME == "SOCIETE COOPERATIVE AGRICOLE DE YAKASSE-ATTOBROU" ~ "COOPROYA", 
             COOP_ABRV_NAME == "COOPERATIVE DES PRODUCTEURS ASSOCIES DE L'AGNEBY" ~ "COOPAA", 
             TRUE ~ COOP_ABRV_NAME
           ),
           IS_BUYER_COOP = if_else(grepl("COOP", COOP_ABRV_NAME), TRUE, NA),
           IS_BUYER_COOP = case_when(
             grepl("PISTEUR|LIBANAIS|ACHETEUR", COOP_ABRV_NAME) ~ FALSE,
             TRUE ~ IS_BUYER_COOP
           ), 
           
           # Make simplified acronyms
           COOP_ABRV_NAME = str_squish(gsub("(COOPERATIVE)|COOPERATIVE|COOPERATIVES|(COOPERATIVE DU VILLAGE)", "", COOP_ABRV_NAME)),
           COOP_SIMPLIF_ABRV_NAME = fn_clean_abrvname3(fn_clean_abrvname2(fn_clean_abrvname1(COOP_ABRV_NAME))), 
           COOP_SIMPLIF_ABRV_NAME = case_when(
             COOP_SIMPLIF_ABRV_NAME == "C A S B" ~ "CASB",
             COOP_SIMPLIF_ABRV_NAME %in% c("", " ", "NA", "N A") ~ NA, 
             TRUE ~ COOP_SIMPLIF_ABRV_NAME
             )
           )
  (simplif_names = sort(unique(hhs_links$COOP_SIMPLIF_ABRV_NAME)))
  
  # this doesn't catch all weird characters but so be it. 
  grep("N[\\]A", x = hhs_links$COOP_ABRV_NAME, value = T)
  grep("N[\]A", x = hhs_links$COOP_ABRV_NAME, value = T)
  
  # coopbs %>% filter(grepl("C A S B", SUPPLIER_ABRVNAME))
  
hhs_links %>% 
  distinct(COOP_ABRV_NAME, .keep_all = T) %>% 
  select(COOP_ABRV_NAME, COOP_SIMPLIF_ABRV_NAME) %>% 
  View()


## Prepare IC2B to join --------

# limit coopbs to unique rows and buying stations which it makes sense to match. 
coopbs_tomatch = 
  coopbs %>% 
  # keep only rows with coordinates (useless otherwise) 
  # and with an abbreviated name to matched on.  
  filter(!is.na(LONGITUDE) & !is.na(SUPPLIER_ABRVNAME)) %>% 
  filter(SIMPLIF_ABRVNAME %in% simplif_names) %>% 
  # just reproduce rounded coords
  # mutate(ROUND_LONGITUDE = round(LONGITUDE, 2), 
  #        ROUND_LATITUDE = round(LATITUDE, 2)) %>% 
  select(COOP_ID, COOP_POINT_ID, COOP_BS_ID, SIMPLIF_ABRVNAME, SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, DISTRICT_NAME,
         # ROUND_LATITUDE, ROUND_LONGITUDE,  
         BS_LONGITUDE = LONGITUDE, BS_LATITUDE = LATITUDE, everything())  %>% 
  arrange(SIMPLIF_ABRVNAME)


## Join coop survey with IC2B -------

#  To give IC2B attributes (at bs-level) to survey coop data 
hhs_links_bs = 
  hhs_links %>% 
  # inner_join to work only on links with coops
  inner_join(coopbs_tomatch, 
             by = join_by("COOP_SIMPLIF_ABRV_NAME" == "SIMPLIF_ABRVNAME"), 
             multiple = "all") # multiple matches are expected, coops have several buying stations in IC2B.

# This introduces duplicates for different reasons: 
# - an abbreviated name in survey data may match several distinct coops with the same abbreviated name 
# - an abbreviated name in survey data may match a single coop but with several buying stations

# Options to handle multiple matches (homonyms in abbreviated names and multiple buying stations)
# - resolve manually
# - apply distance threshold
# - apply nearest match 
# Currently we do nearest match. 

coopbs_tomatch$COOP_BS_ID %>% unique() %>% length()
hhs_links_bs$COOP_BS_ID %>% unique() %>% length()

# names(hhs_links_bs)
hhs_links_bs %>% 
  select(COOP_ID, COOP_POINT_ID, COOP_BS_ID, HH_SURVEY_ID,
         COOP_ABRV_NAME, 
         SUPPLIER_ABRVNAME, COOP_SIMPLIF_ABRV_NAME, SUPPLIER_FULLNAME, 
         BS_LONGITUDE, BS_LATITUDE) %>% 
  View()

# not all households have a link with at least 1 bs
hhs_links_bs$HH_SURVEY_ID %>% unique() %>% length()
hhs_links$HH_SURVEY_ID %>% unique() %>% length()
hhs$HH_SURVEY_ID %>% unique() %>% length()


# Spatialize -----------

sfhhs_links_bs = 
  #hhs_bs
  hhs_links_bs %>% 
  # first, join the village GPS data (this matches perfectly)
  inner_join(hhs_morevars %>% 
               select(HH_SURVEY_ID, intd_village, 
                      X_loc_location_x_y_latitude, X_loc_location_x_y_longitude,
                      loc_y_2, loc_x_2, avlat, avlong), 
             by = "HH_SURVEY_ID") %>% 
  
  mutate(
    X_loc_location_x_y_longitude = as.numeric(X_loc_location_x_y_longitude),
    X_loc_location_x_y_latitude = as.numeric(X_loc_location_x_y_latitude),
    avlong = as.numeric(avlong),
    avlat = as.numeric(avlat),
    PRO_LONGITUDE = case_when(
      is.na(X_loc_location_x_y_longitude) ~ avlong, 
      TRUE ~ X_loc_location_x_y_longitude
    ),
    PRO_LATITUDE = case_when(
      is.na(X_loc_location_x_y_latitude) ~ avlat, 
      TRUE ~ X_loc_location_x_y_latitude
    )
  ) %>% 
  filter(!is.na(PRO_LONGITUDE)) %>% 
  # remove outlier 
  filter(PRO_LATITUDE > 4 & PRO_LONGITUDE < 2) %>%
  st_as_sf(coords = c("PRO_LONGITUDE", "PRO_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 

  # no row lost! 

  # DO SOME 
  # sfhhs_links_bs %>% 
  #   select(HH_SURVEY_ID, intd_village, 
  #          X_loc_location_x_y_latitude, X_loc_location_x_y_longitude,
  #          loc_y_2, loc_x_2, avlat, avlong)
  # 
  # sfhhs_links_bs %>% select(avlat, avlong) %>% summary()
  # sfhhs_links_bs$loc_x_2 %>% unique()
  # sfhhs_links_bs$loc_y_2 %>% unique()
  
  
nrow(hhs_links_bs)
nrow(sfhhs_links_bs)

### Add departments
sfhhs_links_bs <- 
  sfhhs_links_bs %>% 
  st_join(departements[,c("LVL_4_CODE", "LVL_4_NAME")],
          join = st_intersects) 

## Make distance var ------------

# start from the above, not the merger
hhs_links_sfbs = 
  sfhhs_links_bs %>% 
  st_drop_geometry() %>% 
  # filter(!is.na(BS_LONGITUDE)) %>% 
  st_as_sf(coords = c("BS_LONGITUDE", "BS_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 

nrow(hhs_links_sfbs)

ggplot()+
  geom_sf(data = sfhhs_links_bs, col = "grey")  +
  geom_sf(data = hhs_links_sfbs, aes(col=COOP_SIMPLIF_ABRV_NAME))  +
  geom_sf(data = departements, fill = "transparent")

ggplot()+
  geom_sf(data = sfhhs_links_bs, aes(col = COOP_SIMPLIF_ABRV_NAME))  +
  geom_sf(data = hhs_links_sfbs, col="black")  +
  geom_sf(data = departements, fill = "transparent")

hhs_links_sfbs$LINK_DISTANCE_METERS <- 
  st_distance(sfhhs_links_bs, hhs_links_sfbs, by_element = TRUE)

summary(hhs_links_sfbs$LINK_DISTANCE_METERS)

hhs_links_bs = 
  hhs_links_sfbs %>% 
  st_drop_geometry()



# Keep closest BS / homonym coop ---------------
# For a given village, keep only the closest buying station of the closest coop among the possibly several 
# buying stations (of possibly different coops) having the same abbreviated name
# So leave the closest abbreviated name of every village 
hhs_links_closestbs =
  hhs_links_bs %>% 
  group_by(VILLAGE_SURVEY_ID, COOP_ABRV_NAME) %>% 
  mutate(SMALLEST_DIST = min(LINK_DISTANCE_METERS),
         LINK_ID = cur_group_id(), 
         IS_CLOSEST_DUPLI = duplicated(SMALLEST_DIST),
         ANY_CLOSEST_DUPLI = anyDuplicated(SMALLEST_DIST)) %>% 
  ungroup() %>% 
  filter(LINK_DISTANCE_METERS == SMALLEST_DIST) %>% 
  # remove the two links (72 and 73) where there are still conflicts that we cannot resolve 
  filter(!LINK_ID %in% c(72, 73))

# hhs_links_closestbs %>% 
#   select(LINK_ID, SMALLEST_DIST, ANY_CLOSEST_DUPLI, 
#          COOP_ID, COOP_POINT_ID, COOP_BS_ID, VILLAGE_SURVEY_ID,
#          COOP_ABRV_NAME, 
#          SUPPLIER_ABRVNAME, COOP_SIMPLIF_ABRV_NAME, SUPPLIER_FULLNAME, 
#          BS_LONGITUDE, BS_LATITUDE) %>% 
#   arrange(desc(LINK_ID), desc(SMALLEST_DIST)) %>% 
#   View()

# check that this goes back to one row per village-abrv name link 
if(hhs_links_closestbs$LINK_ID %>% unique() %>% length() != nrow(hhs_links_closestbs)){
  stop("unexpected structure")
}

hhs_links_closestbs$VILLAGE_SURVEY_ID %>% unique() %>% length() 
nrow(hhs_links_closestbs) 


# Add links with other buyers than coops... would be here but it's only 5 obs. so don't bother for now. 
# Identify these links
hhs_otherlinks = 
  hhs_links %>% 
  filter(is.na(COOP_ABRV_NAME))

hhs_otherlinks$village_cocoa_buyer %>% unique()
hhs_otherlinks$VILLAGE_SURVEY_ID %>% unique() %>% length()
# it's only five villages that don't sell to a coop at all. 


# Export ----

toexport = 
  hhs_links_closestbs %>% 
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





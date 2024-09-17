

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
# filter to 2022 because we need a cross-section and it's the year when survey data is valid for 

# ic2b = read.csv(here("input_data", "dataverse_files", "IC2B_coopyear.csv")) %>% 
coopbs <- 
  read.csv(
    file = here("temp_data/private_IC2B/IC2B_v2_coop_bs_year.csv")) %>% 
  filter(YEAR == 2022) %>% 
  select(-LVL_4_NAME)


# Departements (districts)
departements <- read_sf("input_data/s3/CIV_DEPARTEMENTS.geojson")

departements = 
  st_transform(departements, crs = civ_crs)


# Join volumes sold to every buyer ------------

hhs_byb$buyer_cocoa_name_quant_b1 %>% summary()
hhs_byb$buyer_cocoa_name_quant_unit_b1 %>% unique()
hhs_byb$buyer_cocoa_name_quant_unit_b1 %>% table()

hhs_byb %>% 
  filter(buyer_cocoa_name_quant_unit_b1 == "") %>% 
  pull(buyer_cocoa_name_quant_b1) %>% summary()

hhs_volbyb =
  hhs_byb %>% 
  select(HH_SURVEY_ID, starts_with("buyer_cocoa_name_")) %>% 
  mutate(
    buyer_cocoa_name_VOLUME_KG_b1 = case_when(
      buyer_cocoa_name_quant_unit_b1 == "kg"  ~ buyer_cocoa_name_quant_b1, 
      buyer_cocoa_name_quant_unit_b1 == ""    ~ buyer_cocoa_name_quant_b1,
      buyer_cocoa_name_quant_unit_b1 == "ton" ~ buyer_cocoa_name_quant_b1*1e3, 
      buyer_cocoa_name_quant_unit_b1 == "bag" ~ buyer_cocoa_name_quant_b1*65
    ), 
    buyer_cocoa_name_VOLUME_KG_b2 = case_when(
      buyer_cocoa_name_quant_unit_b2 == "kg"  ~ buyer_cocoa_name_quant_b2, 
      buyer_cocoa_name_quant_unit_b2 == ""    ~ buyer_cocoa_name_quant_b2,
      buyer_cocoa_name_quant_unit_b2 == "ton" ~ buyer_cocoa_name_quant_b2*1e3, 
      buyer_cocoa_name_quant_unit_b2 == "bag" ~ buyer_cocoa_name_quant_b2*65,
    ),
    buyer_cocoa_name_VOLUME_KG_b3 = case_when(
      buyer_cocoa_name_quant_unit_b3 == "kg"  ~ buyer_cocoa_name_quant_b3, 
      buyer_cocoa_name_quant_unit_b3 == ""    ~ buyer_cocoa_name_quant_b3,
      buyer_cocoa_name_quant_unit_b3 == "ton" ~ buyer_cocoa_name_quant_b3*1e3, 
      buyer_cocoa_name_quant_unit_b3 == "bag" ~ buyer_cocoa_name_quant_b3*65,
    )
  )


hhs = 
  hhs %>% 
  left_join(
    hhs_volbyb %>% select(HH_SURVEY_ID, starts_with("buyer_cocoa_name_VOLUME_KG")), 
    by = "HH_SURVEY_ID"
  )

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
    BUYER_NAME_1 = case_when(
      buyer_cocoa_name_1 == "other" ~ buyer_cocoa_name_other_1, 
      TRUE ~ buyer_cocoa_name_1
    ),
    BUYER_NAME_2 = case_when(
      buyer_cocoa_name_2 == "other" ~ buyer_cocoa_name_other_2, 
      TRUE ~ buyer_cocoa_name_2
    ),
    BUYER_NAME_3 = case_when(
      buyer_cocoa_name_3 == "other" ~ buyer_cocoa_name_other_3, 
      TRUE ~ buyer_cocoa_name_3
    )
    # this adds no info and there is no volume var. for buyer 4 anyway.  
    # BUYER_NAME_4 = case_when(
    #   buyer_cocoa_name_4 == "other" ~ buyer_cocoa_name_other_4, 
    #   TRUE ~ buyer_cocoa_name_4
    # )
  ) %>% 
  # reshape to have one row per HH-buyer link
  pivot_longer(
    cols = starts_with("BUYER_NAME_"), 
    values_to = "BUYER_NAME"
  ) %>% 
  rename(BUYER_RANK = name) %>% 
  # adjust the non-pivoted volume vars: 
  mutate(LINK_VOLUME_KG = case_when(
    BUYER_RANK == "BUYER_NAME_1" ~ buyer_cocoa_name_VOLUME_KG_b1,
    BUYER_RANK == "BUYER_NAME_2" ~ buyer_cocoa_name_VOLUME_KG_b2,
    BUYER_RANK == "BUYER_NAME_3" ~ buyer_cocoa_name_VOLUME_KG_b3,
    TRUE ~ NA
  )) %>%
  
  # convert the keys responded in buyer_cocoa_name into names
  left_join(sc_coop_names, # %>% rename(BUYER_NAME_EXTERN = COOP_ABRV_NAME), 
            by = join_by("BUYER_NAME" == "COOP_SURVEY_KEY")) %>% 
  mutate(BUYER_NAME = case_when(
    !is.na(COOP_ABRV_NAME) ~ COOP_ABRV_NAME, 
    TRUE ~ BUYER_NAME
  )) 
  hhs_links$BUYER_NAME %>% str_trans() %>% unique()
  
  # Now clean these strings
  hhs_links = 
    hhs_links %>% 
    mutate(BUYER_NAME = str_trans(BUYER_NAME), 
           BUYER_NAME = case_when(
             BUYER_NAME %in% c("", " ", "NA", "N A", "999", "1", "COOPERATIVE 1", "N[\\]A", "N[/]A", "N[//]A") | grepl("OUBLIE|CONNAIS PAS", BUYER_NAME) ~ NA, 
             TRUE ~ BUYER_NAME
           ), 
           BUYER_NAME = case_when(
             BUYER_NAME == "SOCIETE COOPERATIVE AGRICOLE DE YAKASSE-ATTOBROU" ~ "COOPROYA", 
             BUYER_NAME == "COOPERATIVE DES PRODUCTEURS ASSOCIES DE L'AGNEBY" ~ "COOPAA", 
             TRUE ~ BUYER_NAME
           ),
           BUYER_IS_COOP = if_else(grepl("COOP", BUYER_NAME), TRUE, NA),
           BUYER_IS_COOP = case_when(
             grepl("PISTEUR|LIBANAIS|ACHETEUR", BUYER_NAME) ~ FALSE,
             TRUE ~ BUYER_IS_COOP
           ), 
           
           # Make simplified acronyms
           BUYER_NAME = str_squish(gsub("(COOPERATIVE)|COOPERATIVE|COOPERATIVES|(COOPERATIVE DU VILLAGE)", "", BUYER_NAME)),
           BUYER_SIMPLIF_NAME = fn_clean_abrvname3(fn_clean_abrvname2(fn_clean_abrvname1(BUYER_NAME))), 
           BUYER_SIMPLIF_NAME = case_when(
             BUYER_SIMPLIF_NAME == "C A S B" ~ "CASB",
             BUYER_SIMPLIF_NAME %in% c("", " ", "NA", "N A") ~ NA, 
             TRUE ~ BUYER_SIMPLIF_NAME
             )
           )
  (simplif_names = sort(unique(hhs_links$BUYER_SIMPLIF_NAME)))
  
  # this doesn't catch all weird characters but so be it. 
  grep("N[\\]A", x = hhs_links$BUYER_NAME, value = T)
  # grep("N[\]A", x = hhs_links$BUYER_NAME, value = T)
  
  # coopbs %>% filter(grepl("C A S B", SUPPLIER_ABRVNAME))
  
hhs_links %>% 
  distinct(BUYER_NAME, .keep_all = T) %>% 
  select(BUYER_NAME, BUYER_SIMPLIF_NAME) %>% 
  View()

# Based on this, remove links with buyers with no info at all (i.e. that can hardly be considered a link)
nrow(hhs_links)
hhs_links = 
  hhs_links %>% 
  filter(!(is.na(BUYER_SIMPLIF_NAME) & is.na(BUYER_IS_COOP)))
nrow(hhs_links)
# this is many rows removed, because for every HH there were rows for all 4 buyers, 
# but buyers 2 to 4 were often left empty. 

# One row is known to be a coop although it's simplified name was cleaned to NA. 
hhs_links %>% 
  filter(is.na(BUYER_SIMPLIF_NAME) & !is.na(BUYER_IS_COOP))

# Some (~50) of these links are duplicated, for some reason. Remove now to avoid confusion when joining IC2B
# hhs_links %>% arrange(HH_SURVEY_ID) %>% View()
# hhs_links %>% filter(HH_SURVEY_ID=="ffnxs") %>% View()
hhs_links =
  hhs_links %>% 
  distinct(HH_SURVEY_ID, BUYER_SIMPLIF_NAME, .keep_all = TRUE) %>% 
  # and remove now useless variables 
  select(!starts_with("buyer_cocoa_name"))


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
         BUYER_LONGITUDE = LONGITUDE, 
         BUYER_LATITUDE = LATITUDE, 
         everything())  %>% 
  arrange(SIMPLIF_ABRVNAME)


## Join coop survey with IC2B -------

#  To give IC2B attributes (at bs-level) to survey coop data 
hhs_links_all = 
  hhs_links %>% 
  # BUT KEEP LINKS WITH OTHER THINGS THAN IC2B COOPS 
  left_join(coopbs_tomatch, 
             by = join_by("BUYER_SIMPLIF_NAME" == "SIMPLIF_ABRVNAME"), 
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
hhs_links_all$COOP_BS_ID %>% unique() %>% na.omit() %>% length()

# names(hhs_links_all)
hhs_links_all %>% 
  select(COOP_ID, COOP_POINT_ID, COOP_BS_ID, HH_SURVEY_ID,
         COOP_ABRV_NAME, 
         SUPPLIER_ABRVNAME, BUYER_SIMPLIF_NAME, SUPPLIER_FULLNAME, 
         BUYER_LONGITUDE, BUYER_LATITUDE) %>% 
  View()

# many households don't have a link with a BS
hhs_links_all %>% filter(is.na(COOP_BS_ID)) %>% nrow()


# Other vars ------------------

## Coop/other indicator -------------
## update it based on matches with IC2B
hhs_links_all = 
  hhs_links_all %>% 
  mutate(BUYER_IS_COOP = case_when(
    !is.na(COOP_BS_ID) ~ TRUE, 
    TRUE ~ BUYER_IS_COOP)) 

# and deem all other cases as other buyers than coops, except for a few exceptions, recognized from there
hhs_links_all %>% 
  filter(is.na(BUYER_IS_COOP)) %>% 
  pull(BUYER_SIMPLIF_NAME) %>% unique() %>% sort()
# grep("ECID", coopbs$SUPPLIER_ABRVNAME, value = T)

exceptions = c("ECID",
               "ECOPAC", "EECOPAKCA", "GBCI", "GVC", "KAYAT", "SC CAOSI", "SCOOCS3A", "SCPCCT CA2", "SCPCCT1", 
               "SKFAT", "SKFRA", "SOCADPD", "SOCOPGA", "SOKOSAT", 
               "SPAAD MAN", "SPAD", "SPAD SARL", "SPAD TOUTOUKO 1", "SPADGAGNAO", "SPADGAGNOA", "SPCCT", 
               "URCG", "UTZ", "VIE", "WACA JACA", "ZPA") 


hhs_links_all = 
  hhs_links_all %>% 
  mutate(BUYER_IS_COOP = case_when(
    BUYER_SIMPLIF_NAME %in% exceptions ~ TRUE, 
    is.na(BUYER_IS_COOP) ~ FALSE,
    TRUE ~ BUYER_IS_COOP)) 

summary(hhs_links_all$BUYER_IS_COOP)

# hhs_links_all %>% 
#   filter(BUYER_IS_COOP) %>% 
#   pull(BUYER_SIMPLIF_NAME) %>% unique() %>% sort()
# 
# hhs_links_all %>% 
#   filter(!BUYER_IS_COOP) %>% 
#   pull(BUYER_SIMPLIF_NAME) %>% unique() %>% sort()


## Volumes ---------------
# LINK_VOLUME_KG was actually produced in the reshape above, after pivot_longer

# Here we just handle volume outliers 
hhs_links_all$LINK_VOLUME_KG %>% summary()
vol_outliers = boxplot.stats(hhs_links_all$LINK_VOLUME_KG, coef = 2)$out %>% sort()

hhs_links_all$LINK_VOLUME_KG %>% summary()

hhs_links_all = 
  hhs_links_all %>% 
  mutate(LINK_VOLUME_KG = case_when(
    LINK_VOLUME_KG %in% vol_outliers ~ median(hhs_links_all$LINK_VOLUME_KG), 
    TRUE ~ LINK_VOLUME_KG
  ))

hhs_links_all$LINK_VOLUME_KG %>% summary()


## Village name  -------------
hhs_links_all = 
  hhs_links_all %>% 
  left_join(hhs_morevars %>% 
              st_drop_geometry() %>% 
              select(HH_SURVEY_ID, PRO_VILLAGE_NAME = intd_village),
            by = "HH_SURVEY_ID") 

anyNA(hhs_links_all$PRO_VILLAGE_NAME)


## Link ID ------------

# At this stage, hhs_links_all has unique, rather valid, links with both buyers found in IC2B and other buyers

# Create a link ID that uniquely identifies them
hhs_links_all = 
  hhs_links_all %>% 
  # group by buyer simplif name and coop bs id necessary because the latter is many times NAs for HH not matched with IC2B.
  group_by(HH_SURVEY_ID, BUYER_SIMPLIF_NAME, COOP_BS_ID) %>% 
  mutate(LINK_ID = cur_group_id()) %>% 
  ungroup()

stopifnot(hhs_links_all$LINK_ID %>% unique() %>% length() == nrow(hhs_links_all))


# SPATIAL OPERATIONS-----------
nrow_save = nrow(hhs_links_all)

# Join village coordinates
hhs_links_all = 
  hhs_links_all %>% 
  # first, join the village GPS data (this matches perfectly) and name
  inner_join(hhs_morevars %>% 
               select(HH_SURVEY_ID, 
                      X_loc_location_x_y_latitude, X_loc_location_x_y_longitude,
                      avlat, avlong), # loc_y_2, loc_x_2, these not necessary
             by = "HH_SURVEY_ID") %>% 
  
  mutate(
    X_loc_location_x_y_longitude = as.numeric(X_loc_location_x_y_longitude),
    X_loc_location_x_y_latitude = as.numeric(X_loc_location_x_y_latitude),
    avlong = as.numeric(avlong),
    avlat = as.numeric(avlat),
    PRO_LONGITUDE = case_when(
      is.na(X_loc_location_x_y_longitude) ~ avlong, # replaces 165 NAs
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
nrow_save == nrow(hhs_links_all)

hhs_links_all %>% 
  filter(is.na(X_loc_location_x_y_latitude)) %>% nrow()
hhs_links_all %>% 
  filter(is.na(X_loc_location_x_y_latitude) & !is.na(X_loc_location_x_y_longitude)) %>% nrow()
hhs_links_all %>% 
  filter(is.na(X_loc_location_x_y_latitude) & !is.na(avlat)) %>% nrow()


## HH departments --------------
hhs_links_all <- 
  hhs_links_all %>% 
  st_join(departements[,c("LVL_4_CODE", "LVL_4_NAME")],
          join = st_intersects) %>% 
  rename(
    PRO_DEPARTMENT_GEOCODE = LVL_4_CODE,
    PRO_DEPARTMENT_NAME = LVL_4_NAME,
  ) 


## Distance ------------

# Not all links can be given a distance, because some are with non-IC2B buyers. 
# (coops not matched or other than coops)
# So we work on the subset for which the distance can be calculated and add it back

sfhhs_links_bs = 
  hhs_links_all %>% 
  filter(!is.na(BUYER_LONGITUDE)) 
  
# start from the above, not the merger
hhs_links_sfbs = 
  sfhhs_links_bs %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("BUYER_LONGITUDE", "BUYER_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 


stopifnot(nrow(hhs_links_sfbs) == nrow(sfhhs_links_bs))

hhs_links_sfbs$LINK_DISTANCE_METERS <- 
  st_distance(sfhhs_links_bs, hhs_links_sfbs, by_element = TRUE)

summary(hhs_links_sfbs$LINK_DISTANCE_METERS)

stopifnot(!is.na(hhs_links_sfbs$COOP_BS_ID))


## Keep closest BS / homonym coop ---------------

# For a given household, keep only the closest buying station of the closest coop among the possibly several 
# buying stations (of possibly different coops) having the same abbreviated name
# So leave the closest abbreviated name of every village 
hhs_links_sfbs_closest =
  hhs_links_sfbs %>% 
  group_by(HH_SURVEY_ID, COOP_ABRV_NAME) %>% 
  mutate(SMALLEST_DIST = min(LINK_DISTANCE_METERS),
         IS_CLOSEST_DUPLI = duplicated(SMALLEST_DIST),
         ANY_CLOSEST_DUPLI = anyDuplicated(SMALLEST_DIST)) %>% 
  ungroup() %>% 
  filter(LINK_DISTANCE_METERS == SMALLEST_DIST) 

# check that this goes back to one row per HH-abrv name link 
if(hhs_links_sfbs_closest$LINK_ID %>% unique() %>% length() != nrow(hhs_links_sfbs_closest)){
  stop("unexpected structure")
}

## Remove outlier matches ---------
summary(hhs_links_sfbs_closest$LINK_DISTANCE_METERS)

# toplot = 
#   rbind(sfhhs_links_bs %>% mutate(COOPERATIVE = FALSE), 
#         hhs_links_sfbs_closest %>% mutate(COOPERATIVE = TRUE) %>% 
#           select(names(sfhhs_links_bs), COOPERATIVE)
#         )
# ggplot() +
#   geom_sf(data = departements, fill = "transparent") +
#   geom_sf(data = toplot %>% filter(COOPERATIVE), aes(col=BUYER_SIMPLIF_NAME), fill = "black", shape = 16)  +
#   geom_sf(data = toplot %>% filter(!COOPERATIVE), aes(col=BUYER_SIMPLIF_NAME), shape = 4, size = 2)  +
#   labs(title = "", col = "Matched coop names", shape = "Coops") +
#   theme_minimal()

# hhs_links_sfbs_closest$HH_SURVEY_ID %>% unique() %>% length() 
# nrow(hhs_links_sfbs_closest) 

# There seems to be a few obivous outliers  
dist_outliers = boxplot.stats(hhs_links_sfbs_closest$LINK_DISTANCE_METERS, coef = 2)$out %>% sort()

# removing first those that are outliers AND are not in the same district
hhs_links_sfbs_closest = 
  hhs_links_sfbs_closest %>% 
  filter(!(LINK_DISTANCE_METERS %in% dist_outliers & DISTRICT_GEOCODE != PRO_DEPARTMENT_GEOCODE))
# ... removes all distance outliers 
# hhs_links_sfbs_closest %>% 
#   filter(LINK_DISTANCE_METERS %in% dist_outliers) %>% View()

# toplot = 
#   rbind(sfhhs_links_bs %>% mutate(COOPERATIVE = FALSE), 
#         hhs_links_sfbs_closest %>% mutate(COOPERATIVE = TRUE) %>% 
#           select(names(sfhhs_links_bs), COOPERATIVE)
#   )
# ggplot() +
#   geom_sf(data = departements, fill = "transparent") +
#   geom_sf(data = toplot %>% filter(COOPERATIVE), aes(col=BUYER_SIMPLIF_NAME), fill = "black", shape = 16)  +
#   geom_sf(data = toplot %>% filter(!COOPERATIVE), aes(col=BUYER_SIMPLIF_NAME), shape = 4, size = 2)  +
#   labs(title = "", col = "Matched coop names", shape = "Coops") +
#   theme_minimal()

## Merge back -------
# with all other links
hhs_links_all = 
  hhs_links_all %>% 
  left_join(hhs_links_sfbs_closest %>% 
              st_drop_geometry() %>% 
              select(LINK_ID, 
                     LINK_DISTANCE_METERS), 
            by = "LINK_ID") %>% 
  # and remove spatialness 
  st_drop_geometry()

summary(hhs_links_all$LINK_DISTANCE_METERS)


# no duplicates to remove
nrow(hhs_links_all)
hhs_links_all %>% 
  distinct(HH_SURVEY_ID, PRO_VILLAGE_NAME, BUYER_SIMPLIF_NAME, COOP_BS_ID, .keep_all = TRUE) %>% nrow()

# hhs_links_all %>% 
#   group_by(HH_SURVEY_ID, PRO_VILLAGE_NAME, BUYER_SIMPLIF_NAME, COOP_BS_ID) %>%
#   filter(n()>1) %>% View()



# hhs_links_all = 
#   hhs_links_all %>% 
#   group_by(PRO_VILLAGE_NAME) %>% 
#   mutate(VILLAGE_BUYER_VOLUME_KG = sum(LINK_VOLUME_KG, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   mutate(LINK_VILLAGE_REL_SIZE = LINK_VOLUME_KG / VILLAGE_BUYER_VOLUME_KG)


# Export ----

toexport = 
  hhs_links_all %>% 
  mutate(LINK_YEAR = 2022, 
         DATA_SOURCE = "SUSTAINCOCOA",
         PRO_ID = paste0("SUSTAINCOCOA_HH_",HH_SURVEY_ID),
         LINK_ID_ONLYACTUAL = paste0("SUSTAINCOCOA_HH_",LINK_ID)) %>% 
  select(YEAR, PRO_ID, COOP_BS_ID, 
         LINK_ID_ONLYACTUAL,
         BUYER_IS_COOP,
         BUYER_LONGITUDE, BUYER_LATITUDE,
         LINK_ACTUALONLY_DISTANCE_METERS = LINK_DISTANCE_METERS, # actual only because distance is computed for all potential links in prepared_main_dataset.R 
         LINK_VOLUME_KG,
         # PRO_VILLAGE_NAME,
         # PRO_DEPARTMENT_GEOCODE, 
         # PRO_DEPARTMENT_NAME,
         PRO_LONGITUDE, PRO_LATITUDE)


write_csv(toexport,
          file = here("temp_data", "preprocessed_sustain_cocoa", "sustain_cocoa_hh_links_standardized.csv"),
          na = "NA", 
          append = FALSE, 
          col_names = TRUE)

## Export with more variables 





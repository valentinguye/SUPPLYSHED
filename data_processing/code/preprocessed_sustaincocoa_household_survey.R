

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
  filter(YEAR == 2022) 

# Licensed buyers / traitants
licens19 = read.csv2(here("input_data", "CCC", "ACHATEURS_AGREES_2019_GEOCODED.csv"))
licens20 = read.csv2(here("input_data", "CCC", "ACHATEURS_AGREES_2020_GEOCODED.csv"))
licens21 = read.csv2(here("input_data", "CCC", "ACHATEURS_AGREES_2021_GEOCODED.csv"))

licens_panel = 
  rbind(licens19 %>% select(NOM, DENOMINATION, LVL_4_CODE) %>% mutate(LINK_YEAR = 2019),
        licens20 %>% select(NOM, DENOMINATION, LVL_4_CODE) %>% mutate(LINK_YEAR = 2020),
        licens21 %>% select(NOM, DENOMINATION, LVL_4_CODE) %>% mutate(LINK_YEAR = 2021)) %>% 
  arrange(NOM, DENOMINATION, LVL_4_CODE) %>% 
  select(NOM, DENOMINATION, LVL_4_CODE)

rm(licens19, licens20, licens21)

# Departements (districts)
departements <- read_sf("input_data/s3/CIV_DEPARTEMENTS.geojson")

departements = 
  st_transform(departements, crs = civ_crs)


# PREPARE VOLUMES ------------

# Join volumes sold to every buyer

hhs_byb$buyer_cocoa_name_quant_b1 %>% summary()
hhs_byb$buyer_cocoa_name_quant_unit_b1 %>% unique()
hhs_byb$buyer_cocoa_name_quant_unit_b1 %>% table()

hhs_byb %>% 
  filter(buyer_cocoa_name_quant_unit_b1 == "") %>% 
  pull(buyer_cocoa_name_quant_b1) %>% summary()

# hhs_byb %>% filter(buyer_cocoa_name_quant_b1 == 999 | 
#                    buyer_cocoa_name_quant_b2 == 999 | 
#                    buyer_cocoa_name_quant_b3 == 999 ) %>% View()

hhs_volbyb =
  hhs_byb %>% 
  select(HH_SURVEY_ID, starts_with("buyer_cocoa_name_")) %>% 
  mutate(
    # Since 999 is sometimes used in this survey to code missing values.  
    buyer_cocoa_name_quant_b1 = if_else(buyer_cocoa_name_quant_b1 == 999, NA, buyer_cocoa_name_quant_b1),
    buyer_cocoa_name_quant_b2 = if_else(buyer_cocoa_name_quant_b2 == 999, NA, buyer_cocoa_name_quant_b2),
    buyer_cocoa_name_quant_b3 = if_else(buyer_cocoa_name_quant_b3 == 999, NA, buyer_cocoa_name_quant_b3),

    buyer_cocoa_name_VOLUME_KG_b1 = case_when(
      buyer_cocoa_name_quant_unit_b1 == "kg"  ~ buyer_cocoa_name_quant_b1, 
      buyer_cocoa_name_quant_unit_b1 == ""    ~ NA, # as advised by Federico, who could not recover credible units using other parts of the survey. 
      buyer_cocoa_name_quant_unit_b1 == "ton" ~ buyer_cocoa_name_quant_b1*1e3, 
      buyer_cocoa_name_quant_unit_b1 == "bag" ~ buyer_cocoa_name_quant_b1*65
    ), 
    buyer_cocoa_name_VOLUME_KG_b2 = case_when(
      buyer_cocoa_name_quant_unit_b2 == "kg"  ~ buyer_cocoa_name_quant_b2, 
      buyer_cocoa_name_quant_unit_b2 == ""    ~ NA,
      buyer_cocoa_name_quant_unit_b2 == "ton" ~ buyer_cocoa_name_quant_b2*1e3, 
      buyer_cocoa_name_quant_unit_b2 == "bag" ~ buyer_cocoa_name_quant_b2*65,
    ),
    buyer_cocoa_name_VOLUME_KG_b3 = case_when(
      buyer_cocoa_name_quant_unit_b3 == "kg"  ~ buyer_cocoa_name_quant_b3, 
      buyer_cocoa_name_quant_unit_b3 == ""    ~ NA,
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

# JOIN WITH IC2B -----

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
             BUYER_NAME %in% c("", " ", "NA", "N A", "999", "1", "COOPERATIVE 1", "N[/]A", "N[//]A") | grepl("OUBLIE|CONNAIS PAS|N[\\]A", BUYER_NAME) ~ NA, 
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
             BUYER_SIMPLIF_NAME %in% c("", " ", "NA", "N A", "N\\\\A") ~ NA, 
             TRUE ~ BUYER_SIMPLIF_NAME
             )
           )
  (simplif_names = sort(unique(hhs_links$BUYER_SIMPLIF_NAME)))
  
  # this doesn't catch all weird characters but so be it. 
  grep("N[\\]A", x = hhs_links$BUYER_NAME, value = T)
  grep("N[\\]A", x = hhs_links$BUYER_SIMPLIF_NAME, value = T)
  
  # coopbs %>% filter(grepl("C A S B", SUPPLIER_ABRVNAME))
  
# hhs_links %>% 
#   distinct(BUYER_NAME, .keep_all = T) %>% 
#   select(BUYER_NAME, BUYER_SIMPLIF_NAME) %>% 
#   View()

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

# Some of links are duplicated in terms of HH-buyer, when the same HH reported twice the same buyer. 
# We need these to be unique for further data handling (e.g. avoid confusion when joining IC2B). 
# After inspection, it seems reasonable to remove those that are also duplicate in terms of volumes. 
hhs_links %>%
  group_by(HH_SURVEY_ID, BUYER_SIMPLIF_NAME) %>%
  filter(n() > 1) %>% View("HH-buyer dup")

hhs_links =
  hhs_links %>% 
  distinct(HH_SURVEY_ID, BUYER_SIMPLIF_NAME, LINK_VOLUME_KG, .keep_all = TRUE) %>% 
  # the few remaining duplicates seem meaningful (buyer is just "pisteur"), so give them distinct buyer names: 
  mutate(BUYER_SIMPLIF_NAME = case_when(
    grepl("PISTEUR", BUYER_SIMPLIF_NAME) ~ paste0(BUYER_SIMPLIF_NAME, " ", BUYER_RANK),
    TRUE ~ BUYER_SIMPLIF_NAME
  )) %>%
  # and one case with BECIDA coop, where volume is NA
  filter(!(HH_SURVEY_ID == "vpghe" & BUYER_NAME == "BECIDA" & is.na(LINK_VOLUME_KG))) %>% 
  # and remove now useless variables 
  select(!starts_with("buyer_cocoa_name") & !c("COOP_ABRV_NAME"))

hhs_links %>%
  group_by(HH_SURVEY_ID, BUYER_SIMPLIF_NAME) %>%
  filter(n() > 1) %>% View()

# hhs_links %>% 
#   filter(grepl("PISTEUR", BUYER_SIMPLIF_NAME)) %>% View()


## Link ID ------------

# At this stage, hhs_links has unique, rather valid, fHH-buyer links (buyer being either a coop or other).  

# Create a link ID that uniquely identifies them
hhs_links = 
  hhs_links %>% 
  # group by buyer simplif name and coop bs id necessary because the latter is many times NAs for HH not matched with IC2B.
  group_by(HH_SURVEY_ID, BUYER_SIMPLIF_NAME) %>% 
  mutate(LINK_ID = cur_group_id()) %>% 
  ungroup()

stopifnot(hhs_links$LINK_ID %>% unique() %>% length() == nrow(hhs_links))


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
  select(COOP_BS_ID, SIMPLIF_ABRVNAME, SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, DISTRICT_GEOCODE,
         # ROUND_LATITUDE, ROUND_LONGITUDE,  
         BUYER_LONGITUDE = LONGITUDE, 
         BUYER_LATITUDE = LATITUDE)  %>% 
  arrange(SIMPLIF_ABRVNAME)


## Join with IC2B -------

#  To give IC2B attributes (at bs-level) to survey coop data 
hhs_links_all_dups = 
  hhs_links %>% 
  # BUT left_join TO KEEP LINKS WITH OTHER THINGS THAN IC2B COOPS 
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
hhs_links_all_dups$COOP_BS_ID %>% unique() %>% na.omit() %>% length()

# names(hhs_links_all_dups)
hhs_links_all_dups %>% 
  select(COOP_ID, COOP_POINT_ID, COOP_BS_ID, HH_SURVEY_ID,
         SUPPLIER_ABRVNAME, BUYER_SIMPLIF_NAME, SUPPLIER_FULLNAME, 
         BUYER_LONGITUDE, BUYER_LATITUDE) %>% 
  View()

# many households don't have a link with a BS (412 links) 
hhs_links_all_dups %>% filter(is.na(COOP_BS_ID)) %>% nrow()


# SPATIAL OPERATIONS-----------
nrow_save = nrow(hhs_links_all_dups)

## Spatialize ------------

# Join village coordinates
sfhhs_links_all_dups = 
  hhs_links_all_dups %>% 
  # first, join the village GPS data (this matches perfectly) and name
  inner_join(hhs_morevars %>% 
               select(HH_SURVEY_ID, 
                      PRO_VILLAGE_NAME = intd_village,
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
nrow_save == nrow(sfhhs_links_all_dups)

sfhhs_links_all_dups %>% 
  filter(is.na(X_loc_location_x_y_latitude)) %>% nrow()
sfhhs_links_all_dups %>% 
  filter(is.na(X_loc_location_x_y_latitude) & !is.na(X_loc_location_x_y_longitude)) %>% nrow()
sfhhs_links_all_dups %>% 
  filter(is.na(X_loc_location_x_y_latitude) & !is.na(avlat)) %>% nrow()


## HH departments --------------
sfhhs_links_all_dups <- 
  sfhhs_links_all_dups %>% 
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

sfhhs_links_bs_dups = 
  sfhhs_links_all_dups %>% 
  filter(!is.na(BUYER_LONGITUDE)) 

hhs_links_sfbs_dups = 
  sfhhs_links_bs_dups %>% # start from the above, not the merger
  st_drop_geometry() %>% 
  st_as_sf(coords = c("BUYER_LONGITUDE", "BUYER_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 


stopifnot(nrow(hhs_links_sfbs_dups) == nrow(sfhhs_links_bs_dups))

hhs_links_sfbs_dups$LINK_DISTANCE_METERS <- 
  st_distance(sfhhs_links_bs_dups, hhs_links_sfbs_dups, by_element = TRUE)

summary(hhs_links_sfbs_dups$LINK_DISTANCE_METERS)

stopifnot(!is.na(hhs_links_sfbs_dups$COOP_BS_ID))


## Keep closest BS / homonym coop ---------------

# For a given household, keep only the closest buying station of the closest coop among the possibly several 
# buying stations (of possibly different coops) having the same abbreviated name
# So leave the closest matched abbreviated name of every village. 
hhs_links_sfbs_closest =
  hhs_links_sfbs_dups %>% 
  group_by(HH_SURVEY_ID, BUYER_SIMPLIF_NAME) %>% 
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

# Some HH-coop matches are clearly too far apart 

# summary(hhs_links_sfbs_closest$LINK_DISTANCE_METERS)
# toplot = 
#   rbind(sfhhs_links_bs_dups %>% mutate(COOPERATIVE = FALSE), 
#         hhs_links_sfbs_closest %>% mutate(COOPERATIVE = TRUE) %>% 
#           select(names(sfhhs_links_bs_dups), COOPERATIVE)
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
(dist_outliers = boxplot.stats(hhs_links_sfbs_closest$LINK_DISTANCE_METERS, coef = 2)$out %>% sort())

# removing first those that are outliers AND are not in the same district
hhs_links_sfbs_closest = 
  hhs_links_sfbs_closest %>% 
  filter(!(LINK_DISTANCE_METERS %in% dist_outliers & DISTRICT_GEOCODE != PRO_DEPARTMENT_GEOCODE))
# ... removes all distance outliers 
stopifnot(
  hhs_links_sfbs_closest %>%
  filter(LINK_DISTANCE_METERS %in% dist_outliers) %>% nrow() == 0)

# toplot = 
#   rbind(sfhhs_links_bs_dups %>% mutate(COOPERATIVE = FALSE), 
#         hhs_links_sfbs_closest %>% mutate(COOPERATIVE = TRUE) %>% 
#           select(names(sfhhs_links_bs_dups), COOPERATIVE)
#   )
# ggplot() +
#   geom_sf(data = departements, fill = "transparent") +
#   geom_sf(data = toplot %>% filter(COOPERATIVE), aes(col=BUYER_SIMPLIF_NAME), fill = "black", shape = 16)  +
#   geom_sf(data = toplot %>% filter(!COOPERATIVE), aes(col=BUYER_SIMPLIF_NAME), shape = 4, size = 2)  +
#   labs(title = "", col = "Matched coop names", shape = "Coops") +
#   theme_minimal()

## Merge back -------
# with links to non-IC2B buyers
# but inner_join would remove those links, while left_join would keep homonym duplicate matches. 
# so full_join on the subset that doesn't match IC2B, but has all latest variables. 

# this is necessary for intersect not to pick "geometry"
hhs_links_bs_closest = 
  hhs_links_sfbs_closest %>% st_drop_geometry()

hhs_links_all = 
  sfhhs_links_all_dups %>% 
  filter(is.na(COOP_BS_ID)) %>% 
  full_join(hhs_links_bs_closest,
             by = intersect(colnames(sfhhs_links_all_dups), colnames(hhs_links_bs_closest))) %>% 
  # and remove spatialness 
  st_drop_geometry()

stopifnot(nrow(filter(sfhhs_links_all_dups, is.na(COOP_BS_ID))) + nrow(hhs_links_bs_closest) == nrow(hhs_links_all))
names(hhs_links_all)
summary(hhs_links_all$LINK_DISTANCE_METERS)
if(anyNA(hhs_links_all$PRO_VILLAGE_NAME)){stop("not expected")}

# no duplicates to remove
stopifnot(
  hhs_links_all %>% 
    distinct(HH_SURVEY_ID, PRO_VILLAGE_NAME, BUYER_SIMPLIF_NAME, COOP_BS_ID, .keep_all = TRUE) %>% nrow() == 
    nrow(hhs_links_all)
)

# hhs_links_all %>% 
#   group_by(HH_SURVEY_ID, PRO_VILLAGE_NAME, BUYER_SIMPLIF_NAME, COOP_BS_ID) %>%
#   filter(n()>1) %>% View()

# hhs_links_all = 
#   hhs_links_all %>% 
#   group_by(PRO_VILLAGE_NAME) %>% 
#   mutate(VILLAGE_BUYER_VOLUME_KG = sum(LINK_VOLUME_KG, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   mutate(LINK_VILLAGE_REL_SIZE = LINK_VOLUME_KG / VILLAGE_BUYER_VOLUME_KG)


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

exceptions = 
  c(# these because they sound/start like coop names, or because a search confirms they are coops 
    # GVC is ancestor of coop status https://www.foodnavigator.com/Article/2024/04/11/How-cocoa-cooperatives-in-Cote-d-Ivoire-work
    # SCPCCT is coop too: https://business.abidjan.net/annonces-legales/25-avis-de-modification/134267-societe-cooperative-des-producteurs-de-cafe-cacao-avec-conseil-dadministration-de-tiassale-scpcct-ca
    "ECID", "ECOPAC", "EECOPAKCA", "GVC", "KAYAT", "SC CAOSI", "SCOOCS3A", "SCPCCT CA2", "SCPCCT1", 
    "SOCADPD", "SOCOPGA", "SOKOSAT", 
    "UTZ", "VIE", "WACA JACA")
# We did not include the following: 
# ZPA is apparently a traitant 
#  "SPCCT", "SPAD", "SPAD SARL", "SPAD TOUTOUKO 1" "SPADGAGNAO", "SPADGAGNOA", "SPAAD MAN", too, 
# according to the names in the licensed buyer lists 
# for "SKFAT", "SKFRA" "GBCI", "URCG", it's not clear, there's no indication in either direction 
for(exc in exceptions){
  grep(exc, unique(licens_panel$DENOMINATION), value = TRUE) %>% print()
}
unique(licens_panel$DENOMINATION) %>% sort()

exceptions = 
  c(exceptions,  
    # these because they are in the list of licensed buyers but should be considered as coops
    "HKF", # in https://www.tdc-enabel.be/wp-content/uploads/2021/07/TomeIRapportCIRAD_Final.pdf we read: 
          # HKF est un traitant privé basé de longue date en Côte d’Ivoire, probablement le plus gros et historiquement un des plus actifs pour convertir ses pisteurs en petites coopératives. 
    "CIPA", # is only CIPAG in license list, and has signs of being a coop https://www.nexpages.com/ci/scoop-ca-cipa-negoce-cafe-cacao-hevea-cote-d-ivoire-4685
    "BARA", # https://bara-ci.org/presentation/
    "INA", "SPAG", # these two, it's not clear what they are. But they are disclosed and certified in IC2B, so let them be coops. 
    "A" # this one clearly because it's just the grepl that matches it. 
  ) 

hhs_links_all = 
  hhs_links_all %>% 
  rowwise() %>% # this rowwise is necessary
  mutate(
    BUYER_IN_LICBUY = any(grepl(BUYER_SIMPLIF_NAME, licens_panel$DENOMINATION))
    ) %>% 
  ungroup() %>%
  mutate(
    BUYER_IS_COOP = case_when(
      BUYER_IN_LICBUY ~ FALSE,
      is.na(BUYER_IS_COOP) ~ FALSE,
      TRUE ~ BUYER_IS_COOP
    ),
    BUYER_IS_COOP = case_when(
      BUYER_SIMPLIF_NAME %in% exceptions ~ TRUE, # this implies switching back to TRUE, see comments in exception making
      TRUE ~ BUYER_IS_COOP
    )
  ) 

# all.equal(hhs_links_all1, hhs_links_all2)

# check there's no conflict with IC2B match
stopifnot(hhs_links_all %>% filter(!is.na(COOP_BS_ID)) %>% pull(BUYER_IS_COOP) %>% all())
# hhs_links_all %>% filter(!is.na(COOP_BS_ID)) %>% filter(!BUYER_IS_COOP) %>% View()

summary(hhs_links_all$BUYER_IS_COOP)
# ( amount of true values not meaningful at this stage, because of all homonym duplicate match )

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
(vol_outliers = boxplot.stats(hhs_links_all$LINK_VOLUME_KG, coef = 2)$out %>% sort())

hhs_links_all$LINK_VOLUME_KG %>% summary()
# summary by buyer type
hhs_links_all %>% 
  select(LINK_VOLUME_KG, BUYER_IS_COOP) %>% 
  split(.$BUYER_IS_COOP) %>% map(summary)
# volumes are quite different depending on buyer type, 
# so if we were to replace outliers by median/avg, we would need to do it by group. 

hhs_links_all = 
  hhs_links_all %>% 
  mutate(LINK_VOLUME_KG = case_when(
    LINK_VOLUME_KG %in% vol_outliers ~ NA, # median(hhs_links_all$LINK_VOLUME_KG), 
    TRUE ~ LINK_VOLUME_KG
  ))

# after removing outliers 
hhs_links_all$LINK_VOLUME_KG %>% summary()
hhs_links_all %>% 
  select(LINK_VOLUME_KG, BUYER_IS_COOP) %>% 
  split(.$BUYER_IS_COOP) %>% map(summary)


# Checks -------------
# Distribution of volumes between coops and other buyers, across the survey
 
hhs_links_all$LINK_VOLUME_KG %>% summary()
hhs_links_all %>% 
  select(LINK_VOLUME_KG, BUYER_IS_COOP) %>% 
  split(.$BUYER_IS_COOP) %>% map(summary)

sc_coop_vol_tonne = 
round(sum(filter(hhs_links_all, BUYER_IS_COOP)$LINK_VOLUME_KG, na.rm = T)/3, 3)
sc_other_vol_tonne = 
  round(sum(filter(hhs_links_all, !BUYER_IS_COOP)$LINK_VOLUME_KG, na.rm = T)/3, 3)

(sc_coop_vol_tonne / (sc_coop_vol_tonne + sc_other_vol_tonne))

# And aggregating first by village 
vlg_vols =
  hhs_links_all %>%
  summarise(.by = "PRO_VILLAGE_NAME",
            # This is the sum of the volumes of all actual links from a village. 
            VLG_VOLUME_KG = if_else(all(is.na(LINK_VOLUME_KG)), NA, sum(LINK_VOLUME_KG, na.rm = TRUE)),
            
            # Now make the sum of the volumes to coops and to others specifically. 
            # so multiplying by BUYER_IS_COOP makes 0 for actual links with other buyers than coops
            VLG_VOLUME_KG_COOPS  = if_else(all(is.na(LINK_VOLUME_KG)), NA, sum(LINK_VOLUME_KG*BUYER_IS_COOP, na.rm = TRUE)), 
            VLG_VOLUME_KG_OTHERS = if_else(all(is.na(LINK_VOLUME_KG)), NA, sum(LINK_VOLUME_KG*!BUYER_IS_COOP, na.rm = TRUE)) 
  ) %>% 
  mutate(
    VLG_PROP_VOLUME_COOPS = case_when(
      VLG_VOLUME_KG != 0 & !is.na(VLG_VOLUME_KG) ~ VLG_VOLUME_KG_COOPS / VLG_VOLUME_KG, 
      TRUE ~ NA), 
    VLG_PROP_VOLUME_OTHERS = case_when(
      VLG_VOLUME_KG != 0 & !is.na(VLG_VOLUME_KG) ~ VLG_VOLUME_KG_OTHERS / VLG_VOLUME_KG, 
      TRUE ~ NA)
  )
# it is very similar
vlg_vols$VLG_PROP_VOLUME_COOPS %>% summary()

paste0("There are ", 
       nrow(hhs_links_all), " HH-buyer links, between ",
       length(unique(hhs_links_all$HH_SURVEY_ID)), " households located in ",
       length(unique(hhs_links_all$PRO_VILLAGE_NAME)), " villages and ",
       length(unique(hhs_links_all$BUYER_SIMPLIF_NAME)), " buyers. ",
       nrow(filter(hhs_links_all, BUYER_IS_COOP)), " links are with cooperatives, ", 
       nrow(filter(hhs_links_all, !is.na(COOP_BS_ID))), " of which are links with IC2B, and ", 
       nrow(filter(hhs_links_all, !BUYER_IS_COOP)), " are links with another kind of buyers. ",
       "Looking at the ", nrow(filter(hhs_links_all, !is.na(LINK_VOLUME_KG))), " links with clean volume information, ",
       "these households sell ", sc_coop_vol_tonne, " tonnes to coops and ", sc_other_vol_tonne, " tonnes to other buyers."
)

# Export ----

## For supply shed model ---------
# Standardize for supply shed model 
toexport = 
  hhs_links_all %>% 
  mutate(LINK_YEAR = 2022, 
         DATA_SOURCE = "SUSTAINCOCOA",
         PRO_ID = paste0("SUSTAINCOCOA_HH_",HH_SURVEY_ID),
         LINK_ID_ONLYACTUAL = paste0("SUSTAINCOCOA_HH_",LINK_ID)) %>% 
  select(LINK_YEAR, PRO_ID, COOP_BS_ID, 
         LINK_ID_ONLYACTUAL,
         BUYER_IS_COOP,
         BUYER_LONGITUDE, BUYER_LATITUDE,
         LINK_ACTUALONLY_DISTANCE_METERS = LINK_DISTANCE_METERS, # actual only because distance is computed for all potential links in prepared_main_dataset.R 
         LINK_VOLUME_KG,
         PRO_VILLAGE_NAME,
         # PRO_DEPARTMENT_GEOCODE, 
         # PRO_DEPARTMENT_NAME,
         PRO_LONGITUDE, PRO_LATITUDE)

write_csv(toexport,
          file = here("temp_data", "preprocessed_sustain_cocoa", "sustain_cocoa_hh_links_standardized.csv"),
          na = "NA", 
          append = FALSE, 
          col_names = TRUE)


## For general use --------------
# Add main IC2B variables of interest 
coopbs_renamed = 
  coopbs %>% 
  filter(!is.na(LONGITUDE) & !is.na(SUPPLIER_ABRVNAME)) %>% 
  filter(SIMPLIF_ABRVNAME %in% simplif_names) %>% 
  rename(COOP_N_FARMERS = TOTAL_FARMERS,
         COOP_ABRVNAME = SUPPLIER_ABRVNAME, 
         COOP_FULLNAME = SUPPLIER_FULLNAME, 
         COOP_KNOWN_BUYERS = TRADER_NAMES, 
         COOP_DISCLOSURE_SOURCES = DISCLOSURE_SOURCES,
         COOP_KNOWN_CERTIFICATIONS = CERTIFICATIONS, 
         COOP_BS_LONGITUDE = LONGITUDE,
         COOP_BS_LATITUDE = LATITUDE
  ) %>% 
  select(starts_with("COOP_"), -COOP_POINT_ID)

hhs_links_all$LINK_ID %>% unique() %>% length() == nrow(hhs_links_all)

toexport_2 = 
  hhs_links_all %>% 
  mutate(HH_BUYER_DISTANCE_METERS = as.numeric(LINK_DISTANCE_METERS)) %>% 
  left_join(coopbs_renamed, 
            by = "COOP_BS_ID") %>% 
  mutate(LINK_ID = row_number()) %>%
  select(LINK_ID, 
         HH_SURVEY_ID,
         HH_VILLAGE_NAME = PRO_VILLAGE_NAME,
         HH_DEPARTMENT_NAME = PRO_DEPARTMENT_NAME,
         HH_BUYER_DISTANCE_METERS, 
         HH_LONGITUDE = PRO_LONGITUDE,
         HH_LATITUDE = PRO_LATITUDE,
         starts_with("LINK_"),
         starts_with("BUYER_"),
         starts_with("COOP_") 
         ) %>% 
  arrange(HH_DEPARTMENT_NAME, HH_VILLAGE_NAME, HH_SURVEY_ID, LINK_ID)

names(toexport_2)
toexport_2$COOP_DISCLOSURE_SOURCES %>% unique()

write_csv(toexport_2,
          file = here("temp_data", "preprocessed_sustain_cocoa", "sustaincocoa_HH_survey-privateIC2B_merge.csv"),
          na = "NA", 
          append = FALSE, 
          col_names = TRUE)






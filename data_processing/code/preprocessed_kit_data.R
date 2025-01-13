

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
dir.create(here("temp_data", "preprocessed_kit"), recursive = TRUE)

# Assets and functions --------------
# use the projected CRS used by BNETD for their 2020 land use map. 
civ_crs <- 32630

# load in particular the function fn_trader_to_group_names, str_trans, ... 
source(here("code", "USEFUL_STUFF_supplyshedproj.R"))

kit = read.dta13(here("input_data", "KIT", "dataverse_files", "0.1 Cocoa_Livelihoods_export_Dataverse_stata13.dta"),
                 convert.factors = TRUE, # we want those labels. this is the default. 
                 generate.factors=TRUE,
                 nonint.factors = TRUE) # this is not the default. Necessary to get labels (and thus values of interest) for company (among other vars)

kit = kit %>% filter(country == "CdI" & !is.na(cocoa_buyers_16)) 

kit$cocoa_buyers_16 %>% summary()
# this replicates the 36% of cooperative outlet share in Table 11.2 in Bymolt et al. 
kit %>% filter(cocoa_buyers_16 == "yes") %>% nrow() / nrow(kit) 

kit = 
  kit %>% 
  mutate(BUYER_IS_COOP = if_else(cocoa_buyers_16 == "yes", TRUE, FALSE))


# Volumes and area -------------
kit$cocoa_land_used_ha %>% summary()
kit$cocoa_sold_kg %>% summary()

# Handle outliers in volume, and NAs in volume and area

# Impute NAs, to avoid loosing these few obs. 
kit = 
  kit %>% 
  mutate(
    LINK_VOLUME_KG = if_else(
      is.na(cocoa_sold_kg), mean(kit$cocoa_sold_kg, na.rm = T), cocoa_sold_kg
    ), 
    PRO_COCOA_FARMLAND_HA = if_else(
      is.na(cocoa_land_used_ha), mean(kit$cocoa_land_used_ha, na.rm = T), cocoa_land_used_ha
    )) 


kit$LINK_VOLUME_KG %>% summary()
(vol_outliers = boxplot.stats(kit$LINK_VOLUME_KG, coef = 2)$out %>% sort())

kit$LINK_VOLUME_KG %>% summary()
# summary by buyer type
kit %>% 
  select(LINK_VOLUME_KG, BUYER_IS_COOP) %>% 
  split(.$BUYER_IS_COOP) %>% map(summary)
# volumes are quite different depending on buyer type, 
# so if we were to replace outliers by median/avg, we would need to do it by group. 

# The outliers don't seem impossible to me, except the 64k kg 
kit = 
  kit %>% 
  mutate(LINK_VOLUME_KG = case_when(
    LINK_VOLUME_KG == 64000 ~ NA, # median(kit$LINK_VOLUME_KG), 
    TRUE ~ LINK_VOLUME_KG
  ))

# after removing outliers 
kit$LINK_VOLUME_KG %>% summary()
kit %>% 
  select(LINK_VOLUME_KG, BUYER_IS_COOP) %>% 
  split(.$BUYER_IS_COOP) %>% map(summary)


# Spatial coordinates -----------
kit$longitude %>% summary()
kit$latitude %>% summary()
# Simpler to handle like that the only missing in coordinates, rather than accomodate the sometimes multiple coordinates for the same village. 
# kit %>% filter(is.na(longitude)) %>% View()
# kit %>% filter(village == "Koudougou 2") %>% View()  
kit = 
  kit %>% 
  mutate(
    longitude = case_when(
      is.na(longitude) & village == "Koudougou 2" ~ -6.4, 
      TRUE ~ longitude
    ), 
    latitude = case_when(
      is.na(latitude) & village == "Koudougou 2" ~ 7.42, 
      TRUE ~ latitude
    )
  )

# Check on map 
kit_sf = kit %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(civ_crs)

ggplot() +
    geom_sf(data = departements, fill = "transparent") +
    geom_sf(data = kit_sf, col = "orange")

# Export ----

# Make other variables used in the workflow
kit = 
  kit %>% 
  mutate(HH_SURVEY_ID = instanceid, 
         LINK_ID = instanceid, 
         PRO_VILLAGE_NAME = str_trans(village),
         PRO_LONGITUDE = longitude, 
         PRO_LATITUDE = latitude,
         COOP_BS_ID = NA, 
         BUYER_LONGITUDE = NA,
         BUYER_LATITUDE = NA, 
         LINK_DISTANCE_METERS = NA)

# Standardize for supply shed model 
toexport = 
  kit %>% 
  mutate(LINK_YEAR = 2017, 
         DATA_SOURCE = "KIT",
         PRO_ID = paste0("KIT_HH_",HH_SURVEY_ID),
         LINK_ID_ONLYACTUAL = paste0("KIT_HH_",LINK_ID)) %>% 
  select(LINK_YEAR, PRO_ID, COOP_BS_ID, 
         LINK_ID_ONLYACTUAL,
         BUYER_IS_COOP,
         BUYER_LONGITUDE, BUYER_LATITUDE,
         LINK_ACTUALONLY_DISTANCE_METERS = LINK_DISTANCE_METERS, # actual only because distance is computed for all potential links in prepared_main_dataset.R 
         LINK_VOLUME_KG,
         PRO_VILLAGE_NAME,
         # PRO_DEPARTMENT_GEOCODE, 
         # PRO_DEPARTMENT_NAME,
         PRO_COCOA_FARMLAND_HA,
         PRO_LONGITUDE, PRO_LATITUDE)


write_csv(toexport,
          file = here("temp_data", "preprocessed_kit", "kit_hh_pseudolinks_standardized.csv"),
          na = "NA", 
          append = FALSE, 
          col_names = TRUE)


# If we were to aggregate to a village
# kit_vil = 
#   kit %>% 
#   summarise(.by = "village", 
#             VIL_mean(HH_COCOA_WEIGHT * HH_SELL_TO_COOP),
#             weighted.mean(x = HH_SELL_TO_COOP, w = HH_COCOA_WEIGHT), 
#             PRO_LONGITUDE = unique(round(longitude, 1)),
#             PRO_LATITUDE = unique(round(latitude, 1)))


library(tidyverse)
library(sf)
library(readxl)
library(xlsx)
library(readstata13)
library(stringr)
library(DescTools)
library(rnaturalearth)
library(ggpubr)
library(units)
library(scales)
library(kableExtra)
library(here)

# Will write preprocessed data there
dir.create(here("temp_data", "preprocessed_wb"), recursive = TRUE)

# Assets and functions --------------
# use the projected CRS used by BNETD for their 2020 land use map. 
civ_crs <- 32630

# load in particular the function fn_trader_to_group_names, str_trans, ... 
source(here("code", "USEFUL_STUFF_supplyshedproj.R"))

departements <- read_sf("input_data/s3/CIV_DEPARTEMENTS.geojson")

departements = 
  st_transform(departements, crs = civ_crs)

 
wb_coords21 = read.dta13(here("input_data", "WB", "CIV_2021_EHCVM-2_v01_M_STATA14", "s00_me_civ2021.dta"), 
                         convert.factors = TRUE, # we want those labels. this is the default. 
                         generate.factors=TRUE,
                         nonint.factors = TRUE) 

wb_sales21 = read.dta13(here("input_data", "WB", "CIV_2021_EHCVM-2_v01_M_STATA14", "s16d_me_civ2021.dta"), 
                        convert.factors = TRUE, # we want those labels. this is the default. 
                        generate.factors=TRUE,
                        nonint.factors = TRUE)

wb_prod21 = read.dta13(here("input_data", "WB", "CIV_2021_EHCVM-2_v01_M_STATA14", "s16a_me_civ2021.dta"), 
                       convert.factors = TRUE, # we want those labels. this is the default. 
                       generate.factors=TRUE,
                       nonint.factors = TRUE) 

# Do not include 2018 actually, because it would put too much weights on WB data. 
# wb_coords18 = read.dta13(here("input_data", "WB", "CIV_2018_EHCVM_v02_M_Stata", "s00_me_civ2018.dta"), 
#                          convert.factors = TRUE, # we want those labels. this is the default. 
#                          generate.factors=TRUE,
#                          nonint.factors = TRUE)
# wb_sales18 = read.dta13(here("input_data", "WB", "CIV_2018_EHCVM_v02_M_Stata", "s16c_me_civ2018.dta"), 
#                         convert.factors = TRUE, # we want those labels. this is the default. 
#                         generate.factors=TRUE,
#                         nonint.factors = TRUE) 
# wb_prod18 = read.dta13(here("input_data", "WB", "CIV_2018_EHCVM_v02_M_Stata", "s16a_me_civ2018.dta"), 
#                        convert.factors = TRUE, # we want those labels. this is the default. 
#                        generate.factors=TRUE,
#                        nonint.factors = TRUE) 

# One HH can have several plots, and can sell to several buyers.  
# The 2021 data is at plot level for sales, but the question was asked for the whole crop activity apparently (according to Jane B). 
# Moreover, ~200 HH are identified with cocoa plots in prod data but not in sales data... 

# So let's start from sales (s16d_me_civ2021.dta) because it is not clear that we could recover all necessary data for these 200 HH. 
# s16d_me_civ2021 is about "production use", so it won't include sales of cocoa that was not produced by the HH. 

# Cocoa sales outlet ----------
wb_sales21$s16dq01 %>% unique() %>% sort()
# Keep only the cocoa plots 
# Since the sales questions were asked at the crop-hh level, we can consider this at the HH level 
wb_sales21_cacao_hh = 
  wb_sales21 %>% 
  filter(s16dq01 == "Cacaoo")
# Indeed, there are as many rows as different housholds. 
nrow(wb_sales21_cacao_hh)
wb_sales21_cacao_hh %>% distinct(grappe, menage, vague) %>% nrow()
# (out of 20k+ plots of 12965 HH in total)
nrow(wb_prod21)
wb_prod21 %>% distinct(grappe, menage, vague) %>% nrow()

# This is the outlet 
wb_sales21_cacao_hh$s16dq08 %>% unique()
wb_sales21_cacao_hh = 
  wb_sales21_cacao_hh %>% 
  filter(!is.na(s16dq08)) %>% 
  mutate(BUYER_IS_COOP = if_else(s16dq08 == "Coopérative", TRUE, FALSE))

wb_sales21_cacao_hh$BUYER_IS_COOP %>% sum()

# Make HH ID -------
wb_sales21_cacao_hh = 
  wb_sales21_cacao_hh %>% 
  mutate(HH_SURVEY_ID = paste0("vague",vague,"_","grappe",grappe,"_","menage",menage ))

stopifnot(wb_sales21_cacao_hh$HH_SURVEY_ID %>% unique() %>% length() == nrow(wb_sales21_cacao_hh))

# Add coordinates -----
wb_sales21_cacao_hh = 
  wb_sales21_cacao_hh %>% 
  left_join(wb_coords21 %>% select(grappe, menage, vague, 
                                   PRO_LATITUDE  = GPS__Latitude, 
                                   PRO_LONGITUDE = GPS__Longitude), 
            by = c("menage", "grappe", "vague"))

# Volumes -------------
# This is the quantity sold in kg
wb_sales21_cacao_hh$s16dq05c %>% summary()
# There is no NA to impute. 
wb_sales21_cacao_hh = 
  wb_sales21_cacao_hh %>% 
  rename(LINK_VOLUME_KG = s16dq05c)

# Remove outliers 
# No crazy outlier either. 321 tonnes is plausible. 
# But remove statistically defined outliers because performance with these data otherwise deteriorates  
(vol_outliers = boxplot.stats(wb_sales21_cacao_hh$LINK_VOLUME_KG, coef = 2)$out %>% sort())

wb_sales21_cacao_hh = 
  wb_sales21_cacao_hh %>% 
  mutate(LINK_VOLUME_KG = case_when(
    LINK_VOLUME_KG %in% vol_outliers ~ NA, 
    TRUE ~ LINK_VOLUME_KG
  ))
wb_sales21_cacao_hh$LINK_VOLUME_KG %>% summary()

# Removes 5 villages
wb_sales21_cacao_hh$grappe %>% unique() %>% length()
wb_sales21_cacao_hh %>% filter(!is.na(LINK_VOLUME_KG)) %>% pull(grappe) %>% unique() %>% length()





# Area ------
wb_prod21$s16aq08 %>% unique() %>% sort()
# Keep only the cocoa plots 
wb_prod21_cacao = 
  wb_prod21 %>% 
  filter(s16aq08 == "Cacaoo")

# 3508 cocoa plots of 3120 HH 
nrow(wb_prod21_cacao)
wb_prod21_cacao %>% distinct(grappe, menage, vague) %>% nrow()

# Make plot area, but this is just for informative purpose, as we do not need it
wb_prod21_cacao = 
  wb_prod21_cacao %>% 
  mutate(
    s16aq09a = as.numeric(s16aq09a),
    PLOT_HA = case_when(
      # s16aq47 is the plot area according to GPS. It is sometimes very very large, 
      # but it apparently corrects many otherwise over-estimated areas.  
      s16aq09b == "Hectare (Ha)"      & !is.na(s16aq47) ~ s16aq47, 
      s16aq09b == "Mètre Carré (m^2)" & !is.na(s16aq47) ~ s16aq47*0.0001, 
      s16aq09b == "Hectare (Ha)"      & is.na(s16aq09a) ~ s16aq09a, 
      s16aq09b == "Mètre Carré (m^2)" & is.na(s16aq09a) ~ s16aq09a*0.0001
    ))

# wb_prod21_cacao %>% select(grappe, menage, vague, s16aq08, s16aq09a, s16aq09b, PLOT_HA) %>% 
#   arrange(vague, grappe, menage) %>% View()

wb_prod21_cacao$s16aq47 %>% summary()
wb_prod21_cacao$PLOT_HA %>% summary()

# Remove outliers 
(area_outliers = boxplot.stats(wb_prod21_cacao$PLOT_HA, coef = 2)$out %>% sort())

wb_prod21_cacao %>% nrow()
wb_prod21_cacao = 
  wb_prod21_cacao %>% 
  mutate(PLOT_HA = case_when(
    PLOT_HA %in% area_outliers ~ NA, 
    TRUE ~ PLOT_HA
    ))
wb_prod21_cacao$PLOT_HA %>% summary()

# Aggregate from plot to HH
wb_cacao_area_hh = 
  wb_prod21_cacao %>% 
  summarise(.by = c("grappe", "menage", "vague"), 
            PRO_COCOA_FARMLAND_HA = sum(PLOT_HA, na.rm = TRUE)
  )
wb_cacao_area_hh$PRO_COCOA_FARMLAND_HA %>% summary()

# Merge to main 
wb_sales21_cacao_hh = 
  wb_sales21_cacao_hh %>% 
  left_join(wb_cacao_area_hh, 
            by = c("grappe", "menage", "vague"))

# Export ----

# Check on map 
wb_sf = wb_sales21_cacao_hh %>% st_as_sf(coords = c("PRO_LONGITUDE", "PRO_LATITUDE"), crs = 4326) %>% st_transform(civ_crs)

ggplot() +
  geom_sf(data = departements, fill = "transparent") +
  geom_sf(data = wb_sf, aes(col = LINK_VOLUME_KG))


# Make other variables used in the workflow
wb_sales21_cacao_hh = 
  wb_sales21_cacao_hh %>% 
  mutate(LINK_ID = HH_SURVEY_ID, 
         PRO_VILLAGE_NAME = paste0("grappe", grappe),
         COOP_BS_ID = NA, 
         BUYER_LONGITUDE = NA,
         BUYER_LATITUDE = NA, 
         LINK_DISTANCE_METERS = NA)

# Standardize for supply shed model 
toexport = 
  wb_sales21_cacao_hh %>% 
  mutate(LINK_YEAR = 2021, 
         DATA_SOURCE = "WB",
         PRO_ID = paste0("WB_HH_",HH_SURVEY_ID),
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
          file = here("temp_data", "preprocessed_wb", "wb_hh_pseudolinks_standardized.csv"),
          na = "NA", 
          append = FALSE, 
          col_names = TRUE)

# Old protocol: 
# When we wanted to remove HH that would sell cocoa without producing it (intermediaries) - which is not necessary in fact. 
# Start from the plot product data, and remove HH that do not have a single hectare of cocoa. 
# Then, aggregate this at HH level (facultatively summing up the area under cocoa). 
# Then, merge the sales data to this, based on HH ID. 
# Then, remove rows corresponding to sales of other stuff than cocoa. 
# Then, aggregate to grappe level: volumes sold to coops / total volumes sold by HH with outlet info.  
# Then merge coordinates of the grappe. 

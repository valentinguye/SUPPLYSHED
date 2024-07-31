# Script to collect and format all input data to run SEI-CPS model for 2022. 
# Author: Valentin Guye 
# output: cote_divoire/cocoa/sei_pcs/v1.1.0/DATA_FOR_SEIPCS_2022.rdata
##=================================================================

# Workstation set-up ------------------------------------------------------

library(tidyverse)
library(aws.s3)
library(sf)
library(readxl)
library(xlsx)
library(RPostgres)
library(dbplyr)
library(DBI)
library(here)

aws.signature::use_credentials()
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

# Useful functions in this script. Incl. to CLEAN TRADER NAMES AND CONVERT VOLUMES TO BEAN EQUIVALENT
source(here("code", "USEFUL_STUFF_manually_copy_pasted.R"))

# Needed data: 
# cam_long: IC2B link data set (list of geolocalised cooperatives with - when available - information 
# disclosed by companies on the number of farmers), in a long format where the row level is a FLOW from a coop 
# to one or more companies (if several companies disclosed this flow) (not necessarily a trader, and possibly missing). 
# --> Comes from cote_divoire/cocoa/logistics/out/CAM_V4.R

# ic2B: Cocoa accountability map (list of geolocalised cooperatives with - when available - information 
# disclosed by companies on the number of farmers), in a panel format where the row level is coop x year. 
# --> Comes from cote_divoire/cocoa/logistics/out/CAM_V4.R

# AA: list of Acheteurs Agréés (licensed buyers) with the department in which they operate
# --> Comes from cote_divoire/cocoa/logistics/originals/ccc/CCC_AA_geocoding.R

# trade_data: shipment data with BEQ and CIF_USD per trader and country of destination
# and grouped trade data: total volume and CIF by trader
# --> Comes from cote_divoire/cocoa/trade/cd/out/preprocessed_civ_cocoa_trade_2022.R

# kit_farm_production: KIT farm production observations (Bymolt et al 2018)
# --> Comes from cote_divoire/cocoa/production_estimates/KIT_production_per_farm_estimates.Rmd

# cocoa_production_pg: Estimated cocoa production per department. 
# --> This is produced by Spatial Team, ask Vivian Ribeiro for the script producing this. 

# civ_departments: shapefile of départements in Côte d'Ivoire

# COCOA ACCOUNTABILITY MAP (CAM) -------------------------------

cam_long <- 
  read.csv(
    file = here("temp_data/private_IC2B/IC2B_v2_link.csv")) %>% 
  filter(YEAR == 2022) %>% # /!\/!\/!\ IMPORTANT TO UPDATE EVERY YEAR 
  # Variables needed in model
  # this is in trase standard format. width = 4 because the max number of coop_id is 5734 currently, so far from being larger than 9999. 
  mutate(SUPPLIER_ID = paste0("CI-COFFEE-COCOA-COOPERATIVE-", str_pad(COOP_ID, width=4, pad = "0"))) %>% 
  select(SUPPLIER_ID, 
         COOP_ID, 
         GEOCODE = DISTRICT_GEOCODE,
         COOPACRONYM = SUPPLIER_ABRVNAME, 
         COOPNAME = SUPPLIER_FULLNAME, 
         TRADER_NAME, # clearly a trader, as per CLEAN TRADER NAME part in CAM_V4.R
         BUYER, # /!\ NOT NECESSARILY A TRADER - see CLEAN TRADER NAME section in CAM_V4.R
         # FLOW_SECOND_BUYER, # this is not used actually. 
         NUM_FARMERS,  # temp note: NUM_FARMERS was named purposedly after the variable used in the 2019 model   
         # For specific certification dummies, I use my naming convention, that starts with "CERT_" to more easily handle all these vars at once when necessary. 
         # But it is not in cam_flow anyway (see CAM_V4.R)
         CERTIFICATIONS, 
         # Keep a flow ID, to merge back variables non-selected info if need be later. 
         FLOW_ID, 
         DISCLOSURE_SOURCE
         # latitude = LATITUDE, # those are actually not used in model
         # longitude = LONGITUDE,
  )
# "TOTAL_FARMERS" is the minimum number of farmers supplying a cooperative (see CAM_V4.R section CLEAN NUMBER FARMERS, step 2.)
# "TOTAL_FARMERS_NONTRADER"  "TOTAL_FARMERS_TRADER"    were variables used to make TOTAL_FARMERS

# The cam at coop level is also needed for the exporting coop part (step 12)
# BUT NOW WE WHAVE TO GROUP ACROSS BUYING STATIONS, because SEI-PCS works on coops, not buying stations. 
# distinct() (i.e. taking the first buying station of a coop) is fine, because all necessary info for SEI-PCS is at coop-level
# and thus identical and equally available for every BS of a coop. 
cam <- 
  read.csv(
    file = here("temp_data/private_IC2B/IC2B_v2_coop_bs_year.csv")) %>% 
  filter(YEAR == 2022) %>% 
  distinct(COOP_ID, .keep_all = TRUE) %>% 
  mutate(SUPPLIER_ID = paste0("CI-COFFEE-COCOA-COOPERATIVE-", str_pad(COOP_ID, width=4, pad = "0")))


# ACHETEURS AGREES ---------------------------
# The object's name does not feature the year, as of 2k20 script, for model scripts to be more similar across years. 
# /!\ In 2022 we don't have the list so we use that of 2021, which is not going to be used in the model anyway because no acheteur agréé can be linked to a trader.

AA <- 
  read_delim(
    get_object("cote_divoire/cocoa/logistics/originals/ccc/ACHATEURS_AGREES_2021_GEOCODED.csv",
               bucket = "trase-storage",
               check_region = T),
    delim = ";"
  ) %>% 
  # Make AA ID and FLOW_ID to be able to row bind to cam_long in the model. 
  arrange(NOM, DENOMINATION, LVL_4_CODE) %>% 
  group_by(NOM, DENOMINATION, LVL_4_CODE) %>% 
  mutate(SUPPLIER_ID = paste0("CI-COFFEE-COCOA-APPROVED-BUYER-", str_pad(cur_group_id(), width=3, pad = "0"))) %>% 
  ungroup() %>%
  mutate(FLOW_ID = SUPPLIER_ID) %>% 
  #make more easy to row-bind to CAM
  rename(GEOCODE = LVL_4_CODE,
         # it's useful to keep these 2 variables below for later, but they are not going to be 
         # used for supplier identification as was the case in 2019 model. 
         COOPACRONYM_AANAME = NOM,
         COOPNAME_AADENOMINATION = DENOMINATION) %>%
  select(SUPPLIER_ID, GEOCODE, COOPACRONYM_AANAME, COOPNAME_AADENOMINATION, FLOW_ID)

# Conversion equivalent factors
# loaded from database here as of 2k20, for use here and in model script. 
cef_cocoa <- fn_load_bean_equivalent()

# TRADE DATA ----------------------------------
# /!\/!\/!\ ADAPT THE YEAR IN THE PATH /!\/!\/!\

# Trader name cleaning and conversion to cocoa bean equivalent are performed here, 
# within a single function written in USEFUL_STUFF.R, called directly in the model

trade_data <- s3read_using(
  object = "cote_divoire/cocoa/trade/cd/out/preprocessed_civ_cocoa_trade_2022.csv",
  FUN = read_delim,
  delim = ";",
  bucket = "trase-storage",
  col_types = cols(HS6 = col_character(),
                   HS_CODE = col_character()),
  opts = c("check_region" = T)
) %>%
  mutate(NET_WEIGHT_KG = NET_WEIGHT_TON*1000 # the model runs in KG !!!
  ) %>% 
  fn_all_trade_name_cleaning(., 
                             exporter_tax_id_column = EXPORTER_TAX_ID,
                             exporter_column = EXPORTER, # name of variable where to clean trader names into trader group names 
                             importer_column = BUYER
  ) %>%
  left_join(cef_cocoa, by = "HS6") %>%
  mutate(BEAN_EQUIVALENT_VOLUME = EQ_FACTOR * NET_WEIGHT_KG) %>% # name of variable where to convert into bean equivalent volume.
  dplyr::select(-EQ_FACTOR) %>%
  filter(!HS6 == "180200") %>% 
  
  # this needs to be done after fn_all_trade_name_cleaning for fn_unique_names_by_exporter_tax_id to work on tax ids in their original format. (not making two tax id variables to limit the number of columns)
  mutate(EXPORTER_TAX_ID = paste0("CI-TRADER-", EXPORTER_TAX_ID)  # it's always width 8 so no need to pad. 
  ) %>%
  # RENAME SOME VARIABLES SO THEY MATCH THE INITIAL (2019) MODEL NAMES
  rename(VOLUME_RAW = NET_WEIGHT_KG, 
         PRODUCT_B_CODE = HS_CODE) %>% 
  
  select(YEAR, COUNTRY_OF_ORIGIN, COUNTRY_OF_DESTINATION, 
         # note that expo ID was added back at the end of the model script, but it's cleaner to have it from here
         EXPORTER_TAX_ID, EXPORTER_ORIGINAL, EXPORTER_CLEAN, EXPORTER_GROUP_CLEAN, HS6, PRODUCT_B_CODE, # PRODUCT_B_CODE is HS code not necessarily trimmed to 6 digits. 
         VOLUME_RAW, BEAN_EQUIVALENT_VOLUME, # FOB - FOB IS NOT IN 2022 CUSTOM DATA. 
         # VARIABLES BELOW WERE NOT IN 2019 MODEL, BECAUSE THEY WERE NOT IN 2019 CUSTIOM DATA. 
         CIF_USD, IMPORTER_ORIGINAL, IMPORTER_GROUP_CLEAN
  ) 

# the inner_join warning is not problematic I think.  
# trader_bean_eq_vol and trader_val_vol are produced in model script as of 2k20. 




# KIT farm production observations ---------------------------------
# This is just the same data used for every year
# (Once cocoa_prod_total_kgs is selected, this is exactly the same as in the data package used by Cécile.)
kit_farm_production <- 
  read_delim(
    get_object("cote_divoire/cocoa/production_estimates/kit_farm_yield_observations.csv",
               bucket = "trase-storage",
               check_region = T),
    delim = ";"
  ) %>%
  select(cocoa_prod_total_kgs)


# Estimated cocoa production per department. This is produced by Spatial Team, ask Vivian Ribeiro for the script producing this. 
cocoa_production_pg <- s3read_using(
  object = "cote_divoire/cocoa/indicators/in/q4_2023/cocoa_production_2019_2022_q4_2023.csv",
  bucket = "trase-storage",
  opts = c("check_region" = T),
  FUN = read_delim,
  delim = ",") %>%
  select(LVL_4_CODE, LVL_4_NAME, 
         production_2022) %>% #     ------ UPDATE HERE ---
  pivot_longer(cols = starts_with("production"), 
               values_to = "COCOA_PRODUCTION_TONNES",
               names_to = "YEAR",
               names_prefix = "production_") %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  rowwise() %>% 
  mutate(LVL4_TRASE_ID_PROD = geocode_to_trase_id(LVL_4_CODE),
         COCOA_PRODUCTION_KG = COCOA_PRODUCTION_TONNES*1000) %>% 
  ungroup() # REMOVES THE ROWWISE 

# 'Départements' (eq. to districts) of Ivory Coast
civ_departments <- s3read_using(
  object = "cote_divoire/spatial/BOUNDARIES/DEPARTEMENT/OUT/CIV_DEPARTEMENTS.geojson",
  FUN = read_sf,
  bucket = "trase-storage",
  opts = c("check_region" = T)) %>%
  select(LVL_4_CODE, LVL_4_NAME)

# SAVE --------------
save(cam_long, cam, AA, trade_data, cef_cocoa, kit_farm_production, cocoa_production_pg, civ_departments,
       object = "temp_data/SEIPCS_adapted_to_IC2B_V2/DATA_FOR_SEIPCS_2022.rdata")

rm(cam_long, cam, AA, cef_cocoa, trade_data, kit_farm_production, cocoa_production_pg, civ_departments)


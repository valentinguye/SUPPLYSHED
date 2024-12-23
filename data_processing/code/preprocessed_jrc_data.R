# Purpose of this script: pre-process JRC survey data provided for the supply shed project
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
dir.create(here("temp_data", "preprocessed_jrc_data"))

# use the projected CRS used by BNETD for their 2020 land use map. 
civ_crs <- 32630


# load in particular the function fn_trader_to_group_names, str_trans, ... 
source(here("code", "USEFUL_STUFF_supplyshedproj.R"))

# READ ------------ 

# Departements (districts)
departements <- read_sf("input_data/s3/CIV_DEPARTEMENTS.geojson")

departements = 
  st_transform(departements, crs = civ_crs)

# kit_farm_production <- 
#   read_delim(
#     get_object("cote_divoire/cocoa/production_estimates/kit_farm_yield_observations.csv",
#                bucket = "trase-storage",
#                check_region = T),
#     delim = ";"
#   ) %>%
#   mutate(cocoa_prod_total_tonnes = cocoa_prod_total_kgs/1000) %>% 
#   select(cocoa_prod_total_tonnes)


# 'cocoa_UCLouvain which contains both producer information (1,219 producers) and buyer information (170 buyers).'
jrc = read.dta13(here("input_data", "JRC", "Data sharing UC Louvain", "cocoa_UCLouvain.dta"),
                 convert.factors = TRUE, # we want those labels. this is the default. 
                 generate.factors=TRUE,
                 nonint.factors = TRUE) # this is not the default. Necessary to get labels (and thus values of interest) for company (among other vars)

cocoa_area = read.dta13(here("input_data", "JRC", "Data sharing UC Louvain", "cocoaland_ha.dta"),
                        convert.factors = TRUE, # we want those labels. this is the default. 
                        generate.factors=TRUE,
                        nonint.factors = TRUE)
jrc = 
  jrc %>% 
  left_join(cocoa_area, 
            by = "interview__key") %>% 
  rename(s02cq3__cocoaland_ha = cocoaland_ha)

# 'refer to the buyers that the pisteurs/ coops then sell the cocoa to. Sometimes more than 
# one is mentioned, therefore there are 221 observations in the data set'
jrc_buyer_roster = read.dta13(here("input_data", "JRC", "Data sharing UC Louvain", "ibuyer_roster_UCLouvain.dta"),
                              convert.factors = TRUE, # we want those labels. this is the default. 
                              generate.factors=TRUE,
                              nonint.factors = TRUE) 

section3a <- read.dta13(here("input_data", "JRC", "Data sharing UC Louvain", "section_3a_UCLouvain.dta"),
                      convert.factors = TRUE, # we want those labels. this is the default. 
                      generate.factors=TRUE,
                      nonint.factors = TRUE) # this is not the default. Necessary to get labels (and thus values of interest) for company (among other vars)
# it's only different variables in there.
intersect(names(jrc), names(section3a))

# Economic activity of the household
econact = read.dta13(here("input_data", "JRC", "Data sharing UC Louvain", "econ_act_UCLouvain.dta"),
                 convert.factors = TRUE, # we want those labels. this is the default. 
                 generate.factors=TRUE,
                 nonint.factors = TRUE) # this is not the default. Necessary to get labels (and thus values of interest) for company (among other vars)
  


# IC2B (private) 
coopbs <- 
  read.csv(
    file = here("temp_data/private_IC2B/IC2B_v2_coop_bs_year.csv")) 



# PREPARE --------

## General ------

### Unfactor ------
jrc$country %>% class()

for(VAR in names(jrc)){
  if(is.factor(jrc[,VAR])){
    jrc = jrc %>% mutate(
      !!as.symbol(VAR) := as.character(!!as.symbol(VAR))
    ) 
  }
  jrc = jrc %>% mutate(
    !!as.symbol(VAR) := case_when(
      !!as.symbol(VAR) == "Ne sait pas" ~ NA,
      TRUE ~ !!as.symbol(VAR)
    )
  )
}
jrc$country %>% class()
jrc$i03cq16 %>% unique()

jrc$company %>% unique()
jrc$i04cq2 %>% unique()

# repeat for buyer roster
jrc_buyer_roster$i04aq15 %>% class()

for(VAR in names(jrc_buyer_roster)){
  if(is.factor(jrc_buyer_roster[,VAR])){
    jrc_buyer_roster = jrc_buyer_roster %>% mutate(
      !!as.symbol(VAR) := as.character(!!as.symbol(VAR))
    ) 
  }
  jrc_buyer_roster = jrc_buyer_roster %>% mutate(
    !!as.symbol(VAR) := case_when(
      !!as.symbol(VAR) == "Ne sait pas" ~ NA,
      TRUE ~ !!as.symbol(VAR)
    )
  )
}


# repeat for producer-intermediary 
section3a$i04aq15 %>% class()

for(VAR in names(section3a)){
  if(is.factor(section3a[,VAR])){
    section3a = section3a %>% mutate(
      !!as.symbol(VAR) := as.character(!!as.symbol(VAR))
    ) 
  }
  section3a = section3a %>% mutate(
    !!as.symbol(VAR) := case_when(
      !!as.symbol(VAR) == "Ne sait pas" ~ NA,
      TRUE ~ !!as.symbol(VAR)
    )
  )
}


### Rename -------

# terminology: 
# buyer = the company that eventually buys cocoa (e.g. Cargill)
# itm = the intermediary between the producer and the buyer
# Rename PRODUCER variables.
jrc = 
  jrc %>% 
  rename(
    s00q4__country = country,
    s00q5__strate = s00q5_0,
    s00q6__reg = Region, 
    s00q7__dst = District,
    s00q8__spf = s00q8,
    s00q9__vil = s00q9,
    PRO_VILLAGE_NAME = Village,
    s00q11__hh_id = s00q11,
    s00q12__itw_latitude = s00q12__Latitude,
    s00q12__itw_longitude = s00q12__Longitude,
    s00q12__itw_accuracy = s00q12__Accuracy,
    s00q12__itw_altitude = s00q12__Altitude, 
    s02aq1__locality_type = s02aq1,
    s02aq2__home_access_type = Home_access,
    s02aq3__km_home_track = s02aq3,
    s02aq4__km_home_paved = s02aq4,
    s02aq5__plot_access_type = s02aq5,
    s02aq6__km_nearest_cp = Salepoint_km, # this is the second s02aq4 in questionnaire pdf. 
    s02aq301__km_home_plot = Cocoa_plot_distance,
    s02dq30__main_itm_type = s02dq30,
    s02dq31__main_itm_type_oth = s02dq31
  )

# Rename INTERMEDIARY variables.
jrc = 
  jrc %>% 
  rename(
    # INTERVIEW ATTRIBUTES
    JRC_BUYER_ID = buyer_id,
    i00q6__itw_ctr = Country_buyer,
    i00q7__itw_reg = Region_buyer,
    i00q8__itw_spf = sp_buyer,
    i00q9__itw_village = EA_buyer,
    i00q20__name_reported = Name_entity,
    i00q21__itw_latitude = i00q21__Latitude,
    i00q21__itw_longitude = i00q21__Longitude,
    i00q21__itw_accuracy = i00q21__Accuracy,
    i00q21__itw_altitude = i00q21__Altitude,
    i00q21__itw_date = i00q21__Timestamp,
    i00q22__is_cp = i00q22, # cp is for Collection Point
    i00q23__is_possible_gps = i00q23,
    i00q24__latitude_cp = i00q24__Latitude,
    i00q24__longitude_cp = i00q24__Longitude,
    i00q24__accuracy_cp = i00q24__Accuracy,
    i00q24__altitude_cp = i00q24__Altitude,
    i00q25__cp_type = i00q25,
    
    # INTERMEDIARY TYPE
    i01bq1__is_cocoa_buying = i01bq1,
    i01bq3__type = i01bq3,
    i01bq3_oth__type_oth = i01bq3_oth,
    i01bq4__name = company,
    i01bq5__is_indep = i01bq5, # this is important to distinguish between interviewee that works for a buyer or who buys with her own money (Annex B[3] of intermediary questionnaire). 
    i01bq6__is_agree = i01bq6,
    i01bq10__suppliers = i01bq10,
    i01bq11__oth_suppliers = i01bq11,
    i02q16__boss_time = i02q16,
    
    # ACTIVITY SCALE
    i03aq1__nb_farmers = i03aq1,
    i03aq2__nb_villages = i03aq2,
    i03aq3__is_all_villages_uni_spf = i03aq3,
    i03aq4__spf_name = i03aq4,
    i03aq5__is_all_villages_uni_dpt = i03aq5,
    i03aq6__dpt_name = i03aq6,   
    i03aq7__is_all_villages_uni_reg = i03aq7,
    i03aq8__reg_name = i03aq8,   
    i03aq9__is_all_villages_uni_dst = i03aq9,
    i03aq10__dst_name = i03aq10,   
    i03aq15__vol_unit = i03aq15,
    i03aq16__kg_per_bag = i03aq16,
    i03aq17__tonne_per_charg = i03aq17,
    i03aq18__vol_main_season = i03aq18,
    i03aq19__vol_small_season = i03aq19,
    i03aq18__tonnes_main_season = v_VolCol_ms, # This is in tonnes (cf Katharina's email from July 12 2024)
    i03aq19__tonnes_small_season = v_VolCol_ls,
    tonnes_both_season = v_VolCol,
    i03aq20__is_vol_correct = i03aq20,
    i03aq25__vehic_type = str_vehic,
    i03aq25__is_vehic_velo = i03aq25__1,
    i03aq25__is_vehic_moto = i03aq25__2,
    i03aq25__is_vehic_3roues = i03aq25__3,
    i03aq25__is_vehic_kya = i03aq25__4,
    i03aq25__is_vehic_camionette = i03aq25__5,
    i03aq25__is_vehic_camram = i03aq25__6,
    i03aq25__is_vehic_cam10roues = i03aq25__7,
    i03aq25__is_vehic_oth = i03aq25__9,
    i03aq25__is_vehic_none = i03aq25__19,
    i03aq25__vehic_type_oth = i03aq25_oth,
    i03aq30__is_basc_used = i03aq30,
    i03aq31__basc_type = i03aq31,
    i03aq31__basc_type_oth = i03aq31_oth,
    i03aq40__is_price_uni = i03aq40,
    i03aq41__price_cfa_per_kg_2019 = i03aq41,
    i03aq42__price_cfa_per_kg_min_2019 = i03aq42,
    i03aq43__price_cfa_per_kg_max_2019 = i03aq43,
    
    # COCOA COLLECTION 
    i03bq1__type = i03bq1,
    i03cq1__etbm_date = i03cq1,
    i03cq2__coop_type = i03cq2,
    i03cq15__coop_nb_members = i03cq15,
    i03cq16__coop_nb_members_2014 = i03cq16,
    i03eq30__is_traced = i03eq30,
    i03eq31__is_all_bags_labelled = i03eq31,
    i03fq1__boss_name = i03fq1,
    i03fq2__boss_type2 = i03fq2,
    i03fq3__boss_type2_oth = i03fq3,
    i03fq4__boss_city = i03fq4,
    i03fq10__boss_ID2 = i03fq10,
    
    # OPERATIONS
    i04aq1__nb_collect_points = i04aq1,
    i04aq2__main_collect_name = i04aq2,
    # note that these 3 variables are misnamed in input data, right name should be i04aq3
    i04aq3__is_connaissement = i01aq3, 
    i04aq4__is_agrement_ccc_2019 = i01aq4,
    i04aq5__is_exp_licensed = i01aq5,
    
    i04aq10__out_vol_2018 = i04aq10,
    i04aq11__out_vol_2019 = i04aq11,
    i04aq12__vol_coop_members_2018 = i04aq12,
    i04aq13__vol_coop_members_2019 = i04aq13,
    
    # MARKET STRUCTURE
    i04cq1__is_uni_buyer = i04cq1,
    i04cq2__nb_buyers = i04cq2,
    i04cq3__change_buyer_ease = i04cq3,
    i04cq4__buyer_change_ease = i04cq4)


jrc_buyer_roster = 
  jrc_buyer_roster %>% 
  rename(
    JRC_BUYER_ID = buyer_id,
    i04aq15__buyer_code = i04aq15,
    i04aq16__tonne_supply_2018 = i04aq16,
    i04aq17__tonne_supply_2019 = i04aq17,
    i04aq18__buyer_city = i04aq18,
    i04aq19__price_cfa_per_kg_2019 = i04aq19,
    i04aq20__km_cp_city = i04aq20,
    i04aq24__buyer_type = i04aq24,
    i04aq24_oth__buyer_type_oth = i04aq24_oth)

# Rename section 3A data 
names(section3a)

# qty_buy_inkg is the kg purchased by this intermediary to this producer in the whole year. 
section3a %>% filter(qty_buy_inkg != light_sold_to_this_buy_kg + main_sold_to_this_buy_kg) %>% nrow()

section3a = 
  section3a %>% 
  rename(
    JRC_BUYER_ID = buyer_id,
    s03aq1_type_code__s03aq1 = s03aq1_id, # 'code acheteur'
    itm_type__s03aq2 = s03aq2,
    itm_type__s03aq2_oth = s03aq2_oth,
    
    # volumes
    # __s03aq301 = s03aq301,
    # __s03aq302 = s03aq302,
    # __s03aq303 = s03aq303,
    # __s03aq304 = s03aq304,
    # __s03aq3_1 = s03aq3_1,
    # __s03aq4_1 = s03aq4_1,
    # use those instead: 
    kg_both_season = qty_buy_inkg, # for consistency with the 
    kg_light_season = light_sold_to_this_buy_kg,
    kg_main_season = main_sold_to_this_buy_kg,
    
    # Weighting 
    # __s03aq5 = s03aq5,
    # __s03aq6 = s03aq6,
    # __s03aq6_oth = s03aq6_oth,
    # __s03aq7 = s03aq7,
    # __s03aq7_oth = s03aq7_oth,
    # __s03aq8 = s03aq8,
    # __s03aq10 = s03aq10, # PRICE (w/o premium)
    # __s03aq11 = s03aq11,
    
    # Reasons for selling to this buyer
    motiv_itm_choice__s03aq12__1 = s03aq12__1,
    motiv_itm_choice__s03aq12__2 = s03aq12__2,
    motiv_itm_choice__s03aq12__3 = s03aq12__3,
    motiv_itm_choice__s03aq12__4 = s03aq12__4,
    motiv_itm_choice__s03aq12__5 = s03aq12__5,
    motiv_itm_choice__s03aq12__6 = s03aq12__6,
    motiv_itm_choice__s03aq12__7 = s03aq12__7,
    motiv_itm_choice__s03aq12__8 = s03aq12__8,
    motiv_itm_choice__s03aq12__9 = s03aq12__9,
    motiv_itm_choice__s03aq12__10 = s03aq12__10,
    motiv_itm_choice__s03aq12__19 = s03aq12__19,
    motiv_itm_choice__s03aq12_oth = s03aq12_oth,
    
    # Trade relationship with the intermediary, incl. in terms of payments
    tradelink_nature__s03aq13 = s03aq13,
    agreement_type__s03aq14 = s03aq14,
    agreement_type_oth__s03aq14_oth = s03aq14_oth,
    tradelink_frequency__s03aq15 = s03aq15,
    tradelink_quality__s03aq16 = s03aq16,
    price_diff__s03aq17 = s03aq17,
    price_satisfaction__s03aq18 = s03aq18,
    other_itm_prices__s03aq19 = s03aq19,
    is_price_qual_dep__s03aq20 = s03aq20, # do you think the price vary with the quality
    is_itm_ever_late__s03aq21 = s03aq21,
    frequency_late__s03aq22 = s03aq22,
    is_itm_indepted__s03aq23 = s03aq23,
    is_every_reject__s03aq24 = s03aq24,
    frequency_reject__s03aq25 = s03aq25,
    
    # reasons for buyer to ever reject supply - don't rename
    # __s03aq26__1 = s03aq26__1,
    # __s03aq26__2 = s03aq26__2,
    # __s03aq26__3 = s03aq26__3,
    # __s03aq26__4 = s03aq26__4,
    # __s03aq26__5 = s03aq26__5,
    # __s03aq26__6 = s03aq26__6,
    # __s03aq26__7 = s03aq26__7,
    # __s03aq26__8 = s03aq26__8,
    # __s03aq26__9 = s03aq26__9,
    # __s03aq26__10 = s03aq26__10,
    # __s03aq26__13 = s03aq26__13,
    # __s03aq26__12 = s03aq26__12,
    # __s03aq26__19 = s03aq26__19,
    # __s03aq26__999 = s03aq26__999,
    # __s03aq27 = s03aq27,
    # __s03aq28 = s03aq28,
    
    traitant_influence__s03aq30 = s03aq30, # La coopérative était-elle en fait un pisteur ou un acheteur privé, par le passé ? Ou appartient-elle à un pisteur ou à un acheteur privé (traitant) ?
    is_member__s03aq31 = s03aq31,
    paid_for_membership__s03aq32 = s03aq32,
    membership_price__s03aq33 = s03aq33,
    years_membership__s03aq34 = s03aq34,
    # __s03aq35 = s03aq35,
    # __s03aq36 = s03aq36,
    
    # decisions on which you could vote at the general assembly - don't rename
    # __s03aq37__1 = s03aq37__1,
    # __s03aq37__2 = s03aq37__2,
    # __s03aq37__3 = s03aq37__3,
    # __s03aq37__4 = s03aq37__4,
    # __s03aq37__5 = s03aq37__5,
    # __s03aq37__6 = s03aq37__6,
    # __s03aq37__7 = s03aq37__7,
    # __s03aq37__8 = s03aq37__8,
    # __s03aq37__9 = s03aq37__9,
    # __s03aq37_oth = s03aq37_oth,
    
    km_itm_hh__s03aq38 = s03aq38,
    is_role_in_coop__s03aq391 = s03aq391,
    role_in_coop__s03aq392 = s03aq392,
    role_in_coop_oth__s03aq393 = s03aq393,
    is_family_link__s03aq394 = s03aq394,
    years_tradelink__s03aq399 = s03aq399,
    
    # certification
    # __s03aq40 = s03aq40,
    # __s03aq41 = s03aq41,
    # __s03aq42 = s03aq42,
    # __s03aq42_oth = s03aq42_oth,
    # __s03aq43 = s03aq43,
    # __s03aq44 = s03aq44,
    # __s03aq45 = s03aq45,
    # __s03aq46 = s03aq46,
    # __s03aq47 = s03aq47,
    # __s03aq48 = s03aq48,
    # __s03aq49 = s03aq49,
    # __s03aq50 = s03aq50,
    # __s03aq51 = s03aq51,
    # __s03aq52 = s03aq52,
    # __s03aq53 = s03aq53,
    # __s03aq54 = s03aq54,
    # __s03aq60 = s03aq60,
    
    # Quelles sont ces techniques que vous avez vu dans les champs école ? - don't rename
    # __s03aq61__1 = s03aq61__1,
    # __s03aq61__2 = s03aq61__2,
    # __s03aq61__3 = s03aq61__3,
    # __s03aq61__4 = s03aq61__4,
    # __s03aq61__5 = s03aq61__5,
    # __s03aq61__6 = s03aq61__6,
    # __s03aq61__7 = s03aq61__7,
    # __s03aq61__8 = s03aq61__8,
    # __s03aq61__9 = s03aq61__9,
    # __s03aq61__19 = s03aq61__19,
    # __s03aq62 = s03aq62,
    # __s03aq63 = s03aq63,
    # __s03aq64 = s03aq64,
    # __s03aq65 = s03aq65,
    # __s03aq66 = s03aq66,
    # __s03aq67 = s03aq67,
    # __s03aq68 = s03aq68,
    # __s03aq70 = s03aq70,
    
     # "buyer_name"                "prop_main_buy"             "prop_light_buy"            "main_sold_to_this_buy_kg" 
     # "light_sold_to_this_buy_kg" "qty_buy_inkg"              "main_sold_inunit"          "light_sold_inunit"        
     # "unit_inkg"                 "prev_main_inkg"            "prev_main_inunit"          "prev_main_left"           
     # "prev_light_inkg"           "prev_light_inunit"         "prev_light_left"           "buy_sum_share_light"      
     # "buy_sum_share_main"        "str_second_main_buyer"     "str_second_light_buyer"    "unit_lab"                 
     # "share_total"               "first_sec"                 "first_sec_short"           "test"                     
     # "buyer_order"               "cert_name2"                "str_noinput"               "str_ifpro"                
  ) %>% 
  mutate(
    kg_main_all = main_sold_inunit*unit_inkg,
    kg_light_all = light_sold_inunit*unit_inkg,
    kh_both_all = kg_main_all + kg_light_all, 
    # and I don't know what this is below, but make the sum across seasons still
    prev_both_inkg = prev_main_inkg + prev_light_inkg
  )

### Buyer ID --------------
jrc = 
  jrc %>% 
  mutate(JRC_BUYER_ID = as.character(JRC_BUYER_ID))

jrc_buyer_roster = 
  jrc_buyer_roster %>% 
  mutate(JRC_BUYER_ID = as.character(JRC_BUYER_ID))

section3a = 
  section3a %>% 
  mutate(JRC_BUYER_ID = as.character(JRC_BUYER_ID))

### Producer ID ------------
# It is not ZD and HH that identify the producers, but the interview key (Katharina's email from 09/08/2024)

jrc = 
  jrc %>% 
  mutate(
    PRODUCER_ID = interview__key
    #PRODUCER_ID = paste0("ZD-",PRO_VILLAGE_NAME,"_HH-",s00q11__hh_id)
  )

jrc$PRODUCER_ID %>% unique() %>% na.omit() %>% length()
jrc$interview__key %>% unique() %>% length()
nrow(jrc)

# Remove these few unexpected/not understood duplicates 
jrc = 
  jrc %>% 
  distinct()

jrc$PRO_VILLAGE_NAME %>% unique() %>% length()
is.na(jrc$PRO_VILLAGE_NAME) %>% sum()

# There is no producer id var in buyer roster, since it is at the buyer level. 
# jrc_buyer_roster   = 
#   jrc_buyer_roster   %>% 
#   mutate(PRODUCER_ID = interview__key)
# we can remove all-var duplicates still
jrc_buyer_roster   = 
  jrc_buyer_roster   %>% 
  distinct()

# Same in section3a
section3a = 
  section3a %>% 
  distinct()

section3a = 
  section3a %>% 
  mutate(PRODUCER_ID = interview__key)

# For some reasons, there are a few (~15) obs. that are duplicates in both producer ID and buyer ID, but
# not in buyer attributes in section3a apparently. So just remove these duplicates 
# section3a %>% 
#   group_by(PRODUCER_ID, JRC_BUYER_ID) %>% 
#   filter(n() > 1) %>% View()
section3a = 
  section3a %>% 
  distinct(PRODUCER_ID, JRC_BUYER_ID, .keep_all = TRUE)

### Village ID ----------
jrc$PRO_VILLAGE_NAME %>% unique()
jrc$PRO_VILLAGE_NAME %>% is.na() %>% sum()
jrc$s00q7__dst %>% unique()

jrc = 
  jrc %>% 
  group_by(s00q7__dst, s00q8__spf, s00q9__vil, PRO_VILLAGE_NAME) %>% 
  mutate(PRO_VILLAGE_ID = cur_group_id()) %>% 
  ungroup()

jrc$PRO_VILLAGE_ID %>% unique() %>% length()
jrc$PRO_VILLAGE_NAME %>% unique() %>% length()

### Make booleans -------
jrc = 
  jrc %>% 
  mutate(across(contains("_is_"), ~if_else(. %in% c("oui", "yes"), TRUE, FALSE)))

# numeric variables 

### Make coordinates of intermediary --------
jrc =
  jrc %>% 
  mutate(
    BUYER_LONGITUDE = case_when(
      is.na(i00q24__longitude_cp) & !is.na(i00q21__itw_longitude) & i00q22__is_cp ~ i00q21__itw_longitude, 
      TRUE ~ i00q24__longitude_cp
    ),
    BUYER_LATITUDE = case_when(
      is.na(i00q24__latitude_cp) & !is.na(i00q21__itw_latitude) & i00q22__is_cp ~ i00q21__itw_latitude, 
      TRUE ~ i00q24__latitude_cp
    )
  ) 
if(nrow(jrc %>% filter(is.na(BUYER_LATITUDE))) != nrow(jrc %>% filter(is.na(BUYER_LATITUDE)))){
  stop("pb in missing gps coords")}

### Recognize cooperatives ------------
jrc = 
  jrc %>% 
  mutate(BUYER_IS_COOP = (grepl("coop", i01bq3__type) | grepl("coop", i01bq3_oth__type_oth)) & 
           # don't count them as coop, as they will have different values than in coops in scales etc. 
           i01bq3__type != "délégué de coopérative")


# IC2B  --------

# filter to coops only 
jrc_coops = 
  jrc %>% 
  filter(BUYER_IS_COOP) 

jrc_coops %>% 
  filter(i03aq1__nb_farmers > 0 | 
           i03cq15__coop_nb_members > 0) %>%
  nrow() == nrow(jrc_coops)

### make coop name ------
# this one is variable formerly called 'company' and about which Katharina writes (in JRC_surveys_variables_for_supply_shed.xlsx):
# "I am sending you this for information. But this info was provided by manually filling in the qustionnaire. I tried to harmonize this information and identify the coop/ company. The variable is called "company"and I am also including it in the dataset."
jrc_coops$i01bq4__name %>% unique()

# check there is no more info in the other naming variables
jrc_coops = 
  jrc_coops %>% 
  mutate(i01bq4__name = case_when(
    is.na(i01bq4__name) & i01bq4 == "ECASO" ~ i01bq4,
    TRUE ~ i01bq4__name
  ))

# let this count as a coop
# jrc_coops %>% 
#   filter(grepl("(project financed by Collibri Foundation, Colruyt Group)", i01bq4__name)) %>% 
#   select(i01bq4__name, i00q20__name_reported, i03fq1__boss_name, everything()) %>% 
#   View()

if(
  jrc_coops %>% 
  filter(is.na(i01bq4__name) & 
         (!is.na(i00q20__name_reported) | 
          !is.na(i03fq1__boss_name))) %>% 
  select(i01bq4__name, i00q20__name_reported, i03fq1__boss_name, everything()) %>% # View()
  nrow() > 0
)stop("there is more name info to take")

jrc_coops = 
  jrc_coops %>% 
  mutate(
    SUPPLIER_ABRVNAME = NA_character_ , 
    SUPPLIER_FULLNAME = NA_character_ ,
    
    i01bq4__name = str_squish(str_trans(i01bq4__name)),
    
    # custom edit
    i01bq4__name = case_when(
      i01bq4__name == "SCOPADA - SOCIETE COOPERATIVE DES PRODUCTEURS DE DAAKO" ~ "SCOPADA (SOCIETE COOPERATIVE DES PRODUCTEURS DE DAAKO)", 
      i01bq4__name == "NOUVELLE GENERATION (PROJECT FINANCED BY COLLIBRI FOUNDATION, COLRUYT GROUP)" ~ "NOUVELLE GENERATION",
      TRUE ~ i01bq4__name
    ),
    
    # then extract what's within and outside parentheses
    SUPPLIER_ABRVNAME = str_squish(str_trim(str_replace(i01bq4__name, "\\s*\\(.*?\\)", ""))),
    SUPPLIER_FULLNAME = str_squish(str_extract(i01bq4__name, "(?<=\\().*?(?=\\))"))
  )

jrc_coops %>% 
  select(SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, i01bq4__name, everything()) %>% 
  View()

jrc_coops %>% 
  filter(is.na(BUYER_LONGITUDE)) %>% 
  select(SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, BUYER_LONGITUDE, BUYER_LATITUDE, everything()) %>% 
  View()


### Remove duplicate coops -----
# Those are from cases where several farmers report to sell to the same coop
jrc_coops %>% 
  arrange(SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, BUYER_LONGITUDE, BUYER_LATITUDE) %>% 
  select(SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, BUYER_LONGITUDE, BUYER_LATITUDE, everything()) %>% 
  View()

# if this passes, it means all the heterogeneity between coops is captured by their identifiers.
if(
jrc_coops %>% 
  select(SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, BUYER_LONGITUDE, BUYER_LATITUDE,
         starts_with("i0")) %>% 
  distinct(.keep_all = TRUE) %>% 
  nrow() != nrow(distinct(jrc_coops, SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, BUYER_LONGITUDE, BUYER_LATITUDE))
){stop("some info in intermediary survey responses are lost")}

jrc_coops = 
  jrc_coops %>% 
  distinct(SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, BUYER_LONGITUDE, BUYER_LATITUDE, .keep_all = TRUE)

nrow(jrc_coops) # so the JRC data provides info on 47 apparently distinct (at this stage) coops.
# This is consistent with what Jens Van Hee counts.  


### Department and locality names --------
# Attribute departement geocode when available from a departement/district/sbf/village name 
# Then attribute the ZD name to the locality name to keep this info into the private IC2B
lvl4 <- departements %>% st_drop_geometry() %>% select(LVL_4_NAME, LVL_4_CODE)

jrc_coops = 
  jrc_coops %>% 
  mutate(DEPARTEMENT_NAME = case_when(
    !is.na(i03aq6__dpt_name) ~ i03aq6__dpt_name,
    TRUE ~ i03aq10__dst_name
  )) %>% 
  left_join(lvl4, by = c("DEPARTEMENT_NAME" = "LVL_4_NAME")) %>% 
  select(everything(), DISTRICT_GEOCODE = LVL_4_CODE, DEPARTEMENT_NAME) %>% # this DEPARTEMENT_NAME var won't be exported
  mutate(LOCALITY_NAME = case_when(
    !is.na(i03aq4__spf_name) ~ i03aq4__spf_name, 
    # is.na(i03aq4__spf_name) ~ i00q9__itw_village, 
    # is.na(i00q9__itw_village) ~ i00q8__itw_spf,
    TRUE ~ NA
    ))


### Trader names ----------
# merge with buyer roster, i.e. the data set of traders sourcing from the coops (aka the 'buyers')

# Pre-process trader names in the roster
jrc_buyer_roster = 
  jrc_buyer_roster %>% 
  mutate(i04aq15__buyer_code = case_when(
    
    i04aq15__buyer_code == "1" ~ "ADM", 
    i04aq15__buyer_code == "2" ~ "AFCOTRADE", 
    i04aq15__buyer_code == "3" ~ "AFRICA SOURCING", 
    i04aq15__buyer_code == "4" ~ "AGRICOM", 
    i04aq15__buyer_code == "5" ~ "ASCOT", 
    i04aq15__buyer_code == "6" ~ "AWAHUS", 
    i04aq15__buyer_code == "7" ~ "BARRY CALLEBAUT", 
    i04aq15__buyer_code == "8" ~ "BICAO", 
    i04aq15__buyer_code == "9" ~ "CAP SA",
    i04aq15__buyer_code == "10" ~ "CARGILL", 
    i04aq15__buyer_code == "11" ~ "CEMOI", 
    i04aq15__buyer_code == "12" ~ "CENTRAL INDUSTRIE", 
    i04aq15__buyer_code == "13" ~ "COCOA TRADE IVOIRE", 
    i04aq15__buyer_code == "14" ~ "COEX CI", 
    i04aq15__buyer_code == "15" ~ "CONDICAF", 
    i04aq15__buyer_code == "16" ~ "COTE IVOIRE COMMODITIES",
    i04aq15__buyer_code == "17" ~ "CYRIAN INTERNATIONAL", 
    i04aq15__buyer_code == "18" ~ "ETC CI SARL", 
    i04aq15__buyer_code == "19" ~ "FILDISO COCOA", 
    i04aq15__buyer_code == "20" ~ "GPA", 
    i04aq15__buyer_code == "21" ~ "GREEN AND BROWN", 
    i04aq15__buyer_code == "22" ~ "ICP", 
    i04aq15__buyer_code == "23" ~ "IVCAO", 
    i04aq15__buyer_code == "24" ~ "IVCOM", 
    i04aq15__buyer_code == "25" ~ "KINEDEN COMMODITIES", 
    i04aq15__buyer_code == "26" ~ "NESTLE", 
    i04aq15__buyer_code == "27" ~ "OCEAN", 
    i04aq15__buyer_code == "28" ~ "OLAM", 
    i04aq15__buyer_code == "29" ~ "OMNIVALUE", 
    i04aq15__buyer_code == "30" ~ "OUTSPAN", 
    i04aq15__buyer_code == "31" ~ "PERFORM WORLD", 
    i04aq15__buyer_code == "32" ~ "PFI SARL", 
    i04aq15__buyer_code == "33" ~ "PLOT ENTREPRISE",
    i04aq15__buyer_code == "34" ~ "PROMONT", 
    i04aq15__buyer_code == "35" ~ "QUANG TIENH IMEX", 
    i04aq15__buyer_code == "36" ~ "S3C", 
    i04aq15__buyer_code == "37" ~ "SACO", 
    i04aq15__buyer_code == "38" ~ "SIFCA", 
    i04aq15__buyer_code == "39" ~ "SOGICAF", 
    i04aq15__buyer_code == "40" ~ "SONEMAT", 
    i04aq15__buyer_code == "41" ~ "SUCDEN CI", 
    i04aq15__buyer_code == "42" ~ "SUSCOM", 
    i04aq15__buyer_code == "43" ~ "SUTEC SA",
    i04aq15__buyer_code == "44" ~ "TAN IVOIRE", 
    i04aq15__buyer_code == "45" ~ "TOUTON", 
    i04aq15__buyer_code == "46" ~ "TROPICAO", 
    i04aq15__buyer_code == "47" ~ "UNICAO", 
    i04aq15__buyer_code == "48" ~ "ZAMACOM", 
    i04aq15__buyer_code == "89" ~"AUTRE",
    
    TRUE ~ i04aq15__buyer_code
  ))
jrc_buyer_roster$i04aq15__buyer_code %>% unique()

jrc_coops_save = jrc_coops

jrc_coops = 
  jrc_coops %>% 
  left_join(jrc_buyer_roster,
            by = "JRC_BUYER_ID", 
            multiple = "all") %>% 
  rowwise() %>% 
  mutate(TRADER_NAME = case_when(
    i04aq15__buyer_code == "AUTRE" | any(str_contains(i04aq15__buyer_code, as.character(1:100), switch = TRUE)) ~ NA,
    TRUE ~ i04aq15__buyer_code
  )) 
jrc_coops$TRADER_NAME %>% unique()


### Number of farmers ------
# There are two kinds of variables: the total number of coop members, reported by the coop respondent
# and the number of farmers for a given link with a trader, inferred from the reported volume and the KIT study's average yield per farmer. 

jrc_coops = 
  jrc_coops %>% 
  mutate(
    TOTAL_FARMERS = case_when(
      !is.na(i03aq1__nb_farmers) ~ i03aq1__nb_farmers,
      !is.na(i03cq15__coop_nb_members) ~ as.integer(i03cq15__coop_nb_members),
      TRUE ~ as.integer(i03cq16__coop_nb_members_2014)
    )#, 
    # NUMBER_FARMERS = i04aq17__tonne_supply_2019 / 1.222 # don't do until we solve the outlier issue in i04aq17
  ) 
jrc_buyer_roster$i04aq17__tonne_supply_2019 %>% summary()

### Certification ------ 
# I didn't ask for this variable actually (i04aq25, i04aq26 etc.)
# Because I prefer assuming that we have more exhaustive info on certification from the other sources at once. 

### Standardize to merge -----

# Prepare for merging with master
if("Ghana" %in% jrc_coops$s00q4__country){stop()}


jrc_coops_merge =
  jrc_coops %>% 
  mutate(COUNTRY_NAME = "IVORY_COAST", 
         YEAR = 2019) %>% # jrc$i00q21__itw_date %>% unique()
  select(YEAR, 
         JRC_BUYER_ID, # to be able to match IC2B data 
         SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME,  
         LONGITUDE = BUYER_LONGITUDE,
         LATITUDE = BUYER_LATITUDE,
         DISTRICT_GEOCODE, LOCALITY_NAME, COUNTRY_NAME, TRADER_NAME, 
         # NUMBER_FARMERS, 
         TOTAL_FARMERS)  # order does not matter

### Export -----

names(jrc_coops_merge)[!names(jrc_coops_merge) %in% c("JRC_BUYER_ID", "DISTRICT_GEOCODE", "LOCALITY_NAME")] <- 
  paste0("DISCL_", names(jrc_coops_merge)[!names(jrc_coops_merge) %in% c("JRC_BUYER_ID", "DISTRICT_GEOCODE", "LOCALITY_NAME")])

# add some variables to keep track of JRC coops in IC2B
jrc_coops_merge = 
  jrc_coops_merge %>% 
  mutate(IS_JRC = TRUE,
         COMPANY = "JOINT RESEARCH CENTER")

write_csv(jrc_coops_merge,
          file = here("temp_data", "preprocessed_jrc_data", "jrc_coops_IC2B_standardized.csv"),
          na = "NA", 
          append = FALSE, 
          col_names = TRUE)



# SUPPLY SHED MODEL - PART 1 ------

# We want 2 sf objects with as many rows, in the same order, to run st_distance(by_element = TRUE) on them. 
# These objects have one row per actual producer-intermediary link, as per jrc object. 
# One represents the producer end (and coords), while the other represents the intermediary end (and coords).  
# In other words, we just want to split the jrc object in two... 

### Filter geo-located links
# We need spatial info on both ends
jrc_geo <- 
  jrc %>% 
  filter(!is.na(s00q12__itw_longitude) & !is.na(s00q12__itw_latitude) & 
           !is.na(BUYER_LONGITUDE) & !is.na(BUYER_LATITUDE)) 

# this is 662 producers linked with 118 buyers
jrc_geo$PRODUCER_ID %>% unique() %>% length()
jrc_geo$JRC_BUYER_ID %>% unique() %>% length()
# of which 159 producers are linked with 29 cooperatives. 
jrc_geo %>% 
  filter(BUYER_IS_COOP) %>% 
  pull(PRODUCER_ID) %>% 
  unique %>% 
  length()
jrc_geo %>% 
  filter(BUYER_IS_COOP) %>% 
  pull(JRC_BUYER_ID) %>% 
  unique %>% 
  length()

## Farmer localization ------------
jrc_geo$s00q12__itw_longitude %>% summary()
jrc_geo$s00q12__itw_latitude %>% summary()
jrc_geo$BUYER_LONGITUDE %>% summary()
jrc_geo$BUYER_LATITUDE %>% summary()

# The following plot shows that some farmers reportedly belonging to the same villages are 
# actually located very far appart. 
village_bbox = 
  jrc_geo %>% 
  st_as_sf(coords = c("s00q12__itw_longitude", "s00q12__itw_latitude"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) %>% 
  st_simplify(dTolerance = 0.001) %>%
  summarise(
    .by = PRO_VILLAGE_ID,
    geometry = st_as_sfc(st_bbox(st_union(geometry)))
  ) %>% 
  mutate(VILLAGE_BBOX_AREA_KM2 = set_units(st_area(geometry), "ha", na.rm = T)) %>% 
  arrange(desc(VILLAGE_BBOX_AREA_KM2))

# Their clearly are outliers 
# ggplot() +
#   geom_sf(data = departements, fill = "transparent") +
#   geom_sf(data = village_bbox, aes(col = as.character(PRO_VILLAGE_ID)), fill = "transparent") 
village_bbox$VILLAGE_BBOX_AREA_KM2 %>% summary()
village_bbox$VILLAGE_BBOX_AREA_KM2 %>% quantile(probs = seq(.7, 1, by = .05))
village_bbox$VILLAGE_BBOX_AREA_KM2 %>% quantile(probs = seq(.95, 1, by = .01)) 


(vlg_size_outliers = boxplot.stats(village_bbox$VILLAGE_BBOX_AREA_KM2, coef = 2)$out %>% sort())

# This may be due to the village ID being badly coded, but this is unlikely, because defining it as a 
# the ID of groups defined as being in the same departement-spf-village-village name.
jrc_geo$PRO_VILLAGE_ID %>% unique() %>% length()
# So 81 IS the right number of villages. 
# However, the coordinates of some farmers in some villages are probably unrealistic. 

# To spot them, compute the distance of every farmer's location to their village's centroid. 
village_ctoid = 
  jrc_geo %>% 
  st_as_sf(coords = c("s00q12__itw_longitude", "s00q12__itw_latitude"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) %>% 
  st_simplify(dTolerance = 0.001) %>%
  summarise(
    .by = PRO_VILLAGE_NAME,
    geometry = st_union(geometry)
  ) %>%
  st_centroid() %>% 
  st_transform(4326)

village_ctoid$VILLAGE_LONGITUDE = st_coordinates(village_ctoid)[,"X"]
village_ctoid$VILLAGE_LATITUDE = st_coordinates(village_ctoid)[,"Y"]

jrc_geo = 
  jrc_geo %>% 
  inner_join(village_ctoid %>% st_drop_geometry(), 
             by = "PRO_VILLAGE_NAME", 
             multiple = "all")

jrc_geo_prosf = 
  jrc_geo %>% 
  st_as_sf(coords = c("s00q12__itw_longitude", "s00q12__itw_latitude"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 

jrc_geo_vlgsf = 
  jrc_geo %>% 
  st_as_sf(coords = c("VILLAGE_LONGITUDE", "VILLAGE_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 

# make the distance of every farmer, to it's village centroid 
if(!all.equal(jrc_geo_prosf$interview__key, jrc_geo_vlgsf$interview__key)){
  stop()
}
jrc_geo$FARM_VLG_DISTANCE_METERS <- 
  st_distance(jrc_geo_prosf, jrc_geo_vlgsf, by_element = TRUE) 

jrc_geo$FARM_VLG_DISTANCE_METERS %>% summary()
(farm_vlg_dist_outliers = boxplot.stats(jrc_geo$FARM_VLG_DISTANCE_METERS, coef = 2)$out %>% sort())

# We do not remove all outliers, because some still make sense (being 2-5km away from the village centroid). 
# Set the threshold at 5km. It's a bit arbitrary, but not too consequential because few obs. at stake. 
# and remember it's 5km from the centroid of other farms in ther same village, not from the actual center. 
# - it means that distance to these farms in opposite direction is greater. 

# We want to remove as many of these oddly coded coordinates, without removing whole villages
# Setting the threshold at 5, 4, or 3km still leaves 77 villages. 
# Setting it at 2439 (the smallest outlier) though, removes 4 more villages. 
# We want to keep these villages which possibly represent a kind of villages more spread apart. 
# Up to 10km does not seem absurd in a remote, rural context. 

jrc_aparts = 
  jrc_geo %>% 
  mutate(FARM_VLG_DISTANCE_METERS = as.numeric(FARM_VLG_DISTANCE_METERS)) %>% 
  filter(FARM_VLG_DISTANCE_METERS >= 10000)  

jrc_geo = 
  jrc_geo %>% 
  mutate(FARM_VLG_DISTANCE_METERS = as.numeric(FARM_VLG_DISTANCE_METERS)) %>% 
  filter(FARM_VLG_DISTANCE_METERS < 10000)  

# Of the 81 villages in total, 4 seem to have a problem in farm coordinates for all surveyd farmers.  
jrc_geo %>% 
  pull(PRO_VILLAGE_ID) %>% unique() %>% length()


rm(jrc_geo_vlgsf, jrc_geo_prosf)

# REDO the distribution of village areas, with outliers removed
village_bbox_2 = 
  jrc_geo %>% 
  st_as_sf(coords = c("s00q12__itw_longitude", "s00q12__itw_latitude"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) %>% 
  st_simplify(dTolerance = 0.001) %>%
  summarise(
    .by = PRO_VILLAGE_ID,
    geometry = st_as_sfc(st_bbox(st_union(geometry)))
  ) %>% 
  mutate(VILLAGE_BBOX_AREA_KM2 = set_units(st_area(geometry), "km2", na.rm = T)) %>% 
  arrange(desc(VILLAGE_BBOX_AREA_KM2))

# Note the distribution of village bounding boxes
village_bbox_2$VILLAGE_BBOX_AREA_KM2 %>% summary()
village_bbox_2$VILLAGE_BBOX_AREA_KM2 %>% quantile(probs = seq(.7, 1, by = .05))
village_bbox_2$VILLAGE_BBOX_AREA_KM2 %>% quantile(probs = seq(.7, 1, by = .05))%>% sqrt()

village_bbox_2$VILLAGE_BBOX_AREA_KM2 %>% quantile(probs = seq(.8, 0.9, by = .01))

village_bbox_2$VILLAGE_BBOX_AREA_KM2 %>% quantile(probs = seq(.95, 1, by = .01)) 
village_bbox_2$VILLAGE_BBOX_AREA_KM2 %>% quantile(probs = seq(.95, 1, by = .01)) %>% sqrt()

rm(village_bbox_2)

## Remove poorly represented villages 
# For the supply shed model specifically, we don't want villages that have too few 
# farmers, because they would not represent the whole village (and cell) well.
# BUT NO, actually we rather remove cells with few farmers after matching JRC data with cells. 
# jrc_geo = 
#   jrc_geo %>% 
#   group_by(PRO_VILLAGE_NAME) %>% 
#   mutate(VILLAGE_N_FARMERS = n()) %>% 
#   ungroup() 
# jrc_geo %>% 
#   summarise(.by = VILLAGE_N_FARMERS, 
#             NUMBER_VILLAGES = length(unique(PRO_VILLAGE_NAME))) %>% 
#   arrange(VILLAGE_N_FARMERS)
# jrc_geo = 
#   jrc_geo %>% 
#   filter(VILLAGE_N_FARMERS > 2)

## Link variables ----------------

### Distance producer-intermediary -------------
jrc_geo_prosf = # redo it since we have removed rows to jrc_geo
  jrc_geo %>% 
  st_as_sf(coords = c("s00q12__itw_longitude", "s00q12__itw_latitude"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 

jrc_geo_itmsf <- 
  jrc_geo %>% 
  st_as_sf(coords = c("BUYER_LONGITUDE", "BUYER_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 

# dept4326 <- st_transform(departements, crs = 4326)

# ggplot() +
#   geom_sf(data = jrc_geo_prosf, aes(col = "black")) + 
#   geom_sf(data = jrc_geo_itmsf, aes(col = "red")) +
#   geom_sf(data = dept4326, fill = "transparent") 

if(!all.equal(jrc_geo_prosf$interview__key, jrc_geo_itmsf$interview__key)){
  stop()
}

# Since the filtering is the same (on the availability of coordinates for both ends), both subsets have the same rows      
jrc_geo$LINK_DISTANCE_METERS <- 
  st_distance(jrc_geo_prosf, jrc_geo_itmsf, by_element = TRUE)

# ggplot(jrc_geo, aes(x=LINK_DISTANCE_METERS)) + 
#   geom_histogram() +
#   theme(axis.title.y = element_blank()) + 
#   labs(x = "Producer-intermediary distance") 

# Here, we do not remove these outliers, because they include very small distances, 
# and because, unlike with the sustaincocoa data, we do not need to do it as a step to get to clean matches with IC2B. 
jrc_geo$LINK_DISTANCE_METERS %>% summary()
(dist_outliers = boxplot.stats(jrc_geo$LINK_DISTANCE_METERS, coef = 2)$out %>% sort())



### Volumes ------------
intersect(section3a$JRC_BUYER_ID, jrc_geo$JRC_BUYER_ID)
jrc_geo$JRC_BUYER_ID %>% anyNA()
# section3a %>% filter(is.na(JRC_BUYER_ID)) %>% View()

# check that all buyer ids in jrc_geo have a match in section3a 
# (not necessarily the case because we removed some rows without coordinates to obtain jrc_geo)
stopifnot(
  length(setdiff(jrc_geo$JRC_BUYER_ID, section3a$JRC_BUYER_ID)) == 0 
)

jrc_geo =
  jrc_geo %>% 
  inner_join(section3a %>% select(PRODUCER_ID, JRC_BUYER_ID, 
                                  LINK_VOLUME_KG = kg_both_season), 
            by = c("PRODUCER_ID", "JRC_BUYER_ID")) # section3a is a producer-level data set, so we match on both producer and buyer ids.  
            # multiple = "all") 
# multiple are not expected, because although one producer may have several buyers in section3a, we removed these as they were very few and not properly given a different buyer ID. 

if(jrc_geo$JRC_BUYER_ID %>% anyNA()){stop("unexpected introduction of NAs")}

# section3a %>% distinct(PRODUCER_ID, JRC_BUYER_ID) %>% nrow()


### Link ID ------------

# Create a link ID that uniquely identifies them
jrc_geo = 
  jrc_geo %>% 
  # group by buyer simplif name and coop bs id necessary because the latter is many times NAs for HH not matched with IC2B.
  group_by(PRODUCER_ID, JRC_BUYER_ID) %>% 
  mutate(LINK_ID = cur_group_id()) %>% 
  ungroup()

stopifnot(jrc_geo$LINK_ID %>% unique() %>% length() == nrow(jrc_geo))


## Join with IC2B ---------

## Restrict to coops
jrc_geo_coops = 
  jrc_geo %>% 
  filter(BUYER_IS_COOP)

nrow(jrc_geo_coops)
length(unique(jrc_geo_coops$JRC_BUYER_ID))
length(unique(jrc_coops_merge$JRC_BUYER_ID))
jrc_geo_coops$LINK_DISTANCE_METERS %>% summary()



### Prepare IC2B to join --------
coopbs19 = 
  coopbs %>% 
  filter(YEAR == 2019) %>% 
  # temporary necessary
  mutate(JRC_BUYER_IDS = gsub(pattern = "NA \\+ ", replacement = "", x = JRC_BUYER_IDS)) #%>% 
  # pull(JRC_BUYER_IDS) %>% unique()

if(anyNA(jrc_geo_coops$JRC_BUYER_ID)){stop("the merge will match all non-JRC coops in IC2B")}
if(nrow(coopbs19) != length(unique(coopbs19$COOP_BS_ID))){stop("there's a pb in coopbs19")}

### Match with IC2B -------

jrc_geo_coops =
  jrc_geo_coops %>% 
  left_join(coopbs19 %>% select(JRC_BUYER_IDS, COOP_BS_ID), 
            by = join_by("JRC_BUYER_ID" == "JRC_BUYER_IDS"))

# they all match
if(anyNA(jrc_geo_coops$COOP_BS_ID)){stop("not all observation is matched back with IC2B, which is not expected.")}
if(
  distinct(jrc_geo, PRODUCER_ID) %>% nrow() != nrow(jrc_geo)
   ){stop("PRODUCER_ID is supposed to identify rows in jrc but this is not the case and will cause troubles")}

### Merge back ---------- 

# add this information back to all jrc geo links
nrchecks = nrow(jrc_geo)
jrc_geo = 
  jrc_geo %>% 
  left_join(
    jrc_geo_coops %>% select("PRODUCER_ID", "COOP_BS_ID"), 
    by = "PRODUCER_ID"
  )
stopifnot(nrow(jrc_geo) == nrchecks)


# Number of JRC coops, in total, in IC2B, and in IC2B that were not known before
jrc$JRC_BUYER_ID %>% unique() %>% length() # 171 buyers
jrc_geo$JRC_BUYER_ID %>% unique() %>% length() # 112 geolocated 

jrc_coops_merge$JRC_BUYER_ID %>% unique() %>% length() # 47 coops
jrc_geo %>% filter(BUYER_IS_COOP) %>% pull(JRC_BUYER_ID) %>% unique() %>% length() # 28 are geolocated coops

coopbs19 %>%
  filter(IS_ANY_JRC) %>% 
  distinct(COOP_ID) %>% nrow() # 47 in IC2B 
  
coopbs19 %>%
  filter(IS_ALL_JRC) %>% 
  distinct(COOP_ID) %>% nrow() # 36 not known before



# Checks -------------
# Distribution of volumes between coops and other buyers, across the survey

jrc_geo$LINK_VOLUME_KG %>% summary()
jrc_geo %>% 
  select(LINK_VOLUME_KG, BUYER_IS_COOP) %>% 
  split(.$BUYER_IS_COOP) %>% map(summary)

jrc_coop_vol_tonne = 
  round(sum(filter(jrc_geo, BUYER_IS_COOP)$LINK_VOLUME_KG, na.rm = T)/3, 3)
jrc_other_vol_tonne = 
  round(sum(filter(jrc_geo, !BUYER_IS_COOP)$LINK_VOLUME_KG, na.rm = T)/3, 3)

(jrc_coop_vol_tonne / (jrc_coop_vol_tonne + jrc_other_vol_tonne))

# And aggregating first by village 
vlg_vols =
  jrc_geo %>%
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
       nrow(jrc_geo), " farmer-buyer links, between ",
       length(unique(jrc_geo$PRODUCER_ID)), " farmers located in ",
       length(unique(jrc_geo$PRO_VILLAGE_NAME)), " villages and ",
       length(unique(jrc_geo$JRC_BUYER_ID)), " buyers. ",
       nrow(filter(jrc_geo, BUYER_IS_COOP)), " links are with cooperatives, ", 
       nrow(filter(jrc_geo, !is.na(COOP_BS_ID))), " of which are links with IC2B, and ", 
       nrow(filter(jrc_geo, !BUYER_IS_COOP)), " are links with another kind of buyers. ",
       "Looking at the ", nrow(filter(jrc_geo, !is.na(LINK_VOLUME_KG))), " links with clean volume information, ",
       "these farmers sell ", jrc_coop_vol_tonne, " tonnes to coops and ", jrc_other_vol_tonne, " tonnes to other buyers."
)

## Export --------------------

# Prepare for merging with master (called civ here)
if("Ghana" %in% jrc_geo$s00q4__country){stop()}

toexport =
  jrc_geo %>% 
  mutate(LINK_YEAR = 2019,
         DATA_SOURCE = "JRC", 
         PRO_ID = paste0("JRC_FARMER_",PRODUCER_ID),
         LINK_ID_ONLYACTUAL = paste0("JRC_FARMER_",LINK_ID)) %>% # jrc$i00q21__itw_date %>% unique()
  # keep only the variables that we can also compute in other data sources than JRC. 
  # + the cocoa farm area to use to weight cells by representativeness
  # and apply some naming conventions (order does not matter)
  select(LINK_YEAR, PRO_ID, COOP_BS_ID, 
         LINK_ID_ONLYACTUAL,
         BUYER_IS_COOP, 
         LINK_ACTUALONLY_DISTANCE_METERS = LINK_DISTANCE_METERS, # actual only because distance is computed for all potential links in prepared_main_dataset.R 
         LINK_VOLUME_KG,
         PRO_VILLAGE_NAME,
         # PRO_DEPARTMENT_NAME = s00q7__dst,
         BUYER_LONGITUDE, # called BUYER_L although it's not only coop's buying stations. 
         BUYER_LATITUDE,
         # VILLAGE_LONGITUDE,
         # VILLAGE_LATITUDE,
         PRO_LONGITUDE = s00q12__itw_longitude, 
         PRO_LATITUDE  = s00q12__itw_latitude, 
         PRO_COCOA_FARMLAND_HA = s02cq3__cocoaland_ha
         )  # order does not matter

# remove any duplicate in these attributes we are interested in (but now they are already removed earlier)
toexport = 
  toexport %>% 
  distinct()

write_csv(toexport,
          file = here("temp_data", "preprocessed_jrc_data", "jrc_links_standardized.csv"),
          na = "NA", 
          append = FALSE, 
          col_names = TRUE)









# SUPPLY SHED MODEL - PART 2 ------
# Here we add or just use more variables (JRC-specific) to the link data.  

jrc$s02aq301__km_home_plot %>% summary()
quantile(jrc$s02aq301__km_home_plot, 0.9, na.rm = TRUE)

## Merger of link data and intermediary data -------
# Isolate intermediary data from the jrc join
itm <- 
  jrc %>% 
  filter(!is.na(JRC_BUYER_ID)) %>% 
  # need to do that because jrc is a join and not a stack of producers and interm. 
  # (so it has several rows for the same interm. when a producer sells to the same guy)
  distinct(JRC_BUYER_ID, .keep_all = TRUE) %>% 
  filter(!is.na(BUYER_LONGITUDE) & !is.na(BUYER_LATITUDE)) %>% 
  select(!starts_with("s0")) %>% 
  st_as_sf(coords = c("BUYER_LONGITUDE", "BUYER_LATITUDE"))

# Among the 170 distinct intermediaries, 146 have coordinates. 
jrc$JRC_BUYER_ID %>% unique() %>%  na.omit() %>% length() # 170
nrow(itm) 

## Link variables -------------
# Merge section3a with producer survey data
link_itm <- 
  left_join(section3a, 
            itm, 
            by = "JRC_BUYER_ID")

## Producer variables -----------

## Intermediary variables --------------












# PLOT ---- 

waf_sf <- ne_countries(country = c("IVORY COAST", "GHANA"), returnclass = "sf") %>% st_geometry() %>% 
  st_sf(crs = 4326)

jrc_pt <- 
  jrc %>% 
  filter(!is.na(s00q12__itw_latitude)) %>% 
  st_as_sf(coords = c("s00q12__itw_longitude", "s00q12__itw_latitude")) %>% 
  st_geometry() %>% 
  st_sf(crs = 4326)

ggplot() +
  geom_sf(data = waf_sf, aes(col = "black")) + #
  geom_sf(data = jrc_pt, aes(col = "red"))

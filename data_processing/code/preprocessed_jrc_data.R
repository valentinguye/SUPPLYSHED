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

# this is a geodetic crs, i.e. not projecting to a plan. But I haven't found a proj one. for Côte d'Ivoire for now. 
civ_crs = 4226 # https://epsg.io/4226 

# load in particular the function fn_trader_to_group_names, str_trans, ... 
source(here("code", "USEFUL_STUFF_manually_copy_pasted.R"))

# READ ------------ 

# Departements (districts)
departements <- s3read_using(
  object = "cote_divoire/spatial/BOUNDARIES/DEPARTEMENT/OUT/CIV_DEPARTEMENTS.geojson",#"cote_divoire/spatial/BOUNDARIES/DEPARTEMENT/OUT/ci_departments_wgs84_level4.geojson", 
  bucket = "trase-storage",
  FUN = read_sf,
  #sheet = "Cacao", 
  #skip = 3,
  opts = c("check_region" = T)
)

kit_farm_production <- 
  read_delim(
    get_object("cote_divoire/cocoa/production_estimates/kit_farm_yield_observations.csv",
               bucket = "trase-storage",
               check_region = T),
    delim = ";"
  ) %>%
  mutate(cocoa_prod_total_tonnes = cocoa_prod_total_kgs/1000) %>% 
  select(cocoa_prod_total_tonnes)


# 'cocoa_UCLouvain which contains both producer information (1,219 producers) and buyer information (170 buyers).'
jrc = read.dta13(here("input_data", "JRC", "Data sharing UC Louvain", "cocoa_UCLouvain.dta"),
                 convert.factors = TRUE, # we want those labels. this is the default. 
                 generate.factors=TRUE,
                 nonint.factors = TRUE) # this is not the default. Necessary to get labels (and thus values of interest) for company (among other vars)


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

# Economic activity of the household
econact = read.dta13(here("input_data", "JRC", "Data sharing UC Louvain", "econ_act_UCLouvain.dta"),
                 convert.factors = TRUE, # we want those labels. this is the default. 
                 generate.factors=TRUE,
                 nonint.factors = TRUE) # this is not the default. Necessary to get labels (and thus values of interest) for company (among other vars)

# it's only different variables in there.
intersect(names(jrc), names(section3a))

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
    s00q10__zd = Village,
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
    i00q6__itw_ctr = Country_buyer,
    i00q7__itw_reg = Region_buyer,
    i00q8__itw_spf = sp_buyer,
    i00q9__itw_village = EA_buyer,
    i00q10__ID = buyer_id,
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
    i04aq15__buyer_code = i04aq15,
    i04aq16__tonne_supply_2018 = i04aq16,
    i04aq17__tonne_supply_2019 = i04aq17,
    i04aq18__buyer_city = i04aq18,
    i04aq19__price_cfa_per_kg_2019 = i04aq19,
    i04aq20__km_cp_city = i04aq20,
    i04aq24__buyer_type = i04aq24,
    i04aq24_oth__buyer_type_oth = i04aq24_oth,
    i00q10__ID = buyer_id)

# Rename section 3A data 

# qty_buy_inkg is the kg purchased by this intermediary to this producer in the whole year. 
section3a %>% filter(qty_buy_inkg != light_sold_to_this_buy_kg + main_sold_to_this_buy_kg) %>% nrow()

section3a = 
  section3a %>% 
  rename(
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

### Producer ID ------------
# FOR NOW, SUPPOSE THAT ZD AND HH IDENTIFY THE PRODUCER (pending a reply from Katharina)
jrc = 
  jrc %>% 
  mutate(producer_id = paste0("ZD-",s00q10__zd,"_HH-",s00q11__hh_id)) 

jrc$producer_id %>% unique() %>% na.omit() %>% length()
jrc$interview__key %>% unique() %>% length()
jrc$s00q10__zd %>% unique() %>% length()
is.na(jrc$s00q10__zd) %>% sum()

### Make booleans -------
jrc = 
  jrc %>% 
  mutate(across(contains("_is_"), ~if_else(. %in% c("oui", "yes"), TRUE, FALSE)))

# numeric variables 

### Make coordinates of intermediary --------
jrc =
  jrc %>% 
  mutate(
    ITM_LONGITUDE = case_when(
      is.na(i00q24__longitude_cp) & !is.na(i00q21__itw_longitude) & i00q22__is_cp ~ i00q21__itw_longitude, 
      TRUE ~ i00q24__longitude_cp
    ),
    ITM_LATITUDE = case_when(
      is.na(i00q24__latitude_cp) & !is.na(i00q21__itw_latitude) & i00q22__is_cp ~ i00q21__itw_latitude, 
      TRUE ~ i00q24__latitude_cp
    )
  ) 
if(nrow(jrc %>% filter(is.na(ITM_LATITUDE))) != nrow(jrc %>% filter(is.na(ITM_LATITUDE)))){
  stop("pb in missing gps coords")}

### Recognize cooperatives ------------
jrc = 
  jrc %>% 
  mutate(IS_COOP = (grepl("coop", i01bq3__type) | grepl("coop", i01bq3_oth__type_oth)) & 
           # don't count them as coop, as they will have different values than in coops in scales etc. 
           i01bq3__type != "délégué de coopérative")


# IC2B  --------

# filter to coops only 
jrc_coops = 
  jrc %>% 
  filter(IS_COOP) 

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
  filter(is.na(ITM_LONGITUDE)) %>% 
  select(SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, ITM_LONGITUDE, ITM_LATITUDE, everything()) %>% 
  View()


### Remove duplicate coops -----
# Those are from cases where several farmers report to sell to the same coop
jrc_coops %>% 
  arrange(SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, ITM_LONGITUDE, ITM_LATITUDE) %>% 
  select(SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, ITM_LONGITUDE, ITM_LATITUDE, everything()) %>% 
  View()

# if this passes, it means all the heterogeneity between coops is captured by their identifiers.
if(
jrc_coops %>% 
  select(SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, ITM_LONGITUDE, ITM_LATITUDE,
         starts_with("i0")) %>% 
  distinct(.keep_all = TRUE) %>% 
  nrow() != nrow(distinct(jrc_coops, SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, ITM_LONGITUDE, ITM_LATITUDE))
){stop("some info in intermediary survey responses are lost")}

jrc_coops = 
  jrc_coops %>% 
  distinct(SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, ITM_LONGITUDE, ITM_LATITUDE, .keep_all = TRUE)

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
            by = "i00q10__ID", 
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
  select(YEAR, SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME,  
         DISTRICT_GEOCODE, LOCALITY_NAME, COUNTRY_NAME, TRADER_NAME, 
         # NUMBER_FARMERS, 
         TOTAL_FARMERS)  # order does not matter

### Export -----

write_csv(jrc_coops_merge,
          file = here("temp_data", "preprocessed_jrc_data", "jrc_coops_IC2B_standardized.csv"),
          na = "NA", 
          append = FALSE, 
          col_names = TRUE)



# SUPPLY SHED MODEL ------

# We want 2 sf objects with as many rows, in the same order, to run st_distance(by_element = TRUE) on them. 
# These objects have one row per actual producer-intermediary link, as per jrc object. 
# One represents the producer end (and coords), while the other represents the intermediary end (and coords).  
# In other words, we just want to split the jrc object in two... 

## Filter geo-located links -----------
# We need spatial info on both ends
jrc_geo <- 
  jrc %>% 
  filter(!is.na(s00q12__itw_longitude) & !is.na(s00q12__itw_latitude) & 
           !is.na(ITM_LONGITUDE) & !is.na(ITM_LATITUDE)) 

# this is 662 producers linked with 118 buyers
jrc_geo$producer_id %>% unique() %>% length()
jrc_geo$i00q10__ID %>% unique() %>% length()
# of which 159 producers are linked with 29 cooperatives. 
jrc_geo %>% 
  filter(IS_COOP) %>% 
  pull(producer_id) %>% 
  unique %>% 
  length()
jrc_geo %>% 
  filter(IS_COOP) %>% 
  pull(i00q10__ID) %>% 
  unique %>% 
  length()


jrc_geo$s00q12__itw_longitude %>% summary()
jrc_geo$s00q12__itw_latitude %>% summary()
jrc_geo$ITM_LONGITUDE %>% summary()
jrc_geo$ITM_LATITUDE %>% summary()

pro_sf <- 
  jrc_geo %>% 
  st_as_sf(coords = c("s00q12__itw_longitude", "s00q12__itw_latitude"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 

itm_sf <- 
  jrc_geo %>% 
  st_as_sf(coords = c("ITM_LONGITUDE", "ITM_LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(civ_crs) 

# dept4326 <- st_transform(departements, crs = 4326)

# ggplot() +
#   geom_sf(data = pro_sf, aes(col = "black")) + 
#   geom_sf(data = itm_sf, aes(col = "red")) +
#   geom_sf(data = dept4326, fill = "transparent") 

if(!all.equal(pro_sf$interview__key, itm_sf$interview__key)){
  stop()
}

## Distance producer-intermediary 

# Since the filtering is the same (on the availability of coordinates for both ends), both subsets have the same rows      
jrc_geo$DISTANCE_PRO_ITM <- 
  st_distance(pro_sf, itm_sf, by_element = TRUE)

jrc_geo_coops = 
  jrc_geo %>% 
  filter(IS_COOP)

jrc_geo$DISTANCE_PRO_ITM %>% summary()
jrc_geo_coops$DISTANCE_PRO_ITM %>% summary()

ggplot(jrc_geo_coops, aes(x=DISTANCE_PRO_ITM)) + 
  geom_histogram() +
  theme(axis.title.y = element_blank()) + 
  labs(x = "Producer-intermediary distance") 


## Export --------------------

# Prepare for merging with master (called civ here)
if("Ghana" %in% jrc_geo$s00q4__country){stop()}

toexport =
  jrc_geo_coops %>% 
  mutate(YEAR = 2019) %>% # jrc$i00q21__itw_date %>% unique()
  # keep only the variables that we can also compute in other data sources than JRC. 
  select(YEAR, PRO_ID, DISTANCE_PRO_ITM, 
         PRO_DEPARTMENT_NAME = s00q7__dst,
         PRO_LONGITUDE = s00q12__itw_longitude, 
         PRO_LATITUDE  = s00q12__itw_latitude)  # order does not matter

# should we add coop identifiers to then match IC2B? 

write_csv(toexport,
          file = here("temp_data", "preprocessed_jrc_data", "jrc_links_standardized.csv"),
          na = "NA", 
          append = FALSE, 
          col_names = TRUE)



## Merger of link data and intermediary data -------
# Isolate intermediary data from the jrc join
itm <- 
  jrc %>% 
  filter(!is.na(i00q10__ID)) %>% 
  # need to do that because jrc is a join and not a stack of producers and interm. 
  # (so it has several rows for the same interm. when a producer sells to the same guy)
  distinct(i00q10__ID, .keep_all = TRUE) %>% 
  filter(!is.na(ITM_LONGITUDE) & !is.na(ITM_LATITUDE)) %>% 
  select(!starts_with("s0")) %>% 
  st_as_sf(coords = c("ITM_LONGITUDE", "ITM_LATITUDE"))

# Among the 170 distinct intermediaries, 146 have coordinates. 
jrc$i00q10__ID %>% unique() %>%  na.omit() %>% length() # 170
nrow(itm) 

## Link variables -------------
# Merge section3a with producer survey data
link_itm <- 
  left_join(section3a, 
            itm, 
            by = join_by(""=="i00q10__ID"))

## Producer variables -----------

## Intermediary variables --------------

link_itm <- 
  left_join(section3a, 
            itm, 
            by = join_by(""=="i00q10__ID"))










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

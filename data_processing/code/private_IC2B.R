# Purpose of this script: integrate private data on cooperatives in the IC2B workflow.

# This script mirrors cam_v4.R in the making of IC2B for SEI-PCS Cote d'Ivoire cocoa v1.1. 
# BUT, it includes in addition not-open access (private) data. 

# So it first consolidates private data with already consolidated public disclosures, and then run the IC2B algoithms on the extended input data. 

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
library(ggpubr)
library(units)
library(scales)
library(kableExtra)
library(here)
library(readstata13)
library(sjmisc)

# Assets and functions -----------------------------------------------------


## PRIVATE INPUTS TO CONSOLIDATE ####

### RA data #### 
# This one is not helpful, because the GPS represents just a "central representative point" according to the Readme attached to it. 
# utzra = st_read(here("input_data", "rainforest_alliance", "private", "UTZ_RA", "MembSumary_2019_CIVGHA_ProtectAreaAnaly.shp"))

### FT data #### 
# It is not completely clear whether variable 'Info on members - Total' represents cooperative members or certified cooperative members. 
# Anyway, it can be used to estimate the minimum total number of farmers in the coop. 

ft = read_xlsx(here("input_data", "fairtrade", "SPOs_DATABASE_GH_CDI_FI_Consultants.xlsx"), sheet = 1, col_names = TRUE) 

### JRC data #### 
jrcmerge = read.csv(here("temp_data", "preprocessed_jrc_data", "jrc_coops_IC2B_standardized.csv"))

## PUBLIC DISCLOSURES, CONSOLIDATED  #### 

civ <- s3read_using(
  object = "world/cocoa/logistics/out/consolidated_disclosures.csv",
  bucket = "trase-storage",
  opts = c("check_region" = T),
  FUN = read_delim,
  delim = ";") %>% 
  filter(DISCL_COUNTRY_NAME == "IVORY_COAST") %>% 
  mutate(DISCL_TOTAL_FARMERS = NA) # for the public IC2B we did not do that. But here we need to welcome this variable from the JRC/Sellare/other coop survey data. 

init <- civ

# country names have already been cleaned in world/~/consolidated_disclosures.R
civ$DISCL_COUNTRY_NAME %>% unique()


# Departements (districts)
departements <- s3read_using(
  object = "cote_divoire/spatial/BOUNDARIES/DEPARTEMENT/OUT/CIV_DEPARTEMENTS.geojson",#"cote_divoire/spatial/BOUNDARIES/DEPARTEMENT/OUT/ci_departments_wgs84_level4.geojson", 
  bucket = "trase-storage",
  FUN = read_sf,
  #sheet = "Cacao", 
  #skip = 3,
  opts = c("check_region" = T)
)
# 
# # List of cleaned trader group names - not necessary currently
# trad_nam <-
#   s3read_using(
#     FUN = function(object) read.csv2(object, na.strings = "NNN"),
#     object = "cote_divoire/trade/cd/export/trader_labels/cote_divoire_traders_from_customs_data_2020_2021_2022.csv",
#     bucket = "trase-storage",
#     opts = c("check_region" = TRUE)
#   )

# FUNCTIONS ####

# Trase palettes etc. for plots
# source(here::here("trase", "tools","traseviz", "R", "theme_trase.R"))

# load in particular the function fn_trader_to_group_names, str_trans, ... 
source(here("code", "USEFUL_STUFF_manually_copy_pasted.R"))

# This helper function is used to impute missing values within a group where the non-missing values are unique. 
unique_unique <- function(col_name){
  unq <- unique(col_name)[!is.na(unique(col_name))]
  if(length(unq) != 1){
    toret <- NA
  } else {
    toret <- unq
  }
  return(toret)
}

# missing_or_unique <- function(col_name){
#   unq <- unique(col_name)
#   if(all(is.na(unq)) | (anyNA(unq) & length(unq) == 2)){
#     toret <- TRUE
#   } else {
#     toret <- FALSE
#   }
#   return(toret)
# }

# This helper function selects the most frequent value of a character or numeric column (for use typically within a group). 
# This is used typically on coop names and on coordinates. 
# [1] and mean() reduce to a single value to accommodate where different values occur equally frequently.
unique_mode <- function(col_name){
  if(is.character(col_name)){
    um <- as.character(Mode(factor(col_name, exclude = NA), na.rm = TRUE))[1]
  } 
  if(is.numeric(col_name)){
    if(length(col_name)>2){
      um <- as.numeric(Mode(col_name, na.rm = TRUE)) %>% mean() 
    } else {
      um <- mean(col_name, na.rm=T)
    }
  } 
  return(um)
}



# This function handles cases where a full name and an abbreviated name are given in the abrv name variable.
fn_clean_abrvname1 <- function(col_name){
  case_when(
    grepl("UNITE COOPERATIVE AGRICOLE DE DANANE", col_name) ~ "COOP UDAN",
    grepl("COOPERATIVE AGRICOLE BACON ESPOIR", col_name) ~ "CABES",
    grepl("COOPERATIVE AGRICOLE ABOTRE DE NIABLE", col_name) ~ "COAANI",
    grepl("COOPERATIVE AGRICOLE BENKADI", col_name) ~ "COOPABENKADI",
    grepl("COOPERATIVE AGRICOLE D'AGNANFOUTOU", col_name) ~ "COESAG",
    grepl("COOPERATIVE AGRICOLE ZEMESS TAABA DE GBABAM", col_name) ~ "COOPAZEG",
    grepl("COOPERATIVE DES PRODUCTEURS DE YAKASSE ATTOBROU", col_name) ~ "COOPROYA",
    grepl("SOCIETE AGRICOLE BINKADI DE BROUDOUGOU PENDA CA", col_name) ~ "SOCABB",
    grepl("SCOOPAO SOCIETE COOPERATIVE DES AGRICULTEURS DE PETIT OUAGA", col_name) ~ "SCOOPAO",
    grepl("SOCIETE COOPERATIVE SIMPLIFIEE AGRICOLE KAMBONOU DE ARRAH", col_name) ~ "SCOAKA",
    grepl("SOCIETE COOPERATIVE ESPERENCE DE KPELEKRO", col_name) ~ "SOCAEK",
    grepl("SOCIETE COOPERATIVE AVEC CONSEIL D'ADMINISTRATION ENTENTE DE SEGUELA", col_name) ~ "COOP CA ES",
    grepl("COOPERATIVE DES PRODUCTEURS AGRICOLES DE NIABLE", col_name) ~ "COOP CA PAN",
    grepl("COOPERATIVE YEYONIAN DU CANTON ANIASSUE", col_name) ~ "COOPYCA",
    grepl("SOCIETE COOPERATIVE AGRICOLE DE GNATO AVEC CONSEIL D'ADMINISTRATION", col_name) ~ "SOCAG",
    grepl("SOCIETE COOPERATIVE AGRICOLE DE BAYOTA", col_name) ~ "SOCABA COOP CA",
    grepl("SPAD GAGNOA", col_name) ~ "SPAD GAGNOA", # ETG adds the name of the manufacturer to some coop names. 
    grepl("SCAT I|SCAT 1", col_name) ~ "SCAT 1",
    grepl("\\(CA\\)$", col_name) ~ " CA ",
    grepl("\\(COOP\\)$", col_name) ~ " COOP ",
    grepl("\\(SCOOP\\)$", col_name) ~ " SCOOP ",
    TRUE ~ col_name
  )
}

# This function removes some common characters used in abreviated coop names
fn_clean_abrvname2 <- function(col_name){
  cleaned_col <- str_trans(str_trim(col_name))
  cleaned_col <- gsub(pattern = "\\.|[(]|[)]| WAREHOUSE$", "", cleaned_col)
  cleaned_col <- gsub(pattern = "\n|\\_|\\/|-", " ", cleaned_col)
  cleaned_col <- gsub(pattern = "Ô", "O", cleaned_col)
  # cleaned_col <- gsub(pattern = "A N E K", "ANEK", cleaned_col) 
  
  cleaned_col <- str_squish(cleaned_col)
  
  return(cleaned_col)
}

# This function removes generic terms like CA COOP and SCOOPS
fn_clean_abrvname3 <- function(col_name){
  gsub(pattern = "COOP CA | COOP CA$|COOP | COOP$|SCOOP | SCOOP$|SCOOPS | SCOOPS$", 
       replacement = "", 
       x = col_name)
}


fn_clean_fullname <- function(col_name){
  fn <- gsub(pattern = "SOCIETE COOPERATIVE |STE COOP |COPERATIVE |COOPRATIVE |SOCIETE AGRICOLE COOPERATIVE |ENTREPRISE COOPERATIVE |ENTREPRISE AGRICOLE COOPERATIVE |ENTREP COOPERA ", 
             replacement = "COOPERATIVE ", 
             x = col_name)
  fn <- gsub(pattern = "DU CAFE&CACAO|DU CAFE & CACAO|DU CAFE& CACAO|DU CAFE &CACAO|DE CAFE&CACAO|DE CAFE & CACAO|DE CAFE& CACAO|DE CAFE &CACAO|DU CAFE-CACAO|DU CAFE - CACAO|DU CAFE- CACAO|DU CAFE -CACAO|DE CAFE-CACAO|DE CAFE - CACAO|DE CAFE- CACAO|DE CAFE- CACAO|CAFE&CACAO|CAFE & CACAO|CAFE& CACAO|CAFE &CACAO|CAFE-CACAO|CAFE - CACAO|CAFE- CACAO|CAFE -CACAO|DU CAFE ET DU CACAO|DE CAFE ET DE CACAO|CAFE ET DU CACAO|CAFE ET DE CACAO|DU CAFE ET CACAO|DE CAFE ET CACAO|CAFE ET CACAO|DU CAFE CACAO|DE CAFE CACAO|CAFE CACAO|CAFECACAO|DU CAFE COCOA",  # hopefully, the latter is the only case of "COCOA" instead of "CACAO" 
             replacement = "STD_CAFE_CACAO_STD", 
             x = fn)
  # Move it back to something more readable, that's just a displaying matter
  fn <- gsub(pattern = "STD_CAFE_CACAO_STD",
             replacement = "CAFE ET CACAO", 
             x = fn)
  return(fn)
}

# and some manual changes where some eyeballing makes us realize that two full names are actually the same coop.
# /!\ these apply to post fn_clean_fullname strings /!\ 
fn_clean_fullname_manual <- function(col_name){
  case_when(
    grepl("COOPERATIVE AGRICOLE SOLIDARITE DE BLOLEQUIN", col_name) ~ "ENTREPRISE COOPERATIVE AGRICOLE DE BLOLEQUIN",
    grepl("COOPERATIVE POUR L'AMELIORAT DU REVENU DU PLANTEUR DE CI", col_name) ~ "COOPERATIVE POUR L'AMELIORATION DU REVENU DU PLANTEUR", 
    grepl("COOPERATIVE SABABOUGNOUMAN DAGADJI", col_name) ~ "COOPERATIVE SABABOUGNOUMAN DE DAGADJI",
    grepl("ROBERT", col_name) & grepl("PORTE", col_name) ~ "ENTREPRISE COOPERATIVE AGRICOLE DES PRODUCTEURS CAFE ET CACAO DE ROBERT-PORTE", # note that "CAFE ET CACAO" is to match the output of the above function
    grepl("MAN EDI ANOUANZE", col_name) ~ "SOCIETE COOPERATIVE AGRICOLE MAN EDI ANOUANZE",
    col_name == "CNIBO" ~ "COOPERATIVE AGRICOLE NIBI D'OKROUYO",
    TRUE ~ col_name
  )
}


# --- CONSOLIDATE PRIVATE DATA WITH PUBLIC DISCLOSURES ----------

## FT #### 
ftpro = 
  ft %>% 
  # remove first empty row
  filter(as.numeric(row.names(ft))>1) %>% 
  # and this column that's useless and just gives info on the row about ECOOKIM as a coop union 
  select(-...22) %>% 
  # remove that row
  filter(Name != "ECOOKIM ( Union des sociétés Coopératives Kimbe)") %>% 
  # remove coops that handle no cocoa
  mutate(across(where(is.character), str_trans)) %>% 
  filter(grepl("COCOA", Product) | is.na(Product)) 

# add other necessary columns

# Certifications
# ftpro %>% 
#   filter(`Certification Status (Certified/decertified/Suspended)` == "PERMISSION TO TRADE" | 
#            is.na(`Certification Status (Certified/decertified/Suspended)`)) %>% View
ftpro$`Programs  NON FT` %>% unique() %>% sort()
ftpro$`Others certification` %>% unique() %>% sort()

ftpro = 
  ftpro %>% 
  # clean other certification values
  mutate(
    `Programs  NON FT` = case_when(
      is.na(`Programs  NON FT`) | `Programs  NON FT` %in% c("-", "NA", "NO", "NON", "NONE", "UNKNOWN") ~ "9999", 
      TRUE ~ `Programs  NON FT`),
    `Others certification` = case_when(
      is.na(`Others certification`) | `Others certification` %in% c("NA", "NO", "NON", "NONE", "UNKNOWN") ~ "9999", 
      TRUE ~ `Others certification`)) %>% 
  # group all reported certifications and programs together. 
  rowwise() %>% 
  mutate(
    CERTIFICATION_NAME = paste(`Programs  NON FT`, `Others certification`, sep = "; "),
    CERTIFICATION_NAME = case_when(
      `Certification Status (Certified/decertified/Suspended)` == "CERTIFIED" ~ paste("FAIRTRADE; ", CERTIFICATION_NAME),
    TRUE ~ CERTIFICATION_NAME
    ),
    CERTIFICATION_NAME = str_squish(CERTIFICATION_NAME)
  )

# COOP NAME
ftpro = 
  ftpro %>%
  mutate(SUPPLIER_ABRVNAME = NA_character_ , 
         SUPPLIER_FULLNAME = NA_character_ ) %>% 
  
  mutate(
    Name = str_squish(Name),
    # custom edit 
    Name = case_when(
      Name == "COOP-CA PUA -SOCIETE COOPERATIVE DES PLANTEURS UNIS D'AGBOVILLE )" ~ "COOP-CA PUA (SOCIETE COOPERATIVE DES PLANTEURS UNIS D'AGBOVILLE )", 
      TRUE ~ Name
    ),
    
    # for other coops, there are several options, 
    # - sometimes acronym within parentheses
    # - sometimes other way round
    # - sometimes only acronym
    # - sometimes only full name
  
    # first extract what's within and outside parentheses
    OUTSIDE = str_trim(str_replace(Name, "\\s*\\(.*?\\)", "")),
    WITHIN = str_extract(Name, "(?<=\\().*?(?=\\))"), 
    # then decide whether each is a full or abreviated name 
    SUPPLIER_FULLNAME = case_when(
      nchar(WITHIN) > nchar(OUTSIDE) ~ WITHIN,
      nchar(WITHIN) < nchar(OUTSIDE) ~ OUTSIDE,
      TRUE ~ SUPPLIER_FULLNAME,
    ),
    SUPPLIER_ABRVNAME = case_when(
      nchar(WITHIN) < nchar(OUTSIDE) ~ WITHIN,
      nchar(WITHIN) > nchar(OUTSIDE) ~ OUTSIDE,
      TRUE ~ SUPPLIER_ABRVNAME,
    ), 
    # handle for ecookim, for which the above character length rule might not work
    # this is consistent with how we handle it below.
    SUPPLIER_ABRVNAME = case_when(
      grepl("ECOOKIM", Name) ~ WITHIN, 
      TRUE ~ SUPPLIER_ABRVNAME
    ), 
    # we don't want to register ECOOKIM as a full name. 
    SUPPLIER_FULLNAME = case_when(
      grepl("ECOOKIM", Name) ~ NA, 
      TRUE ~ SUPPLIER_FULLNAME
    ), 
    # this handles cases where within or outside is NA and the > conditions above were not working. 
    # it uses typical full name elements
    SUPPLIER_FULLNAME = case_when(
      is.na(SUPPLIER_FULLNAME) & grepl("COOPERATIVE|AGRICULTEUR|PRODUCTEUR", WITHIN) ~ WITHIN,
      is.na(SUPPLIER_FULLNAME) & grepl("COOPERATIVE|AGRICULTEUR|PRODUCTEUR", OUTSIDE) ~ OUTSIDE,
      TRUE ~ SUPPLIER_FULLNAME
    ),
    SUPPLIER_ABRVNAME = case_when(
      is.na(SUPPLIER_FULLNAME) & !grepl("COOPERATIVE|AGRICULTEUR|PRODUCTEUR", WITHIN) ~ WITHIN, 
      is.na(SUPPLIER_FULLNAME) & !grepl("COOPERATIVE|AGRICULTEUR|PRODUCTEUR", OUTSIDE) ~ OUTSIDE, 
      TRUE ~ SUPPLIER_ABRVNAME
    ),
    # finally, handle cases where within is NA, i.e. there was no parentheses (full names have been given at previous stage)
    SUPPLIER_ABRVNAME = case_when(
      is.na(SUPPLIER_ABRVNAME) & is.na(WITHIN) & !grepl("COOPERATIVE|AGRICULTEUR|PRODUCTEUR", OUTSIDE) ~ OUTSIDE, 
      TRUE ~ SUPPLIER_ABRVNAME
    )
  ) %>% 
  select(OUTSIDE, WITHIN, SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, Name, everything()) 

ftpro %>% distinct(SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME) %>% nrow()

# 'region' are departments. 
ftpro$Region %>% unique() %>% length()
# Call it area name to match civ at this point. 
ftpro = 
  ftpro %>% 
  mutate(AREA_NAME = Region)

ftpro %>% View()

# Traders/buyers --- DO NOT INCLUDE THIS INFORMATION FOR NOW, because: 
# it's not clear how it would be useful, 
# how it would not introduce bias in whatever we use it for,
# and how we would handle TRADER_NAME values coming from this data set. 
# 
# # Split rows with several trader links into several rows. 
# ftpro <- 
#   ftpro %>%
#   mutate(
#     TRADER_NAME = Traders,
#     TRADER_NAME = list(str_split(TRADER_NAME, pattern = ",|;|-|/")),
#     TRADER_NAME = map(TRADER_NAME, str_squish)
#   ) %>%
#   unnest(cols = c(TRADER_NAME)) 
# 
# ftpro = 
#   ftpro %>% 
#   mutate(TRADER_NAME = if_else(TRADER_NAME %in% c("UNKNOWN", "", " ", "NO", "NOT REACHED", "( FT)"), 
#                                NA, TRADER_NAME))

# remaining column names
names(ftpro)
ftpro = 
  ftpro %>% 
  mutate(NUMBER_FARMERS = as.numeric(`Infos on members - Total`)) 

# Prepare for merging with master (called civ here)
ftmerge =
  ftpro %>% 
  mutate(COUNTRY_NAME = "IVORY_COAST", 
         YEAR = 2019) %>% # that's arbitrary, just make it start the same year as all disclosures start, with the CAM, 
                          # so it's repeated the same way.
  select(YEAR, SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, # TRADER_NAME, 
         AREA_NAME, COUNTRY_NAME, CERTIFICATION_NAME, NUMBER_FARMERS)  # order does not matter

names(ftmerge) <- paste0("DISCL_", names(ftmerge))

ftmerge$COMPANY <- "FAIRTRADE"

initcoln <- ncol(civ)
initrown <- nrow(civ)
civ <- full_join(civ, ftmerge, 
                  by = intersect(colnames(civ), colnames(ftmerge)), multiple = "all") 

if(ncol(civ) != initcoln | nrow(civ)==initrown){stop("something went wrong in consolidating disclosure data.")}

# rm(ft, ftpro, ftmerge)

civ_ft <- civ 


## JRC ####

names(jrcmerge)[!names(jrcmerge) %in% c("DISTRICT_GEOCODE", "LOCALITY_NAME")] <- 
  paste0("DISCL_", names(jrcmerge)[!names(jrcmerge) %in% c("DISTRICT_GEOCODE", "LOCALITY_NAME")])

jrcmerge$COMPANY <- "JOINT RESEARCH CENTER"
names(jrcmerge)

initcoln <- ncol(civ)
initrown <- nrow(civ)
civ <- full_join(civ, jrcmerge, 
                 by = intersect(colnames(civ), colnames(jrcmerge)), multiple = "all") 

if(ncol(civ) != initcoln | nrow(civ)==initrown){stop("something went wrong in consolidating disclosure data.")}




# rm(jrc, jrc_coops, jrcmerge)

civ_jrc <- civ 

# --- CLEAN TRADER NAME ----------------------------------
# This is necessary to eventually match with custom data (and eases inspection for now) 
unique(civ$DISCL_TRADER_NAME) %>% sort()
unique(civ$COMPANY) %>% sort()

# We clean names, and identify which are actual cocoa traders manually. 

# **************** THIS IS NOT ONLY FOR SEI-PCS PURPOSE SO LEAVE IT HERE ***********************
# it is used to better infer the coop-level number of farmers here below, a strictly CAM-purpose 
# - and it just makes the trader name variable more meaningful for external CAM

# CLEANING 

## TRADER_NAME CREATED HERE
civ <- 
  civ %>%
  mutate(TRADER_NAME = DISCL_TRADER_NAME)

# Preliminary, minor cleaning of rows that have several traders separated by / (which creates one row in this case)
# There is only one case, "Blommer/Olam", and in this case the trader is OLAM. 
civ <- 
  civ %>% 
  mutate(TRADER_NAME = case_when(
    TRADER_NAME == "Blommer/Olam" ~ "OLAM", 
    TRUE ~ TRADER_NAME
  )
  )
# OTHERWISE, BLOMMER IS IN COMPANY VARIABLE A LOT (COMING FROM THE INITIAL CAM)
# BUT IT IS NOT A TRADER, ALTHOUGH IT SAYS IT SOURCES DIRECTLY FROM COOPS, IT SOURCES EVERYTHING FROM OLAM
# https://www.idhsustainabletrade.com/publication/flourish/olam-blommer-hershey/
# WHICH IS CONFIRMED AS IT BARELY APPEARS IN CUSTOMS DATA

# MAIN NAME CLEANING 
civ <- mutate(civ,
              TRADER_NAME =  fn_trader_to_group_names(fn_clean_corp_accronyms(str_trans(TRADER_NAME))),
              COMPANY =      fn_trader_to_group_names(fn_clean_corp_accronyms(str_trans(COMPANY)))) 

civ %>%  
  filter(COMPANY == "FAIRTRADE") %>% 
  distinct(DISCL_SUPPLIER_ABRVNAME, DISCL_SUPPLIER_FULLNAME) %>% 
  nrow() 

# at this point, DISCL_TRADER_NAME and TRADER_NAME are equally missing
all.equal(is.na(civ$DISCL_TRADER_NAME), is.na(civ$TRADER_NAME))

unique(civ$TRADER_NAME) %>% sort()
unique(civ$COMPANY) %>% sort()

# Manual edit of info from CAM V3 
# --> Remove from the company variable (which is supposed to give the name of companies that did disclose something once) 
# names of companies that never disclosed and are there spuriously, because of the CAM v3 (checked case by case). 
non_disclosing_companies <- c("COCOASOURCE")
civ %>% filter(COMPANY %in% non_disclosing_companies) %>% View()
# Note: There are also  Albert Heijn, Blommer and Touton but these did make disclosures (although we didn't collect them). 
# So it's possible that the CAM v3 has the info from these disclosures
# For Albert Heijn, it's a manufacturer sourcing from SOCOOPACDI, in collaboration with Tony's. Leave it since they do disclose this themselves. https://static.ah.nl/binaries/ah/content/assets/ah-nl/core/about/duurzaamheid/duurzaamheidsverslag-mensenrechten.pdf
# For Cocoasource it's different: it is a trader, and the info doesn't come from their own disclosures but from Tony and RA. 
civ <- 
  civ %>% 
  mutate(
    # Move Cocoasource to the trader name (leave the possibility that non_disclosing_companies contains non-trading companies)
    TRADER_NAME = case_when(
      COMPANY == "COCOASOURCE" & IS_CAM_V3 ~ "COCOASOURCE", 
      TRUE ~ TRADER_NAME
    ),
    # Remove it from the company var. 
    COMPANY = case_when(
      COMPANY %in% non_disclosing_companies & IS_CAM_V3 ~ NA, 
      TRUE ~ COMPANY
    ), 
    # Make sure it is the trader of Tony
    TRADER_NAME = case_when(
      grepl("TONY", COMPANY, ignore.case = TRUE) & IS_CAM_V3 ~ "COCOASOURCE", 
      TRUE ~ TRADER_NAME
    )
  )
# IS_CAM_V3 conditions makes sure that potential future disclosures by cocoasource are not affected. 


# IDENTIFY ACTUAL TRADERS 

# Not programmatically by looking for these names in customs data, because some appear there while they are not actual traders, 
# they only have small fractions of their likely total sourcing identified through exporter or importer in customs data. 

# Instead, we remove names of traders identified as making disclosures but which volumes spotted in customs data do not reflect total sourcing from Ivory Coast. 
# Note this information is still available in DISCL_TRADER_NAME ! 

# AND REMOVE ECOOKIM AS WELL: we don't want to count it as a disclosing trader because flows from 
# these cooperatives can be the same as those disclosed by other companies. In other words, we 
# treat Ecookim as a source of information for the CAM, like manufacturers, Rainforest Alliance and Fairtrade, 
# and not as a trader in the sense that we want to understand it in Trase.  
# this is to remove COMPANY values (in CAM V4) for those identified as making disclosures, 
# but for which volumes spotted in customs data do not reflect total sourcing from Ivory Coast.  
non_trading_companies <- 
  c("ALBERT HEIJN", "ALFRED RITTER", "ALTER ECO", 
    "BLOMMER", "COLRUYT", 
    "ECOOKIM", 
    "FERRERO",  "HERSHEY", 
    "MARS", "MONDELEZ", "NESTLE", "PURATOS", "RAINFOREST ALLIANCE", "FAIRTRADE",
    "JOINT RESEARCH CENTER",
    "TONY'S CHOCOLONELY", "VALRHONA")
# # Code to check their volumes in cd (read trade_data from DATA_FOR_SEIPCS_*)
# for(pot_trad in sort(unique(civ$COMPANY))){
#   vol_tonnes <- trade_data %>% filter(EXPORTER_GROUP_CLEAN==pot_trad | IMPORTER_GROUP_CLEAN==pot_trad) %>% 
#     pull(BEAN_EQUIVALENT_VOLUME) %>% sum() / 1000  
#   
#   print(paste0(pot_trad, ":", vol_tonnes, " MT"))
# }
# Companies found to have 0 volume, it's clear they are no traders. Then, it's case investigation: 
# Blommer & Hershey, see above 
# Ferrero sources more than 200 k MT in total, so it's unlikely that the 1626.6 MT found here in cd reflect all its sourcing.         
# Ethiquable: LEAVE IT AS A TRADER, as it is actually possible that they source 150 MT, since they say they sourced 40 MT in 2014 https://www.ethiquable.coop/fiche-producteur/sceb-commerce-equitable-cote-divoire-cacao
# Note: but Ethiquable buys from ECOOKIM actually in customs data, so it won"t appear in sankey eventually

civ <- 
  civ %>% 
  mutate( 
    # This is to do systematically what we did while collecting disclosures manually, in case it was omitted
    TRADER_NAME = case_when(
      is.na(TRADER_NAME) ~ COMPANY, 
      TRUE ~ TRADER_NAME
    ), 
    # But remove names of traders identified as making disclosures but which volumes spotted in customs data do not reflect total sourcing from Ivory Coast.  
    TRADER_NAME = case_when(
      TRADER_NAME %in% non_trading_companies ~ NA, # non_trading_companies object is defined in USEFUL_STUFF.R
      TRUE ~ TRADER_NAME
    ),
    # Also, remove trader names that were collected from non-trading company disclosures, *when this trader made a disclosure it self*. 
    # Otherwise, these flows will eventually be counted as additional direct sourcing for those traders, while they already report all their direct sourcing in their own disclosures.   
    # For instance, this removes the trader name values for the disclosures from Mondelez, while it leaves the values for Cocoasource which was disclosed by Tony's only (thanks to the last condition of the 3 below)
    TRADER_NAME = case_when(
      TRADER_NAME != COMPANY & COMPANY %in% non_trading_companies & TRADER_NAME %in% civ$COMPANY ~ NA, 
      TRUE ~ TRADER_NAME
    )
  )
# these are the flows removed from disclosing traders' account of direct supply by the third case_when above. 
# civ %>% filter(TRADER_NAME != COMPANY & COMPANY %in% non_trading_companies & TRADER_NAME %in% civ$COMPANY) %>% View()



exper_cln <- civ$TRADER_NAME %>% unique() %>% sort()
exper_cln

# civ %>%  filter(!is.na(TRADER_NAME) ) %>% View()
# civ %>%  filter(label == "ECOM" & TRADER_NAME=="OLAM" ) %>% View()


sum(!is.na(civ$DISCL_TRADER_NAME))
sum(!is.na(civ$TRADER_NAME))
sum(!is.na(civ$COMPANY))

civ %>%  
  filter(COMPANY == "FAIRTRADE") %>% 
  distinct(DISCL_SUPPLIER_ABRVNAME, DISCL_SUPPLIER_FULLNAME) %>% 
  nrow() 

civ %>%  
  filter(COMPANY == "JOINT RESEARCH CENTER") %>% 
  distinct(DISCL_SUPPLIER_ABRVNAME, DISCL_SUPPLIER_FULLNAME) %>% 
  nrow() 

# SEVERAL SUPPLIERS IN THE SAME ROW
# bc <- 
#   filter(civ, COMPANY == "BARRY CALLEBAUT") %>% 
#   mutate(DISCL_SUPPLIER_ABRVNAME = gsub(pattern = " WAREHOUSE", 
#                                         replacement = "",
#                                         x = DISCL_SUPPLIER_ABRVNAME)
#   )
# unique(bc$DISCL_SUPPLIER_ABRVNAME)[grepl(pattern = "ECOOAS", x = unique(bc$DISCL_SUPPLIER_ABRVNAME))]



# --- COOP IDENTIFICATION, FROM NAMES AND COORDINATES -----------------------------
# Eventual goal is to have a unique identifier for cooperatives, based on abbreviated names, full names and coordinates information. 

# General rationale: 
# 1/ Pre-process identifying attributes
# 2/ We progressively impute missing information within groups of rows that show no sign of being distinct cooperatives.
# 3/ Then, we homogenize information within groups that show signs of being the same cooperatives.  
# 4/ Finally, we differentiate information between groups that show signs of being distinct cooperatives. 

## 2/ Missing imputation stage. 
# We impute missing information on one variable where there is a unique value for this variable within: 
# Step 1 - groups identified by one of the two other variables. 
#           Order matters here: imputations on a variable affect the next grouping. 
#           The non-missing variable we group on is thus full name, then coordinates, then abrv name, following the certainty with which these variables distinguish cooperatives. 

#           Since this process reduces the number of missing values in grouping variables, grouping can be more accurate on every iteration. 
#           Thus, we iterate the process until no more imputation is made. 
#           And we include Step 2 below in the loop, this is equivalent to looping once on Step 1 only first. 

#           Note: don't condition on the 3d var to be missing (as done initially), to allow for larger groups, which is more conservative
#           ... (spuriously giving the same name to rows with clearly distinct coords, but this is not an issue)

#           Note 2: to be most conservative, we don't impute missing coordinates based on either abrv or full name alone, 
#           ... because it would risk identifying several rows as a single coop, while they are actually 
#           ... different ones with the same coordinates (which is possible in the data, see Note 3 below). 
#           illustrative case:  civ %>% filter(grepl("COOPALBA", DISCL_SUPPLIER_ABRVNAME)) %>% View()

#           Note 3: We do not use coordinates as a bijective identifier of cooperatives. 
#           I.e., truly distinct coops may have identical coordinates, but truly identical coops cannot have different coordinates. 
#           Indeed, in many instances, there are very different names associated with identical/close coordinates. 
#           This is possibly the result of a "town-centroid" approach in the initial building of the Cocoa Accountability Map, 
#           ... resulting in truly distinct coops from the same area getting similar coordinates. 

# Step 2 - groups identified by identical-identical combinations of the two other variables. 
#           In practice, we apply this step only to the abrv-full name combination, 
#           ... because the two other combinations (abrv-coords and full-coords) are sufficient to identify a coop, 
#           ... and thus missings are directly filled in the homogenization stage (next). 

# We run steps in this order, and not the other way round, to maximize information used at each step
# (imputed rows in Step 1 become sufficiently filled to be picked up and filled in Step 2, 
# ... while the reverse order would directly upgrade rows with NA on 1 variable only, and thus not use them as information for imputations on rows with NAs on 2 variables). 


## 3/ Homogenization stage. 
# Arbitrarily choose a common name (abrv or full) for rows that share both the same other name (abrv or full) and the same coordinates.  
# This stage comes after and not before the missing imputation stage, to safely prevent this arbitrariness to alter the grouping in the imputation stage. 

# Finally, spot groups identified by coords and either abrv or full name, within which the other name (full or abrv resp.) is only NA. 
# Replace these NAs with the identifying name (abrv or full resp.). 

# In this stage, we use simplified abbreviated names and rounded coordinates,
# ... some rows may be the same coop but with different generic terms in the abrv name. 
# ... or because coordinates are not pointing to the exact same location. 
# (we do not use these in imputation stage, because the criteria there is to show no sign of being distinct cooperative)


## 4/ Differentiate distinct cooperatives, i.e. make a unique coop ID. 
# Based on imputed and homogenized abrv names, full names and coords. 
# There, we use even more rounded coordinates (1 digit), to distinguish homonym coops only if they are located in significantly different places (~10km apart). 


# Preliminary checks: 
if(
  civ %>% filter(is.na(DISCL_LONGITUDE) & !is.na(DISCL_LATITUDE)) %>% nrow() > 0 |
  civ %>% filter(is.na(DISCL_LATITUDE) & !is.na(DISCL_LONGITUDE)) %>% nrow() > 0 ){
  stop("some coordinates are missing in one dimension only and this will screw the cleaning routine")
}


# # # # # # # # 

### STAGE 1 PRE-PROCESSING -----------------

# Prepare / clean abrv and full names and coordinates.  
# Use 3-decimal coordinates, i.e. precise at 111m at equator, 
# ... as we won't use differences smaller than that to infer anything. 
civ <- 
  civ %>% 
  mutate(SUPPLIER_ABRVNAME = fn_clean_abrvname2(fn_clean_abrvname1(str_trans(str_squish(DISCL_SUPPLIER_ABRVNAME)))), # eyeballed-based cleaning of abrv names
         SUPPLIER_FULLNAME = str_trans(str_squish(DISCL_SUPPLIER_FULLNAME)), # lighter clean of full names
         SUPPLIER_FULLNAME = fn_clean_fullname_manual(fn_clean_fullname(SUPPLIER_FULLNAME)),  # gsub(pattern="STE COOP ", replacement="SOCIETE COOPERATIVE", x = SUPPLIER_FULLNAME),
         LONGITUDE = round(as.double(DISCL_LONGITUDE), 3), 
         LATITUDE = round(as.double(DISCL_LATITUDE), 3))
# We clean but leave generic terms in the main abrv variable (i.e. don't apply fn_clean_abrvname3 yet), 
# ... because their presence may actually distinguish cooperatives.(e.g. "ABCD COOP CA" and "ABCD SCOOPS" may truly be different cooperatives). 
# We make simplified abrv name later, based on infered abrv names meanwhile. 
civ %>% filter(grepl("CAFE", SUPPLIER_FULLNAME) & !grepl("STD_CAFE", SUPPLIER_FULLNAME)) %>% View()
# ECOOKIM PRELIMINARY STEP FOR 
# Manually fill in information from ECOOKIM network of 23 coops

# civ %>% filter(SUPPLIER_ABRVNAME == "ECOOKIM") %>% View()
# civ %>% filter(COMPANY == "ECOOKIM" & DISCL_YEAR == 2020) %>% View()
# First, those with only ECOOKIM as information in abrv name, give it NA, this is no information
civ <- 
  civ %>% 
  mutate(
    SUPPLIER_ABRVNAME = case_when(
      SUPPLIER_ABRVNAME == "ECOOKIM" & 
        DISTRICT_GEOCODE == "CI-2.3.1_1" ~ "CAKIB", 
      TRUE ~ SUPPLIER_ABRVNAME
    ), 
    SUPPLIER_ABRVNAME = case_when(
      SUPPLIER_ABRVNAME == "ECOOKIM" & 
        DISTRICT_GEOCODE == "CI-9.1.4_1" ~ "SICOPAG", 
      TRUE ~ SUPPLIER_ABRVNAME
    ), 
    # this one can be either of three (coopasid, soutra, capedig). The coordinates lead to nothing. 
    SUPPLIER_ABRVNAME = case_when(
      SUPPLIER_ABRVNAME == "ECOOKIM COOP CA" & 
        DISTRICT_GEOCODE == "CI-8.2.2_1" ~ "ECOOKIM", # leave it as ecookim, it will live as a distinct coop of its own... 
      TRUE ~ SUPPLIER_ABRVNAME
    )
  )

# Then remove ECOOKIM from ABRV names that have more info than that. 
civ <- 
  civ %>% 
  mutate(SUPPLIER_ABRVNAME = gsub(pattern = "ECOOKIM ", # The space matters
                                  replacement = "", 
                                  SUPPLIER_ABRVNAME), 
         SUPPLIER_FULLNAME = case_when(
           SUPPLIER_FULLNAME == "ECOOKIM" ~ NA, 
           TRUE ~ SUPPLIER_FULLNAME)
  )
# civ %>% filter(grepl("ECOOKIM", TRADER_NAME, ignore.case=T)) %>% View()
# civ %>% filter(grepl("ECOOKIM", DISCL_SUPPLIER_ABRVNAME, ignore.case=T)) %>% View()
# civ %>% filter(grepl("ECOOKIM", DISCL_SUPPLIER_FULLNAME, ignore.case=T)) %>% View()
# civ %>% filter(grepl("KIMBE", DISCL_SUPPLIER_FULLNAME, ignore.case=T)) %>% View()

# and remove these rows (currently two, from CAM and JRC) that have only the generic info for the whole union
civ <- 
  civ %>% 
  filter(!(SUPPLIER_ABRVNAME == "ECOOKIM" & 
             (SUPPLIER_FULLNAME == "UNION DES SOCIETES COOPERATIVE KIMBE" | is.na(SUPPLIER_FULLNAME))
           )
         )


# TONY CHOCOLONELY PRELIMINARY STEP
civ <- 
  civ %>% 
  mutate(
    # Force ECAM coop to be identified as the same coop across CAM source and direct pull of Tony's disclosure
    SUPPLIER_ABRVNAME = case_when(
      grepl("ECAM", DISCL_SUPPLIER_ABRVNAME) & COMPANY == "TONY'S CHOCOLONELY" ~ "ECAM", 
      TRUE ~ SUPPLIER_ABRVNAME
    ),
    SUPPLIER_FULLNAME = case_when(
      grepl("ECAM", DISCL_SUPPLIER_ABRVNAME) & COMPANY == "TONY'S CHOCOLONELY" ~ "COOPERATIVE DES AGRICULTEURS MODERNES", 
      TRUE ~ SUPPLIER_FULLNAME
    ),
    LONGITUDE = case_when(
      grepl("ECAM", DISCL_SUPPLIER_ABRVNAME) & COMPANY == "TONY'S CHOCOLONELY" ~ -7.344635, 
      TRUE ~ LONGITUDE
    ),
    LATITUDE = case_when(
      grepl("ECAM", DISCL_SUPPLIER_ABRVNAME) & COMPANY == "TONY'S CHOCOLONELY" ~ 6.750040, 
      TRUE ~ LATITUDE
    ), 
    
    SUPPLIER_ABRVNAME = case_when(
      grepl("SOCOOPACDI", DISCL_SUPPLIER_ABRVNAME) & COMPANY == "TONY'S CHOCOLONELY" ~ "SOCOOPACDI", 
      TRUE ~ SUPPLIER_ABRVNAME
    ),
    SUPPLIER_FULLNAME = case_when(
      grepl("SOCOOPACDI", DISCL_SUPPLIER_ABRVNAME) & COMPANY == "TONY'S CHOCOLONELY" ~ "COOPERATIVE AGRICOLE STD_CAFE_CACAO_STD DE DIVO", 
      TRUE ~ SUPPLIER_FULLNAME
    ),
    LONGITUDE = case_when(
      grepl("SOCOOPACDI", DISCL_SUPPLIER_ABRVNAME) & COMPANY == "TONY'S CHOCOLONELY" ~ -5.363000, 
      TRUE ~ LONGITUDE
    ),
    LATITUDE = case_when(
      grepl("SOCOOPACDI", DISCL_SUPPLIER_ABRVNAME) & COMPANY == "TONY'S CHOCOLONELY" ~ 5.84200, 
      TRUE ~ LATITUDE
    )
  )


### STAGE 2 - IMPUTING MISSINGS ---------------

keep_imputing <- 1
while(keep_imputing > 0){
  
  #### Step 1 -------------
  
  # Notes on code in this step: 
  # It's necessary to condition imputation (with case_when) to rows where the var. to impute is actually missing. 
  # ... because unique_unique returns NA, and not identity, when there are several unique values within group. 
  
  # After imputation, there's code to check the imputed rows. 
  # The second condition (is.na()) filters to imputations that are not spurious (recall: spurious ones get corrected later) 
  # This also displays rows that were not modified but were used to impute other rows in groups. 
  
  ### ### ### ### ### 
  # FULL NAME 
  
  # Group on full-missing coords, impute abrv 
  civ0 <- civ
  civ <- 
    civ %>%
    group_by(SUPPLIER_FULLNAME) %>% #, missing_coords
    mutate(SUPPLIER_ABRVNAME = case_when(
      is.na(SUPPLIER_ABRVNAME) ~ unique_unique(SUPPLIER_ABRVNAME), #missing_coords & 
      TRUE ~ SUPPLIER_ABRVNAME
    )) %>% 
    ungroup()
  
  all_imp_rows <- setdiff(civ, civ0)
  all_imp_rows %>% nrow() %>% print()
  worthit_imp_grps1 <- civ %>% filter(SUPPLIER_FULLNAME %in% all_imp_rows$SUPPLIER_FULLNAME & 
                                        is.na(LONGITUDE))
  
  ### ### ### ### ### 
  # COORDINATES
  
  # Group on coords, impute abrv 
  civ0 <- civ
  civ <- 
    civ %>%
    group_by(LONGITUDE, LATITUDE) %>%
    mutate(SUPPLIER_ABRVNAME = case_when(
      is.na(SUPPLIER_ABRVNAME) ~ unique_unique(SUPPLIER_ABRVNAME), 
      TRUE ~ SUPPLIER_ABRVNAME
    )) %>% 
    ungroup()
  
  all_imp_rows <- setdiff(civ, civ0)
  all_imp_rows %>% nrow() %>% print()
  worthit_imp_grps2 <- civ %>% filter(LONGITUDE %in% all_imp_rows$LONGITUDE & 
                                        LATITUDE %in% all_imp_rows$LATITUDE &
                                        is.na(SUPPLIER_FULLNAME))
  
  
  # Group on coords, impute full
  civ0 <- civ
  civ <- 
    civ %>%
    group_by(LONGITUDE, LATITUDE) %>% 
    mutate(SUPPLIER_FULLNAME = case_when(
      is.na(SUPPLIER_FULLNAME) ~ unique_unique(SUPPLIER_FULLNAME), 
      TRUE ~ SUPPLIER_FULLNAME
    )) %>% 
    ungroup()
  
  all_imp_rows <- setdiff(civ, civ0)
  all_imp_rows %>% nrow() %>% print()
  worthit_imp_grps3 <- civ %>% filter(LONGITUDE %in% all_imp_rows$LONGITUDE & 
                                        LATITUDE %in% all_imp_rows$LATITUDE &
                                        is.na(SUPPLIER_ABRVNAME))
  
  ### ### ### ### ### 
  # ABBREVIATED NAME
  
  # Group on abrv, impute full 
  civ0 <- civ
  civ <- 
    civ %>%
    group_by(SUPPLIER_ABRVNAME) %>% 
    mutate(SUPPLIER_FULLNAME = case_when(
      is.na(SUPPLIER_FULLNAME) ~ unique_unique(SUPPLIER_FULLNAME), 
      TRUE ~ SUPPLIER_FULLNAME
    )) %>% 
    ungroup()
  
  all_imp_rows <- setdiff(civ, civ0)
  all_imp_rows %>% nrow() %>% print()
  worthit_imp_grps4 <- civ %>% filter(SUPPLIER_ABRVNAME %in% all_imp_rows$SUPPLIER_ABRVNAME & 
                                        is.na(LONGITUDE))
  
  # civ %>% filter(grepl("ECAKOOG", SUPPLIER_ABRVNAME)) %>% View()
  # civ %>% filter(grepl("COOPALBA", SUPPLIER_ABRVNAME)) %>% View()
  
  
  ### Step 2 -------------
  
  civ <- 
    civ %>% 
    mutate(missing_coords = is.na(LONGITUDE) & is.na(LATITUDE), 
           missing_abrv = is.na(SUPPLIER_ABRVNAME), 
           missing_full = is.na(SUPPLIER_FULLNAME))
  
  civ0 <- civ
  civ <- 
    civ %>%
    group_by(SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME) %>% 
    mutate(
      LONGITUDE = case_when(
        (missing_coords & !missing_abrv & !missing_full) ~ unique_unique(LONGITUDE), 
        TRUE ~ LONGITUDE
      ), 
      LATITUDE = case_when(
        (missing_coords & !missing_abrv & !missing_full) ~ unique_unique(LATITUDE), 
        TRUE ~ LATITUDE
      )
    ) %>% 
    # here, what can happen is that there's discrepancy only in one coordinate dimension. 
    # With data at time of writing, it's the case of ADEDO coop only.
    # This case requires rounding one coordinate to ~11.1km, this is quite big, but the two names are identical and 
    # ... the other coordinate is equal at 3 decimals. 
    mutate(
      LONGITUDE = case_when(
        (is.na(LONGITUDE) & !is.na(LATITUDE) & !missing_abrv & !missing_full) ~ unique_unique(round(LONGITUDE, 1)), 
        TRUE ~ LONGITUDE
      ), 
      LATITUDE = case_when(
        (is.na(LATITUDE) & !is.na(LONGITUDE) & !missing_abrv & !missing_full) ~ unique_unique(round(LONGITUDE, 1)), 
        TRUE ~ LATITUDE
      )
    ) %>% 
    ungroup()
  
  imp_rows <- setdiff(civ, civ0)
  imp_rows %>% nrow() %>% print()
  
  imp_grps7 <- civ %>% filter(SUPPLIER_ABRVNAME %in% imp_rows$SUPPLIER_ABRVNAME & 
                                SUPPLIER_FULLNAME %in% imp_rows$SUPPLIER_FULLNAME)
  
  # imp_grps7 %>% arrange(SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME) %>% View()
  
  civ <- 
    civ %>% 
    mutate(missing_coords = is.na(LONGITUDE) & is.na(LATITUDE), 
           missing_abrv = is.na(SUPPLIER_ABRVNAME), 
           missing_full = is.na(SUPPLIER_FULLNAME))
  
  # Determine whether to reiterate loop 
  if(nrow(worthit_imp_grps1) > 0 | 
     nrow(worthit_imp_grps2) > 0 | 
     nrow(worthit_imp_grps3) > 0 | 
     nrow(worthit_imp_grps4) > 0 | 
     nrow(imp_rows) > 0){
    
    keep_imputing <- 1 
  } else {
    keep_imputing <- 0
  }
  
}

# civ %>% filter(grepl("INDENIE", SUPPLIER_FULLNAME, ignore.case = T)) %>% View()
# civ %>% filter(grepl("ECSP", SUPPLIER_ABRVNAME, ignore.case = T)) %>% View()

civ %>%  
  filter(COMPANY == "FAIRTRADE") %>% 
  distinct(SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME) %>% 
  nrow() 

### STAGE 3 - HOMOGENIZATION ----------------------------------------------------------------

# Prepare simplified abrv names (from already imputed ones)
civ <- mutate(civ, SIMPLIF_ABRVNAME = fn_clean_abrvname3(SUPPLIER_ABRVNAME))

# Prepare rounded coordinates (from already imputed ones)
# Rounding at 2 decimal place in degrees implies that 1.0050 and 1.0051 round to 1.00 and 1.01 resp. 
# this is a spurious difference of ~0.01 degree which is ~1.1km at equator. 
# This also means that things as different as 1.0051 and 1.0099 (apart of ~0.5km) are deemed at the same place. 

# Use TWO decimal places and not one as before, because this is now to identify buying stations, and not coops. 

civ <- 
  mutate(civ, 
         ROUND_LONGITUDE = round(LONGITUDE, 2), 
         ROUND_LATITUDE = round(LATITUDE, 2),
         # Correct manually spotted cases where even the 1-digit rounding keeps two rows distinct although they are clearly alike. 
         # One way to automatize and improve this would be by looking, within rows with the same names, at those which coordinates' have a '5' in the digits, or something like that... 
         ROUND_LONGITUDE = case_when(
           SUPPLIER_FULLNAME == "COOPERATIVE POUR L'AMELIORATION DU REVENU DU PLANTEUR" & grepl("CAREPCI", SUPPLIER_ABRVNAME) ~ -7.4,
           TRUE ~ ROUND_LONGITUDE
         ),
         ROUND_LATITUDE = case_when(
           SUPPLIER_FULLNAME == "COOPERATIVE POUR L'AMELIORATION DU REVENU DU PLANTEUR" & grepl("CAREPCI", SUPPLIER_ABRVNAME) ~ 6.7,
           TRUE ~ ROUND_LATITUDE
         )
  )


civ %>% filter(grepl("CAREPCI", SUPPLIER_ABRVNAME)) %>% View()


# civ %>% filter(ROUND_LONGITUDE==-8.01&ROUND_LATITUDE==6.57) %>% View()
# civ %>% filter(ROUND_LONGITUDE==-5.84&ROUND_LATITUDE==5.48) %>% View()

### ### ### ### ### ### 

# Group on full name and round coords, homogenize abrv name based on unique simplified abrv name. 
# ... (i.e., it homogenizes also cases where, e.g., "COOP CA ABCD" and "ABCD COOP CA" have the same full name and round coords).
civ0 <- civ
civ <- 
  civ %>% 
  group_by(SUPPLIER_FULLNAME, ROUND_LONGITUDE, ROUND_LATITUDE) %>% 
  mutate(
    SUPPLIER_ABRVNAME = case_when(
      (missing_abrv & !missing_full & !missing_coords) ~ unique_mode(SIMPLIF_ABRVNAME), 
      TRUE ~ SIMPLIF_ABRVNAME
    ),
    # Just because it's used in the next step, we also homogenize the simplif name column
    SIMPLIF_ABRVNAME = case_when(
      (missing_abrv & !missing_full & !missing_coords) ~ unique_mode(SIMPLIF_ABRVNAME),
      TRUE ~ SIMPLIF_ABRVNAME
    )
  ) %>% 
  ungroup()

imp_rows <- civ[is.na(civ0$SUPPLIER_ABRVNAME) & !is.na(civ$SUPPLIER_ABRVNAME),]
imp_rows %>% nrow() %>% print() # this is really just imputations. 
# The above code chunck changed lots of other rows, not by imputing NAs but by changing values to homogenize. 

imp_grps8 <- civ %>% filter(ROUND_LONGITUDE %in% imp_rows$ROUND_LONGITUDE & 
                              SUPPLIER_FULLNAME %in% imp_rows$SUPPLIER_FULLNAME)
# imp_grps8 %>% arrange(SUPPLIER_FULLNAME, ROUND_LONGITUDE, ROUND_LATITUDE) %>% View()

civ <- 
  civ %>% 
  mutate(missing_coords = is.na(LONGITUDE) & is.na(LATITUDE), 
         missing_abrv = is.na(SUPPLIER_ABRVNAME), 
         missing_full = is.na(SUPPLIER_FULLNAME))

# Group on simplif abrv name and round coords, homogenize full name 
civ0 <- civ
civ <- 
  civ %>% 
  group_by(SIMPLIF_ABRVNAME, ROUND_LONGITUDE, ROUND_LATITUDE) %>% 
  mutate(
    SUPPLIER_FULLNAME = case_when(
      (missing_full & !missing_abrv & !missing_coords) ~ unique_mode(SUPPLIER_FULLNAME), 
      TRUE ~ SUPPLIER_FULLNAME
    )
  ) %>% 
  ungroup()

imp_rows <- civ[is.na(civ0$SUPPLIER_FULLNAME) & !is.na(civ$SUPPLIER_FULLNAME),]
imp_rows %>% nrow() %>% print() # this is really just imputations. 
# The above code chunck changed lots of other rows, not by imputing NAs but by changing values to homogenize. 

imp_grps9 <- civ %>% filter(ROUND_LONGITUDE %in% imp_rows$ROUND_LONGITUDE & 
                              SIMPLIF_ABRVNAME %in% imp_rows$SIMPLIF_ABRVNAME)
# imp_grps9 %>% arrange(SIMPLIF_ABRVNAME, ROUND_LONGITUDE, ROUND_LATITUDE) %>% View()

civ <- 
  civ %>% 
  mutate(missing_coords = is.na(LONGITUDE) & is.na(LATITUDE), 
         missing_abrv = is.na(SUPPLIER_ABRVNAME), 
         missing_full = is.na(SUPPLIER_FULLNAME))



### ### ### ### ### 
## FILL ALL-MISSING within cleanly identified (ie. by a name and coords) coops.  
civ %>% filter(missing_abrv) %>% nrow()
civ %>% filter(missing_full) %>% nrow()

# Where there's no abrv name, give the full name
civ0 <- civ
civ <- 
  civ %>% 
  group_by(SUPPLIER_FULLNAME, ROUND_LONGITUDE, ROUND_LATITUDE) %>% 
  mutate(
    SUPPLIER_ABRVNAME = case_when(
      all(missing_abrv) & !is.na(SUPPLIER_FULLNAME) ~ SUPPLIER_FULLNAME, 
      TRUE ~ SUPPLIER_ABRVNAME
    )
  ) %>% 
  ungroup()
# civ %>% filter(missing_abrv) %>% arrange(SUPPLIER_FULLNAME, LONGITUDE) %>% View()
civ %>% filter(is.na(SUPPLIER_ABRVNAME)) %>% nrow()

# note: for ADA, this does not cover all full names because supplier is not always missing. 
# These should have been filled at imputation stage then. But the abrv name was not yet similar. 
# And that's correct not to make these imputations, because initially, these empty full name rows for ADA 
# have no other information than abrv name COOP-CA ADA, which is not enough to assume it's the same coop as ADA WAREHOUSE or others. 

civ <- 
  civ %>% 
  group_by(SUPPLIER_ABRVNAME, ROUND_LONGITUDE, ROUND_LATITUDE) %>% 
  mutate(
    SUPPLIER_FULLNAME = case_when(
      all(missing_full) & !is.na(SUPPLIER_ABRVNAME) ~ SUPPLIER_ABRVNAME, 
      TRUE ~ SUPPLIER_FULLNAME
    )
  ) %>% 
  ungroup()
# civ %>% filter(missing_full) %>% arrange(SUPPLIER_ABRVNAME, LONGITUDE) %>% View()
# civ %>% filter(is.na(SUPPLIER_FULLNAME)) %>% arrange(SUPPLIER_ABRVNAME, LONGITUDE) %>% View()
civ %>% filter(is.na(SUPPLIER_FULLNAME)) %>% nrow()


# civ %>% filter(grepl("INDENIE", SUPPLIER_FULLNAME, ignore.case = T)) %>% View()
# civ %>% filter(grepl("ADA", SUPPLIER_ABRVNAME, ignore.case = T)) %>% View()




### STAGE 4 - DIFFERENTIATE ---------------------------------
civ %>% filter(is.na(LONGITUDE) & !is.na(LATITUDE)) %>% nrow() 
civ %>% filter(is.na(LATITUDE) & !is.na(LONGITUDE)) %>% nrow()

# At this point, all NAs that can reasonably be imputed have been so. Thus, remaining NAs are considered as distinct values. 
# Recall that with 2-digit rounding, points apart from up to 1.1km are deemed to have the same location. 
# Since we also condition on both abrv and full names being the same, this can extend this constraint to 1-digit rounding: 
# I.e., we deem two coops different if, despite having the same abrv and full names, they are more than 11.1km apart. 

# Make a long concatenation ID 
civ <- 
  civ %>% 
  mutate(CCTN_COOP_POINT_ID = paste0(SUPPLIER_ABRVNAME, "_", ROUND_LONGITUDE, "_", ROUND_LATITUDE, "_", SUPPLIER_FULLNAME)) %>%
  arrange(CCTN_COOP_POINT_ID)

civ_save1 <- civ

### Buying station IDs -----------------------------
# Here, we make ids for distinct combinations of geo coordinates and full and abbreviated names. 
# We call these buying station IDs, because we consider that the same coop can have 
# several locations when it has several buying stations. 


# Don't make stable IDs because stable ids are necessary and built only for the public data set. 
# here, we just want arbitrary IDs. They don't need to match with the public data set because 
# we will never merge those, as they are intrinsically built differently by the addition of 
# private inputs in the process above
arbitrary_ids = 
    civ %>% 
    distinct(CCTN_COOP_POINT_ID) %>%
    mutate(COOP_POINT_ID = row_number()) # Arbitrary BS id, based on order (arrange above)
civ = 
  civ %>% 
  left_join(arbitrary_ids, 
            by = "CCTN_COOP_POINT_ID") 

if(length(unique(civ$COOP_POINT_ID)) != nrow(arbitrary_ids)){stop("something wrong")}

rm(arbitrary_ids)


# this is just to keep track of where the info comes from, at point level. 
civ <- 
  civ %>% 
  group_by(COOP_POINT_ID) %>% 
  mutate(
    IS_ALL_CAM_V3 = case_when(
      !all(IS_CAM_V3) ~ FALSE, 
      TRUE ~ TRUE 
    ), 
    IS_ANY_CAM_V3 = case_when(
      !any(IS_CAM_V3) ~ FALSE, 
      TRUE ~ TRUE 
    )) %>% 
  ungroup() %>% 
  select(-IS_CAM_V3)

civ <- ungroup(civ)

civ %>% #filter(grepl("COASADA", SUPPLIER_ABRVNAME, ignore.case = T)) %>% 
  arrange(SUPPLIER_FULLNAME) %>% 
  filter(is.na(DISCL_LONGITUDE & !is.na(LONGITUDE))) %>% 
  # filter(SUPPLIER_ABRVNAME=="SOCEADAHS") %>% 
  select(DISCL_SUPPLIER_ABRVNAME, DISCL_SUPPLIER_FULLNAME, DISCL_LONGITUDE, DISCL_LATITUDE, 
         SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, LONGITUDE, LATITUDE, ROUND_LONGITUDE, ROUND_LATITUDE, 
         COOP_POINT_ID) %>%  View()

civ %>% filter(grepl("COPACOL", SUPPLIER_ABRVNAME, ignore.case = T)) %>% 
  arrange(SUPPLIER_FULLNAME) %>% 
  # filter(is.na(DISCL_LONGITUDE & !is.na(LONGITUDE))) %>% 
  # filter(SUPPLIER_ABRVNAME=="SOCEADAHS") %>% 
  select(COMPANY, DISCL_SUPPLIER_ABRVNAME, DISCL_SUPPLIER_FULLNAME, DISCL_LONGITUDE, DISCL_LATITUDE, 
         SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, LONGITUDE, LATITUDE, ROUND_LONGITUDE, ROUND_LATITUDE, 
         COOP_POINT_ID) %>%  View()


civ <- select(civ, -missing_full, -missing_coords, -missing_abrv, 
              -ROUND_LONGITUDE, -ROUND_LATITUDE)

# civ_save <- civ

## Sanity checks 
#### 
# In several manually checked instances, the disclosed area name does not match with the imputed coordinates in Google maps. 
# (Notably for CEMOI coops). 
# It's not clear that the disclosed area name should prevail though, since homonyms spatially distant can be common (e.g. DIANGOBO)
# So perhaps it's not crazy to rely on our imputation based on coop name homonyms instead and 
# assume that ggmaps does not know all localities by their names / that companies disclosed area names non-rigorously.   
# Sometimes, googling a name gives the prefecture, but the district has the same name. 
# civ %>% filter(grepl("COOPAME", SUPPLIER_ABRVNAME, ignore.case = T)) %>% View()

# Systematic sanity check: where a district is available, does the imputed geolocation fall within? 
# Test not relevant, because district geocodes were attributed on a within join (in CAM_get_data.R) in the first place. 
# Tested anyway, and indeed beside three edge imprecisions, there is no mismatch. 


# For the same reason, we do not impute coordinates from disclosed area names... 

# 
# imp_coords <- 
#   civ %>% 
#   select(COOP_POINT_ID, DISTRICT_GEOCODE, LONGITUDE, LATITUDE) %>% 
#   filter(!is.na(LONGITUDE)) %>% 
#   st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>% 
#   st_transform(crs = st_crs(departements))
# 
# # Join by st_within
# sf_use_s2(FALSE)
# imp_coords <- st_join(imp_coords, 
#                        departements[,c("LVL_4_CODE")], 
#                        join = st_intersects)
# 
# pb_ids <- imp_coords %>% 
#   filter(!is.na(DISTRICT_GEOCODE) & DISTRICT_GEOCODE != LVL_4_CODE) %>% 
#   select(COOP_POINT_ID) %>% 
#   st_drop_geometry() %>% unlist()
# 
# mismatches <- imp_coords %>% filter(COOP_POINT_ID %in% pb_ids) 
# departements[departements$LVL_4_CODE %in% mismatches$DISTRICT_GEOCODE,] %>% st_geometry() %>% plot()
# mismatches %>% st_geometry() %>% plot(add=T, col = "red")
# 
# civ %>% filter(grepl("ECSP", SUPPLIER_ABRVNAME, ignore.case = T)) %>% View()



# ATTRIBUTE DISTRICT GEOCODE -----------------------------------------

# Do this here, at point (and not coop) level, i.e. before grouping buying stations in distinct coops, 
# because now district is an attribute of a point, not necessarily a coop

civ0 <- civ
### From coordinates  ----
# Make spatial
civ_coords <- 
  civ %>%
  select(COOP_POINT_ID, DISTRICT_GEOCODE, LONGITUDE, LATITUDE) %>%
  filter(!is.na(LONGITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = st_crs(departements))

# Join by st_within
sf_use_s2(FALSE)
civ_coords <- st_join(civ_coords,
                      departements[,c("LVL_4_CODE")],
                      join = st_intersects) 
# by the way there's no conflict (but 3 edge imprecision) between initial geocode var and the one just added, 
# bc they come from the same process. See discussion in Sanity checks section above. 
# civ_coords %>% filter(!is.na(DISTRICT_GEOCODE) & DISTRICT_GEOCODE != LVL_4_CODE)

# Moreover, there are 4 coops which coordinates fell in NA LVL_4_CODE and DISTRICT_GEOCODE
mis <- civ_coords %>% filter(!is.na(geometry) & is.na(LVL_4_CODE) & is.na(DISTRICT_GEOCODE)) 

mis <- st_transform(mis, crs = 4326)

# library(rnaturalearth)
# countries_sf <- ne_countries(type = "countries", country = c("Ivory Coast", "Ghana", "Cameroon", "Benin", "Guinea", "Sierra Leone", "Togo", "Nigeria", "Liberia"), scale = "small", returnclass = "sf")
# ggplot() +
#   geom_sf(data = countries_sf, fill = NA) +
#   geom_sf(data = mis) +
#   # geom_sf(data = data_sf, aes(size = FARMERS), alpha = 0.2) +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   coord_sf(datum = NA) 

# This is 2 in the sea, and two in Liberia. 
# Remove their coordinates from the main data 
civ <- 
  civ %>% mutate(
    LONGITUDE = case_when(
      COOP_POINT_ID %in% mis$COOP_POINT_ID ~ NA, 
      TRUE ~ LONGITUDE
    ), 
    LATITUDE = case_when(
      COOP_POINT_ID %in% mis$COOP_POINT_ID ~ NA, 
      TRUE ~ LATITUDE
    ))
rm(mis)

# Merge the geocode infered from coordinates
civ_coords <- 
  civ_coords %>% 
  st_drop_geometry() %>% 
  select(COOP_POINT_ID, LVL_4_CODE) %>% 
  distinct(COOP_POINT_ID, .keep_all = TRUE) # keep only distinct coop ids for the join below

civ <- 
  civ %>% 
  left_join(civ_coords, by = "COOP_POINT_ID")

# Take geocode infered from coordinates where it was not already available 
# (and keep initial district geocode in (3) conflicting cases)
civ <- 
  civ %>% 
  mutate(DISTRICT_GEOCODE = case_when(
    is.na(DISTRICT_GEOCODE) ~ LVL_4_CODE, 
    TRUE ~ DISTRICT_GEOCODE
  )) %>% 
  select(-LVL_4_CODE)

# civ %>% filter(COMPANY == "ECOOKIM" & DISCL_YEAR == 2020) %>% pull(DISTRICT_GEOCODE) 

# Extend district info from some disclosures to others within the same coop 
civ <- 
  civ %>%
  group_by(COOP_POINT_ID) %>% 
  mutate(
    DISTRICT_GEOCODE = case_when(
      is.na(DISTRICT_GEOCODE) ~ unique_unique(DISTRICT_GEOCODE), 
      TRUE ~ DISTRICT_GEOCODE
    )
  ) %>% 
  ungroup()

all_imp_rows <- setdiff(civ, civ0)
all_imp_rows %>% nrow() %>% print()
worthit_imp_grps_dept <- civ %>% filter(COOP_POINT_ID %in% all_imp_rows$COOP_POINT_ID)

# NOT all obs. with a geocode have coordinates. 
civ %>% filter(is.na(LONGITUDE) & !is.na(DISTRICT_GEOCODE)) %>% nrow()
# All obs. with coordinates have a geocode 
civ %>% filter(!is.na(LONGITUDE) & is.na(DISTRICT_GEOCODE)) %>% nrow()


### From area name ----

# Attribute District geocode where still not available from coordinates etc. above, but 
# where a locality name is provided. 
civ$AREA_NAME <- fn_clean_department_names(str_trans(civ$DISCL_AREA_NAME))
unique(civ$AREA_NAME) %>% sort()
# there are quite dirty names 
civ %>% filter(is.na(LONGITUDE)) %>% pull(AREA_NAME) %>% unique() %>% sort()

lvl4 <- departements %>% st_drop_geometry() %>% select(LVL_4_NAME, LVL_4_CODE)
lvl4$LVL_4_NAME %>% sort()

# join the LVL_4_CODE column, based on equality of disclosed area name and LVL_4_NAME
civ <- 
  civ %>% 
  left_join(lvl4, by = c("AREA_NAME" = "LVL_4_NAME"))
# civ %>% select(COOP_POINT_ID, AREA_NAME, DISTRICT_NAME, DISTRICT_GEOCODE, LVL_4_CODE) %>% View()

# attribute LVL_4_CODE to DISTRICT_GEOCODE when the latter is missing (which is also when coordinates are missing at this point)
civ <- 
  civ %>% 
  mutate(DISTRICT_GEOCODE = case_when(
    is.na(DISTRICT_GEOCODE) ~ LVL_4_CODE, 
    TRUE ~ DISTRICT_GEOCODE
  )) %>% 
  select(-LVL_4_CODE)

# civ %>% filter(is.na(DISTRICT_GEOCODE) & !is.na(LVL_4_CODE))


# Homogenize district info 
if(anyNA(civ$COOP_POINT_ID)){stop("this is not expected and an issue for the homogenization below")}

# one case is problematic, with SOCAS being disclosed both in DIVO and ABOISSO, 
# and no more info to decide.
# Based on below link, let's force it being in Aboisso
# https://www.goafricaonline.com/ci/80521-socas-cafe-cacao-aboisso-cote-ivoire
civ <- 
  civ %>% 
  mutate(
    DISTRICT_GEOCODE = case_when(
      DISCL_SUPPLIER_ABRVNAME == "SOCAS COOP CA" ~ "CI-3.2.1_1", 
      TRUE ~ DISTRICT_GEOCODE
    )#, 
    # DISTRICT_NAME = case_when(
    #   DISCL_SUPPLIER_ABRVNAME == "SOCAS COOP CA" ~ "ABOISSO", 
    #   TRUE ~ DISTRICT_NAME)
    )

# homogenize
civ <- 
  civ %>% 
  group_by(COOP_POINT_ID) %>% 
  mutate(
    DISTRICT_GEOCODE = case_when(
      !all(is.na(DISTRICT_GEOCODE)) & is.na(DISTRICT_GEOCODE) ~ unique_mode(DISTRICT_GEOCODE),
      all(is.na(DISTRICT_GEOCODE)) ~ NA_character_, 
      TRUE ~ DISTRICT_GEOCODE
    )) %>% 
  ungroup()

civ %>% filter(COOP_POINT_ID==4) %>% View()

# check that if a link to a coop has a known district, then all links from this coop have
if(
  civ %>% 
  group_by(COOP_POINT_ID) %>% 
  mutate(NOT_HOMOGENIZED = !all(is.na(DISTRICT_GEOCODE)) & !all(!is.na(DISTRICT_GEOCODE))) %>% 
  ungroup() %>% 
  filter(NOT_HOMOGENIZED) %>% nrow() > 0
){stop("department info homogenization not complete")}  


# Make clean district name based on this, replacing the previous variable that never made sense (was empty). 
civ <- 
  civ %>% 
  left_join(lvl4, 
            by = c("DISTRICT_GEOCODE" = "LVL_4_CODE")) %>% 
  mutate(DISTRICT_NAME = LVL_4_NAME) 

civ$DISTRICT_NAME %>% unique() %>% sort()

if(
  civ %>% filter(!is.na(DISTRICT_GEOCODE) & is.na(DISTRICT_NAME)) %>% nrow() != 0 | 
  civ %>% filter(is.na(DISTRICT_GEOCODE) & !is.na(DISTRICT_NAME)) %>% nrow() != 0 ){
  stop("something unexpected in matches between district names and geocodes")
}

# BUYING STATIONS -----
# Regroup spatially distinct points into the same coop, as they can represent a coop's buying stations.

civ_save_preBS <- civ

### ### ### ### ### 
## IDENTIFY BUYING STATIONS 
# Now, we want to detect buying stations of the same coop. 
# We construct a first condition for being buying stations (BS) of the same coop: being in the same district. 

# We tested an alternative definition, called being close by, i.e. either in the same district, or within a set distance to each others. 
# However, the additional distance condition implies lots of complications in terms of programming implementation
# and adds nothing, for a distance of 100km, as demonstrated by the code chunk below. 

# (We don't use rounded coordinates because this is quite rough as it introduces arbitrary cut-offs)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# All code block commented out below is to show that using a spatially explicit definition of 
# proximity to identify buying stations of the same coop is useless, conditional on using the district.  

# # Because the distance criteria is relational, and not absolute, it cannot be a single value in a column
# # it has to be indexes of those in the same district, to which we then add indexes of those within distance. 
# 
# # Later, we could refine this parameter with something learned empirically.
# hq_bs_max_km <- 100
# 
# civ <- 
#   civ %>% 
#   group_by(DISTRICT_GEOCODE) %>% 
#   mutate(DISTR_INDEXES = list(cur_group_rows())) %>% 
#   ungroup() 
# 
# # Spatialize
# civ_sf <-
#   civ %>%
#   # temporarily give fake coordinates where missing, to be able to work on the full df, and thus work with readily valid indexes
#   mutate(
#     TMP_LONGITUDE = case_when(
#       is.na(LONGITUDE) ~ 0,
#       TRUE ~ LONGITUDE
#     ),
#     TMP_LATITUDE = case_when(
#       is.na(LATITUDE) ~ 90,
#       TRUE ~ LATITUDE
#     )
#   ) %>%
#   st_as_sf(coords = c("TMP_LONGITUDE", "TMP_LATITUDE"), crs = 4326)
# 
# # for every spatialized point, find the index of all points closer than hq_bs_max_km
# distances_civ <-
#   civ_sf %>%
#   st_distance()
# # this produces a list column, with every element being a vector of the index of points closer than the threshold.
# civ_sf$DISTANCE_INDEXES <- apply(distances_civ, 1, function(x) {which(x<=hq_bs_max_km*1e3) }) # st_distance returns meters
# # /!\ these are indexes OF CIV_SF, not civ.
# 
# civ <-
#   civ_sf %>%
#   st_drop_geometry()
# 
# # remove the indexes of points with fake coordinate - they are not actually close-by at the north pole.
# civ <-
#   civ %>%
#   rowwise() %>%
#   mutate(
#     DISTANCE_INDEXES = case_when(
#       is.na(LONGITUDE) ~ list(integer(0)),
#       TRUE ~ list(DISTANCE_INDEXES)),
#     # merge close and distr indexes
#     CLOSEBY_INDEXES = list(sort(unique(DISTR_INDEXES, DISTANCE_INDEXES)))
#   )
# 
# # Because close by is not an absolute value but relational, again, we cannot just go from there and use
# # CLOSEBY_INDEXES as a grouping variable.
# # E.g., two coops 10km apart won't be equal on CLOSEBY_INDEXES if one is close to points the other one isn't close to.
# # Restrict the indexes to those that share names in addition to being close by,
# # and group points that have common indexes
# civ2 <-
#   civ %>%
#   rowwise() %>%
#   # in a given row, the value of CLOSEBY_INDEXES is the vector of indexes of rows close to this row's location
#   # civ[CLOSEBY_INDEXES,]$SUPPLIER_FULLNAME] are these rows' coop full names
#   # we return only elements of CLOSEBY_INDEXES for which there is a name match
#   mutate(
#     CLOSEBY_FULL_INDEXES = list(sort(CLOSEBY_INDEXES[which(SUPPLIER_FULLNAME == civ[CLOSEBY_INDEXES, ]$SUPPLIER_FULLNAME)]) ),
#     CLOSEBY_ABRV_INDEXES = list(sort(CLOSEBY_INDEXES[which(SIMPLIF_ABRVNAME == civ[CLOSEBY_INDEXES, ]$SIMPLIF_ABRVNAME)]) ),
# 
#     DISTR_FULL_INDEXES = list(sort(DISTR_INDEXES[which(SUPPLIER_FULLNAME == civ[DISTR_INDEXES, ]$SUPPLIER_FULLNAME)]) ),
#     DISTR_ABRV_INDEXES = list(sort(DISTR_INDEXES[which(SIMPLIF_ABRVNAME == civ[DISTR_INDEXES, ]$SIMPLIF_ABRVNAME)]) ),
# 
#     DISTANCE_FULL_INDEXES = list(sort(DISTANCE_INDEXES[which(SUPPLIER_FULLNAME == civ[DISTANCE_INDEXES, ]$SUPPLIER_FULLNAME)]) ),
#     DISTANCE_ABRV_INDEXES = list(sort(DISTANCE_INDEXES[which(SIMPLIF_ABRVNAME == civ[DISTANCE_INDEXES, ]$SIMPLIF_ABRVNAME)]) )
#   )
# # note that which() gets rid of NAs, it only returns indexes of elements that evaluate as TRUE.
# # --> when there is no other match than self match, the indexes var is length 1.
# # --> when the name is NA, the value returned in the list is numeric(0)
# # # Store the latter apart (currently there's none, as all rows have either a full or an abrv name)
# # civ2_noidx <-
# #   civ2 %>%
# #   filter(length(COORDS_FULL_INDEXES) + length(COORDS_ABRV_INDEXES) == 0)
# # # and remove them from the initial df
# # civ2 <-
# #   civ2 %>%
# #   filter(length(COORDS_FULL_INDEXES) + length(COORDS_ABRV_INDEXES) > 0)
# 
# civ2 <-
#   civ2 %>%
#   mutate(n_matched_DISTR_FULL = length(DISTR_FULL_INDEXES),
#          n_matched_DISTANCE_FULL = length(DISTANCE_FULL_INDEXES),
#          n_matched_CLOSEBY_FULL = length(CLOSEBY_FULL_INDEXES),
#          DISTANCE_ADDS_MATCHES_FULL = n_matched_CLOSEBY_FULL > n_matched_DISTR_FULL,
#          DISTR_ADDS_MATCHES_FULL = n_matched_CLOSEBY_FULL > n_matched_DISTANCE_FULL,
#          # repeat test for ABRV
#          n_matched_DISTR_ABRV = length(DISTR_ABRV_INDEXES),
#          n_matched_DISTANCE_ABRV = length(DISTANCE_ABRV_INDEXES),
#          n_matched_CLOSEBY_ABRV = length(CLOSEBY_ABRV_INDEXES),
#          DISTANCE_ADDS_MATCHES_ABRV = n_matched_CLOSEBY_ABRV > n_matched_DISTR_ABRV,
#          DISTR_ADDS_MATCHES_ABRV = n_matched_CLOSEBY_ABRV > n_matched_DISTANCE_ABRV)
# 
# # District does add matches, i.e. there are points with the same name that
# # are farther than 50km away, but are in the same district
# civ2 %>%
#   filter(DISTR_ADDS_MATCHES_FULL) %>% nrow()
# civ2 %>%
#   filter(DISTR_ADDS_MATCHES_ABRV) %>% nrow()
# # Spatially explicit distance does not add matches, i.e. all points with the same name
# # are in the same district - there is never one that is within 50km but in a different district.
# civ2 %>%
#   filter(DISTANCE_ADDS_MATCHES_FULL) %>% nrow()
# civ2 %>%
#   filter(DISTANCE_ADDS_MATCHES_ABRV) %>% nrow()
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# So, we just repeat from stage 3 above, with district simply replacing the rounded coordinates as a grouping variable. 

#### Homogenize across buying stations -------

civ <- 
  civ %>% 
  mutate(missing_distr = is.na(DISTRICT_GEOCODE), 
         missing_abrv = is.na(SUPPLIER_ABRVNAME), 
         missing_full = is.na(SUPPLIER_FULLNAME))


# Group on full name and district, homogenize abrv name based on unique simplified abrv name. 
# ... (i.e., it homogenizes also cases where, e.g., "COOP CA ABCD" and "ABCD COOP CA" have the same full name and disttrict).
civ0 <- civ
civ <- 
  civ %>% 
  group_by(SUPPLIER_FULLNAME, DISTRICT_GEOCODE) %>% 
  mutate(
    SUPPLIER_ABRVNAME = case_when(
      (missing_abrv & !missing_full & !missing_distr) ~ unique_mode(SIMPLIF_ABRVNAME), 
      TRUE ~ SIMPLIF_ABRVNAME
    ),
    # Just because it's used in the next step, we also homogenize the simplif name column
    SIMPLIF_ABRVNAME = case_when(
      (missing_abrv & !missing_full & !missing_distr) ~ unique_mode(SIMPLIF_ABRVNAME),
      TRUE ~ SIMPLIF_ABRVNAME
    ), 
    # store the different names, for inspection purpose
    U_SIMPLIF_ABRVNAME = case_when(
      (!missing_full & !missing_distr) ~ paste0(na.omit(unique(SIMPLIF_ABRVNAME)), collapse = " + ")
    )
  ) %>% 
  ungroup()

imp_rows <- civ[is.na(civ0$SUPPLIER_ABRVNAME) & !is.na(civ$SUPPLIER_ABRVNAME),]
imp_rows %>% nrow() %>% print() # this is really just imputations. 
# The above code chunck changed lots of other rows, not by imputing NAs but by changing values to homogenize. 

imp_grps10 <- civ %>% filter(DISTRICT_GEOCODE %in% imp_rows$DISTRICT_GEOCODE & 
                              SUPPLIER_FULLNAME %in% imp_rows$SUPPLIER_FULLNAME)
# imp_grps10 %>% arrange(SUPPLIER_FULLNAME, DISTRICT_GEOCODE) %>% View()

civ <- 
  civ %>% 
  mutate(missing_distr = is.na(DISTRICT_GEOCODE), 
         missing_abrv = is.na(SUPPLIER_ABRVNAME), 
         missing_full = is.na(SUPPLIER_FULLNAME))

# Group on simplif abrv name and district, homogenize full name 
civ0 <- civ
civ <- 
  civ %>% 
  group_by(SIMPLIF_ABRVNAME, DISTRICT_GEOCODE) %>% 
  mutate(
    SUPPLIER_FULLNAME = case_when(
      (missing_full & !missing_abrv & !missing_distr) ~ unique_mode(SUPPLIER_FULLNAME), 
      TRUE ~ SUPPLIER_FULLNAME
    ),
    # store different names, for indicative purpose
    U_SUPPLIER_FULLNAME = case_when(
      (!missing_abrv & !missing_distr) ~ paste0(na.omit(unique(SUPPLIER_FULLNAME)), collapse = " + ")
    )
  ) %>% 
  ungroup()

imp_rows <- civ[is.na(civ0$SUPPLIER_FULLNAME) & !is.na(civ$SUPPLIER_FULLNAME),]
imp_rows %>% nrow() %>% print() # this is really just imputations. 
# The above code chunck changed lots of other rows, not by imputing NAs but by changing values to homogenize. 

imp_grps11 <- civ %>% filter(DISTRICT_GEOCODE %in% imp_rows$DISTRICT_GEOCODE & 
                              SIMPLIF_ABRVNAME %in% imp_rows$SIMPLIF_ABRVNAME)

# we could use this to inspect and manually improve in fn_clean_fullname_manual
imp_grps11 %>% arrange(SIMPLIF_ABRVNAME, DISTRICT_GEOCODE) %>% View()

civ0 %>% filter(SUPPLIER_ABRVNAME=="CNIBO") %>% View("before")
civ %>% filter(SUPPLIER_ABRVNAME=="CNIBO") %>% View("after")

civ <- 
  civ %>% 
  mutate(missing_distr = is.na(DISTRICT_GEOCODE), 
         missing_abrv = is.na(SUPPLIER_ABRVNAME), 
         missing_full = is.na(SUPPLIER_FULLNAME))


### ### ### ### ### 
## FILL ALL-MISSING within cleanly identified (ie. by a name and district) coops.  
civ %>% filter(missing_abrv) %>% nrow()
civ %>% filter(missing_full) %>% nrow()

# Where there's no abrv name, give the full name
civ0 <- civ
civ <- 
  civ %>% 
  group_by(SUPPLIER_FULLNAME, DISTRICT_GEOCODE) %>% 
  mutate(
    SUPPLIER_ABRVNAME = case_when(
      all(missing_abrv) & !is.na(SUPPLIER_FULLNAME) ~ SUPPLIER_FULLNAME, 
      TRUE ~ SUPPLIER_ABRVNAME
    )
  ) %>% 
  ungroup()
# civ %>% filter(missing_abrv) %>% arrange(SUPPLIER_FULLNAME, LONGITUDE) %>% View()
civ %>% filter(is.na(SUPPLIER_ABRVNAME)) %>% nrow()

civ <- 
  civ %>% 
  group_by(SUPPLIER_ABRVNAME, DISTRICT_GEOCODE) %>% 
  mutate(
    SUPPLIER_FULLNAME = case_when(
      all(missing_full) & !is.na(SUPPLIER_ABRVNAME) ~ SUPPLIER_ABRVNAME, 
      TRUE ~ SUPPLIER_FULLNAME
    )
  ) %>% 
  ungroup()

civ %>% filter(is.na(SUPPLIER_FULLNAME)) %>% nrow()

### Differentiate coops ---------------------------------

# At this point, all NAs that can reasonably be imputed have been so. Thus, remaining NAs are considered as distinct values. 
# Recall that with 2-digit rounding, points apart from up to 1.1km are deemed to have the same location. 
# Since we also condition on both abrv and full names being the same, this can extend this constraint to 1-digit rounding: 
# I.e., we deem two coops different if, despite having the same abrv and full names, they are more than 11.1km apart. 

# Make a long concatenation ID 
civ <- 
  civ %>% 
  mutate(CCTN_COOP_ID = paste0(SUPPLIER_ABRVNAME, "_", DISTRICT_GEOCODE, "_", SUPPLIER_FULLNAME)) %>%
  arrange(CCTN_COOP_ID)

### Coop IDs -----------------------------
# Here, we make ids for distinct combinations of DISTRICT and full and abbreviated names. 
# We call these COOP IDs, because we consider that the same coop can have 
# several locations within the same district, when it has several buying stations. 

# Don't make stable IDs because stable ids are necessary and built only for the public data set. 
# here, we just want arbitrary IDs. They don't need to match with the public data set because 
# we will never merge those, as they are intrinsically built differently by the addition of 
# private inputs in the process above
arbitrary_ids = 
  civ %>% 
  distinct(CCTN_COOP_ID) %>%
  mutate(COOP_ID = row_number()) # Arbitrary COOP id, based on order (arrange above)
civ = 
  civ %>% 
  left_join(arbitrary_ids, 
            by = "CCTN_COOP_ID") 

if(length(unique(civ$COOP_ID)) != nrow(arbitrary_ids)){stop("something wrong")}

rm(arbitrary_ids)

civ$COOP_POINT_ID %>% unique() %>% length()
civ$COOP_ID %>% unique() %>% length()
# and the number of distinct coops with identification based on 1-decimal rounded coordinates was 5984


# There are several things to handle from here:

# Homogenize - i.e. attribute the mode and store a vector of unique values... 


# CLEAN CERTIFICATION -------------------------
# temp_fun <- function(s){s[!grepl(pattern = "_CO1", s)] %>% sort()}
# unique(civ$DISCL_CERTIFICATION_NAME)

# check_if_utz <- function(x){grepl("UTZ", x) & !grepl("DECERTIFIED", x)}
# check_if_ra <- function(x){grepl("RAINFOREST ALLIANCE|RFA|; RA|RA;", x) & !grepl("NOT GRANTED|SUSPEND", x)}
# check_if_ft <- function(x){grepl("FAIRTRIDE|FAIRTRADE|FAITRIDE|FAIR TRADE", x) & !grepl("SUSPENDED", x)}
# check_if_cocoalife <- function(x){grepl("COCOALIF|COCOA LIF|CACAOLIF|CACAO LIF", x)}

civ_save <- civ

# Make separate columns for the certification types
civ <-
  civ %>%
  mutate(
    CERT_LIST = str_squish(str_trans(DISCL_CERTIFICATION_NAME)),
    
    # add "," where only a space separates distinct certifications
    CERT_LIST = gsub(pattern = "HORIZONS FAIRTR", "HORIZONS, FAIRTR", x = CERT_LIST),
    CERT_LIST = gsub(pattern = "HORIZONS RAINFO", "HORIZONS, RAINFO", x = CERT_LIST),
    CERT_LIST = gsub(pattern = "HORIZONS FERMIC", "HORIZONS, FERMIC", x = CERT_LIST),
    
    CERT_LIST = gsub(pattern = "LIFE FAIRTR", "LIFE, FAIRTR", x = CERT_LIST),
    CERT_LIST = gsub(pattern = "LIFE RAINFO", "LIFE, RAINFO", x = CERT_LIST),
    
    CERT_LIST = gsub(pattern = "UTZ FAIRTR", "UTZ, FAIRTR", x = CERT_LIST),
    CERT_LIST = gsub(pattern = "UTZ RAINFO", "UTZ, RAINFO", x = CERT_LIST),
    CERT_LIST = gsub(pattern = "UTZ COCOA LIFE", "UTZ, COCOA LIFE", x = CERT_LIST),
    CERT_LIST = gsub(pattern = "UTZ FERMICOA", "UTZ, FERMICOA", x = CERT_LIST),
    
    CERT_LIST = gsub(pattern = "FAIRTRADE UTZ", "FAIRTRADE, UTZ", x = CERT_LIST),
    CERT_LIST = gsub(pattern = "FAIRTRADE RAINFO", "FAIRTRADE, RAINFO", x = CERT_LIST),
    
    CERT_LIST = gsub(pattern = "ALLIANCE UTZ", "ALLIANCE, UTZ", x = CERT_LIST),
    CERT_LIST = gsub(pattern = "ALLIANCE FAIRTR", "ALLIANCE, FAIRTR", x = CERT_LIST),
    
    CERT_LIST = gsub(pattern = "COCOA HORIZONS COCOA LIFE", "COCOA HORIZONS, COCOA LIFE", x = CERT_LIST),
    CERT_LIST = gsub(pattern = "COCOA HORIZONS UTZ", "COCOA HORIZONS, UTZ", x = CERT_LIST),
    CERT_LIST = gsub(pattern = "COCOA HORIZONS UTZ FERMICOA COCOA LIFE", "COCOA HORIZON, UTZ, FERMICOA, COCOA LIFE", x = CERT_LIST),
    CERT_LIST = gsub(pattern = "FERMICOA COCOA LIFE", "FERMICOA, COCOA LIFE", x = CERT_LIST),
    
    CERT_LIST = gsub(pattern = "RAAGRICULTURE BIOLOGIQUE", "RAINFOREST ALLIANCE, AGRICULTURE BIOLOGIQUE", x = CERT_LIST),
    
    CERT_LIST = str_split(CERT_LIST, pattern = ";|,|\\/|\\&|-| AND | OR | ET "),
    CERT_LIST = map(CERT_LIST, str_squish),
    CERT_LIST = map(CERT_LIST, ~ gsub("UTZ_.*", "UTZ", .x)) # handles the many "UTZ_CO1000009236" style instances
  ) 

unique(unlist(civ$CERT_LIST)) %>% sort()

# PUT COMPANY NAMES IN PARENTHESES AS A WAY TO DIFFERENTIATE SSIs FROM CERTIFICATION

# this function should take as input a character vector of length > 1. 
fn_standard_certification_names <- function(x) {
  y <- x
  if(grepl("RAINFOREST ALLIANCE|RFA|OLD-RA|NEW-RA|^RA$", x) & !grepl("NOT GRANTED|SUSPEND", x)){
    y <- "RAINFOREST ALLIANCE"
  } 
  if(grepl("FAIRTRADE|FAIR TRADE|FAITRADE|FAIRTRIDE|FAITRIDE|FT USA|^FT$", x) & !grepl("SUSPENDED", x)){
    y <- "FAIRTRADE"
  }
  if(grepl("FAIR FOR LIFE", x)){
    y <- "FAIR FOR LIFE"
  }
  if(grepl("UTZ|UTS", x) & !grepl("DECERTIFIED", x)){
    y <- "UTZ"
  } 
  if(grepl("BIOLOGIQUE|^BIO$|^AB$", x)){
    y <- "AGRICULTURE BIOLOGIQUE"
  }
  if(grepl("COCOA PROMISE|^CCP$|CARGILL", x)){
    y <- "CARGILL COCOA PROMISE (CARGILL)"
  }
  if(grepl("HORIZON", x)){
    y <- "COCOA HORIZONS (BARRY CALLEBAUT)"
  }
  if(grepl("OLAM PROGRAMMES", x)){
    y <- "OLAM PROGRAMMES (OLAM)"
  }
  if(grepl("COCOA PLAN", x)){
    y <- "COCOA PLAN (NESTLE)"
  }
  if(grepl("COCOALIF|COCOA LIF|CACAOLIF|CACAO LIF|COCO LIF", x)){
    y <- "COCOA LIFE (MONDELEZ)"
  }
  if(grepl("SUSTAINABLE ORIGINS", x)){
    y <- "SUSTAINABLE ORIGINS (BLOMMER)"
  }
  if(grepl("TRANSPARENCE CACAO", x)){
    y <- "TRANSPARENCE CACAO (CEMOI)"
  }
  if(grepl("COCOA FOR GOOD", x)){
    y <- "COCOA FOR GOOD (HERSHEY)"
  }
  if(grepl("RESPONSIBLY SOURCED COCOA", x)){
    y <- "RESPONSIBLY SOURCED COCOA (MARS)"
  }
  if(grepl("SUCDEN|FUCHS", x)){
    y <- "SUCDEN PROGRAMMES (SUCDEN)"
  }
  if(grepl("^TRACE$|CACAO TRACE|CACAOTRACE|CACAO-TRACE", x)){
    y <- "CACAO-TRACE (PURATOS)"
  }
  if(grepl("FERMICOA|4C|4 C|^ASA$|PROGRAMME DE L'UNION ECOOKIM|^AVEC$|^CACAO$|CACAO AMI DES FORETS|CAIR INTERNATIONAL|^CARE$|CE 834|CIV [(]GIZ[)]|COCOA PRACTICISE|COH SACO|EQUITE 2|NOVATION VERTE|^CLMRS$|^CMS$|^COCOA ACTION$|COCOACTION|^COH$|COOPACADEMY2|GROUPEMENT|ICRAFT|^ICS$|^INO$|^N0$|^OILP$|SASSANDRA|CHILD LABOUR|PILOT|^ECOCERT$|IMPACTUM|CHILDREN|AGROFORESTRY|AGROMAP|ASCA|^GAL$|^GIZ$|^ICI$|^LANTEUR$|^MICRO$|^MOCA$|^NEW$|^NA$|^NEANT$|^OLD$|^PP$|PRODUCTIVITY PACKAGE|^PRO$|^PROPLANTEUR$|^RCCP$|^STB$|COACHING|SOLIDARIDAD|RESPONSIBLY SOURCED COCOA|^SOCIAL$|^STARBUCK$", x)){
    y <- "OTHER PROGRAMMES OR CERTIFICATIONS"
  }
  if(grepl("DECERTIFIED", x)){y <- NA}
  if(grepl("NOT GRANTED", x)){y <- NA}
  if(grepl("SUSPEND", x)){y <- NA}
  if(grepl("NOT CERTIFIED", x)){y <- NA}
  if(grepl("SUPPLIER STANDARD", x)){y <- NA}
  if(grepl("2007", x)){y <- NA}
  if(grepl("NOP )", x)){y <- NA}
  if(grepl("9999", x)){y <- NA}
  if(!is.na(x) & nchar(x)==0){y <- NA}
  # if(is.na(x)){y <- "9999"}
  
  return(y)
}
# civ %>% filter(grepl("2007", DISCL_CERTIFICATION_NAME)) %>% View()
# civ %>% filter(grepl("NOP )", DISCL_CERTIFICATION_NAME, ignore.case = T)) %>% View()
# civ %>% filter(grepl("trace", DISCL_CERTIFICATION_NAME, ignore.case = T)) %>% View()
civ <- 
  civ %>% 
  mutate(CERT_LIST = map(CERT_LIST, ~modify(.x, fn_standard_certification_names)))

unique(civ$CERT_LIST) 
unique(unlist(civ$CERT_LIST)) %>% sort()
anyNA(civ$CERT_LIST)

# Note: certification is (already) at the link level, i.e. specific to the value in every row in the COMPANY column. 
# Indeed, this was inputed as such in disclosure scraping, and this seems to be the case in the CAM as well. 

# CERT_LIST is turned into a more workable variable CERTIFICATIONS below. 
# We don't produce CERTIFIED variable anymore. 


# LINK SIZE IN NUMBER FARMERS -------------------------

#### Extract CAM info (incl. from RFA) ----------------------------
# First step is to extract information from the CAM, stored in CAM_BUYERS column. 

# Convert CAM to long format based on coop size, one row per disclosed coop size
# (The difference with what is done in CAM_to_traders_volumes_FOB_capped_2019_CLEAN.Rmd 
# ... is that we keep all rows, we do not remove rows with missing farm size info)

civ <- 
  civ %>% 
  mutate(COOP_SIZE = str_split(CAM_BUYERS, pattern = ";"),
         COOP_SIZE = map(COOP_SIZE, str_squish)) %>%
  unnest(cols = c(COOP_SIZE)) %>%
  # drop_na(COOP_SIZE) %>%
  mutate(
    DISCL_NUMBER_FARMERS = case_when(
      !is.na(COOP_SIZE) ~ as.double(gsub(" \\(.*", "", COOP_SIZE)),
      TRUE ~ DISCL_NUMBER_FARMERS), 
    BUYER = gsub(".*\\(", "", COOP_SIZE),
    BUYER = str_squish(gsub("\\)","", BUYER)),
    BUYER = gsub("BC", "BARRY CALLEBAUT", BUYER),
    BUYER = gsub("RA", "RAINFOREST ALLIANCE", BUYER),
    BUYER = gsub("CARGILL UTZ", "CARGILL", BUYER)
  ) %>% select(-COOP_SIZE)
# This spuriously attributed a number of farmers to some company-coop links (i.e. rows): Remove them

civ0 <- civ # (store for later check)

# A note on info from the CAM on the number of farmers and their buyers. 
# The BUYER variables is the company disclosing (according to the CAM) to buy to the corresponding number of farmers in DISCL_NUMBER_FARMERS
# Buyers can be
# - exporters (here Cargill, BC, Touton), 
# - manufacturers (Blommer, Mars, Tony's)
# - Rainforest alliance. 

# RAINFOREST ALLIANCE disclosures of number of farmers: 
# The problem with RFA is that several companies may disclose having a link with a coop, 
# but not the size of these links, and we only know the number of RFA farmers, 
# such that it's spurious to attribute this size to every company's link with this coop. 
# Rather, we want to have companies' link sizes missing, and an extra row that representing the 
# link between the coop and RFA. 

# Separately store coop-level numbers of farmers disclosed by Rainforest Alliance as per the CAM 
# this does not include other RFA disclosures added in addition to the CAM. 
# Don't keep duplicates (there when several companies are linked to a coop that has RFA farmers info in the CAM)
# The first row taken by distinct() is one of these companies, arbitrarily. Switch it to RFA.  
temp_rfa <- 
  civ %>% 
  filter(BUYER == "RAINFOREST ALLIANCE") %>% 
  distinct(COOP_ID, DISCL_NUMBER_FARMERS, 
           .keep_all = TRUE) %>% 
  mutate(COMPANY = "RAINFOREST ALLIANCE", 
         # and turn down the trader name variable, not to count this flow twice (same rational as in CLEAN TRADER NAME section above)
         TRADER_NAME = case_when(
           TRADER_NAME %in% c(civ$COMPANY, non_disclosing_companies) ~ NA, 
           TRUE ~ TRADER_NAME
         )
  ) 

# In the main data, switch the spurious RFA number of farmers to NA, 
# (but keep the rows, as they inform on the link with companies)
# and stack the RFA info
civ <- 
  civ %>% 
  mutate(DISCL_NUMBER_FARMERS = case_when(
    BUYER == "RAINFOREST ALLIANCE" ~ NA, 
    TRUE ~ DISCL_NUMBER_FARMERS
  )) %>% 
  rbind(temp_rfa)
rm(temp_rfa)

# Now for non-RFA buyers: 
# Spot company-coop links (rows) for which farmer info is disclosed in CAM, but not by this company
# Store them separately, switch their farmers to NA, and keep only one non-duplicate (but there are no duplicates anyway)

# In the main data, among rows with some CAM_BUYERS info, remove rows where there is no info on the number of farmers sourced from by this company. 
# Stack back the separately stored data from above. 

temp_spur <- 
  civ %>% 
  group_by(COOP_ID, DISCL_YEAR) %>%
  mutate(
    buyerz = list(BUYER),
    buyerz = map(buyerz, unique)) %>% 
  ungroup() %>% 
  filter(
    !is.na(CAM_BUYERS) & !(COMPANY %in% buyerz), 
  ) %>% 
  # In order not to lose the information from the CAM V3 about connections between traders and manufacturers, 
  # we store in the trader name variable: either the name of the disclosing company, if it's not identified as a non-trading company (see above), 
  # or the name of the buying company, if it's not identified as non-trading company... 
  # NOPE, because actually the CAM V3 does not give such info: 
  # Nestlé can be the company and only one trader has the number of farmers, this doesn't mean it's the trader for Nestlé. 
  # And a trader can be the company and only one Nestlé has the number of farmers, this doesn't mean that it's the trader for Nestlé either.  
  # So, let the trader name be NA. 
  
  # mutate(DISCL_TRADER_NAME = case_when(
  #   !(COMPANY %in% non_trading_companies) ~ COMPANY, 
  #   !(BUYER %in% non_trading_companies) ~ BUYER, 
  #   TRUE ~ NA
  # )) %>% 
  mutate(DISCL_NUMBER_FARMERS = NA) %>% 
  distinct(COOP_ID, DISCL_YEAR, COMPANY, 
           .keep_all = TRUE) %>% 
  select(-buyerz)

# Do not keep missing company rows that don't satisfy the other conditions, 
# i.e. don't add | is.na(COMPANY) in the filter condition
# Those are all and only RFA disclosures which full information is already comprised 
# in rows that have been given RAINFOREST ALLIANCE instead of NA as per the COMPANY var. 
civ <- 
  civ %>% 
  filter(
    (is.na(CAM_BUYERS) | COMPANY == BUYER) # | is.na(COMPANY)
  ) %>% 
  rbind(temp_spur) %>% 
  arrange(COOP_ID, DISCL_YEAR, COMPANY, TRADER_NAME)

rm(temp_spur)

# civ0 %>%
#   filter(COOP_ID %in% c(403, 406, 5584)) %>%
#   select(COOP_ID, DISCL_YEAR, COMPANY, DISCL_TRADER_NAME, TRADER_NAME,
#          DISCL_NUMBER_FARMERS, BUYER, CAM_BUYERS) %>%
#   arrange(COOP_ID, DISCL_YEAR, COMPANY) %>%
#   View()
# 
# civ %>%
#   filter(COOP_ID %in% c(403, 406, 5584)) %>%
#   select(COOP_ID, DISCL_YEAR, COMPANY, DISCL_TRADER_NAME, TRADER_NAME,
#          DISCL_NUMBER_FARMERS, BUYER, CAM_BUYERS) %>%
#   arrange(COOP_ID, DISCL_YEAR, COMPANY) %>%
#   View()
# 
# na_comp_not_buyers <- 
#   civ0 %>% 
#   filter(is.na(COMPANY) & !is.na(CAM_BUYERS)) %>% 
#   pull(COOP_ID)
# 
# civ0 %>%
#   filter(COOP_ID %in% na_comp_not_buyers) %>%
#   select(COOP_ID, DISCL_YEAR, COMPANY, DISCL_TRADER_NAME, TRADER_NAME,
#          DISCL_NUMBER_FARMERS, BUYER, CAM_BUYERS) %>%
#   arrange(COOP_ID, DISCL_YEAR, COMPANY) %>%
#   View()
# 
# 
# civ0 %>%
#   filter(COOP_ID %in% c(582, 5653, 448)) %>%
#   select(COOP_ID, DISCL_YEAR, COMPANY, DISCL_TRADER_NAME, TRADER_NAME,
#          DISCL_NUMBER_FARMERS, BUYER, CAM_BUYERS) %>%
#   arrange(COOP_ID, DISCL_YEAR, COMPANY) %>%
#   View()
# 
# civ %>%
#   filter(COOP_ID %in% c(582, 5653, 448)) %>%
#   select(COOP_ID, DISCL_YEAR, COMPANY, DISCL_TRADER_NAME, TRADER_NAME,
#          DISCL_NUMBER_FARMERS, BUYER, CAM_BUYERS) %>%
#   arrange(COOP_ID, DISCL_YEAR, COMPANY) %>%
#   View()


if(nrow(civ0[civ0$DISCL_YEAR != 2019,]) != nrow(civ[civ$DISCL_YEAR != 2019,])){
  stop("Some rows post 2019 were mistakenly removed while handling 2019 CAM data on number of farmers.")
}
rm(civ0)

# civ %>% filter(COMPANY == "RAINFOREST ALLIANCE") %>% View()

civ_save2 <- civ

#### Flow size extrapol. (sei-pcs) ------------------------ 
# Here, we impute the number of farmers, where missing. 

# Rationale: 
# Number of farmer at the disclosure level is NOT representative of the coop, 
# ... but only of the disclosing company's sourcing volume.

# So we want to distinguish two things: 
# - the number of farmers that a company source from (for Trase)
# - the total number of farmers in a cooperative. 
# In Renier et al./ sei-pcs 2019, the assumption is that companies disclose farmers that supply them only, 
# ... and thus 2- is the sum of 1-. 

# First, impute missing cases where info is available from the same disclosing company the same year, but from a different source 
# (mostly, this is where the CAM has no info but additionally scraped data has it, 
# but we also change non-equal values to their average, to account for discrepancies btw. sources, eg. the CAM and new disclosures). 

# Note that we have chekced that there are no over-use of zeros in the disclosed number of 
# farmers, which would lead to under-estimating direct supply of traders not disclosing their number of farmers.
# That's only 2 coops from Cargill (and is consistent across the sources of info) 
# civ %>% filter(DISCL_NUMBER_FARMERS==0)

# # Imputed values are stored in THREE variables
# 1 - NB_FARMERS_COMPANY_YEAR has just the first kind of imputations (within the same company and coop)

# 2 - NUM_FARMERS is the variable for SEIPCS. It has the first kind of imputations PLUS the second kind of 
#     imputations (within the same coop, by averaging flow sizes disclosed by other companies with the same coop. In 2019 model, this was done in model script.
#     The rationale of doing this hezre is that we can use information within coop but across years, to get 
#     more conservative imputations before the third, more imprecise, step.   
#     The third kind of imputation is still made in SEIPCS models, by Monte Carlo. 

# 3 - NUM_FARMERS_EXTRAPOLATED has all three kinds of imputations; it is not meant for SEIPCS but for analyses that 
#     would use the output of this script directly (i.e. not after running the SEIPCS model). 


##### 1st kind of imputations #####
civ %>% summarise(.by = DISCL_YEAR, NB_FARMERS = sum(DISCL_NUMBER_FARMERS, na.rm = T)) %>% arrange(DISCL_YEAR) 
# at this stage, the total number of farmers across links varies a lot from one year to another, and includes some double counting (of farmers disclosed by actors at different levels of the chain)

civ <- 
  civ %>%
  group_by(COOP_ID, DISCL_YEAR, COMPANY, TRADER_NAME) %>% 
  mutate(
    NB_FARMERS_COMPANY_YEAR = round(mean(DISCL_NUMBER_FARMERS, na.rm=TRUE), 0) # this yields NaN for coops with only missing info
  ) %>% 
  ungroup()

# Then impute missing number of farmers for the same link, in another year when 
# the same link has been disclosed but the farmer info hasn't. 
# (extension of links to years when they have not been disclosed *at all* occurs later in this script)

# we group by coop ID, company AND TRADER_NAME, to avoid using info on coop-manufacturer flows 
# (which could otherwise aggregate flows through several traders) to impute size of coop-trader flow. 
civ <- 
  civ %>%
  group_by(COOP_ID, COMPANY, TRADER_NAME) %>% 
  mutate(
    NB_FARMERS_COMPANY_YEAR = case_when(
      is.na(NB_FARMERS_COMPANY_YEAR) ~ round(mean(NB_FARMERS_COMPANY_YEAR, na.rm=TRUE), 0), # this yields NaN for coops with only missing info
      TRUE ~ NB_FARMERS_COMPANY_YEAR
    )) %>% 
  ungroup()

# for certification schemes, this also imputes nb of farmers from the same scheme in another year 
# typically, the RFA disclosures in 2019 (through the CAM) and 2022, and the FT disclosure in 2022.
# (but there almost no common coop id linked to RFA between 2019 and 2022 disclosures...)

civ %>% summarise(.by = DISCL_YEAR, NB_FARMERS = sum(NB_FARMERS_COMPANY_YEAR, na.rm = T)) %>% arrange(DISCL_YEAR) 

##### 2nd kind of imputations #####

# Then impute within COOP_ID, in order to impute missings from values potentially disclosed by other companies 
# this is the equivalent to: "Where a company did not report the number of farmers 
# they purchased from for a given cooperative, but the number was reported by other 
# companies, the sourcing from that cooperative was assigned the mean value of 
# the sizes disclosed by these other companies." in Renier et al. 

# Do not count RFA or FT farmers in the average, to impute the missing number of farmers as supply flows (as recommended in CAM_to_traders_volumes_FOB_capped_2019_CLEAN.Rmd)
civ <- mutate(civ,
              # %in% operator makes sur that COMPANY = NA is not NA on this dummy, but TRUE  
              NOT_RFA = !COMPANY %in% c("RAINFOREST ALLIANCE"),
              NOT_FT = !COMPANY %in% c("FAIRTRADE"),
              NOT_RFA_FT = !COMPANY %in% c("RAINFOREST ALLIANCE", "FAIRTRADE")# this includes disclosures by RFA as per the CAM. 
              )

# # what is the average number of farmers per coop every year 
# civ %>% summarise(.by = DISCL_YEAR, 
#                   AVG_NUM_FARM_PER_COOP = mean(NUM_FARMERS, na.rm = TRUE) ) %>% 
#   arrange(DISCL_YEAR)

# the same year first
civ <- 
  civ %>%
  # group on NOT_RFA_FT (not on NOT_RFA, NOT_FT) to impute nb of farmers across (and not within) certification schemes. 
  # (disclosure for the same coop by the same scheme but in another year has already been done in 1st imputation kind above
  # this imputes disclosure for the same coop by another scheme but in the same year.) 
  group_by(COOP_ID, DISCL_YEAR, NOT_RFA_FT) %>% 
  mutate(
    NUM_FARMERS = case_when(
      is.na(NB_FARMERS_COMPANY_YEAR) ~ round(mean(NB_FARMERS_COMPANY_YEAR, na.rm=TRUE), 0),# this yields NaN for coops with only missing info
      TRUE ~ NB_FARMERS_COMPANY_YEAR
    )) %>% 
  ungroup()

civ %>% filter(NOT_RFA_FT) %>% pull(NUM_FARMERS) %>% sum(na.rm = T)
civ %>% filter(!NOT_RFA_FT) %>% pull(NUM_FARMERS) %>% sum(na.rm = T)

# Then, if still not imputed, impute from different years 
civ <- 
  civ %>%
  group_by(COOP_ID, NOT_RFA_FT) %>% 
  mutate(
    NUM_FARMERS = case_when(
      is.na(NUM_FARMERS) ~ round(mean(NUM_FARMERS, na.rm=TRUE), 0), 
      TRUE ~ NUM_FARMERS
    )) %>% 
  ungroup()

# civ %>% filter(NOT_RFA_FT) %>% pull(NUM_FARMERS) %>% sum(na.rm = T)
# civ %>% filter(!NOT_RFA_FT) %>% pull(NUM_FARMERS) %>% sum(na.rm = T)

civ %>% summarise(.by = DISCL_YEAR, NB_FARMERS = sum(NUM_FARMERS, na.rm = T)) %>% arrange(DISCL_YEAR) 


# this illustrates different kinds of imputations.
# civ %>%
#   filter(COOP_ID %in% c(1079, 1407, 2782, 2857, 4674, 4512, 710, 2856, 2857)) %>%
#   select(COOP_ID, DISCL_YEAR, COMPANY, DISCL_TRADER_NAME, TRADER_NAME,
#          DISCL_NUMBER_FARMERS, BUYER, CAM_BUYERS, NUM_FARMERS, NUM_FARMERS_EXTRAPOLATED) %>%
#   arrange(COOP_ID, DISCL_YEAR, COMPANY) %>%
#   View()

##### 3rd kind of imputations #####

# AND THEN, if still not imputed, give the number of farmers for flows with other coops, within the same year
# This is similar to what is done in Step 5 of the SEIPCS model, but without the 1000 draws. 
# Use mean to match the method in SEIPCS and ease comparisons 
# There is quite a difference: median is ~600 while mean is ~800 farmers per flow

civ <- 
  civ %>%
  # still group by year, to have it equivalent to what seipcs does (drawing in NUM_FARMERS values in the model's year)
  # still treat separately the nb of farmers disclosed by certification schemes  
  group_by(DISCL_YEAR, NOT_RFA_FT) %>% 
  # Necessary to name it differently, for SEIPCS to pick up NUM_FARMERS variable untouched by this step; 
  mutate(
    NUM_FARMERS_EXTRAPOLATED = case_when(
      is.na(NUM_FARMERS) ~ round(mean(NUM_FARMERS, na.rm=TRUE), 0), 
      TRUE ~ NUM_FARMERS
    )) %>% 
  ungroup()

civ %>% summarise(.by = DISCL_YEAR, NB_FARMERS = sum(NUM_FARMERS_EXTRAPOLATED, na.rm = T)) %>% arrange(DISCL_YEAR) 

civ %>% filter(NOT_RFA_FT) %>% pull(NUM_FARMERS_EXTRAPOLATED) %>% sum(na.rm = T)
civ %>% filter(!NOT_RFA_FT) %>% pull(NUM_FARMERS_EXTRAPOLATED) %>% sum(na.rm = T)

# Those remaining with NA NUM_FARMERS_EXTRAPOLATED are rainforest alliance links in 2023 and 2021 
civ$NUM_FARMERS_EXTRAPOLATED %>% summary()
civ %>% filter(is.na(NUM_FARMERS_EXTRAPOLATED)) %>% pull(COMPANY) %>% unique()
civ %>% filter(is.na(NUM_FARMERS_EXTRAPOLATED)) %>% View()

if(civ %>% filter(is.na(NUM_FARMERS_EXTRAPOLATED)) %>% pull(COMPANY) %>% unique() == "RAINFOREST ALLIANCE"){
  civ <- 
    civ %>%
    group_by(NOT_RFA, NOT_FT) %>% 
    mutate(
      NUM_FARMERS_EXTRAPOLATED = case_when(
        is.na(NUM_FARMERS_EXTRAPOLATED) ~ round(mean(NUM_FARMERS_EXTRAPOLATED, na.rm=TRUE), 0), 
        TRUE ~ NUM_FARMERS_EXTRAPOLATED
      )) %>% 
    ungroup()
}

civ %>% filter(NOT_RFA_FT) %>% pull(NUM_FARMERS_EXTRAPOLATED) %>% sum(na.rm = T)
civ %>% filter(!NOT_RFA_FT) %>% pull(NUM_FARMERS_EXTRAPOLATED) %>% sum(na.rm = T)

civ %>% summarise(.by = DISCL_YEAR, NB_FARMERS = sum(NUM_FARMERS_EXTRAPOLATED, na.rm = T)) %>% arrange(DISCL_YEAR) 

if(anyNA(civ$NUM_FARMERS_EXTRAPOLATED)){stop("Farmer number extrapolation not complete")}

# civ %>% summarise(.by = DISCL_YEAR, 
#                   AVG_NUM_FARM_PER_COOP = mean(NUM_FARMERS_EXTRAPOLATED, na.rm = TRUE)) %>% 
#   arrange(DISCL_YEAR)

# civ %>% filter(COMPANY %in% c("FAIRTRADE", "RAINFOREST ALLIANCE")) %>% View()

civ %>% filter(is.na(COMPANY)) %>% View()
civ$NUM_FARMERS_EXTRAPOLATED %>% summary()

# EXTEND EARLIER DISCLOSURES TO SUBSEQUENT YEARS -----------------------------
# ... when we have no disclosure at all from a company those years  

# For instance, in 2020, we have almost no disclosure from traders:
# civ %>% filter(DISCL_YEAR == 2020) %>% pull(TRADER_NAME) %>% unique()

# First, handle certification schemes. 
# - Repeat Fairtrade which is only disclosed per the private data set deemed valid for 2019.
civ %>% filter(COMPANY=="FAIRTRADE") %>% select(DISCL_YEAR) %>% table()

# - And handle RFA which discloses several years: 2019 through the CAM, and 2022 through their website, 
# with suspended coops in 2021 and an additional one in 2023  

rfa_202123 = 
  civ %>% filter(COMPANY == "RAINFOREST ALLIANCE" & DISCL_YEAR %in% c(2020, 2021, 2023))

civ = 
  civ %>% filter(!(COMPANY == "RAINFOREST ALLIANCE" & DISCL_YEAR %in% c(2020, 2021, 2023)))

civ %>% filter(COMPANY == "RAINFOREST ALLIANCE") %>% select(DISCL_YEAR) %>% table()

civ$REPEATED_FROM_PAST_YEAR <- FALSE # see below

for(year in 2020:max(unique(civ$DISCL_YEAR))){
  
  civ_past_year <- filter(civ, DISCL_YEAR == year-1)
  civ_year <- filter(civ, DISCL_YEAR == year)
  
  # companies known to disclose as of past year
  certification_past_year <- unique(civ_past_year$COMPANY)
  # keep only certification schemes
  certification_past_year <- certification_past_year[certification_past_year %in% c("FAIRTRADE",
                                                                        "RAINFOREST ALLIANCE")]
  
  for(scheme in certification_past_year){
    
    if(!(scheme %in% unique(civ_year$COMPANY))){ # if this company is not among the companies that disclosed in the current year... 
      # then impute the suppliers of this company in this year from what it disclosed in the past year
      to_stack <- 
        civ_past_year %>% 
        filter(COMPANY == scheme) %>% 
        mutate(DISCL_YEAR = year, # need to change the year
               REPEATED_FROM_PAST_YEAR = TRUE  # flag that information is just being repeated in this year and company
        ) 
      
      civ <- rbind(civ, to_stack)
      rm(to_stack)
    }
  }

  rm(scheme, civ_year, civ_past_year, certification_past_year)
}

# remove the two suspended coops
rfa_suspended <- rfa_202123 %>% filter(grepl("Suspend", DISCL_CERTIFICATION_NAME)) 
new_rfa <- 
  rfa_202123 %>% 
  filter(DISCL_CERTIFICATION_NAME == "RFA") %>% 
  mutate(REPEATED_FROM_PAST_YEAR = FALSE)
  
civ = 
  civ %>% 
  filter(!(COOP_ID %in% rfa_suspended$COOP_ID & DISCL_YEAR == 2021))

civ = 
  civ %>% 
  rbind(new_rfa)


# Rationales for company disclosures: 
# Start from baseline year, 2019, for which and only for which we have near complete coverage of companies. 
# For every company present a given year, check whether it has made a disclosure the next year. 
# ... (starting in 2020, but when 2020 has been imputed then look at all companies in 2020, to capture possibly new ones)
# We are at the disclosure level (working on COMPANY variable), not trader level. 

for(year in 2020:max(unique(civ$DISCL_YEAR))){
  
  civ_past_year <- filter(civ, DISCL_YEAR == year-1)
  civ_year <- filter(civ, DISCL_YEAR == year)
  
  # companies known to disclose as of past year
  companies_past_year <- unique(civ_past_year$COMPANY)
  # remove NAs (recall: there are NAs in company - they are all the coops not linked to a company in the CAM)
  # and remove certification schemes
  companies_past_year <- companies_past_year[!(is.na(companies_past_year) | 
                                                 companies_past_year %in% c("JOINT RESEARCH CENTER",   # (of course, we don't repeat Joint Research Center)
                                                                            "FAIRTRADE",
                                                                            "RAINFOREST ALLIANCE")
                                               )]
  
  for(comp in companies_past_year){
    
    if(!(comp %in% unique(civ_year$COMPANY))){ # if this company is not among the companies that disclosed in the current year... 
      # then impute the suppliers of this company in this year from what it disclosed in the past year
      to_stack <- 
        civ_past_year %>% 
        filter(COMPANY == comp) %>% 
        mutate(DISCL_YEAR = year, # need to change the year
               REPEATED_FROM_PAST_YEAR = TRUE  # flag that information is just being repeated in this year and company
        ) 
      
      civ <- rbind(civ, to_stack)
      rm(to_stack)
    }
  }
  # # Stack annually the coops never linked with a company - NOT NECESSARY anymore, bc handled in the next step
  # and this did not repeat the coops that WERE disclosed one and then stopped being disclosed (thus coops were "disappearing")
  # never_linked_year <- 
  #   civ %>% 
  #   filter(is.na(COMPANY)) %>% 
  #   mutate(DISCL_YEAR = year)
  # 
  # civ <- rbind(civ, never_linked_year)
  # rm(never_linked_year)
  
  rm(civ_year, civ_past_year, companies_past_year)
}
if(anyNA(civ$NUM_FARMERS_EXTRAPOLATED)){stop("Farmer number extrapolation not complete")}

summary(civ$NUM_FARMERS_EXTRAPOLATED)
civ %>% filter(NUM_FARMERS_EXTRAPOLATED==0) %>% nrow()

civ %>% filter(COMPANY != "RAINFOREST ALLIANCE") %>% summarise(.by = DISCL_YEAR, NB_FARMERS = sum(NUM_FARMERS_EXTRAPOLATED, na.rm = T)) %>% arrange(DISCL_YEAR) 
civ %>% summarise(.by = DISCL_YEAR, NB_FARMERS = sum(NUM_FARMERS_EXTRAPOLATED, na.rm = T)) %>% arrange(DISCL_YEAR) 

# It is important that this repetition of whole disclosures comes before the next step, and not after, as was the case in IC2B v1.0, 
# because then, links counted in the min coop size process are not omitted just because a company did not disclose in a given year. 
# However, the coop existence extrapolation must occur after the coop minimum size, to extrapolate this information over time. 


### COOP NB OF FARMERS #### 
# Use the most imputed version throughout
# The goal is to know a minimum number of farmers supplying a cooperative. 
# This is either the number of members directly reported by the coop, 
# or the max number of farmers between the sum of disclosed links with traders and 
# the sum of disclosed links with non-traders, number of farmers disclosed by Fairtrade, and the sum of number of farmers disclosed by Rainforest Alliance (RFA).  
# in computing either, we don't want to double-count across years or disclosures of the same purchase disclosed by different companies or different sources 
# Hence the unique_* variables
civ0_minsize <- civ

civ <- 
  civ %>% 
  # exclude from the trader category, companies that are not in the cleaned trader list, as well 
  # as companies that are different from the trader, to count only once farmer bases of flows where we left multi-tier info (because the trader did not disclose itself)    
  # (this is what the & (is.na(TRADER_NAME) | COMPANY == TRADER_NAME) does). 
  mutate(NON_TRADER = !is.na(COMPANY) & !(COMPANY %in% exper_cln) & NOT_RFA_FT & (is.na(TRADER_NAME) | COMPANY == TRADER_NAME)  , 
         TRADER     = !is.na(COMPANY) & ((COMPANY %in% exper_cln) | (TRADER_NAME %in% exper_cln)) & NOT_RFA_FT, 
         NO_DISCLOSING = is.na(COMPANY)
         ) %>% 
  
    # NON_TRADER
    group_by(COOP_ID, DISCL_YEAR, NON_TRADER) %>% 
    mutate(unique_company_link = (NON_TRADER & !duplicated(COMPANY)),
           TOTAL_FARMERS_NONTRADER = sum(NUM_FARMERS_EXTRAPOLATED*unique_company_link)) %>% # leave na.rm = F such that coops with only missing info get NA and not 0, but this does not exist anymore, because number of farmers has been extrapolated for all links
    # Populate other cells in the column
    group_by(COOP_ID, DISCL_YEAR) %>% 
    mutate(TOTAL_FARMERS_NONTRADER = max(TOTAL_FARMERS_NONTRADER, na.rm = T)) %>% 
   
    # if NUM_FARMERS_EXTRAPOLATED had NAs, we would need to add this in the mutate above: TOTAL_FARMERS_FT = na_if(TOTAL_FARMERS_FT, -Inf)
    
    # TRADER
    group_by(COOP_ID, DISCL_YEAR, TRADER) %>% 
    mutate(unique_trader_link = (TRADER & !duplicated(TRADER_NAME)),
           TOTAL_FARMERS_TRADER = sum(NUM_FARMERS_EXTRAPOLATED*unique_trader_link)) %>% 
    # Populate other cells in the column
    group_by(COOP_ID, DISCL_YEAR) %>% 
    mutate(TOTAL_FARMERS_TRADER = max(TOTAL_FARMERS_TRADER, na.rm = T)) %>% 
    
    # Rainforest Alliance (RFA)
    group_by(COOP_ID, DISCL_YEAR, NOT_RFA) %>% 
    mutate(unique_rfa_link = (!NOT_RFA & !duplicated(COMPANY)),
           TOTAL_FARMERS_RFA = sum(NUM_FARMERS_EXTRAPOLATED*unique_rfa_link)) %>% 
    # Populate other cells in the column
    group_by(COOP_ID, DISCL_YEAR) %>% 
    mutate(TOTAL_FARMERS_RFA = max(TOTAL_FARMERS_RFA, na.rm = T)) %>% 
    
    # Fairtrade
    group_by(COOP_ID, DISCL_YEAR, NOT_FT) %>% 
    mutate(unique_ft_link = (!NOT_FT & !duplicated(COMPANY)),
           TOTAL_FARMERS_FT = sum(NUM_FARMERS_EXTRAPOLATED*unique_ft_link)) %>% 
    # Populate other cells in the column
    group_by(COOP_ID, DISCL_YEAR) %>% 
    mutate(TOTAL_FARMERS_FT = max(TOTAL_FARMERS_FT, na.rm = T)) %>% 
  
  # Cette partie permet d'inclure l'info déduite d'autres années, pour les coops qui une année n'ont aucune disclo d'aucune entreprise. 
  # (une 20aine de cas)
    # No disclosing company
    group_by(COOP_ID, DISCL_YEAR, NO_DISCLOSING) %>% 
    mutate(unique_nodiscl_link = (NO_DISCLOSING & !duplicated(COMPANY)), # this works on NAs
           TOTAL_FARMERS_NODISCL = sum(NUM_FARMERS*unique_nodiscl_link)) %>% # NUM_FARMERS here, not extrapolated
    # Populate other cells in the column
    group_by(COOP_ID, DISCL_YEAR) %>% 
    mutate(TOTAL_FARMERS_NODISCL = max(TOTAL_FARMERS_NODISCL, na.rm = T), 
           TOTAL_FARMERS_NODISCL = na_if(TOTAL_FARMERS_NODISCL, -Inf)) %>% 
    
    # select(-unique_trader_link) %>%NB_FARMERS_COOP_YEAR2
    ungroup() %>% 
    rowwise() %>% 
    mutate(
      # the case when is to leave the value unchanged when it's already 
      # disclosed by the coop itself in that year (e.g. in JRC data).
      TOTAL_FARMERS = case_when( 
        is.na(DISCL_TOTAL_FARMERS) ~ max(
          c_across(cols = all_of(c("TOTAL_FARMERS_TRADER", "TOTAL_FARMERS_NONTRADER", "TOTAL_FARMERS_RFA", "TOTAL_FARMERS_FT", "TOTAL_FARMERS_NODISCL"))),
          na.rm = TRUE),
        TRUE ~ DISCL_TOTAL_FARMERS),
      TOTAL_FARMERS = na_if(TOTAL_FARMERS, -Inf) 
    ) %>% 
    ungroup() %>% 
    # 
    # !! Very important action here !!
    # we consider that the total number of farmers for coops that have no link disclosed is NA, and not 0, as is currently attributed by the code above.
# this is ok now that we have done the last step above.     
mutate(TOTAL_FARMERS = case_when(
      is.na(COMPANY) &  TOTAL_FARMERS == 0 ~ NA, 
      TRUE ~ TOTAL_FARMERS
    )
    )
print("The warnings that 'no non-missing arguments to max; returning -Inf' are not problematic.")

civ %>% filter(TOTAL_FARMERS == 10788) %>% View()
# the one with 10788 farmers is a lot, but it soundly derives from the estimation rule, as expected.  
civ$TOTAL_FARMERS %>% summary()

civ %>% summarise(.by = DISCL_YEAR, NB_FARMERS = sum(TOTAL_FARMERS, na.rm = T)) %>% arrange(DISCL_YEAR) 
civ %>% summarise(.by = DISCL_YEAR, NB_FARMERS = sum(NUM_FARMERS_EXTRAPOLATED, na.rm = T)) %>% arrange(DISCL_YEAR) 

tmp <- civ %>% group_by(COOP_ID, DISCL_YEAR) %>% 
  mutate(ANYNA = anyNA(COMPANY)) %>% 
  ungroup() %>% 
  filter(ANYNA) 

tmp$COOP_ID %>% unique() %>%length()
nrow(tmp)


# civ$TOTAL_FARMERS %>% summary()
# civ %>% filter(TOTAL_FARMERS > 3000) %>% View()

summary(civ$TOTAL_FARMERS)
civ %>% filter(TOTAL_FARMERS==0) %>% nrow()
civ %>% filter(TOTAL_FARMERS==0 & is.na(COMPANY)) %>% View()

rm(civ0_minsize)


# For coops with total farmers known from non-trading companies only, (or vice-versa)
# ... we could distribute uniformly the total to traders (non-traders)
# but we don't, because this implies one additional assumption that the sum is the total, 
# while in many cases the coop may source from much more farmers than those disclosed. 

# civ %>%
#   filter(COOP_ID %in% c(259, 140, 1145, 1079, 1407, 2782, 2857, 4674, 4512, 710, 2856, 2857)) %>%
#   select(COOP_ID, DISCL_YEAR, COMPANY, DISCL_TRADER_NAME, TRADER_NAME,
#          DISCL_NUMBER_FARMERS, BUYER, CAM_BUYERS, NUM_FARMERS, NUM_FARMERS_EXTRAPOLATED,
#          TRADER, NON_TRADER,
#          #unique_company_link, unique_trader_link,
#          TOTAL_FARMERS_NONTRADER, TOTAL_FARMERS_TRADER,
#          TOTAL_FARMERS) %>%
#   arrange(COOP_ID, DISCL_YEAR, COMPANY) %>%
#   View()


# This is equivalent to the 591 cooperatives for which "the trading companies or
# certification bodies themselves reported the number of farmers per cooperative"
# in Renier et al. 
# It does not change between the raw DISCL_* var and the imputed NUM_FARMERS, 
# since imputations for NUM_FARMERS are always *within* coop ID, to more links. 

civ %>% 
  filter(!is.na(DISCL_NUMBER_FARMERS)) %>% 
  pull(COOP_ID) %>% 
  unique() %>% 
  length()

civ %>% 
  filter(!is.na(NUM_FARMERS)) %>% 
  pull(COOP_ID) %>% 
  unique() %>% 
  length()

# it changes for NUM_FARMERS_EXTRAPOLATED. Makes sense since this is extrapolated from across coops
civ %>% 
  filter(!is.na(NUM_FARMERS_EXTRAPOLATED)) %>% 
  pull(COOP_ID) %>% 
  unique() %>% 
  length()





# EXTRAPOLATE COOP EXISTENCE FROM PAST TO FUTURE ####
# This DOES NOT produce a balanced panel. 
# But it implies that the number of coops is only growing. 

# generic row, with variable names - see below
values <- rep(NA, ncol(civ))
variables <- names(civ)
gen_row <- data.frame(variables, values) %>%
  pivot_wider(names_from = variables, values_from = values)
gen_row <- 
  gen_row %>% 
  # leave COOP_ID
  select(-DISCL_YEAR, -DISCL_COUNTRY_NAME, -DISCL_AREA_NAME, -DISTRICT_GEOCODE, -IS_ALL_CAM_V3, -IS_ANY_CAM_V3,
         -contains("ABRVNAME"), -contains("FULLNAME"), -contains("LONGITUDE"), -contains("LATITUDE"), -contains("TOTAL_FARMERS"))

for(year in (min(civ$DISCL_YEAR)+1):max(civ$DISCL_YEAR)){
  # coops that existed in the previous year: 
  coops_past_year <- civ %>% filter(DISCL_YEAR == year-1) %>% pull(COOP_ID) %>% unique()
  # coops disclosed the current year
  coops_year <- civ %>% filter(DISCL_YEAR == year) %>% pull(COOP_ID) %>% unique()
  
  # coops that existed, but do not appear anymore in the current year, bc no one disclosed them 
  # but they most likely still exist!
  coops_disappeared <- coops_past_year[!(coops_past_year %in% coops_year)]
  
  # take one row for each of these, to keep its coop-level information
  disappeared <- 
    civ %>% 
    filter(COOP_ID %in% coops_disappeared & DISCL_YEAR == year-1) %>% 
    select(COOP_ID, DISCL_YEAR, DISCL_COUNTRY_NAME, DISCL_AREA_NAME, DISTRICT_GEOCODE, IS_ALL_CAM_V3, IS_ANY_CAM_V3,
           contains("ABRVNAME"), contains("FULLNAME"), contains("LONGITUDE"), contains("LATITUDE"), contains("TOTAL_FARMERS")) %>% 
    distinct(COOP_ID, DISCL_YEAR, .keep_all = TRUE) %>% 
    mutate(DISCL_YEAR = year) # and change the year to the current one
  
  # add NA values for all other variables. 
  disappeared_full <- 
    disappeared %>% 
    left_join(gen_row, by = "COOP_ID") %>% 
    # flag 
    mutate(REPEATED_FROM_PAST_YEAR = TRUE)
  
  # reorder columns 
  disappeared_full <- disappeared_full[names(civ)]
  # stack
  civ <- rbind(civ, disappeared_full)
  
  rm(coops_past_year, coops_year, coops_disappeared, disappeared, disappeared_full)
}

# verify that the number of coops is growing
print("Coops cumulatively disclosed were: ")
for(year in sort(unique(civ$DISCL_YEAR))){
  civ %>% filter(DISCL_YEAR == year) %>%  pull(COOP_ID) %>% unique() %>% length() %>% paste0(" in ", year) %>% print()
}
summary(civ$TOTAL_FARMERS)
civ %>% summarise(.by = DISCL_YEAR, NB_FARMERS = sum(TOTAL_FARMERS, na.rm = T)) %>% arrange(DISCL_YEAR) 


# REMOVE DUPLICATES -----------------------------
# At this point, within a year, several rows can reflect the same coop obviously 
# (as several companies may have disclosed to buy from the same coop), 
# but also the same link can come from different sources of disclosure data - typically, the previous CAM and data scraped by UCLouvain team). 

# We thus remove duplicates (i.e. make row ids) in 2 different ways: 
# 1- one row per coop per year (aggregating across links) - which is what we publish 
# 2- one row per actual link (aggregating across sources) - which is what is inputed in SEI-PCS. 

# (we DON'T try to make one row per sourcing flow (links to several companies that are on different stages of the supply chain are merged))

civ_coopyear <- 
  civ %>% 
  group_by(COOP_ID, DISCL_YEAR) %>% 
  mutate(DISCLOSURE_SOURCES = paste0(na.omit(unique(COMPANY)), collapse = " + "), 
         TRADER_NAMES = paste0(na.omit(unique(TRADER_NAME)), collapse = " + "), 
         CERTIFICATIONS = paste0(na.omit(unique(unlist(CERT_LIST))), collapse = " + ")) %>% 
  ungroup() %>% 
  distinct(COOP_ID, DISCL_YEAR, .keep_all = TRUE) %>% 
  # remove variables that make no sense at this level of aggregation
  select(-COMPANY, -TRADER_NAME, -DISCL_TRADER_NAME,
         -LOCALITY_NAME, 
         -starts_with("CERT_"),
         -DISCL_NUMBER_FARMERS, -CAM_BUYERS, -BUYER, -NOT_RFA, -NOT_FT,
         -NUM_FARMERS, -NUM_FARMERS_EXTRAPOLATED, -NON_TRADER,-TRADER, -unique_company_link, -unique_trader_link) %>%
  select(COOP_ID, DISCL_YEAR, SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, LATITUDE, LONGITUDE, 
         DISTRICT_NAME, DISTRICT_GEOCODE,
         DISCLOSURE_SOURCES, TRADER_NAMES, CERTIFICATIONS, 
         TOTAL_FARMERS_NONTRADER, TOTAL_FARMERS_TRADER, TOTAL_FARMERS_RFA, TOTAL_FARMERS_FT, TOTAL_FARMERS, 
         everything()) %>% 
  rename(YEAR = DISCL_YEAR)

# /!\ FLOW SECOND BUYER VARIABLE IS DEPRECATED, because in cases like Mondelez buys to a coop through a trader that made its own disclosure,
# and civ_flow does not reflect distinct flows anymore. 
# So I have renamed this object civ_seipcs since this is the object input in SEI-PCS model. 

civ_seipcs <-
  civ %>%
  mutate(
    # ERASE ALREADY EXISTING BUYER VARIABLE, TO PRODUCE THE ONE THAT WILL EVENTUALLY BE USED IN SEIPCS
    BUYER = case_when( # the most upstream disclosed actor that buys this flow. /!\ NOT NECESSARILY A TRADER
      is.na(TRADER_NAME) & !COMPANY %in% c("FAIRTRADE", "RAINFOREST ALLIANCE", "JOINT RESEARCH CENTER") ~ COMPANY,
      TRUE ~ TRADER_NAME
    )
  ) %>%
  group_by(COOP_ID, DISCL_YEAR, BUYER) %>%
  mutate(CERTIFICATIONS = paste0(na.omit(unique(unlist(CERT_LIST))), collapse = " + ")) %>%
  ungroup() %>%
  distinct(COOP_ID, DISCL_YEAR, BUYER,
           .keep_all = TRUE) %>%
  # Make a flow-level ID
  group_by(DISCL_YEAR) %>%
  mutate(FLOW_ID = paste0(DISCL_YEAR, "-", row_number())) %>%
  ungroup() %>%
  select(-DISCL_TRADER_NAME, #-TRADER_NAME,
         -LOCALITY_NAME, # + these ones that never made sense, it's empty.
         -starts_with("CERT_"), # we remove cert_ vars (and make certifications var above) because those are at the link level, not the
         # flow, and there might be small differences (if two companies don't disclose the same info on certification about the same flow).
         -CAM_BUYERS, -NOT_RFA, -NOT_FT,
         -unique_company_link, -unique_trader_link) %>% # -NON_TRADER,-TRADER, 
  select(FLOW_ID, COOP_ID, DISCL_YEAR, SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, LATITUDE, LONGITUDE,
         DISTRICT_NAME, DISTRICT_GEOCODE,
         BUYER, COMPANY, TRADER_NAME, # KEEP TRACK OF TRADER_NAME, for SEI-PCS
         CERTIFICATIONS,
         NUM_FARMERS, NUM_FARMERS_EXTRAPOLATED,
         TOTAL_FARMERS_NONTRADER, TOTAL_FARMERS_TRADER, TOTAL_FARMERS_RFA, TOTAL_FARMERS_FT, TOTAL_FARMERS,
         everything()) %>%
  rename(YEAR = DISCL_YEAR,
         DISCLOSURE_SOURCE = COMPANY, # note that this is not the same variable as DISCLOSURE_SOURCE*S* in civ_coopyear above. This one is always length 1.
         IS_TRADER = TRADER) 

civ_coopyear %>% summarise(.by = YEAR, NB_FARMERS = sum(TOTAL_FARMERS, na.rm = T)) %>% arrange(YEAR) 


civ_seipcs %>%
  filter(COOP_ID %in% c(259, 140, 1145, 1079, 1407, 2782, 2857, 4674, 4512, 710, 2856, 2857)) %>%
  select(FLOW_ID, COOP_ID, YEAR, BUYER, DISCLOSURE_SOURCE,
         NUM_FARMERS, NUM_FARMERS_EXTRAPOLATED,
         CERTIFICATIONS,
         TOTAL_FARMERS_NONTRADER, TOTAL_FARMERS_TRADER, TOTAL_FARMERS) %>%
  arrange(FLOW_ID, COOP_ID, YEAR, BUYER) %>%
  View()


# EXPORT ---------------------
dir.create(here("temp_data/private_IC2B"))

write_csv(civ_coopyear,
          file = here("temp_data/private_IC2B/IC2B_v2_coop.csv"),
          na = "NA", 
          append = FALSE, 
          col_names = TRUE)

write_csv(civ_seipcs,
          file = here("temp_data/private_IC2B/IC2B_v2_link.csv"),
          na = "NA", 
          append = FALSE, 
          col_names = TRUE)








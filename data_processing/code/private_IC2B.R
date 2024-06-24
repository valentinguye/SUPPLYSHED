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

# Assets and functions -----------------------------------------------------


## PRIVATE INPUTS TO CONSOLIDATE ####

### RA data #### 
# This one is not helpful, because the GPS represents just a "central representative point" according to the Readme attached to it. 
# utzra = st_read(here("input_data", "rainforest_alliance", "private", "UTZ_RA", "MembSumary_2019_CIVGHA_ProtectAreaAnaly.shp"))

### FT data #### 
# It is not completely clear whether variable 'Info on members - Total' represents cooperative members or certified cooperative members. 
# Anyway, it can be used to estimate the minimum total number of farmers in the coop. 

ft = read_xlsx(here("input_data", "fairtrade", "SPOs_DATABASE_GH_CDI_FI_Consultants.xlsx"), sheet = 1, col_names = TRUE) 

## PUBLIC DISCLOSURES, CONSOLIDATED  #### 

civ <- s3read_using(
  object = "world/cocoa/logistics/out/consolidated_disclosures.csv",
  bucket = "trase-storage",
  opts = c("check_region" = T),
  FUN = read_delim,
  delim = ";") %>% 
  filter(DISCL_COUNTRY_NAME == "IVORY_COAST")

init <- civ

# country names have already been cleaned in world/~/consolidated_disclosures.R
civ$DISCL_COUNTRY_NAME %>% unique()

# This was saved through a previous run of the present script. 
previous_arbitrary_ids <- 
  s3read_using(
    object = "cote_divoire/cocoa/logistics/out/all_arbitrary_coop_ids_ever.csv",
    bucket = "trase-storage",
    opts = c("check_region" = T),
    FUN = read_delim,
    delim = ";")


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

# necessary to load custom data to get exporter names only (trad_nam is exporter and importer names alltogether)
cd20 <- 
  s3read_using(object = "cote_divoire/cocoa/trade/cd/originals/2020/IVORY_EXPORT_180100_180200_180310_180320_180400_180500_YEAR2020.xlsx", 
               bucket = "trase-storage",
               FUN = read_excel,
               opts = c("check_region" = TRUE))

cd21 <- 
  s3read_using(object = "cote_divoire/cocoa/trade/cd/originals/2021/IVORY_COAST_EXPORT_180100_180200_180310,_180400_180500 _YEAR2021.xlsx", 
               bucket = "trase-storage",
               FUN = read_excel,
               opts = c("check_region" = TRUE))

cd22 <- 
  s3read_using(object = "cote_divoire/cocoa/trade/cd/originals/2022/IVORY_EXPORT_2022.xlsx", 
               bucket = "trase-storage",
               FUN = read_excel,
               opts = c("check_region" = TRUE))

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
    # then decide whether each is a full or abreviated name (if they feature typical full names)
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
    SUPPLIER_FULLNAME = case_when(
      is.na(SUPPLIER_FULLNAME) & grepl("COOPERATIVE|AGRICULTEUR|PRODUCTEUR", WITHIN) ~ WITHIN,
      is.na(SUPPLIER_FULLNAME) & grepl("COOPERATIVE|AGRICULTEUR|PRODUCTEUR", OUTSIDE) ~ OUTSIDE,
      TRUE ~ SUPPLIER_FULLNAME
    ),
    SUPPLIER_ABRVNAME = case_when(
      is.na(SUPPLIER_FULLNAME) & !grepl("COOPERATIVE|AGRICULTEUR|PRODUCTEUR", WITHIN) ~ WITHIN, 
      is.na(SUPPLIER_FULLNAME) & !grepl("COOPERATIVE|AGRICULTEUR|PRODUCTEUR", OUTSIDE) ~ OUTSIDE, 
      TRUE ~ SUPPLIER_ABRVNAME
    )
  ) %>% 
  select(OUTSIDE, WITHIN, SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, Name, everything()) 

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
         YEAR = 2022) %>% # that's arbitrary
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


### STAGE 3 - HOMOGENIZATION ----------------------------------------------------------------

# Prepare simplified abrv names (from already imputed ones)
civ <- mutate(civ, SIMPLIF_ABRVNAME = fn_clean_abrvname3(SUPPLIER_ABRVNAME))

# Prepare rounded coordinates (from already imputed ones)
# Rounding at 2 decimal places in degrees implies that 1.005 and 1.0051 round to 1 and 1.01 resp. 
# this is a spurious difference of 0.01 degree which is 1.1km at equator. 
# This also means that things as different as 0.0051 and 1.005 (apart from 1.1km) are deemed at the same place. 
civ <- 
  mutate(civ, 
         ROUND_LONGITUDE = round(LONGITUDE, 1), 
         ROUND_LATITUDE = round(LATITUDE, 1),
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

## HOMOGENIZE
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
  mutate(CCTN_COOP_ID = paste0(SUPPLIER_ABRVNAME, "_", ROUND_LONGITUDE, "_", ROUND_LATITUDE, "_", SUPPLIER_FULLNAME)) %>%
  arrange(CCTN_COOP_ID)

### MAKE STABLE COOP IDS -----------------------------

# I ran this code once, on the 12th of January 2024, to take the first screenshot of the coop IDs arbitrarily created according to the arrange just above.   
# (12/01/2024 is at the time of 2020, 2021 and 2022 SEIPCS updates, and before any disclosure made in 2024 is added).

# initial_arbitrary_ids <-
#   civ %>%
#   # select(CCTN_COOP_ID) %>%
#   # keep only one row of each, don't need duplicates, now that we have only these two columns
#   distinct(CCTN_COOP_ID) %>%
#   mutate(COOP_ID = row_number()) # Arbitrary coop id, based on order (arrange above)

# # This initial screenshot shall not be overwritten in future runs of this script
# s3write_using(initial_arbitrary_ids,
#               object = paste0("cote_divoire/cocoa/logistics/originals/arbitrary_coop_ids_", today(), ".csv"),
#               bucket = "trase-storage",
#               FUN = write_delim, delim = ";",
#               opts = c("check_region" = T)
# )
# s3write_using(initial_arbitrary_ids,
#               object = "cote_divoire/cocoa/logistics/out/all_arbitrary_coop_ids_ever.csv",
#               bucket = "trase-storage",
#               FUN = write_delim, delim = ";",
#               opts = c("check_region" = T)
# )

# The join creates the COOP_ID column in civ, and fills it with previous IDs where there is a match,
# i.e. where all attributes are exactly the same as in the previous version
civ <- 
  civ %>% 
  left_join(previous_arbitrary_ids, 
            by = "CCTN_COOP_ID") 

# Split into 2 parts
# coops that remain identically identified through the addition of disclosures. 
civ <-
  civ %>% 
  filter(!is.na(COOP_ID)) 

# Newly disclosed or identified coops 
new_discl <- 
  civ %>% 
  filter(is.na(COOP_ID)) 

# For new combinations of attributes, create new arbitrary COOP IDs, making sure that new ones were never given before.   
# For this, let's start counting from the highest number in existing IDs
highest_id <- max(previous_arbitrary_ids$COOP_ID)

# condition to any new coop disclosed/identified, otherwise code crashes.
if(nrow(new_discl) > 0){ 
  
  # make brand new ids in the dataset of newly disclosed/identified coops
  new_discl$COOP_ID <- # (this way, not to throw a warning)
    new_discl %>% 
    group_by(CCTN_COOP_ID) %>% 
    group_indices() + highest_id 
  
}

# Stack already existing and new ones. 
civ <- 
  rbind(
    civ, 
    new_discl
  )

# Update the list of all arbitrary coop ids ever created: 
updated_arbitrary_ids <-
  civ %>% # start from all the currently active ones 
  select(CCTN_COOP_ID, COOP_ID) %>%
  # add those in the previous version that are not in the current one
  rbind(previous_arbitrary_ids) %>% 
  # remove duplicates
  distinct(CCTN_COOP_ID, .keep_all = TRUE)  

# This is to be overwritten and updated every time this script is run on more disclosure data 
s3write_using(updated_arbitrary_ids,
              object = "cote_divoire/cocoa/logistics/out/all_arbitrary_coop_ids_ever.csv",
              bucket = "trase-storage",
              FUN = write_delim, delim = ";",
              opts = c("check_region" = T)
)

# **************

# this is just to keep track of where the info comes from
civ <- 
  civ %>% 
  group_by(COOP_ID) %>% 
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



length(unique(civ$COOP_ID))
civ <- ungroup(civ)

civ %>% #filter(grepl("COASADA", SUPPLIER_ABRVNAME, ignore.case = T)) %>% 
  arrange(SUPPLIER_FULLNAME) %>% 
  filter(is.na(DISCL_LONGITUDE & !is.na(LONGITUDE))) %>% 
  # filter(SUPPLIER_ABRVNAME=="SOCEADAHS") %>% 
  select(DISCL_SUPPLIER_ABRVNAME, DISCL_SUPPLIER_FULLNAME, DISCL_LONGITUDE, DISCL_LATITUDE, 
         SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, LONGITUDE, LATITUDE, ROUND_LONGITUDE, ROUND_LATITUDE, 
         COOP_ID) %>%  View()

civ %>% filter(grepl("COPACOL", SUPPLIER_ABRVNAME, ignore.case = T)) %>% 
  arrange(SUPPLIER_FULLNAME) %>% 
  # filter(is.na(DISCL_LONGITUDE & !is.na(LONGITUDE))) %>% 
  # filter(SUPPLIER_ABRVNAME=="SOCEADAHS") %>% 
  select(COMPANY, DISCL_SUPPLIER_ABRVNAME, DISCL_SUPPLIER_FULLNAME, DISCL_LONGITUDE, DISCL_LATITUDE, 
         SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, LONGITUDE, LATITUDE, ROUND_LONGITUDE, ROUND_LATITUDE, 
         COOP_ID) %>%  View()


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
#   select(COOP_ID, DISTRICT_GEOCODE, LONGITUDE, LATITUDE) %>% 
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
#   select(COOP_ID) %>% 
#   st_drop_geometry() %>% unlist()
# 
# mismatches <- imp_coords %>% filter(COOP_ID %in% pb_ids) 
# departements[departements$LVL_4_CODE %in% mismatches$DISTRICT_GEOCODE,] %>% st_geometry() %>% plot()
# mismatches %>% st_geometry() %>% plot(add=T, col = "red")
# 
# civ %>% filter(grepl("ECSP", SUPPLIER_ABRVNAME, ignore.case = T)) %>% View()



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

# this function should take as input a character vector of length > 1. 
fn_standard_certification_names <- function(x) {
  y <- x
  if(grepl("RAINFOREST ALLIANCE|RFA|OLD-RA|NEW-RA|^RA$", x) & !grepl("NOT GRANTED|SUSPEND", x)){
    y <- "RAINFOREST ALLIANCE"
  } 
  if(grepl("FAIRTRADE|FAIR TRADE|FAITRADE|FAIRTRIDE|FAITRIDE|FT USA|^FT$", x) & !grepl("SUSPENDED", x)){
    y <- "FAIRTRADE"
  }
  if(grepl("UTZ|UTS", x) & !grepl("DECERTIFIED", x)){
    y <- "UTZ"
  } 
  if(grepl("HORIZON", x)){
    y <- "COCOA HORIZONS"
  }
  if(grepl("COCOA PLAN", x)){
    y <- "COCOA HORIZONS"
  }
  if(grepl("COCOALIF|COCOA LIF|CACAOLIF|CACAO LIF", x)){
    y <- "COCOA LIFE"
  }
  if(grepl("FAIR FOR LIFE", x)){
    y <- "FAIR FOR LIFE"
  }
  if(grepl("^TRACE$|CACAO TRACE|CACAOTRACE|CACAO-TRACE", x)){
    y <- "CACAO-TRACE"
  }
  if(grepl("BIOLOGIQUE|^BIO$|^AB$", x)){
    y <- "AGRICULTURE BIOLOGIQUE"
  }
  if(grepl("4C|^ASA$|PROGRAMME DE L'UNION ECOOKIM|^CACAO$|^CARE$|CE 834|CENTRE D'INNOVATION VERTE|^CLMRS$|^CMS$|^COCOA ACTION$|^COH$|COOPACADEMY2|PILOT|^GAL$|^GIZ$|^ICI$|^LANTEUR$|^MICRO$|^MOCA$|^NEW$|^NA$|^NEANT$|^OLD$|^PP$|PRODUCTIVITY PACKAGE|^PRO$|^PROPLANTEUR$|^RCCP$|RESPONSIBLY SOURCED COCOA|^SASSANDRA$|^SOCIAL$|^STARBUCK$", x)){
    y <- "OTHER PROGAM"
  }
  if(grepl("DECERTIFIED", x)){y <- "9999"}
  if(grepl("NOT GRANTED", x)){y <- "9999"}
  if(grepl("SUSPEND", x)){y <- "9999"}
  if(grepl("NOT CERTIFIED", x)){y <- "9999"}
  if(grepl("SUPPLIER STANDARD", x)){y <- "9999"}
  if(grepl("2007", x)){y <- "9999"}
  if(grepl("NOP )", x)){y <- "9999"}
  if(!is.na(x) & nchar(x)==0){y <- "9999"}
  # if(is.na(x)){y <- NULL}
  
  return(y)
}
# civ %>% filter(grepl("2007", DISCL_CERTIFICATION_NAME)) %>% View()
# civ %>% filter(grepl("NOP )", DISCL_CERTIFICATION_NAME, ignore.case = T)) %>% View()
# civ %>% filter(grepl("trace", DISCL_CERTIFICATION_NAME, ignore.case = T)) %>% View()
civ <- 
  civ %>% 
  mutate(CERT_LIST = map(CERT_LIST, ~modify(.x, fn_standard_certification_names)))

unique(unlist(civ$CERT_LIST)) %>% sort()

# Note: certification is (already) at the link level, i.e. specific to the value in every row in the COMPANY column. 
# Indeed, this was inputed as such in disclosure scraping, and this seems to be the case in the CAM as well. 

# The right way to do it is to clean certification from CAM, (and to have it in a separate column in the first place)
# because for other sources than the CAM, certif information has already been inputed per link. 
# left_join(civ, 
#           civ %>% 
#             filter(grepl(";", DISCL_CERTIFICATION_NAME)) %>% 
#             summarise(n_sets_certif = length(unique(unlist(CERT_LIST))), 
#                       .by = COOP_ID), 
#           by = "COOP_ID") %>% 
#   arrange(COOP_ID) %>%
#   select(COOP_ID, n_sets_certif, everything()) %>% View()


# BINARIZE
civ <- 
  civ %>% 
  mutate(
    CERT_UTZ = map_lgl(CERT_LIST, ~ any(grepl("UTZ", .x))),
    CERT_RAINFOREST_ALLIANCE = map_lgl(CERT_LIST, ~ any(grepl("RAINFOREST ALLIANCE", .x))),
    CERT_FAIRTRADE = map_lgl(CERT_LIST, ~ any(grepl("FAIRTRADE", .x))),
    CERT_OLAM = map_lgl(CERT_LIST, ~ any(grepl("OLAM PROGRAMMES", .x))),
    CERT_BARRY_CALLEBAUT = map_lgl(CERT_LIST, ~ any(grepl("COCOA HORIZONS", .x))),
    CERT_CARGILL = map_lgl(CERT_LIST, ~ any(grepl("CARGILL COCOA PROMISE", .x))),
    CERT_CEMOI = map_lgl(CERT_LIST, ~ any(grepl("TRANSPARENCE CACAO", .x))),
    CERT_HERSHEY = map_lgl(CERT_LIST, ~ any(grepl("COCOA FOR GOOD", .x))),
    CERT_MARS = map_lgl(CERT_LIST, ~ any(grepl("RESPONSIBLY SOURCED COCOA", .x))),
    CERT_MONDELEZ = map_lgl(CERT_LIST, ~ any(grepl("COCOA LIFE", .x))),
    CERT_FAIR_FOR_LIFE = map_lgl(CERT_LIST, ~ any(grepl("FAIR FOR LIFE", .x))),
    CERT_OLAM = map_lgl(CERT_LIST, ~ any(grepl("SUSTAINABLE ORIGINS", .x))),
    CERT_NESTLE = map_lgl(CERT_LIST, ~ any(grepl("COCOA PLAN", .x))),
    CERT_FERMICOA = map_lgl(CERT_LIST, ~ any(grepl("FERMICOA", .x))),
    CERT_PURATOS = map_lgl(CERT_LIST, ~ any(grepl("CACAO-TRACE", .x))),
    
    
    CERTIFIED = ifelse(CERT_UTZ |
                         CERT_RAINFOREST_ALLIANCE |
                         CERT_FAIRTRADE |
                         CERT_BARRY_CALLEBAUT |
                         CERT_CARGILL |
                         CERT_CEMOI |
                         CERT_FAIR_FOR_LIFE |
                         CERT_FERMICOA |
                         CERT_HERSHEY |
                         CERT_MONDELEZ |
                         CERT_NESTLE |
                         CERT_OLAM |
                         CERT_PURATOS, T, F)
  )

# NUMBER FARMERS -------------------------

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

#### Flow size extrapol. (sei-pcs) ------------------------ 
# Here, we impute the number of farmers, where missing. 

# Rationale: 
# Number of farmer  at the disclosure level is NOT representative of the coop, 
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

##### 2nd kind of imputations #####

# Then impute within COOP_ID, in order to impute missings from values potentially disclosed by other companies 
# this is the equivalent to: "Where a company did not report the number of farmers 
# they purchased from for a given cooperative, but the number was reported by other 
# companies, the sourcing from that cooperative was assigned the mean value of 
# the sizes disclosed by these other companies." in Renier et al. 

# Do not count RFA or FT farmers in the average, to impute the missing number of farmers as supply flows (as recommended in CAM_to_traders_volumes_FOB_capped_2019_CLEAN.Rmd)
civ <- mutate(civ, 
              NOT_RFA = COMPANY != "RAINFOREST ALLIANCE",
              NOT_FT = COMPANY != "FAIRTRADE")

# # what is the average number of farmers per coop every year 
# civ %>% summarise(.by = DISCL_YEAR, 
#                   AVG_NUM_FARM_PER_COOP = mean(NUM_FARMERS, na.rm = TRUE) ) %>% 
#   arrange(DISCL_YEAR)

civ_save <- civ

# the same year first
civ <- 
  civ %>%
  group_by(COOP_ID, DISCL_YEAR, NOT_RFA, NOT_FT) %>% 
  mutate(
    NUM_FARMERS = case_when(
      is.na(NB_FARMERS_COMPANY_YEAR) ~ round(mean(NB_FARMERS_COMPANY_YEAR, na.rm=TRUE), 0),# this yields NaN for coops with only missing info
      TRUE ~ NB_FARMERS_COMPANY_YEAR
    )) %>% 
  ungroup()


# Then, if still not imputed, impute from different years 
civ <- 
  civ %>%
  group_by(COOP_ID, NOT_RFA, NOT_FT) %>% 
  mutate(
    NUM_FARMERS = case_when(
      is.na(NUM_FARMERS) ~ round(mean(NUM_FARMERS, na.rm=TRUE), 0), 
      TRUE ~ NUM_FARMERS
    )) %>% 
  ungroup()

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
  group_by(DISCL_YEAR, NOT_RFA, NOT_FT) %>% 
  # Necessary to name it differently, for SEIPCS to pick up NUM_FARMERS variable untouched by this step; 
  mutate(
    NUM_FARMERS_EXTRAPOLATED = case_when(
      is.na(NUM_FARMERS) ~ round(mean(NUM_FARMERS, na.rm=TRUE), 0), 
      TRUE ~ NUM_FARMERS
    )) %>% 
  ungroup()

# Those remaining with NA NUM_FARMERS_EXTRAPOLATED are rainforest alliance links in 2023, 2021 and 2019
civ$NUM_FARMERS_EXTRAPOLATED %>% summary()
civ %>% filter(is.na(NUM_FARMERS_EXTRAPOLATED)) %>% pull(COMPANY) %>% unique()

# civ %>% summarise(.by = DISCL_YEAR, 
#                   AVG_NUM_FARM_PER_COOP = mean(NUM_FARMERS_EXTRAPOLATED, na.rm = TRUE)) %>% 
#   arrange(DISCL_YEAR)

# civ %>% filter(COMPANY %in% c("FAIRTRADE", "RAINFOREST ALLIANCE")) %>% View()

### Coop minimum size #### 
# Use the most imputed version throughout
# The goal is to know a minimum number of farmers supplying a cooperative. 
# This is the max number of farmers between the sum of disclosed links with traders and 
# the sum of disclosed links with non-traders, number of farmers disclosed by Fairtrade, and the sum of number of farmers disclosed by Rainforest Alliance (RFA).  
# in computing either, we don't want to double-count across years or disclosures of the same purchase disclosed by different companies or different sources 
# Hence the unique_* variables
civ0_minsize <- civ

civ <- 
  civ %>% 
  # exclude from the trader category, companies that are not in the cleaned trader list, as well 
  # as companies that are different from the trader, to count only once farmer bases of flows where we left multi-tier info (because the trader did not disclose itself)    
  # (this is what the & (is.na(TRADER_NAME) | COMPANY == TRADER_NAME) does). 
  mutate(NON_TRADER = !is.na(COMPANY) & !(COMPANY %in% exper_cln) & NOT_RFA & NOT_FT & (is.na(TRADER_NAME) | COMPANY == TRADER_NAME)  , 
         TRADER     = !is.na(COMPANY) & ((COMPANY %in% exper_cln) | (TRADER_NAME %in% exper_cln)) & NOT_RFA & NOT_FT) %>% 
  # NON_TRADER
  group_by(COOP_ID, DISCL_YEAR, NON_TRADER) %>% 
  mutate(unique_company_link = (NON_TRADER & !duplicated(COMPANY)),
         TOTAL_FARMERS_NONTRADER = sum(NUM_FARMERS_EXTRAPOLATED*unique_company_link)) %>% # leave na.rm = F such that coops with only missing info get NA and not 0
  
  # Populate other cells in the column
  group_by(COOP_ID, DISCL_YEAR) %>% 
  mutate(TOTAL_FARMERS_NONTRADER = max(TOTAL_FARMERS_NONTRADER, na.rm = T), 
         TOTAL_FARMERS_NONTRADER = na_if(TOTAL_FARMERS_NONTRADER, -Inf)) %>% 
  
  # TRADER
  group_by(COOP_ID, DISCL_YEAR, TRADER) %>% 
  mutate(unique_trader_link = (TRADER & !duplicated(TRADER_NAME)),
         TOTAL_FARMERS_TRADER = sum(NUM_FARMERS_EXTRAPOLATED*unique_trader_link)) %>% 
  
  # Populate other cells in the column
  group_by(COOP_ID, DISCL_YEAR) %>% 
  mutate(TOTAL_FARMERS_TRADER = max(TOTAL_FARMERS_TRADER, na.rm = T), 
         TOTAL_FARMERS_TRADER = na_if(TOTAL_FARMERS_TRADER, -Inf)) %>% 
  
  # Rainforest Alliance (RFA)
  group_by(COOP_ID, DISCL_YEAR, !NOT_RFA) %>% 
  mutate(unique_rfa_link = (!NOT_RFA & !duplicated(COMPANY)),
         TOTAL_FARMERS_RFA = sum(NUM_FARMERS_EXTRAPOLATED*unique_rfa_link)) %>% 
  # Populate other cells in the column
  group_by(COOP_ID, DISCL_YEAR) %>% 
  mutate(TOTAL_FARMERS_RFA = max(TOTAL_FARMERS_RFA, na.rm = T), 
         TOTAL_FARMERS_RFA = na_if(TOTAL_FARMERS_RFA, -Inf)) %>% 
  
  # Fairtrade
  group_by(COOP_ID, DISCL_YEAR, !NOT_FT) %>% 
  mutate(unique_ft_link = (!NOT_FT & !duplicated(COMPANY)),
         TOTAL_FARMERS_FT = sum(NUM_FARMERS_EXTRAPOLATED*unique_ft_link)) %>% 
  # Populate other cells in the column
  group_by(COOP_ID, DISCL_YEAR) %>% 
  mutate(TOTAL_FARMERS_FT = max(TOTAL_FARMERS_FT, na.rm = T), 
         TOTAL_FARMERS_FT = na_if(TOTAL_FARMERS_FT, -Inf)) %>% 
  
  # select(-unique_trader_link) %>%NB_FARMERS_COOP_YEAR2
  ungroup() %>% 
  rowwise() %>% 
  mutate(TOTAL_FARMERS = max(
    c_across(cols = all_of(c("TOTAL_FARMERS_TRADER", "TOTAL_FARMERS_NONTRADER", "TOTAL_FARMERS_RFA", "TOTAL_FARMERS_FT"))),
    na.rm = TRUE), 
    TOTAL_FARMERS = na_if(TOTAL_FARMERS, -Inf)
  ) %>% 
  ungroup()
print("The warnings that 'no non-missing arguments to max; returning -Inf' are not problematic.")

# civ$TOTAL_FARMERS %>% summary()
# civ %>% filter(TOTAL_FARMERS > 3000) %>% View()

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

# ATTRIBUTE DISTRICT (departement) GEOCODE -----------------------------------------

civ0 <- civ
# First on the basis of coordinates 
# Make spatial
civ_coords <- 
  civ %>%
  select(COOP_ID, DISTRICT_GEOCODE, LONGITUDE, LATITUDE) %>%
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
      COOP_ID %in% mis$COOP_ID ~ NA, 
      TRUE ~ LONGITUDE
    ), 
    LATITUDE = case_when(
      COOP_ID %in% mis$COOP_ID ~ NA, 
      TRUE ~ LATITUDE
    ))
rm(mis)

# Merge the geocode infered from coordinates
civ_coords <- 
  civ_coords %>% 
  st_drop_geometry() %>% 
  select(COOP_ID, LVL_4_CODE) %>% 
  distinct(COOP_ID, .keep_all = TRUE) # keep only distinct coop ids for the join below

civ <- 
  civ %>% 
  left_join(civ_coords, by = "COOP_ID")

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
  group_by(COOP_ID) %>% 
  mutate(
    DISTRICT_GEOCODE = case_when(
      is.na(DISTRICT_GEOCODE) ~ unique_unique(DISTRICT_GEOCODE), 
      TRUE ~ DISTRICT_GEOCODE
    )
  ) %>% 
  ungroup()

all_imp_rows <- setdiff(civ, civ0)
all_imp_rows %>% nrow() %>% print()
worthit_imp_grps_dept <- civ %>% filter(COOP_ID %in% all_imp_rows$COOP_ID)

# NOT all obs. with a geocode have coordinates. 
civ %>% filter(is.na(LONGITUDE) & !is.na(DISTRICT_GEOCODE))
# All obs. with coordinates have a geocode 
civ %>% filter(!is.na(LONGITUDE) & is.na(DISTRICT_GEOCODE))

# Attribute District geocode where still not available from coordinates etc. above, but 
# where a locality name is provided. 
civ$DISCL_AREA_NAME <- toupper(civ$DISCL_AREA_NAME)
unique(civ$DISCL_AREA_NAME)

civ0 <- civ 

lvl4 <- departements %>% st_drop_geometry() %>% select(LVL_4_NAME, LVL_4_CODE)

# join the LVL_4_CODE column, based on equality of disclosed area name and LVL_4_NAME
civ <- 
  civ %>% 
  left_join(lvl4, by = c("DISCL_AREA_NAME" = "LVL_4_NAME"))
# civ %>% select(COOP_ID, DISCL_AREA_NAME, DISTRICT_NAME, DISTRICT_GEOCODE, LVL_4_CODE) %>% View()

# attribute LVL_4_CODE to DISTRICT_GEOCODE when the latter is missing (which is also when coordinates are missing at this point)
civ <- 
  civ %>% 
  mutate(DISTRICT_GEOCODE = case_when(
    is.na(DISTRICT_GEOCODE) ~ LVL_4_CODE, 
    TRUE ~ DISTRICT_GEOCODE
  )) %>% 
  select(-LVL_4_CODE)

# civ %>% filter(is.na(DISTRICT_GEOCODE) & !is.na(LVL_4_CODE))


# Make clean district name based on this, replacing the previous variable that never made sense (was empty). 
civ <- 
  civ %>% 
  left_join(lvl4, 
            by = c("DISTRICT_GEOCODE" = "LVL_4_CODE")) %>% 
  mutate(DISTRICT_NAME = LVL_4_NAME) 




# EXTEND EARLIER DISCLOSURES TO SUBSEQUENT YEARS -----------------------------
# ... when we have no disclosure from a company those years  

# For instance, in 2020, we have almost no disclosure from traders:
civ %>% filter(DISCL_YEAR == 2020) %>% pull(TRADER_NAME) %>% unique()

# Rationales: 
# Start from baseline year, 2019, for which and only for which we have near complete coverage of companies. 
# For every company present a given year, check whether it has made a disclosure the next year. 
# ... (starting in 2020, but when 2020 has been imputed then look at all companies in 2020, to capture possibly new ones)
# We are at the disclosure level (working on COMPANY variable), not trader level. 

# unique(civ$COMPANY)

civ$REPEATED_FROM_PAST_YEAR <- FALSE # see below

for(year in 2020:max(unique(civ$DISCL_YEAR))){
  
  civ_past_year <- filter(civ, DISCL_YEAR == year-1)
  civ_year <- filter(civ, DISCL_YEAR == year)
  
  # companies known to disclose as of past year
  companies_past_year <- unique(civ_past_year$COMPANY)
  # remove NAs (recall: there are NAs in company - they are all the coops not linked to a company in the CAM)
  companies_past_year <- companies_past_year[!is.na(companies_past_year)]
  
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
         -contains("ABRVNAME"), -contains("FULLNAME"), -contains("LONGITUDE"), -contains("LATITUDE"))

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
           contains("ABRVNAME"), contains("FULLNAME"), contains("LONGITUDE"), contains("LATITUDE")) %>% 
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
         -DISCL_NUMBER_FARMERS, -CAM_BUYERS, -BUYER, -NOT_RFA,
         -NUM_FARMERS, -NUM_FARMERS_EXTRAPOLATED, -NON_TRADER,-TRADER, -unique_company_link, -unique_trader_link) %>%
  select(COOP_ID, DISCL_YEAR, SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, LATITUDE, LONGITUDE, 
         DISTRICT_NAME, DISTRICT_GEOCODE,
         DISCLOSURE_SOURCES, TRADER_NAMES, CERTIFIED, CERTIFICATIONS, 
         TOTAL_FARMERS_NONTRADER, TOTAL_FARMERS_TRADER, TOTAL_FARMERS, 
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
      is.na(TRADER_NAME) & !COMPANY %in% c("FAIRTRADE", "RAINFOREST ALLIANCE") ~ COMPANY,
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
         -CAM_BUYERS, -NOT_RFA,
         -unique_company_link, -unique_trader_link) %>% # -NON_TRADER,-TRADER, 
  select(FLOW_ID, COOP_ID, DISCL_YEAR, SUPPLIER_ABRVNAME, SUPPLIER_FULLNAME, LATITUDE, LONGITUDE,
         DISTRICT_NAME, DISTRICT_GEOCODE,
         BUYER, COMPANY, TRADER_NAME, # KEEP TRACK OF TRADER_NAME, for SEI-PCS
         CERTIFICATIONS,
         NUM_FARMERS, NUM_FARMERS_EXTRAPOLATED,
         TOTAL_FARMERS_NONTRADER, TOTAL_FARMERS_TRADER, TOTAL_FARMERS_RFA, TOTAL_FARMERS,
         everything()) %>%
  rename(YEAR = DISCL_YEAR,
         DISCLOSURE_SOURCE = COMPANY, # note that this is not the same variable as DISCLOSURE_SOURCE*S* in civ_coopyear above. This one is always length 1.
         IS_TRADER = TRADER) 

civ_seipcs %>%
  filter(COOP_ID %in% c(259, 140, 1145, 1079, 1407, 2782, 2857, 4674, 4512, 710, 2856, 2857)) %>%
  select(FLOW_ID, COOP_ID, YEAR, BUYER, DISCLOSURE_SOURCE,
         NUM_FARMERS, NUM_FARMERS_EXTRAPOLATED,
         CERTIFICATIONS,
         TOTAL_FARMERS_NONTRADER, TOTAL_FARMERS_TRADER, TOTAL_FARMERS) %>%
  arrange(FLOW_ID, COOP_ID, YEAR, BUYER) %>%
  View()


# EXPORT ---------------------

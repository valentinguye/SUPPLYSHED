# Functions used to process Cote d'Ivoire cocoa SEI-PCS
# Several functions in this script are twins: they serve the same purpose, but are slightly different:
# one, generally written by Erasmus, would be called in the 2019 model run,
# the other, written by Valentin, would be called in 2020- model runs

# Load HS codes
# source(here::here("trase/tools/r/processing_helpers/get_hs_codes.R"))


str_trans <- function(x) {
  x %>%
    stringi::stri_trans_toupper() %>%
    stringi::stri_trans_general("Latin-ASCII")
}


rm_problem_characters <- function(x) {
  gsub(';|\r|\t|\n|"', "", x) # note that '"' needs to be removed, to pass check for export
}

fn_clean_corp_accronyms <- function(col_name) {
  str_squish(
    gsub("[.]| SL$| LTD$| CO$| LLC| INC| AS$| SRL$| BV$| SAS$| GMBH$|OOO LIMITED$| -SA$| (SAS)$| S.A.$| S.A.S$| - S.A.S$|-SA$|(SA)$", "", col_name)
  )
}


# fn_clean_abrvname1n fn_clean_abrvname2 and fn_clean_abrvname3 below are used in private_IC2B.R 

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


fn_clean_abrvname3 <- function(col_name){
  gsub(pattern = "COOP CA | COOP CA$|COOP-CA | COOP-CA$|COOPCA | COOPCA$|COOPCA-|-COOPCA$|COOP-CA-|-COOP-CA$|COOP | COOP$|COOP-|-COOP$|SCOOP | SCOOP$|SCOOP-|-SCOOP$|SCOOPS | SCOOPS$|SCOOPS-|-SCOOPS$", 
       replacement = "", 
       x = col_name)
}



# old, not used  
fn_clean_names <- function(col_name) {
  str_squish(
    gsub("[.]| SL$| LTD$| CO$| LLC| INC| AS$| SRL$| BV$| SAS$| GMBH$|OOO LIMITED$", "", col_name)
  )
}


# these are the1 tax ids
fn_unique_names_by_exporter_tax_id <- function(tax_id_var, name_var) {
  case_when(
    # From 2020 customs data
    tax_id_var == "6000427U" ~ "SACO BARRY CALLEBAUT",
    tax_id_var == "4111053M" ~ "TAN IVOIRE",
    tax_id_var == "1544858G" ~ "SUTEC", # this is SOCIETE D'USINAGE DE TRANSFORMATION ET D'EXPORTATION CAFE CACAO
    tax_id_var == "1430949D" ~ "FILDISI COCOA INDUSTRY",
    tax_id_var == "1105686V" ~ "OLAM COCOA PROCESSING",
    tax_id_var == "1021063M" ~ "EXPORT TRADING CORPORATION CI",
    tax_id_var == "0815790B" ~ "OMNI VALUE",
    tax_id_var == "0045653L" ~ "PLOT ENTREPRISE",
    # from 2021 cd (only)
    tax_id_var == "2024256X" ~ "CONDICAF", # this is SOCIETE DE CONDITIONNEMENT DE CAFE CACAO https://lespagesvertesci.net/entreprise-109&condicaf-sa-conditionnement-de-cafe-cacao-agroindustrie-agroalimentaire-cacao-cafe-transformateur-annuaire-agriculture-lespagesvertes
    tax_id_var == "1625460E" ~ "IVCOM",
    tax_id_var == "1552322W" ~ "AWAHUS",
    tax_id_var == "0043280R" ~ "CEMOI TRADING",
    tax_id_var == "0815951Y" ~ "S3C",

    tax_id_var == "1900340W" ~ "SACC", # this is SOCIETE AGRICOLE DE CAFE ET DE CACA

    tax_id_var == "1438677G" ~ "SOCIETE COOPERATIVE AVEC CONSEIL D'ADMINISTRATION", # we don"t have more info than that

    # Exporting coops (make it match given group name below, just cleaner)
    tax_id_var == "0426127Q" ~ "ECOOKIM",
    tax_id_var == "1429731V" ~ "TIBONI (COOPERATIVE)",
    tax_id_var == "1434255V" ~ "SOCODENI (COOPERATIVE)",
    tax_id_var == "1422679A" ~ "SCAT (COOPERATIVE)",
    tax_id_var == "1538742D" ~ "SOCAK KATANA (COOPERATIVE)",
    tax_id_var == "1538573U" ~ "CAADA (COOPERATIVE)",
    tax_id_var == "1534655S" ~ "CADESA (COOPERATIVE)",
    tax_id_var == "1269848S" ~ "CAYAT (COOPERATIVE)",
    tax_id_var == "1103355E" ~ "SOCODD (COOPERATIVE)",
    tax_id_var == "0731661Q" ~ "CNEK (COOPERATIVE)",
    tax_id_var == "0717191U" ~ "SOPLAD (COOPERATIVE)",
    tax_id_var == "0915977W" ~ "CAREPCI (COOPERATIVE)",

    TRUE ~ name_var
  )
}

# The above associations are spotted by reading in trade data and running the following lines, to show where there are several names for a single tax id.
# trade_data <- trade_data %>% group_by(EXPORTER_TAX_ID) %>%  mutate(N_DIFF_ORIGINAL_NAMES = length(unique(EXPORTER)))
# trade_data %>% select(contains("COUNTRY") | contains("EXPORT") | contains("ORIGINAL"))  %>% filter(N_DIFF_ORIGINAL_NAMES > 1) %>% distinct(EXPORTER_TAX_ID, EXPORTER, .keep_all = T) %>% arrange(desc(EXPORTER_TAX_ID)) %>% View()
# And on trade_data %>% fn_all_trade_name_cleaning()
# trade_data <- trade_data %>% group_by(EXPORTER_TAX_ID) %>%  mutate(N_DIFF_ORIGINAL_NAMES = length(unique(EXPORTER_CLEAN)))
# trade_data %>% select(contains("COUNTRY") | contains("EXPORT") | contains("ORIGINAL"))  %>% filter(N_DIFF_ORIGINAL_NAMES > 1) %>% distinct(EXPORTER_TAX_ID, EXPORTER_CLEAN, .keep_all = T) %>% arrange(desc(EXPORTER_TAX_ID)) %>% View()


# Function to match pre-cleaned trader names to their groups (written by Valentin Guye)
# note: order matters. For instance, if a name is "ECOM P/C FERRERO", the function will
# map it to either ECOM or FERRERO depending on which group is coded first in the function
# Cases were I noticed that order matters are commented in the function.
# For "PC| P/C" instances, this is not handled systematically yet, and so no special rule is
# applied currently - the eventual group name can be either the one before or after "P/C".
# Volumes associated with "P/C" are not significant.
fn_trader_to_group_names <- function(col_name) {
  case_when(
    grepl("BARRY|CALLEBAUT|CALLIBAUT|CALLEBEAUT|CALLEABUT|CALLEBOUT|COLLEBAUT|CALLEEBAUT|SACO|BIOPARTENAIRE|B C COCOA|BC COCOA|BC AG|BC-COCOA|BCCOCOA|BG COCOA|BC COCA|BC-EDDYSTON|C COCOA AG GLOBAL", col_name) ~ "BARRY CALLEBAUT",
    grepl("CARGIL|CARGRILL|CARILL|CARLL", col_name) ~ "CARGILL",
    # whether this
    grepl("SUCDEN|SUCCDEN|DENREE|DENRREE|DENRÉE|DENRRÉE|SUCRES ET DUREES|SUCRES & SENREES|SUCRES ET DENRES", col_name) ~ "SUCDEN",
    # ECOM must be handled before OLAM, to handle the 3 shipments in 2020 that are ECOM PC OLAM
    # this one must be above ECOM, otherwise it's turned into ECOM.
    grepl("AGROFORCE COM|AGROSFORCE COM|AGROFORCECOM", col_name) ~ "AGROFORCE COMMODITIES",

    # it's important that Theobroma, an ECOM subsidiary, be before ocean ~ cocoasource, because theobroma is often called with its address, which is in OCEANENWEG
    grepl("ECOM|ZAMACOM|ZACACOM|ECCOM|AGROTRADE|THEOBROMA|THEBROMA|THEOBRAMA|THOBROMA|ATLANTIC|DUTCH COCOA|TULIP", col_name) ~ "ECOM",
    # source: https://www.ecomtrading.com/products-services/cocoa/

    grepl("COCOASOURCE|OCEAN|OCEAN SA|OCEAN-SA|DIDWA COMMODITIES|COCOA SOURCE|COCOASSOURCE|COCO SOURCE|COCOA SOUR|COCASOUR|COCOASOUR", col_name) ~ "COCOASOURCE", # didwa commodities is in GHA.

    grepl("OLAM|OUTSPAN|UNICAO|OALM INTER|OLMA INTER|OLAN INTER", col_name) ~ "OLAM",
    grepl("CEMOI|CEMOA", col_name) ~ "CEMOI",

    grepl("UNILEVER|UNILIVER|UNILIIVER", col_name) ~ "UNILEVER",
    grepl("MONDELEZ", col_name) ~ "MONDELEZ",
    grepl("FERRERO|FERERO|FERERRO", col_name) ~ "FERRERO",
    grepl("NESTLE|NETSLE", col_name) ~ "NESTLE",
    grepl("MARS", col_name) ~ "MARS",
    grepl("TOUTO|TOUOTON|TOUTAN SA 1 RUE RENE MAGNE CIDEX 13", col_name) ~ "TOUTON",
    # SAF and CIPEXI have gone bankrupt in july 2018, and have become SACC which bought it:
    # https://www.jeuneafrique.com/mag/746599/economie-entreprises/cote-divoire-saf-cacao-redemarrage-sous-surveillance/
    # https://www.jeuneafrique.com/mag/821861/economie-entreprises/cote-divoire-saf-cacao-une-debacle-qui-laisse-des-traces/
    grepl("SAFCACAO|SAF-CACAO|SAF$|CIPEXI|SOCIETE AGRICOLE DE CAFE ET DE CACAO-SAS|SOCIETE AGRICOLE DE CAFE ET DE CAC|SACC$", col_name) ~ "SACC",
    # It is in the CAM, but it is not in the coop section in official list, and it is registered as a socitéré de transofrmation here https://www.pamdagro.ci/filieres-agricoles/cafe-cacao?tla=4&Px2ZaaCBoL=46&typeSearch=17&gLimit=TS96380s
    
    grepl("FUJI|BLOMMER|INDUSTRIAL FOOD SERVICES|HARALD INDUSTRIA E COMERCIO", col_name) ~ "BLOMMER",
    grepl("GUITTARD", col_name) ~ "GUITTARD",
    grepl("HERSHEY", col_name) ~ "HERSHEY",
    grepl("ARMAJARO|AFRICA SOURCING|AFRCA SOURCING", col_name) ~ "AFRICA SOURCING",
    grepl("INTERREGIONALE VICTOIRE", col_name) ~ "UIREVI",
    grepl("SUCSO", col_name) ~ "SUCSO",
    grepl("TONY", col_name) ~ "TONY'S CHOCOLONELY",
    grepl("AGRI COM|AGRICOM|AGRICULTURAL COMMODITIES|AGRI COMMODITIES & FINANCE", col_name) ~ "ETC GROUP", # https://www.difc.ae/public-register/agri-commodities-finance-ltd/
    # ETG is also in the ETC group apparently
    grepl("ETG$|ETG COM|ETC-CI|BEYOND BEAN|EXPORT TRADING|EXPORT-TRADING|COUNT|COCOANECT", col_name) ~ "ETC GROUP", # https://www.etgworld.com/index.html
    # I now add COUNT|COCOANECT because cocoanect has been integrated in ETG https://www.etgworld.com/global-strategic-integration.html and Nitidae report https://www.nitidae.org/files/90b65fa3/document_technique_agriculture_redd_marche_ghana_cotedivoire_cacao_tracao.pdf
    # grepl("COUNT|COCOANECT", col_name) ~ "COCOANECT",
    grepl("FILDISI|ONEM|PLADIS", col_name) ~ "FILDISI",
    grepl("FUCHS & HOFFMANN|KRUGER|KRÜGER|WILHELM REUSS", col_name) ~ "KRUGER",
    grepl("SUSCOM|SUSCOM CI|SUSCOM-CI", col_name) ~ "SUSCOM",


    grepl("PLOT ENTERPRISE|PLOT ENTREPRISE", col_name) ~ "PLOT ENTREPRISE",

    # Other names, spoted in exper_imper, handled in this same function, but affecting most probably custom data only
    grepl("USINAGE DE TRANSFORMATION", col_name) ~ "SUTEC",
    grepl("CONDITIONNEMENT DE CAFE", col_name) ~ "CONDICAF",
    grepl("FACTA INTERNA", col_name) ~ "FACTA INTERNATIONAL",
    grepl("ACS-AMERICA", col_name) ~ "ACS-AMERICA COCOA SUPPLIERS",
    grepl("ACT INTERNA", col_name) ~ "ACT INTERNATIONAL AG KRAMERMATT",
    grepl("AFCO TRAD|AFCOTRAD|AFCO-TRAD", col_name) ~ "AFCO TRADING", # AFCO TRADING exports and imports. AFRICAO TRADING is only an importer. SO it's probably not the same group.
    grepl("AFRICAO TRADING", col_name) ~ "AFRICAO TRADING",
    grepl("AGROFOREST", col_name) ~ "AGROFOREST",
    grepl("ALCAO EOOD", col_name) ~ "ALCAO EOOD",

    grepl("AGROTRADE LIMITED", col_name) ~ "AGROTRADE LIMITED",
    grepl("ALBION INDUSTRIE", col_name) ~ "ALBION INDUSTRIES",
    grepl("ALBRECHT & DILL TRAD|ALBRECHT &DILL|ALBRECHT & DIL TRAD", col_name) ~ "ALBRECHT & DILL TRADING",
    grepl("ALFAGRA UAB", col_name) ~ "ALFAGRA UAB",
    grepl("ALINDA-VELCO|ALINDA VELCO", col_name) ~ "ALINDA-VELCO",
    grepl("ALTINMARKA GIDA", col_name) ~ "ALTINMARKA GIDA",
    grepl("ARASCO", col_name) ~ "ARASCO",
    grepl("ARGINI ", col_name) ~ "ARGINI",
    grepl("AROLA", col_name) ~ "AROLA",
    grepl("ASCOT", col_name) ~ "ASCOT",
    grepl("ASIA VIEW ENT", col_name) ~ "ASIA VIEW ENTERPRISES",
    grepl("BICAO", col_name) ~ "BICAO",
    grepl("BD BUSINESS", col_name) ~ "BD BUSINESS",
    grepl("C C T INT|CC T INT|CCT INT|C CT INT", col_name) ~ "C C T INTERNATIONAL",
    grepl("C STEINWEG|CSTEINWEG", col_name) ~ "C STEINWEG HANDELSVEEM",
    grepl("CEVA LOGISTICS", col_name) ~ "CEVA LOGISTICS",
    grepl("CHINA PLAITED PRODUCTS", col_name) ~ "CHINA PLAITED PRODUCTS",
    grepl("COMOD TRAD|COMOC TRADING|COMOD TARDING|COMMOD TRAING|COMMOD TRADING|COMOD TRSING|COMODTRADING|COMOD TRANDID|COMOD TRATING", col_name) ~ "COMOD TRADING",
    grepl("COMOD P/C", col_name) ~ "COMOD P/C",
    grepl("COCOA TEAM", col_name) ~ "COCOA TEAM",
    grepl("CWT COM", col_name) ~ "CWT COMMODITIES",
    grepl("DE MAN MET SMAAK", col_name) ~ "DE MAN MET SMAAK",
    grepl("DOMORI", col_name) ~ "DOMORI",
    grepl("EL SHAMADAN|ELSHAMADAN", col_name) ~ "EL SHAMADAN INDUSTRIAL FOOD",
    grepl("ESCONDIDO", col_name) ~ "ESCONDIDO",
    grepl("ETHIQUABLE", col_name) ~ "ETHIQUABLE",
    grepl("F PACHE INDUS|FPACHE INDUS", col_name) ~ "FPACHE INDUSTRIAL Y COMMERCIAL",
    grepl("GUAN CHONG|GCB", col_name) ~ "GUAN CHONG COCOA",
    grepl("GORELE HAZELNUT", col_name) ~ "GORELE HAZELNUT GIDA TICARET",
    grepl("GUANGXI JIA SHUN", col_name) ~ "GUANGXI JIA SHUN TRADING",
    grepl("GVOLPI & F", col_name) ~ "GVOLPI & FFAVILLI",
    grepl("HD COTTERELL|HDCOTTERELL", col_name) ~ "HD COTTERELL ELLERHOLZDAMM",
    grepl("HENRY BATH", col_name) ~ "HENRY BATH & SON",
    grepl("HOLDING FINANCIERE 127, RUE DE MU|HOLDING FINANCIERE DE MU|HOLDING FINANCIERE DE MÜ", col_name) ~ "HOLDING FINANCIERE DE MUHLENBACH",
    grepl("HUYSER M|HUYSER/MOLLER|HUYSER,M|HUYSER, M", col_name) ~ "HUYSER MOLLER",
    grepl("IBERCACAO|IBBERCACAO", col_name) ~ "IBERCACAO",
    grepl("IBERCOMMODITIES", col_name) ~ "IBERCOMMODITIES", # these seem actually different to IBERCACAO
    grepl("INTERPORTO RIV", col_name) ~ "INTERPORTO RIVALTA",
    grepl("IVCOM", col_name) ~ "IVCOM",
    grepl("JB COCO|JB CACA|JB FOO", col_name) ~ "JB FOODS", # https://www.jbcocoa.com/
    grepl("JS COCOA", col_name) ~ "JS COCOA",
    grepl("KATOEN NATIE|KATOENNATIE", col_name) ~ "KATOEN NATIE",
    grepl("KEMOFINA", col_name) ~ "KEMOFINA",
    grepl("LE FOUR DU KHALIF", col_name) ~ "LE FOUR DU KHALIFE",
    grepl("LIFE BV|LIVE BV", col_name) ~ "LIFE BV",
    grepl("LOGIKA", col_name) ~ "LOGIKA",
    grepl("M/SCS MARIS|M/SCSMARIS", col_name) ~ "MARIS IMPEX",
    grepl("MANTARIA 4492 RUE|MANTORIA 4492 RUE", col_name) ~ "MANTARIA",
    grepl("MONER COCOA|MONERCOCOA", col_name) ~ "MONER COCOA",
    grepl("NATRA CACAO", col_name) ~ "NATRA CACAO",
    grepl("NEDERLAND", col_name) ~ "NEDERLAND",
    grepl("NEDEX", col_name) ~ "NEDEX",
    grepl("NENUPHAR", col_name) ~ "NENUPHAR",
    grepl("NILA GENERAL TRADING", col_name) ~ "NILA GENERAL TRADING",
    grepl("PANAMIR", col_name) ~ "OU PANAMIR",
    grepl("PATISEN", col_name) ~ "PATISEN",
    grepl("PEACOCK TRAD", col_name) ~ "PEACOCK TRADING",
    grepl("PETROFORCE", col_name) ~ "PETROFORCE TRADING",
    grepl("POT O POHT", col_name) ~ "POT O POHT",
    grepl("PPC GRYF", col_name) ~ "PPC GRYF",
    grepl("PTJEBE KOKO|PT JEBE KOKO", col_name) ~ "PTJEBE KOKO KAWASAN INDUSTRI",
    grepl("QUAST & CONS", col_name) ~ "QUAST & CONS",
    grepl("RAM TEKSTIL GIDA|RAM TESK", col_name) ~ "RAM TESK GIDA",
    grepl("ROCKWIN", col_name) ~ "ROCKWINDS",
    grepl("S-TRADE OU VAN", col_name) ~ "S-TRADE OU VANANARVA",
    grepl("SAFAL COM", col_name) ~ "SAFAL COMMODITIES",
    grepl("SARL SOBCO", col_name) ~ "SOBCO",
    grepl("SEN ALIM", col_name) ~ "SEN ALIM",
    grepl("SENALIA", col_name) ~ "SENALIA",
    grepl("SEVILLE PRODUCTS", col_name) ~ "SEVILLE PRODUCTS",
    grepl("SINTAG", col_name) ~ "SINTAG",
    grepl("SOCOMEX FAR|SOCOMAX FAR", col_name) ~ "SOCOMEX",
    grepl("SOCOMHA", col_name) ~ "SOCOMHA",
    grepl("SOTUIA", col_name) ~ "SOTUIA",
    grepl("AWAHUS", col_name) ~ "SOCIETE AWAHUS SERVICES",
    grepl("SUNBEEN IMPEX", col_name) ~ "SUNBEEN IMPEX",
    grepl("TRADING & SERVI|TRADING ET SERVI", col_name) ~ "TRADING & SERVICES",
    grepl("TANMONDIAL|TAN IVOIR|TAN MOND|TAN MODIAL", col_name) ~ "TANMONDIAL",
    grepl("TRANSCAO", col_name) ~ "TRANSCAO NEGOCE",
    grepl("TRC COCO|TRC SPECIALTY COMMODITIES|TRC COTE D'IVOIRE", col_name) ~ "TRC GROUP",
    grepl("TRILINI INTERNA", col_name) ~ "TRILINI INTERNATIONAL",
    grepl("UAB BALTIC COCOA", col_name) ~ "UAB BALTIC COCOA",
    grepl("UCOM SARL", col_name) ~ "UCOM",
    grepl("UNIFOOD", col_name) ~ "UNIFOOD",
    grepl("UPS-SCS", col_name) ~ "UPS-SCS",
    grepl("VICOCOA & AGROFOOD", col_name) ~ "VICOCOA & AGROFOODS",
    grepl("VIGOLIN", col_name) ~ "VIGOLIN",
    grepl("VOLLERS", col_name) ~ "VOLLERS",
    grepl("WALTE MATTER|WALTER|WATTER MALTER", col_name) ~ "WALTER MATTER",
    grepl("YIWU XINDU", col_name) ~ "YIWU XINDU",
    grepl("#VALUE", col_name) ~ "",
    # those below are found in Ghana cd 2021
    grepl("DAARNHOUWER", col_name) ~ "DAARNHOUWER",
    grepl("CHOCOMAC|BD ASSO", col_name) ~ "BD ASSOCIATES",

    grepl("TACHIBANA", col_name) ~ "TACHIBANA",

    # ECOOKIM is treated as a trader, not a cooperative (it's a network of > 20 coops)
    grepl("ECOOKIM|UNION DES SOCIETES COOPERATIVE KIMB|ENTREPRISE COOPERATIVE KIMB", col_name) ~ "ECOOKIM",

    # We don't deem these as being exporting coops (in official lists, they are not in this category, and they are not in the CAM)
    grepl("S 3 C|S3C|SOCIETE DE COMMERCIALISATION DE CAF", col_name) ~ "S3C",

    # these are only in FAIRTRADE data
    grepl("IVORY COCOA PRODUCT|^ICP", col_name) ~ "IVORY COCOA PRODUCTS",
    grepl("KINEDEN", col_name) ~ "KINEDEN",
    
    TRUE ~ col_name
  )
}

# EXPORTING COOPERATIVES
fn_exporting_coop_names <- function(col_name){

  # All known exporting coops across years should be listed here.
  # The given acronym (on the right) should match the corresponding one in the CAM, plus " (COOPERATIVE)" (see Section 11 in SEI_PCS_COTE_DIVOIRE_COCOA_*.R scripts.)
  # cam %>% filter(grepl("SUCAFINA", SUPPLIER_FULLNAME) | grepl("GUIGL", SUPPLIER_FULLNAME))
  # cam %>% filter(grepl("SOCAT", SUPPLIER_ABRVNAME))
  # It may match several the acronyms of several coops in the CAM (e.g. for 'CASB') this is not an issue for here. This is handled in sei-pcs model scripts.

  case_when(
    # (don't match on the left the acronym if it's too short, it could match other strings in trader names)
    grepl("AGRICOLE AWANE DE|CAADA", col_name) ~ "CAADA (COOPERATIVE)",
    grepl("BEKELE DE FRESC|C A B F|CABF$", col_name) ~ "CABF (COOPERATIVE)",
    grepl("CASB-SCOOPS", col_name) ~ "CASB (COOPERATIVE)",
    grepl("COOP AMELIO REVENU PLANTEUR DE|COOP AMELIO REVENU PLANTE CI|CAREPCI", col_name) ~ "CAREPCI (COOPERATIVE)",
    grepl("DEV DE SASS", col_name) ~ "CADESA (COOPERATIVE)",
    grepl("AGRICOLE YAKASSE|CAYAT", col_name) ~ "CAYAT (COOPERATIVE)",
    grepl("CAY WANDA|CAYWANDA|YETIKON WANDA", col_name) ~ "CAYWANDA (COOPERATIVE)",
    grepl("CNEK|ESPRIT |KETESSO", col_name) ~ "CNEK (COOPERATIVE)",
    grepl("STE COOP AGRICUL DE GAGNY CAR", col_name) ~ "COOPAGAGNY (COOPERATIVE)",
    grepl("ECAKOOG COOP-CA", col_name) ~ "ECAKOOG (COOPERATIVE)",
    grepl("ROBERT-PORTE", col_name) ~ "ECAPR (COOPERATIVE)",
    grepl("STE COOP SIMP PRO AGR MOY CAV|STE COOP SIMP PROD AGR MOY CAV|COOPARM", col_name) ~ "COOPARM (COOPERATIVE)",
    grepl("ENT COOP AGRI MOD DE L'INDENIE|ECAMOI", col_name) ~ "ECAMOI (COOPERATIVE)",
    grepl("ENTREPCOOPAGRIMODMEAGUI", col_name) ~ "ECAMOM (COOPERATIVE)",
    grepl("SOCIETE COOP AGRICOLE ALLIANCE", col_name) ~ "SCAA (COOPERATIVE)",
    grepl("STE COOP AGRI DE TOUIH", col_name) ~ "SCAT (COOPERATIVE)",
    grepl("SCOABIA|ANONKLON BIANOUAN", col_name) ~ "SCOABIA (COOPERATIVE)",
    grepl("SOCIETE COOPERATIVE ANOUANZE DE CI", col_name) ~ "SOCACI (COOPERATIVE)",
    grepl("COOPERATIVE ETRAYAWIEN COTIERE|ETRAYAWLEN", col_name) ~ "SCOOPECO (COOPERATIVE)",
    grepl("KATANA", col_name) ~ "SOCAK KATANA (COOPERATIVE)",
    grepl("SOCODD|DIALOGUE DJEKANOU", col_name) ~ "SOCODD (COOPERATIVE)",
    grepl("STE COOP DESI-CAO NIAPIDOU|SOCODENI", col_name) ~ "SOCODENI (COOPERATIVE)",
    grepl("PLANTEURS DE DJIBOUA", col_name) ~ "SOPLAD (COOPERATIVE)",
    grepl("SIMP TIBONI OK|STE COOP AVEC CONS D'ADM TIBONI OKROUYO|STE COOP AVEC CONS D'ADM TIBONI OK", col_name) ~ "TIBONI (COOPERATIVE)",
    grepl("WAGAJACA", col_name) ~ "WAGAJACA (COOPERATIVE)",

    grepl("ADZOPE NO", col_name) ~ "SOCAAN (COOPERATIVE)", # this is COOPERATIVE AGRICOLE ADZOPE NORD

    TRUE ~ col_name
  )
}

# recognize CFI signatories (some companies only signed for civ of gha, see WCF CFI page)
fn_is_cfi_signatory_civ <- function(col_name){
  if_else(
    col_name %in% c("BARRY CALLEBAUT",
                    "BLOMMER",
                    "CARGILL",
                    "CEMOI",
                    # "CLASEN",
                    "COCOASOURCE",
                    # "HALBA",
                    "COCOCO CHOCOLATIERS",
                    "ECOM",
                    "ETC GROUP",
                    "FERRERO",
                    "GUAN CHONG COCOA",
                    "GENERAL MILLS",
                    "GODIVA",
                    "GUITTARD",
                    "HERSHEY",
                    "INDCRESA",
                    "JB FOODS",
                    "LINDT",
                    "MARKS & SPENCER",
                    # "MARS",
                    "MEIJI",
                    "MONDELEZ",
                    "NESTLE",
                    "OLAM",
                    "PURATOS",
                    "SAINSBURY'S",
                    "SIAT",
                    "STARBUCKS",
                    "SUCDEN",
                    # "TOMS GROUP",
                    "TOUTON",
                    "UNILEVER",
                    "UPL",
                    "VALRHONA"
                    # "WHITTAKER"
    ),
    TRUE,
    FALSE)
}

fn_is_cfi_signatory_gha <- function(col_name){
  if_else(
    col_name %in% c("BARRY CALLEBAUT",
                   "BLOMMER",
                   "CARGILL",
                   "CLASEN",
                   "HALBA",
                   "COCOCO CHOCOLATIERS",
                   "ECOM",
                   "ETC GROUP",
                   "FERRERO",
                   # "GUAN CHONG COCOA",
                   "GENERAL MILLS",
                   "GODIVA",
                   "GUITTARD",
                   "HERSHEY",
                   # "INDCRESA",
                   # "JB FOODS",
                   "LINDT",
                   "MARKS & SPENCER",
                   "MARS",
                   "MEIJI",
                   "MONDELEZ",
                   "NESTLE",
                   "OLAM",
                   # "PURATOS",
                   "SAINSBURY'S",
                   "SUCDEN",
                   "TOMS GROUP",
                   "TOUTON",
                   "UPL",
                   "VALRHONA",
                   "WHITTAKER"
                   ),
    TRUE,
    FALSE)
}


# Identify countries in the EU
# count switzerland in, since its exports to the EU will have to comply
fn_is_in_eu <- function(col_name){
  if_else(
    col_name %in% c("AUSTRIA", 
                    "BELGIUM", 
                    "BULGARIA", 
                    "CROATIA",
                    "CYPRUS",
                    "CZECH REPUBLIC",
                    "DENMARK",
                    "ESTONIA",
                    "FINLAND",
                    "FRANCE", 
                    "GERMANY", 
                    "GREECE", 
                    "HUNGARY",
                    "IRELAND", 
                    "ITALY", 
                    "LATVIA", 
                    "LITHUANIA", 
                    "LUXEMBOURG",
                    "MALTA",
                    "NETHERLANDS", 
                    "POLAND",
                    "PORTUGAL",
                    "ROMANIA",
                    "SLOVAKIA",
                    "SLOVENIA", 
                    "SPAIN", 
                    "SWEDEN",
                      "SWITZERLAND"), 
    TRUE, 
    FALSE
  )
}

# OLD FUNCTION (2019 model)
fn_standard_trader_names <- function(col_name) {
  case_when(
    grepl("CARGIL", col_name) ~ "CARGILL",
    grepl("DENREES|SUCDEN", col_name) ~ "SUCDEN",
    grepl("OLAM|OUTSPAN|UNICAO", col_name) ~ "OLAM",
    grepl("CEMOI", col_name) ~ "CEMOI",
    grepl("CALLEBAUT|SACO|BIOPARTENAIRE", col_name) ~ "BARRY CALLEBAUT",
    grepl("UNILEVER|UNILIVER", col_name) ~ "UNILEVER",
    grepl("MONDELEZ", col_name) ~ "MONDELEZ",
    grepl("MARS", col_name) ~ "MARS",
    grepl("TOUTO", col_name) ~ "TOUTON",
    grepl("ECOM AGROTRADE|ZAMACOM", col_name) ~ "ECOM",
    grepl("SAFCACAO", col_name) ~ "SAF-CACAO",
    # grepl("CIPEXI|SAF", col_name) ~ "SOCIETE AGRICOLE DE CAFE ET DE CACA", #bought in 2019 & those names not included in cd_2019
    grepl("CNEK|ESPRIT", col_name) ~ "CNEK",
    grepl("BLOMMER", col_name) ~ "BLOMMER",
    grepl("GUITTARD", col_name) ~ "GUITTARD",
    grepl("HERSHEY", col_name) ~ "HERSHEY",
    grepl("ARMAJARO", col_name) ~ "AFRICA SOURCING",
    grepl("INTERREGIONALE VICTOIRE", col_name) ~ "UIREVI",
    grepl("SUCSO", col_name) ~ "SUCSO",
    # decreasing size of exporting coop names; accronyms
    col_name == "ENTREPRISE COOPERATIVE KIMBE" ~ "ECOOKIM",
    col_name == "S 3 C" ~ "S3C",
    col_name == "SOCIETE AGRICOLE DE CAFE ET DE CACA" ~ "SACC",
    col_name == "C A B F" ~ "CABF",
    col_name == "COOPERATIVE ANONKLON BIANOUAN" ~ "SCOABIA",
    col_name == "COOPERATIVE DIALOGUE DJEKANOU" ~ "SCOOPS SOCODD",
    col_name == "ENTREPCOOPAGRIMODMEAGUI" ~ "ECAMOM",
    col_name == "SOCIETE DES PLANTEURS DE DJIBOUA" ~ "COOP CA SOPLAD",
    col_name == "STE COOP AGRI DE TOUIH" ~ "SCAT",
    col_name == "STE COOP AGRI DEV DE SASS" ~ "COOP CA CADESA",
    col_name == "STE COOP AGRICUL DE GAGNY CAR" ~ "SCAGC",
    col_name == "STE COOP DESI-CAO NIAPIDOU" ~ "SCOOPS SOCODENI",
    col_name == "CAY WANDA" ~ "CAYWANDA",
    col_name == "COOP AMELIO REVENU PLANTE CI" ~ "CAREPCI",
    col_name == "STE COOP AGR KATANA COOP CA" ~ "SOCAK KATANA",
    col_name == "STE COOP SIMP TIBONI OKROUYO" ~ "SCOOPS TIBONI",
    col_name == "STE IV DE CACAO ET CAFE" ~ "ICC SCOOPS",
    col_name == "COOPERATIVE AGRICOLE WAGAJACA" ~ "WAGAJACA SCOOPS",
    col_name == "COOPERATIVE AGRICOLE YAKASSE-A" ~ "SCOOPS YAT",
    col_name == "SOCIETE COOP AGRICOLE ALLIANCE" ~ "SCAA SCOOPS",
    col_name == "CAGC (COOPAGRICGRD CHANTIER DIVO)" ~ "CAGC COOP CA",
    col_name == "COOP AGRI SEMENCE DE DIVO" ~ "COOPASD COOP CA",
    col_name == "COOP AGRICOL BADEYA DE SOUBRE" ~ "COOABAS",
    col_name == "COOPERATIVE AGRI I (COOPADIS)" ~ "COOPADIS",
    col_name == "COOPERATIVE AGRICOLE DE TOUDOUGOU" ~ "CAT EXPORT COOP CA",
    col_name == "COOPERATIVE CAFE CACAO DE VAVOUA" ~ "SCOOPS COCCAV",
    col_name == "SCTE COOP AGRI UNION DE GD LAH" ~ "COOPAUGRA SCOOPS",
    col_name == "STE COOP DARLIMANE DE KOKOLOPO" ~ "COOP CA SOCOPAK",
    col_name == "UNITE COOPERATIVE DE MEAGUI" ~ "UCOM",
    TRUE ~ col_name
  )
}

# Function to clean exporter names, (EQ-factor conversion is in a separate function as of 2020)
fn_all_trade_name_cleaning <- function(trade_df, exporter_tax_id_column, exporter_column, importer_column) {

  # Clean exporter names
  trade_df <- trade_df %>%
    rename(EXPORTER_ORIGINAL = {{ exporter_column }},
           IMPORTER_ORIGINAL = {{ importer_column }},
           EXPORTER_TAX_ID = {{ exporter_tax_id_column }}) %>%
    mutate(
      # the purpose of this is just to have single name values for every exporter tax ids.
      # clean exporting coop names first
      EXPORTER_CLEAN = fn_exporting_coop_names(fn_clean_corp_accronyms(str_trans(EXPORTER_ORIGINAL))),
      # then this
      EXPORTER_CLEAN = fn_unique_names_by_exporter_tax_id(tax_id_var = EXPORTER_TAX_ID, name_var = str_trans(EXPORTER_CLEAN)),

      # Note that exporter tax ids provided by customs data is more disagregated than exporter groups made below
      EXPORTER_GROUP_CLEAN = fn_trader_to_group_names(fn_clean_corp_accronyms(str_trans(EXPORTER_ORIGINAL))),
      # clean exporting coop names
      EXPORTER_GROUP_CLEAN = fn_exporting_coop_names(str_trans(EXPORTER_GROUP_CLEAN)),

      IMPORTER_GROUP_CLEAN = fn_trader_to_group_names(fn_clean_corp_accronyms(str_trans(IMPORTER_ORIGINAL))),
      # clean exporting coop names, they appear in IMPORTER as well!
      IMPORTER_GROUP_CLEAN = fn_exporting_coop_names(str_trans(IMPORTER_GROUP_CLEAN))
    )
  return(trade_df)
}

# Load bean equivalent conversion factors
fn_load_bean_equivalent <- function(){
  # ... requires first connecting to database.
  # Connect to database
  # NB: Database credentials must be saved in computer's environmental variable, as described in
  # https://github.com/sei-international/TRASE/blob/master/doc/Trase-Database.md#4-an-r-script

  # [1] Load cocoa equivalence factors from database
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "trase",
    host = "trase-db-instance.c6jsbtgl0u2s.eu-west-1.rds.amazonaws.com",
    port = 5432
  )
  
  commodities_tbl <- tbl(con, in_schema("splitgraph", "commodities"))

  cef_cocoa <- commodities_tbl %>%
    filter(commodity == 'COCOA') %>%
    distinct(HS6 = substr(hs_code, 0, 6), eq_factor) %>%
    collect() %>%  # To fetch the result into a local tibble
    rename(EQ_FACTOR = eq_factor)
  
  # Check that there is only one EQ_FACTOR per HS6
  stopifnot(
    cef_cocoa %>% group_by(HS6) %>% count() %>% ungroup() %>% pull(n) %>% max() == 1
  )

  return(cef_cocoa)
}

fn_trade_data_clean_up <- function(trade_df, exporter_column, raw_volume_column, user, db_pass = Sys.getenv("DB_PASSWORD_TS")) {
  # Function to clean exporter names, add EQ-factors, drop rows of cocoa waste (HS6 == "180200")
  # and convert cocoa to CBE (cocoa bean equivalents)
  # ... requires first connecting to database.
  # NB: Database password must be saved in computer's environmental variables as DB_PASSWORD_TS
  # ... or specified in function call


  # [1] Load cocoa equivalence factors from database
  # Connect to database
  # NB: Database password must be saved in computer's environmental variables as DB_PASSWORD_TS
  # ... or specified in function call
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "trase",
    host = "trase-db-instance.c6jsbtgl0u2s.eu-west-1.rds.amazonaws.com",
    port = 5432,
    user = user, # Change this to your username
    password = db_pass
  )

  # [2] Identify the relevant 'com_id' based on the HS code
  # ... cocoa begins with '180'
  data <- tbl(con, "commodity_code_values") %>% collect()
  cocoa_com_id <- data %>%
    filter(grepl("^180", value)) %>%
    distinct(com_id, value)


  # [3] Identify the relevant node_id - i.e. country - CIV in this case
  wcg <- tbl(con, "node_names") %>%
    filter(name %in% c("COTE D'IVOIRE")) %>%
    dplyr::select(node_id, name) %>%
    collect()


  # [4] Extract relevant eq factors
  cef <- tbl(con, "commodity_equivalence_factors") %>% collect()
  cef_cocoa <- cef %>%
    dplyr::select(com_id, node_id, eq_factor) %>%
    inner_join(wcg, by = "node_id") %>%
    inner_join(cocoa_com_id, by = "com_id") %>%
    rename(HS_CODE = value) %>%
    janitor::clean_names("screaming_snake")


  # [5] Simplify HS_CODE to HS6
  cef_cocoa <- cef_cocoa %>%
    mutate(HS6 = str_sub(HS_CODE, start = 1, end = 6)) %>%
    distinct(HS6, EQ_FACTOR)


  # Check that there is only one EQ_FACTOR per HS6
  stopifnot(
    cef_cocoa %>% group_by(HS6) %>% count() %>% ungroup() %>% pull(n) %>% max() == 1
  )


  # [6] Clean exporter names, add EQ-factors
  # ... also drop rows of cocoa waste (HS6 == "180200")
  # ... convert cocoa to CBE (cocoa bean equivalents)
  trade_df <- trade_df %>%
    rename(EXPORTER_ORIGINAL = {{ exporter_column }}) %>%
    mutate(
      EXPORTER_CLEAN = fn_clean_names(EXPORTER_ORIGINAL),
      EXPORTER_CLEAN = fn_standard_trader_names(EXPORTER_CLEAN)
    ) %>%
    left_join(cef_cocoa, by = "HS6") %>%
    mutate(BEAN_EQUIVALENT_VOLUME = EQ_FACTOR * {{ raw_volume_column }}) %>%
    dplyr::select(-EQ_FACTOR) %>%
    filter(!HS6 == "180200")
  return(trade_df)
}


geocode_to_trase_id <- function(series) {
  code <- series  # CI-2.1.2_1
  code <- gsub("\\.", "0", code)  # CI-20102_1
  code <- gsub("CI-", "", code)  # 20102
  code <- gsub("_1", "", code)  # 20102
  if(nchar(code)==5){
    code <- paste0("CI-0", code)
  } else if (nchar(code) == 6){
    code <- paste0("CI-", code)
  }
  code <- gsub("UNKNOWN", "CI-XXXXXX", code)
  return(code)
}


fn_fr_to_en_country_names <- function(col_name){
  case_when(
    col_name == "Arabie Saoudite" ~  "Saoudi Arabia",
    col_name == "Emirats Arabes Unis" ~  "United Arab Emirates",
    col_name == "Etats-Unis" ~  "United States",
    col_name == "Grece" ~  "Greece",
    col_name == "Inde" ~  "India",
    col_name == "Pays-bas" ~  "Netherlands",
    col_name == "Russie, Federation de" ~  "Russia",
    col_name == "Syrienne, Republique arabe" ~  "Syria",
    col_name == "Autre pays" ~  "Unknown",
    TRUE ~ col_name
  )
}

# aligns some typoed disclosed area names with department names in S3~ cote_divoire/spatial/BOUNDARIES/DEPARTEMENT/OUT/CIV_DEPARTEMENTS.geojson
fn_clean_department_names <- function(col_name){
  case_when(
    grepl("ABENGOUROU" , col_name) ~ "ABENGOUROU",
    grepl("AGNIBLEKRO" , col_name) ~ "AGNIBILEKRO",
    grepl("ABOISSO" , col_name) ~ "ABOISSO",
    grepl("AFFERY" , col_name) ~ "AKOUPE", # https://fr.wikipedia.org/wiki/Aff%C3%A9ry
    grepl("BANGOLO" , col_name) ~ "BANGOLO",
    grepl("BONDOUKOU" , col_name) ~ "BONDOUKOU",
    grepl("BOUAFLE" , col_name) ~ "BOUAFLE",
    grepl("DALOA" , col_name) ~ "DALOA",
    grepl("DIVO" , col_name) ~ "DIVO",
    grepl("DUEKOUE" , col_name) ~ "DUEKOUE",
    grepl("GAGNOA" , col_name) ~ "GAGNOA",
    grepl("GUIGLO" , col_name) ~ "GUIGLO",
    grepl("GRAND-LAHOU|GRAND LAHOU" , col_name) ~ "GRAND-LAHOU",
    grepl("GRAND-BASSAM|GRAND BASSAM" , col_name) ~ "GRAND-BASSAM",
    grepl("HERMAKONO|HERMANKONO" , col_name) ~ "DIVO", # https://fr.wikipedia.org/wiki/Hermankono-Di%C3%A8s
    grepl("LAKOTA" , col_name) ~ "LAKOTA",
    grepl("OPOUYO|OUPOUYO|OUPOYO|SOOUBRE" , col_name) ~ "SOUBRE",
    grepl("SEGUELLA|SEGUELA" , col_name) ~ "SEGUELA",
    grepl("SINFRA" , col_name) ~ "SINFRA",
    grepl("TAABO" , col_name) ~ "TAABO",
    grepl("TABOU" , col_name) ~ "TABOU",
    grepl("YAKASSE|ABONGOUA" , col_name) ~ "YAKASSE-ATTOBROU", # https://fr.wikipedia.org/wiki/Abongoua
    
    grepl("SAN-PEDO|SAN PEDRO|GABIADJI|GAGNY" , col_name) ~ "SAN-PEDRO",
    
    TRUE ~ col_name
      
  )
}
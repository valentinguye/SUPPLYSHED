library(tidyverse)
library(aws.s3)
library(sf)

cfarms <- s3read_using(
  object = "cote_divoire/cocoa/logistics/originals/CARGILL/2023-03-20-cargill_farms_CIV_2019-2020.geojson",
  FUN = read_sf,
  bucket = "trase-storage",
  opts = c("check_region" = T)
)

# just drop LOCATION_N variable, which has same info as KML_ATTRIB
data_sf <- select(data_sf, -LOCATION_N)

all.equal(df$Location_N, df$KML_Attrib)

df[df$Location_N!=df$KML_Attrib,] %>% nrow()

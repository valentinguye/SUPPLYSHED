
##### 0. PACKAGES, WD, OBJECTS #####


### PACKAGES ###
library(tidyverse)
library(aws.s3)
library(sf)
library(raster)
library(rnaturalearth)
library(readxl)
library(xlsx)
library(stringr)
library(DescTools)
library(here)
aws.signature::use_credentials()
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")


# This is probably the only thing to adapt (dependgin on needs)
ne_civ <- ne_countries(country = "Ivory Coast", returnclass  = "sf") 
ext <- extent(st_bbox(ne_civ))


# Download for these crops: maize, cassava, plantain, yam, cocoyam, rice, sorghum and millet

# Renaming layers with the following name mapping matrix
mapmat_data <- c("coc", "Cocoa")

# 3 of these are not in the data downloaded (but that does matter): Millet, Maizesilage, and Pasture
mapmat <- matrix(data = mapmat_data, 
                 nrow = length(mapmat_data)/2,
                 ncol = 2, 
                 byrow = TRUE)

colnames(mapmat) <- c("abrev", "Names")


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#### PREPARE SUITABILITY INDICES ####

dir.create(here("temp_data", "GAEZ_v4", "AES_index_value", "Rain-fed", "Low-input", "Cocoa"), recursive = TRUE) 

datadir <- here("input_data", "GAEZ_v4", "AES_index_value", "Rain-fed", "Low-input", "Cocoa")
targetdir <- here("temp_data", "GAEZ_v4", "AES_index_value", "Rain-fed", "Low-input", "Cocoa")
tmpdir <- here("temp_data", "tmp")

if (!dir.exists(tmpdir)) dir.create(tmpdir, recursive = TRUE)
if (dir.exists(targetdir)) {
  file.remove(list.files(path = targetdir,
                         pattern = ".tif", full.names = TRUE))
} else dir.create(targetdir, recursive = TRUE)



files <- list.files(path = datadir, pattern = ".tif")
crops <- unlist(strsplit(files, split = ".tif"))
for(crop in crops){
  crops[grepl(crop, crops)] <- mapmat[grepl(crop, paste0("sxLr_", mapmat[,"abrev"])),"Names"]
}



## Import most crops
for (j in 1:length(files)) {
  #if (any(crops[j] == cropsToAggregate)) next
  print(files[j])
  # unzip(zipfile = here(datadir, files[j]), exdir = tmpdir)
  dt <- raster(here(datadir, files[j]))
  
  # crop to COTE D'IVOIRE AOI
  dt_trop <- crop(dt, ext)
  
  # A few points are -0.09 with no apparent reason. 
  dt_trop[dt_trop<0] <- NA 
  
  names(dt_trop) <- crops[j]
  writeRaster(dt_trop,
              filename = here(targetdir, paste0(crops[j], ".tif")),
              overwrite = TRUE)
}
rm(dt, dt_trop)

rm(rasterlist_gaez, gaez_cocoa)

rm(ext, mapmat, mapmat_data)
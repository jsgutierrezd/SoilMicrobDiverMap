#===============================================================================
# Section: Covariates
# Group: Management
# Procedure: 05
# Description: Read Geopackage files with NLESS and field information from 
#         2017 to 2021 but with columns of interest such as, N
#         fertilization, Field ID, Farm ID, Crop name by field
#         in English and the amount of fertilizer in KgN/km2. Then rasterize
#         these some attributes into 10  resolution.
# Output: Raster files with the cedure
# Produced by Sebastian Gutierrez - Aarhus University
# May 2023
#===============================================================================


rm(list = ls())
Sys.setenv(language="EN")

# 1) Set working directory ------------------------------------------------

setwd("~/AARHUS_PhD/DSMactivities/2_Biodiversity/SoilMicrobDiverMap")


# 2) Load libraries -------------------------------------------------------
pckg <- c('terra',
          'tidyverse',
          'magrittr',
          'readxl')

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(pckg,usePackage)

# 3) Load Geopackage files ------------------------------------------------
files <- list.files("EnvironmentalLayers/Fertilizer/NLESdata_fields_sp/",
                    pattern = "shortened.gpkg$",
                    full.names = TRUE
)
files

fert1721sp <- lapply(files,function(x){
  vect(x)
})
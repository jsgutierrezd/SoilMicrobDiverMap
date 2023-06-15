#===============================================================================
# Section: Covariates
# Group: Management
# Procedure: 07
# Description: Read tif files about tillage erosion.
# Output: Tillage erosion raster file with the same spatial setting as the 
#         other layers.
# Produced by Sebastian Gutierrez - Aarhus University
# June 2023
#===============================================================================

gc()
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

# 3) Load raster files ----------------------------------------------------
ref <- rast("O:/Tech_AGRO/Jord/DSM/Covariates/national_10m/national_10m_stack_20230323/dhm2015_terraen_10m.tif")
tero <- rast("O:/Tech_AGRO/Jord/GHE/Watem_DK/ExerciseWatem2022DK/Output/RawOutputs/DK_TerodepNoFilter.tif") %>% 
  resample(ref)
plot(tero)

# END ---------------------------------------------------------------------



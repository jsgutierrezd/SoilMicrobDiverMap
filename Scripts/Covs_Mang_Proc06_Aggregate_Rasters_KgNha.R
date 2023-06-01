#===============================================================================
# Section: Covariates
# Group: Management
# Procedure: 06
# Description: Read tif files with the information of applid N in KgN/ha and
#         aggregate the five years into two layers, one with the mean and one 
#         with the standard deviation to see some variability.
# Output: Two raster files for each variables.
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
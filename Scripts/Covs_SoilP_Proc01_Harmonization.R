#===============================================================
# Section: Covariates
# Group: Soil properties
# Procedure: 01
# Description: Harmonization of soil properties-related 
#              covariates
# Output: Set of sil properties-related covariates harmonized
#         at 10 m resolution
# Produced by Sebastian Gutierrez - Aarhus University
# May 2023
#===============================================================

rm(list = ls())
Sys.setenv(language="EN")

# 1) Set working directory ------------------------------------------------

setwd("~/AARHUS_PhD/DSMactivities/2_Biodiversity/SoilMicrobDiverMap")


# 2) Load libraries -------------------------------------------------------
pckg <- c('terra',
          'tidyverse',
          'magrittr')

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(pckg,usePackage)


# 3) Load texture maps from Anders ----------------------------------------

files <- list.files("O:/Tech_AGRO/Projekt/ProjJORD/DigiJord/maps_test11/",
                    pattern = "tif$",
                    full.names = TRUE
)
files

octext <- rast(files)
octext <- octext[[c(2:4,6,7)]]
names(octext) <- c("Clay","Coarse_sand","Fine_sand","Silt","OC")


# 4) BD and pH 30m --------------------------------------------------------
bd30 <- rast("C:/Users/au704633/OneDrive - Aarhus Universitet/Documents/AARHUS_PhD/DSMactivities/1_SOCseq/INPUTS/RASTER/GSOCMap/BD/BDKabAdh_wetlands.tif") %>% 
  resample(octext,method="bilinear") %>% round(digits = 2)


files <- list.files("O:/Tech_Agro-data1/Geodata/Denmark_national/Natural_ressources/Soil_geology/Texture3D_2014/Ph/",
                    pattern = "tif$",
                    full.names = TRUE
)
files

pH <- rast(files[c(2,8,5)])
pH <- 

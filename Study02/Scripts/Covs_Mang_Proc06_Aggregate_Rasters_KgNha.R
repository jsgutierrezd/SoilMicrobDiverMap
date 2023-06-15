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

# 3) Load raster files ----------------------------------------------------

filenames <- c("commer_fert","livestock_man","other_org_fert","cattle_slur","pig_slur","mink_slur",
               "soild_livest_man","liquid_livest_man","deep_litter","degassed_biom","sewage_sludge")
# i=1
layers <- list()
for (i in 1:length(filenames)) {
  files <- list.files("O:/Tech_AGRO/Jord/Sebastian/SoilMicrobialDiversity/Layers10m/Nfert/",
                      pattern = paste0(filenames[i]),
                      full.names = TRUE
  )
  files <- rast(files)
  layers[[i]] <- files 
  
}

layers

# 4) Aggregate variables using mean and sd --------------------------------

# Multiannual average
meanlayers <- rast()
# i=1
for (i in 1:length(filenames)) {
  tmp <- mean(layers[[i]]) %>% round(digits = 2)
  meanlayers <- c(meanlayers,tmp)
  }
names(meanlayers) <- paste0(filenames,"_mean")
writeRaster(meanlayers,filename = paste0("O:/Tech_AGRO/Jord/Sebastian/SoilMicrobialDiversity/Layers10m/Nfert/AggregatedLayers/",
                                     names(meanlayers),".tif"),overwrite=T)

# Multiannual standard deviation
sdlayers <- rast()
for (i in 1:length(filenames)) {
  tmp <- stdev(layers[[i]]) %>% round(digits = 2)
  sdlayers <- c(sdlayers,tmp)
}
names(sdlayers) <- paste0(filenames,"_sd")
writeRaster(sdlayers,filename = paste0("O:/Tech_AGRO/Jord/Sebastian/SoilMicrobialDiversity/Layers10m/Nfert/AggregatedLayers/",
                                         names(sdlayers),".tif"),overwrite=T)

# END ---------------------------------------------------------------------



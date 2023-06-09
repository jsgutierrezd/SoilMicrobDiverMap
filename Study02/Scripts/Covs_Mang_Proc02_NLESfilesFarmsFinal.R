#===============================================================
# Section: Covariates
# Group: Management
# Procedure: 02
# Description: Read Geopackage files from 2017 to 2021, select 
#              columns of interest (those columns related to 
#              fertilizer application)
# Output: Geopackage files with NLESS and field information from 
#         2017 to 2021 but with columns of interest such as, N
#         fertilization, Field ID, Farm ID, Crop name by field
#         in English
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
                    pattern = "gpkg$",
                    full.names = TRUE
)
files

fert1721 <- lapply(files,function(x){
  vect(x)
})


# 4) Select columns of interest, area calculation and crop name -----------
selnames <-  readRDS("EnvironmentalLayers/Fertilizer/NLESdata_fields_sp/SelectedAttributes.rds")
crops <- read_excel("EnvironmentalLayers/Fertilizer/NLESdata/AFGROEDETABEL2021.xlsx") %>% 
  select(AFGROEDEKODE,ENGELSKNAVN)
names(crops) <- c("Afgkode","Crop")

years <- c(2017:2021)
# i=1
for (i in 1:length(years)) {
  fert1721[[i]]<- fert1721[[i]][,selnames]
  fert1721[[i]]$areakm2 <- expanse(fert1721[[i]])/1000000
  fert1721[[i]] <- terra::merge(fert1721[[i]],
                                crops,
                                all.x=T,
                                by.x="Afgkode",
                                by.y="Afgkode")
  writeVector(fert1721[[i]],filename = paste0(getwd(),
                                         "/EnvironmentalLayers/Fertilizer/NLESdata_fields_sp/",
                                         "NLES",years[i],"_w_fields_shortened",".gpkg"),
              overwrite=T)
  }

# names(fert1721[[1]])

# END ---------------------------------------------------------------------











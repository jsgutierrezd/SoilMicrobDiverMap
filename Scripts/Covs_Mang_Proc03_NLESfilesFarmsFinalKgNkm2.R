#===============================================================================
# Section: Covariates
# Group: Management
# Procedure: 03
# Description: Read Geopackage files with NLESS and field information from 
#         2017 to 2021 but with columns of interest such as, N
#         fertilization, Field ID, Farm ID, Crop name by field
#         in English. Then calculate the amount of applied N by unit of area
#         (km2, ha, m2,...etc)
# Output: CSV files with the calculation according to the procedure
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
                    pattern = "shortened.*\\.gpkg$",
                    full.names = TRUE
)
files

fert1721 <- lapply(files,function(x){
  vect(x) %>% as.data.frame() # Converting to a list of data frames
})

farmarea <- list()
years <- c(2017:2021)

for (i in 1:length(years)) {
  fert1721[[i]]$VMP3_ID <- as.factor(fert1721[[i]]$VMP3_ID)
  farmarea[[i]]<- aggregate(x=fert1721[[i]]$areakm2,
                         by=list(fert1721[[i]]$VMP3_ID),
                         FUN=sum,na.rm=T)
  names(farmarea[[i]]) <- c("VMP3_ID","areakm2byfarm")
  farmarea[[i]] <- subset(farmarea[[i]],VMP3_ID!=0)
  fert1721[[i]] <- dplyr::left_join(fert1721[[i]],
                         farmarea[[i]],
                         by="VMP3_ID")
  
  fert1721[[i]]$KgN_km2_HandelsIalt <- fert1721[[i]]$KgN_HandelsIalt/
    fert1721[[i]]$areakm2byfarm
  
  fert1721[[i]]$KgN_km2_HusDyrIalt <- fert1721[[i]]$KgN_HusDyrIalt/
    fert1721[[i]]$areakm2byfarm
  
  fert1721[[i]]$KgN_km2_AndenOrgIalt <- fert1721[[i]]$KgN_AndenOrgIalt/
    fert1721[[i]]$areakm2byfarm
  
  fert1721[[i]]$KgN_km2_HU_Kvaeggylle <- fert1721[[i]]$KgN_HU_Kvaeggylle/
    fert1721[[i]]$areakm2byfarm
  
  fert1721[[i]]$KgN_km2_HU_Svinegylle <- fert1721[[i]]$KgN_HU_Svinegylle/
    fert1721[[i]]$areakm2byfarm
  
  fert1721[[i]]$KgN_km2_HU_Minkgylle <- fert1721[[i]]$KgN_HU_Minkgylle/
    fert1721[[i]]$areakm2byfarm
  
  fert1721[[i]]$KgN_km2_HU_FastGoedning <- fert1721[[i]]$KgN_HU_FastGoedning/
    fert1721[[i]]$areakm2byfarm
  
  fert1721[[i]]$KgN_km2_HU_Ajle <- fert1721[[i]]$KgN_HU_Ajle/
    fert1721[[i]]$areakm2byfarm
  
  fert1721[[i]]$KgN_km2_HU_DybStr <- fert1721[[i]]$KgN_HU_DybStr/
    fert1721[[i]]$areakm2byfarm
  
  fert1721[[i]]$KgN_km2_HU_AfgBiomasse <- fert1721[[i]]$KgN_HU_AfgBiomasse/
    fert1721[[i]]$areakm2byfarm
  
  fert1721[[i]]$KgN_km2_AO_SlamRensingsanlaeg <- fert1721[[i]]$KgN_AO_SlamRensingsanlaeg/
    fert1721[[i]]$areakm2byfarm
  
  names(fert1721[[i]])
  readr::write_csv(fert1721[[i]],paste0(getwd(),
                                              "/EnvironmentalLayers/Fertilizer/NLESdata_fields_KgNkm2/",
                                              "NLES",years[i],"_w_fields_KgNkm2",".csv"))
}


# END ---------------------------------------------------------------------




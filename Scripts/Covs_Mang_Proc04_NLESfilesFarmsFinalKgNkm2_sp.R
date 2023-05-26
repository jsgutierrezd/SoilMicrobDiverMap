#===============================================================================
# Section: Covariates
# Group: Management
# Procedure: 04
# Description: Read CSV with the calculation according to the procedure 03
#         and read the Geopackage files with NLESS and field information from 
#         2017 to 2021 but with columns of interest such as, N
#         fertilization, Field ID, Farm ID, Crop name by field
#         in English. Then merge together these two files into one Geopackage
#         file that will be rasterized afterwards.
# Output: Geopackage files with the calculation according to the procedure
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


# 4) Load CSV files -------------------------------------------------------

files <- list.files("EnvironmentalLayers/Fertilizer/NLESdata_fields_KgNkm2/",
                    pattern = ".csv$",
                    full.names = TRUE
)
files

fert1721 <- lapply(files,function(x){
  readr::read_csv(x) %>%
    select(IMK_ID,areakm2byfarm:KgN_km2_AO_SlamRensingsanlaeg) 
})
names(fert1721[[1]])


years <- c(2017:2021)
# i=1
for (i in 1:length(years)) {
  fert1721sp[[i]] <- terra::merge(fert1721sp[[i]],
                                fert1721[[i]],
                                all.x=T,
                                by.x="IMK_ID",
                                by.y="IMK_ID")
  writeVector(fert1721sp[[i]],filename = paste0(getwd(),
                                              "/EnvironmentalLayers/Fertilizer/NLESdata_fields_sp/",
                                              "NLES",years[i],"_w_fields_shortened_KgNkm2",".gpkg"),
              overwrite=T)
}


# END ---------------------------------------------------------------------



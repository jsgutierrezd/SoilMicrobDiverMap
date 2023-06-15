#===============================================================
# Section: Covariates
# Group: Management
# Procedure: 01
# Description: Read NLES files from 2017 to 2021 and join them
#              for field maps from the same years by IMK_ID
# Output: Geopackage files with NLESS and field information from 
#         2017 to 2021
# Produced by Lars Uldall-Jessen - Aarhus University
# Modified by Sebastian Gutierrez - Aarhus University
# May 2023
#===============================================================

rm(list = ls())
Sys.setenv(language="EN")

# 1) Set working directory ------------------------------------------------

setwd("~/AARHUS_PhD/DSMactivities/2_Biodiversity/SoilMicrobDiverMap")


# 2) Load libraries -------------------------------------------------------
pckg <- c('sf',
          'tidyverse')

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(pckg,usePackage)

# 3) NLES files from 2017-2021 --------------------------------------------

# NLES 2017 ---------------------------------------------------------------

NLES2017 <- read.table("EnvironmentalLayers/Fertilizer/NLESdata/NLES2017.csv", 
                       sep=";", 
                       header=T)
names(NLES2017)[names(NLES2017)=="imk_id"] <- 
  toupper(names(NLES2017)
          [names(NLES2017)=="imk_id"])

Marker_2017 <- st_read("O:/Tech_Agro-data1/Geodata/Denmark_national/Agriculture/DMKFields/Mark_2017_final_imk.shp")
Marker2017_NLES <- Marker_2017 %>% 
  left_join(NLES2017, 
            by=c("IMK_ID"))

st_write(Marker2017_NLES, 
         "EnvironmentalLayers/Fertilizer/NLESdata_fields_sp/NLES2017_w_fields.gpkg",
         append=FALSE)

# NLES 2018 ---------------------------------------------------------------

NLES2018 <- read.table("EnvironmentalLayers/Fertilizer/NLESdata/NLES2018.csv",
                       sep=";",
                       header=T)
names(NLES2018)[names(NLES2018)=="imk_id"] <-
  toupper(names(NLES2018)
          [names(NLES2018)=="imk_id"])

Marker_2018 <- st_read("O:/Tech_Agro-data1/Geodata/Denmark_national/Agriculture/DMKFields/Marker_2018_imk.shp")
Marker2018_NLES <- Marker_2018 %>%
  left_join(NLES2018,
            by=c("IMK_ID"))

st_write(Marker2018_NLES,
         "EnvironmentalLayers/Fertilizer/NLESdata_fields_sp/NLES2018_w_fields.gpkg",
         append=FALSE)

# NLES 2019 ---------------------------------------------------------------

NLES2019 <- read.table("EnvironmentalLayers/Fertilizer/NLESdata/NLES2019.csv", 
                       sep=";", 
                       header=T)
names(NLES2019)[names(NLES2019)=="imk_id"] <- 
  toupper(names(NLES2019)
          [names(NLES2019)=="imk_id"])

Marker_2019 <- st_read("O:/Tech_Agro-data1/Geodata/Denmark_national/Agriculture/DMKFields/Marker_2019_slut.shp")
Marker2019_NLES <- Marker_2019 %>% 
  left_join(NLES2019, 
            by=c("IMK_ID"))

st_write(Marker2019_NLES, 
         "EnvironmentalLayers/Fertilizer/NLESdata_fields_sp/NLES2019_w_fields.gpkg",
         append=FALSE)

# NLES 2020 ---------------------------------------------------------------

NLES2020 <- read.table("EnvironmentalLayers/Fertilizer/NLESdata/NLES2020.csv", 
                       sep=";", 
                       header=T)
names(NLES2020)[names(NLES2020)=="imk_id"] <- 
  toupper(names(NLES2020)
          [names(NLES2020)=="imk_id"])

Marker_2020 <- st_read("O:/AUIT_Geodata/Denmark/Agriculture/Fields/Marker_2020_slut.shp")
Marker2020_NLES <- Marker_2020 %>% 
  left_join(NLES2020, 
            by=c("IMK_ID"))

st_write(Marker2020_NLES, 
         "EnvironmentalLayers/Fertilizer/NLESdata_fields_sp/NLES2020_w_fields.gpkg",
         append=FALSE)

# NLES 2021 ---------------------------------------------------------------

NLES2021 <- read.table("EnvironmentalLayers/Fertilizer/NLESdata/NLES2021.csv", 
                       sep=";", 
                       header=T)
names(NLES2021)[names(NLES2021)=="imk_id"] <- 
  toupper(names(NLES2021)
          [names(NLES2021)=="imk_id"])

Marker_2021 <- st_read("O:/Tech_Agro-data1/Geodata/Denmark_national/Agriculture/DMKFields/Marker_2021_slut.shp")
Marker2021_NLES <- Marker_2021 %>% 
  left_join(NLES2021, 
            by=c("IMK_ID"))

st_write(Marker2021_NLES, 
         "EnvironmentalLayers/Fertilizer/NLESdata_fields_sp/NLES2021_w_fields.gpkg",
         append=FALSE)


# END ---------------------------------------------------------------------
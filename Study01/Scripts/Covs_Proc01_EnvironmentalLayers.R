#===============================================================
# Section: Covariates
# Group: NA-Not applicable
# Procedure: 01
# Description: Read the Excel file named "Covariates_list.xlsx",
#              which contains the names and description of the 
#              covariates that are available for the soil
#              microbial diversity mapping process. Then the 
#              total dataset of covariates is filtered based on
#              the list.
# Output: The initial dataset of covariates that we are using to 
#         map the target variables.
# Modified by Sebastian Gutierrez - Aarhus University
# June 2023
#===============================================================

rm(list = ls())
Sys.setenv(language="EN")

# 1) Set working directory ------------------------------------------------

setwd("~/AARHUS_PhD/DSMactivities/2_Biodiversity/SoilMicrobDiverMap")


# 2) Load libraries -------------------------------------------------------
pckg <- c('terra',
          'readr',
          'tools')

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(pckg,usePackage)

# 3) Load Excel table -----------------------------------------------------

envlyr <- read_csv("Study01/Docs/Covariates_list.csv")
names(envlyr)

# 4) Load covariates 10 m-resolution --------------------------------------

files <- list.files("O:/Tech_AGRO/Jord/DSM/Covariates/national_10m/national_10m_stack_20230323/",
                    pattern = ".tif$",
                    full.names = TRUE
)
files
names.total <- file_path_sans_ext(
  basename(files)
)
names.total

# 5) Load only the covariates described in the Excel file -----------------

layers <- rast(files[names.total%in%envlyr$name])
names(layers) # 137 raster layers


# 6) Load additional covariates 10 m--resolution --------------------------

#Soil properties
files <- list.files("O:/Tech_AGRO/Projekt/ProjJORD/DigiJord/maps_test11/",
                    pattern = "tif$",
                    full.names = TRUE
)
files

octext <- rast(files)
octext <- octext[[c(2:4,6,7)]]
names(octext) <- c("Clay","Coarse_sand","Fine_sand","Silt","OC")

bd <- rast("O:/Tech_AGRO/Jord/Sebastian/SoilMicrobialDiversity/Layers10m/bd.tif")

pH <- rast("O:/Tech_AGRO/Jord/Sebastian/SoilMicrobialDiversity/Layers10m/pH.tif")

soil <- c(octext,bd,pH)

layers <- c(layers,soil)
saveRDS(names(layers),"Study01/Docs/NamesEnvLayers.rds")

# END ---------------------------------------------------------------------



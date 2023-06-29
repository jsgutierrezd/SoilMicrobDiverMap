#===============================================================
# Step: 01-Data preparation
# Procedure: 01-Environmental layers and regression matrix
# Description: Read the Excel file named "Covariates_list.xlsx",
#              which contains the names and description of the 
#              covariates that are available for the soil
#              microbial diversity mapping process. Then the 
#              total dataset of covariates is filtered based on
#              the list.
#              The environmental layers corresponding to basic 
#              soil properties are also loaded.
#              With this initial dataset of environmental layers
#              we made the multipoint extraction and the result is
#              the regression matrix.
# Output: The initial dataset of covariates that we are using to 
#         map the target variables and the regression matrix.
# Modified by Sebastian Gutierrez - Aarhus University
# June 2023
#===============================================================
gc()
rm(list = ls())
Sys.setenv(language="EN")

# 1) Set working directory ------------------------------------------------

setwd("~/AARHUS_PhD/DSMactivities/2_Biodiversity/SoilMicrobDiverMap")


# 2) Load libraries -------------------------------------------------------
pckg <- c('terra',
          'readr',
          'tools',
          'magrittr',
          'readxl',
          'tidyverse')

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
names(layers) # 103 raster layers


# 6) Load additional covariates 10 m--resolution --------------------------

#Soil properties
files <- list.files("O:/Tech_AGRO/Projekt/ProjJORD/DigiJord/maps_test11/",
                    pattern = "tif$",
                    full.names = TRUE
)
files

octext <- rast(files)
octext <- octext[[c(2,7)]]
names(octext) <- c("Clay","OC")

bd <- rast("O:/Tech_AGRO/Jord/Sebastian/SoilMicrobialDiversity/Layers10m/bd.tif")

pH <- rast("O:/Tech_AGRO/Jord/Sebastian/SoilMicrobialDiversity/Layers10m/pH.tif")

soil <- c(octext,bd,pH)

layers <- c(soil,layers) # 107 raster layers

#N fertilization and sewage sludge layers (commercial, manure,sewage sludge)

files <- list.files("O:/Tech_AGRO/Jord/Sebastian/SoilMicrobialDiversity/Layers10m/Nfert/AggregatedLayers/",
                    pattern = "tif$",
                    full.names = TRUE
)
files
nfert <- rast(files[c(3,4,11,12,19,20)])

#to fill NA values with zero
ref <- soil$Clay/soil$Clay 
nfert[is.na(nfert)] <- 0

nfert <- nfert*ref

layers <- c(soil,layers,nfert) 
names(layers)
saveRDS(names(layers),"Study01/Docs/NamesEnvLayers.rds")

# 7) Load the point dataset

data <- read_excel("C:/Users/au704633/OneDrive - Aarhus Universitet/Documents/AARHUS_PhD/DSMactivities/2_Biodiversity/Metadata/test_mapping_noduplicates_Metadata_LandUse.xlsx") %>% 
  filter(mfd_areat0==YearWise_LandUse) %>% 
  filter(mfd_areat0=="Agriculture"|mfd_areat0=="Natural") %>% 
  select(fieldsampl:longitude)

data_sp <- vect(data,geom=c("longitude", "latitude"), crs="epsg:4326",keepgeom=T) %>% 
  project("epsg:25832")

writeVector(data_sp,"C:/Users/au704633/OneDrive - Aarhus Universitet/Documents/AARHUS_PhD/DSMactivities/2_Biodiversity/Metadata/test_mapping_noduplicates_Metadata_LandUse.shp")
# 8) Multi-point extraction -----------------------------------------------
start <- Sys.time()
data <- cbind(data,terra::extract(layers,data_sp)) %>% na.omit
Sys.time()-start
colSums(is.na(data))
data$ID <- NULL
saveRDS(names(data)[8:120],"Study01/Docs/NamesPredsTotal.rds")
write_csv(data,"C:/Users/au704633/OneDrive - Aarhus Universitet/Documents/AARHUS_PhD/DSMactivities/2_Biodiversity/Metadata/RegMatStudy1.csv")

# write_csv(na.omit(data),"C:/Users/au704633/OneDrive - Aarhus Universitet/Documents/AARHUS_PhD/DSMactivities/2_Biodiversity/Metadata/RegMatStudy1borrar.csv")

# END ---------------------------------------------------------------------






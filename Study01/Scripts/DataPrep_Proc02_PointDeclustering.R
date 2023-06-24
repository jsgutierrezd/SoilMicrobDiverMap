#===============================================================
# Step: 01-Data preparation
# Procedure: 02-Point data declustering 
# Description: Read the dataset previously filtered using 
#              univariate and multivariate methods and
#              decluster the points.
# Output: The indices of each point after declustering.
# Modified by Sebastian Gutierrez - Aarhus University
# June 2023
#===============================================================

gc()
rm(list = ls())
Sys.setenv(language="EN")
# 1) Set working directory
setwd("~/AARHUS_PhD/DSMactivities/2_Biodiversity/SoilMicrobDiverMap")

# 2) Load libraries

pckg <- c('raster',
          'rgdal',
          'sp',
          'magrittr',
          'readr'
)

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(pckg,usePackage)

source("Study01/Scripts/Functions/decluster.R")


# 3) Dataset --------------------------------------------------------------
data <- read_csv("C:/Users/au704633/OneDrive - Aarhus Universitet/Documents/AARHUS_PhD/DSMactivities/2_Biodiversity/Metadata/RegMatStudy1Filtered.csv") %>% 
  data.frame()
coordinates(data) <- ~longitude+latitude
proj4string(data) <- CRS('+init=epsg:4326')
data <- spTransform(data,CRS("+init=epsg:25832"))  
summary(data)
names(data)

# 4) Declustering ---------------------------------------------------------
start <- Sys.time()
ws <- decluster(pts = data, gridspace = c(6000,6000), nshifts = 3, cores = 18, savegrids = TRUE)
ws
saveRDS(ws,"Study01/Docs/weightsdeculster.rds")
print(Sys.time() - start)


# END ---------------------------------------------------------------------



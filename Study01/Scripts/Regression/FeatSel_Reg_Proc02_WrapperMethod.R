#===============================================================
# Step: 02-Feature selection
# Problem: Regression
# Procedure: 02-Wrapper method 
# Description: Read the dataset previously filtered using 
#              univariate methods and use wrapper (multivariate)
#              methods to find the optimal combination of 
#              environmental layers to maximize the model 
#              performance of the target variables
# Output: The final dataset of predictors that we are using to 
#         model the target variables.
# Modified by Sebastian Gutierrez - Aarhus University
# June 2023
#===============================================================

rm(list = ls())
Sys.setenv(language="EN")

# 1) Set working directory ------------------------------------------------

setwd("~/AARHUS_PhD/DSMactivities/2_Biodiversity/SoilMicrobDiverMap")


# 2) Load libraries -------------------------------------------------------
pckg <- c('magrittr',
          'readr',
          'caret',
          'parallel',
          'doParallel',
          'Boruta')

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(pckg,usePackage)

ws <- readRDS("Study01/Docs/weightsdeculster.rds")
source("Study01/Scripts/Functions/weightedvalidation.R")

# 3) Load Excel table -----------------------------------------------------
data <- read_csv("C:/Users/au704633/OneDrive - Aarhus Universitet/Documents/AARHUS_PhD/DSMactivities/2_Biodiversity/Metadata/RegMatStudy1Filtered.csv") %>% 
  na.omit() %>% as.data.frame
names(data)


# 4) Boruta ---------------------------------------------------------------
# data$Nitrospira <- ifelse(data$Nitrospira==0,0,1) %>% as.factor()
# table(data$Nitrospira)
names(data) # Removing IMK and OGC
boruta <- Boruta(x = data[,c(8:27,32:35,39:75)],
               y = data[,5],
               doTrace = 0,
               # ntree = 500,
               maxRuns=500,
               # weight=ws$w,
              getImp=getImpXgboost
              )
boruta <- TentativeRoughFix(boruta)
boruta
(namesbor <- names(boruta$finalDecision[boruta$finalDecision %in% c("Confirmed")]))
saveRDS(namesbor,"Study01/Docs/NamesPredsRegressionBorutaNoWeights.rds")


# END ---------------------------------------------------------------------








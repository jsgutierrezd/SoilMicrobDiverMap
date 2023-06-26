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


# 4) Recursive feature elimination ----------------------------------------

cl <- makeCluster(detectCores()-2, type='PSOCK')
registerDoParallel(cl)

control <- rfeControl(functions=caretFuncs, 
                      method="repeatedcv", 
                      number=5,
                      repeats=5,
                      allowParallel = T
                      # saveDetails = T
                      )
start <- Sys.time()
rfe <- rfe(x=data[,8:68],
               y=data[,5], 
               method = "rf",
               sizes=c(1:30),
               case.weights=ws$w,
               rfeControl=control)
print(Sys.time() - start)
plot(rfe, type=c("g", "o"))
predictors(rfe)
stopCluster(cl=cl)
namesRFE <- predictors(rfe)
saveRDS(namesRFE,"Study01/Docs/NamesPredsRegressionRFE.rds")


# 5) Boruta ---------------------------------------------------------------

boruta <- Boruta(x = data[,8:68],
               y = data[,5],
               doTrace = 0,
               ntree = 500,
               maxRuns=500,
              getImp=getImpXgboost,
              weight=ws$w)
boruta <- TentativeRoughFix(boruta)
boruta
namesbor <- names(boruta$finalDecision[boruta$finalDecision %in% c("Confirmed")])
saveRDS(namesbor,"Study01/Docs/NamesPredsRegressionBoruta.rds")


# END ---------------------------------------------------------------------








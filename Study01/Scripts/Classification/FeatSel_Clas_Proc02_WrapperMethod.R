#===============================================================
# Step: 02-Feature selection
# Problem: Classification
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
          'doParallel')

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


### Seleccion de variables --> algoritmo RFE
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)

control <- rfeControl(functions=rfFuncs, 
                      method="repeatedcv", 
                      number=5, 
                      repeats=5,
                      allowParallel = T,
                      saveDetails = T)

rfmodel <- rfe(x=data[,8:68], 
               y=data[,5], 
               sizes=c(1:20), 
               rfeControl=control,
               weights=ws$w
)
plot(rfmodel, type=c("g", "o"))
predictors(rfmodel)[1:9]

length(ws$w)


{start <- Sys.time()
  print(Sys.time() - start)}






data.num <- readRDS("Study01/Docs/NamesNumEnvLayers.rds")
x <- data[,data.num] %>% as.data.frame
y <- data$shannon.di


normalization <- preProcess(x)
x <- predict(normalization, x)
x <- as.data.frame(x) %>% na.omit

x
y


ga_ctrl <- gafsControl(functions = rfGA,
                       method = "repeatedcv",
                       repeats = 5,allowParallel = T)
start <- Sys.time()
set.seed(10)
rf_ga <- gafs(x = x, y = y,
              iters = 5,
              gafsControl = ga_ctrl)
rf_ga
Sys.time()-start

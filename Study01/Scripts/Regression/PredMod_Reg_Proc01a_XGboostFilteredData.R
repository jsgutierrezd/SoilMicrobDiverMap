#===============================================================
# Step: 03-Predictive modeling
# Problem: Regression
# Procedure: 01_XGBoost algorithm 
# Especification: a) Filtered dataset
# Description: Read the dataset previously with the final candidates to be
#              predictors of Shannon diversity index and fit the predictive
#              model using the XGBoost algorithm
# Output: The model to predict the Shannon diversity index with
#         the retained variables
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
source("Study01/Scripts/Functions/goof.R")
predsBor <- readRDS("Study01/Docs/NamesPredsRegressionBorutaNoWeights.rds")
predsBorW <- readRDS("Study01/Docs/NamesPredsRegressionBorutaWeights.rds")


# 3) Load Excel table -----------------------------------------------------
data <- read_csv("C:/Users/au704633/OneDrive - Aarhus Universitet/Documents/AARHUS_PhD/DSMactivities/2_Biodiversity/Metadata/RegMatStudy1Filtered.csv") %>% 
  na.omit() %>% as.data.frame
names(data)
hist(log(data$shannon.di))



# 4) Option 1: Data splitting 70-30 ---------------------------------------

set.seed(16)
inTrain <- createDataPartition(y = data$shannon.di, p = .70, list = FALSE) 
train_data <- data[ inTrain,]
dim(train_data)
test_data <- data[-inTrain,]
dim(test_data)
# Model fitting -----------------------------------------------------------

names(data)
fm <- as.formula(paste("shannon.di  ~", paste0(predsBor,
                                               collapse = "+")))
fm

THREADS = parallel::detectCores()-2
rctrlG <- trainControl(method="cv",
                       allowParallel = THREADS,
                       savePredictions = 'final',
                       # summaryFunction = WeightedSummary
                       
)

grid <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  max_depth = 2,
  eta = seq(from = 0.03, to = 0.05, by = 0.010),
  gamma = c(0, 0.1,0.2,0.3),
  colsample_bytree = 0.30,
  min_child_weight = 2,
  subsample = 1
)

set.seed(1039)
model_xgb <- caret::train(fm,
                          data=train_data,
                          method = "xgbTree",
                          trControl = rctrlG,
                          tuneGrid = grid,
                          metric = 'RMSE',
                          maximize = FALSE,
                          na.action = na.pass,
                          # objective ='reg:tweedie',
                          objective ='reg:squarederror'
)
model_xgb$finalModel$tuneValue

test_pred <- predict(model_xgb,test_data[,predsBor])
train_pred <- predict(model_xgb,train_data[,predsBor])
goof(train_pred,train_data$shannon.di)
goof(test_pred,test_data$shannon.di)


hist(test_pred)
hist(train_pred)
hist(data$shannon.di)




plot(test_pred,test_data$shannon.di)


plot(varImp(model_xgb),top=length(predsBor))
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(min(x$results$RMSEw),quantile(x$results$RMSEw, probs = probs))) +
    theme_bw()
}
tuneplot(model_xgb)
# https://www.kaggle.com/code/pelkoja/visual-xgboost-tuning-with-caret 

# saveRDS(model_xgb,"AAUAgriculturalLands/Outputs/Models/ModelShannonAgr26042023.rds")
# shap_values <- xgb.plot.shap(data = as.matrix(data[,BorPC2]), model = model_xgb$finalModel, top_n = 5, plot = F)



# 5) Option 2: Cross-validation with weights ------------------------------

# Model fitting -----------------------------------------------------------

names(data)
fm <- as.formula(paste("shannon.di  ~", paste0(predsBorW,
                                               collapse = "+")))
fm

THREADS = parallel::detectCores()-2
rctrlG <- trainControl(method="cv",
                       allowParallel = THREADS,
                       savePredictions = 'final',
                       summaryFunction = WeightedSummary
                       
)

grid <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  max_depth = 2,
  eta = seq(from = 0.03, to = 0.05, by = 0.010),
  gamma = c(0, 0.1,0.2,0.3),
  colsample_bytree = 0.30,
  min_child_weight = 2,
  subsample = 1
)

set.seed(1039)
model_xgb <- caret::train(fm,
                          data=data,
                          method = "xgbTree",
                          trControl = rctrlG,
                          tuneGrid = grid,
                          metric = 'RMSEw',
                          maximize = FALSE,
                          weights=ws$w,
                          na.action = na.pass,
                          # objective ='reg:tweedie',
                          objective ='reg:squarederror'
)
model_xgb$finalModel$tuneValue

# Validation measures considering weights
# Weighted RMSE
RMSEw <- function(d, w)
{
  sqe <- w*(d[, 1] - d[, 2])^2
  msqe <- sum(sqe)/sum(w)
  out <- sqrt(msqe)
  return(out)
}


RMSEw(d=data.frame((model_xgb$pred$pred),(model_xgb$pred$obs)),w=ws$w)
# Weighted R^2
R2w <- function(d, w)
{
  require(boot)
  out <- boot::corr(d[, 1:2], w)^2
  return(out)
}

R2w(d=data.frame((model_xgb$pred$pred),(model_xgb$pred$obs)),w=ws$w)

MECw <- function(d, w)
{
  obs <- sum(w*(d[, 1] - mean(d[, 1]))^2)
  pred.obs <- sum(w*(d[, 2] - d[, 1])^2)
  out <- (obs-pred.obs)/obs
  return(out)
}

MECw(d=data.frame((model_xgb$pred$obs),(model_xgb$pred$pred)),w=ws$w)

plot(varImp(model_xgb),top=length(predsBor))
goof(model_xgb$pred$pred,model_xgb$pred$obs)

hist(model_xgb$pred$pred)
hist(model_xgb$pred$obs)
plot(model_xgb$pred$obs,model_xgb$pred$pred)

tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(min(x$results$RMSEw),quantile(x$results$RMSEw, probs = probs))) +
    theme_bw()
}
tuneplot(model_xgb)
# https://www.kaggle.com/code/pelkoja/visual-xgboost-tuning-with-caret 

# saveRDS(model_xgb,"AAUAgriculturalLands/Outputs/Models/ModelShannonAgr26042023.rds")
# shap_values <- xgb.plot.shap(data = as.matrix(data[,BorPC2]), model = model_xgb$finalModel, top_n = 5, plot = F)



install.packages("ParBayesianOptimization")
library(ParBayesianOptimization)






library("xgboost")
library("ParBayesianOptimization")

ws$grids
Folds <- list(
  Fold1 = as.integer(seq(1,nrow(data),by = 3))
  , Fold2 = as.integer(seq(2,nrow(data),by = 3))
  , Fold3 = as.integer(seq(3,nrow(data),by = 3))
)

dtrain <- data.frame(data$shannon.di,data[,predsBor])

scoringFunction <- function(max_depth, min_child_weight, subsample) {
  
  dtrain <- xgb.DMatrix(agaricus.train$data,label = agaricus.train$label)
  
  Pars <- list( 
    booster = "gbtree"
    , eta = 0.02
    , max_depth = max_depth
    , min_child_weight = min_child_weight
    , subsample = subsample
    , objective = "reg:squarederror"
    , eval_metric = "rmse"
  )
  
  xgbcv <- xgb.cv(
    params = Pars
    , data = dtrain
    , nround = 500
    , folds = 5
    , prediction = TRUE
    , showsd = TRUE
    , early_stopping_rounds = 5
    , maximize = TRUE
    , verbose = 0)
  
  return(
    list( 
      Score = max(xgbcv$evaluation_log$test_rmse_mean)
      , nrounds = xgbcv$best_iteration
    )
  )
}


bounds <- list( 
  max_depth = c(2L,5L)
    , min_child_weight = c(1,10)
  , subsample = c(0.25, 1)
)


set.seed(1234)
optObj <- bayesOpt(
  FUN = scoringFunction
  , bounds = bounds
  , initPoints = 4
  , iters.n = 3
)
optObj$scoreSummary

getBestPars(optObj)

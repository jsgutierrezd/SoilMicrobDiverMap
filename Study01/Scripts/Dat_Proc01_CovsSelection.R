#===============================================================
# Section: Point dataset of soil microbial diversity
# Group: Covariates selection
# Procedure: 01
# Description: Read the dataset of soil microbial diversity with
#              the coordinates and the target variables and make the extraction.
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


# 3) Load regression matrix -----------------------------------------------

data1 <- read_csv("C:/Users/au704633/OneDrive - Aarhus Universitet/Documents/AARHUS_PhD/DSMactivities/2_Biodiversity/Metadata/RegMatStudy1.csv")

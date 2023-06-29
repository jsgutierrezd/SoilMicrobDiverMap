#===============================================================
# Step: 02-Feature selection
# Problem: Regression
# Procedure: 01-Filter method 
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
pckg <- c('magrittr',
          'readr',
          'caret')

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(pckg,usePackage)


# 3) Load regression matrix -----------------------------------------------

data <- read_csv("C:/Users/au704633/OneDrive - Aarhus Universitet/Documents/AARHUS_PhD/DSMactivities/2_Biodiversity/Metadata/RegMatStudy1.csv")
names(data)

# 4) Split into categorical and numerical predictors ----------------------
targets <- data %>% dplyr::select(fieldsampl:longitude)

data.num <- data %>% 
  dplyr::select(Clay:flow_accumulation,
                hillyness:longitudinal_curvature,
                maximal_curvature:sewage_sludge_sd
                )

data.cat <- data %>% 
  dplyr::select(-c(names(targets),
                   names(data.num)))

# 5) Detect variables with near-zero variance -----------------------------

# nzv <- nearZeroVar(as.data.frame(data.num),
#                    saveMetrics= T,
#                    foreach = T,
#                    allowParallel = T,
#                    names=T)
# nzv
# data.num <- data.num[,-nzv$nzv]


# 6) Detect highly-correlated variables -----------------------------------

corMat <- as.matrix(data.num) %>% 
  cor(method = "pearson",
      use = "complete.obs")

highlyCor <- findCorrelation(corMat, 
                                  cutoff = .80,
                                  exact=T,
                                  names=T)
data.num <- data.num %>% 
  dplyr::select(-all_of(highlyCor))

# corMat <- as.matrix(data.num) %>% 
#   cor(method = "spearman",
#       use = "complete.obs")
# highlyCor <- findCorrelation(corMat,
#                              cutoff = .80,
#                              exact=T,
#                              names=T)
# data.num <- data.num %>% 
#   dplyr::select(-all_of(highlyCor))
saveRDS(names(data.num),"Study01/Docs/NamesNumEnvLayers.rds")
saveRDS(names(data.cat),"Study01/Docs/NamesCatEnvLayers.rds")
saveRDS(names(targets),"Study01/Docs/NamesTargetsEnvLayers.rds")


# 8) Dataset with the remaining variables ---------------------------------

data.total <- data.frame(targets,data.num,data.cat)
names(data.total)
write_csv(data.total,
          "C:/Users/au704633/OneDrive - Aarhus Universitet/Documents/AARHUS_PhD/DSMactivities/2_Biodiversity/Metadata/RegMatStudy1Filtered.csv")

# END ---------------------------------------------------------------------


# COR <- cor(as.matrix(targets$shannon.di), as.matrix(data.num),method = "spearman",use = "complete.obs")
# COR
# x <- subset(reshape::melt(COR), value != 1 | !is.na(value))
# x <- x[with(x, order(-abs(x$value))),]
# x
# 
# # tiff("AalborgUniversity/Outputs/Figures/PointsNitrospiraSpearmanCorr.jpeg",
# #     width = 50,height = 30,units = "cm",res=700)
# # x11()
# ggplot(x, aes(x = reorder(X2, -value), y = value)) +
#   geom_bar(stat = "identity") +
#   coord_flip()+
#   labs(x="Variable",y="Spearman Corr Coef")+
#   theme_bw()+
#   scale_y_continuous(breaks=seq(-3, 3, 0.05))+
#   theme(text = element_text(size = 20))+
#   annotate("text", x=30,y=0.15,label="Nitrospira relative abundance",
#            size=10)
# # dev.off()
# # dev.off()
# # dev.off()



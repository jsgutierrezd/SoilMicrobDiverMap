#===============================================================================
# Section: Covariates
# Group: Management
# Procedure: 05
# Description: Read Geopackage files with NLESS and field information from 
#         2017 to 2021 but with columns of interest such as, N
#         fertilization, Field ID, Farm ID, Crop name by field
#         in English and the amount of fertilizer in KgN/km2. Then rasterize
#         these some attributes into 10  resolution.
# Output: Raster files with the cedure
# Produced by Sebastian Gutierrez - Aarhus University
# May 2023
#===============================================================================

gc()
rm(list = ls())
Sys.setenv(language="EN")

# 1) Set working directory ------------------------------------------------

setwd("~/AARHUS_PhD/DSMactivities/2_Biodiversity/SoilMicrobDiverMap")


# 2) Load libraries -------------------------------------------------------
pckg <- c('terra',
          'tidyverse',
          'magrittr',
          'readxl')

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(pckg,usePackage)

# 3) Load Geopackage files ------------------------------------------------
files <- list.files("EnvironmentalLayers/Fertilizer/NLESdata_fields_sp/",
                    pattern = "shortened_KgNkm2.gpkg$",
                    full.names = TRUE
)
files

fert1721sp <- lapply(files,function(x){
  vect(x) 
})


# 4) Load reference raster ------------------------------------------------

ref <- rast("O:/Tech_AGRO/Jord/DSM/Covariates/national_10m/national_10m_stack_20230323/dhm2015_terraen_10m.tif")

# 5) Rasterize attributes -------------------------------------------------
start <- Sys.time()
year <- 1
Nrast1 <- rast()
for (i in 40:50) {
  tmp <- rasterize(fert1721sp[[year]],ref,names(fert1721sp[[year]][,i]))
  Nrast1 <- c(Nrast1,tmp)
}
Nrast1 <- Nrast1/100 %>% round(digits=2)
Nrast1[Nrast1<0] <- 0


year <- 2
Nrast2 <- rast()

for (i in 40:50) {
  tmp <- rasterize(fert1721sp[[year]],ref,names(fert1721sp[[year]][,i]))
  Nrast2 <- c(Nrast2,tmp)
}
Nrast2 <- Nrast2/100 %>% round(digits=2)
Nrast2[Nrast2<0] <- 0


year <- 3
Nrast3 <- rast()

for (i in 40:50) {
  tmp <- rasterize(fert1721sp[[year]],ref,names(fert1721sp[[year]][,i]))
  Nrast3 <- c(Nrast3,tmp)
}
Nrast3 <- Nrast3/100 %>% round(digits=2)
Nrast3[Nrast3<0] <- 0


year <- 4
Nrast4 <- rast()

for (i in 40:50) {
  tmp <- rasterize(fert1721sp[[year]],ref,names(fert1721sp[[year]][,i]))
  Nrast4 <- c(Nrast4,tmp)
}
Nrast4 <- Nrast4/100 %>% round(digits=2)
Nrast4[Nrast4<0] <- 0


year <- 5
Nrast5 <- rast()
for (i in 40:50) {
  tmp <- rasterize(fert1721sp[[year]],ref,names(fert1721sp[[year]][,i]))
  Nrast5 <- c(Nrast5,tmp)
}
Nrast5 <- Nrast5/100 %>% round(digits=2)
Nrast5[Nrast5<0] <- 0
print(Sys.time() - start)



gc()
Nrast1 <- round(Nrast1,digits=2)
names(Nrast1) <- paste0(c("commer_fert","livestock_man","other_org_fert","cattle_slur","pig_slur","mink_slur",
                        "soild_livest_man","liquid_livest_man","deep_litter","degassed_biom","sewage_sludge"),"_2017")
writeRaster(Nrast1,filename = paste0("O:/Tech_AGRO/Jord/Sebastian/SoilMicrobialDiversity/Layers10m/Nfert/",
                                     names(Nrast1),".tif"),overwrite=T)


gc()
Nrast2 <- round(Nrast2,digits=2)
names(Nrast2) <- paste0(c("commer_fert","livestock_man","other_org_fert","cattle_slur","pig_slur","mink_slur",
                          "soild_livest_man","liquid_livest_man","deep_litter","degassed_biom","sewage_sludge"),"_2018")
writeRaster(Nrast2,filename = paste0("O:/Tech_AGRO/Jord/Sebastian/SoilMicrobialDiversity/Layers10m/Nfert/",
                                     names(Nrast2),".tif"),overwrite=T)


gc()
Nrast3 <- round(Nrast3,digits=2)
names(Nrast3) <- paste0(c("commer_fert","livestock_man","other_org_fert","cattle_slur","pig_slur","mink_slur",
                          "soild_livest_man","liquid_livest_man","deep_litter","degassed_biom","sewage_sludge"),"_2019")
writeRaster(Nrast3,filename = paste0("O:/Tech_AGRO/Jord/Sebastian/SoilMicrobialDiversity/Layers10m/Nfert/",
                                     names(Nrast3),".tif"),overwrite=T)

gc()
Nrast4 <- round(Nrast4,digits=2)
names(Nrast4) <- paste0(c("commer_fert","livestock_man","other_org_fert","cattle_slur","pig_slur","mink_slur",
                          "soild_livest_man","liquid_livest_man","deep_litter","degassed_biom","sewage_sludge"),"_2020")
writeRaster(Nrast4,filename = paste0("O:/Tech_AGRO/Jord/Sebastian/SoilMicrobialDiversity/Layers10m/Nfert/",
                                     names(Nrast4),".tif"),overwrite=T)

gc()
Nrast5 <- round(Nrast5,digits=2)
names(Nrast5) <- paste0(c("commer_fert","livestock_man","other_org_fert","cattle_slur","pig_slur","mink_slur",
                          "soild_livest_man","liquid_livest_man","deep_litter","degassed_biom","sewage_sludge"),"_2021")
writeRaster(Nrast5,filename = paste0("O:/Tech_AGRO/Jord/Sebastian/SoilMicrobialDiversity/Layers10m/Nfert/",
                                     names(Nrast5),".tif"),overwrite=T)

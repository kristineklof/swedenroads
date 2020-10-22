#=================================================================#
#            Swedenroads: load dataset for analysis 
#=================================================================#
source("LoadInstall.R")
deps <- c("sf", "data.table","tidyverse", "lwgeom","survival","fasttime")
LoadInstall(deps)

#source("ImportNVDB_Data.R")
source("SurvivalAnalysisData.r")



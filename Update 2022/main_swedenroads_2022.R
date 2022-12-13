#=================================================================#
#              Swedenroads: create 2022 data
#=================================================================#

source("LoadInstall.R")
deps <- c("sf", "data.table","tidyverse","survival","fasttime","survminer",
          "openxlsx", "writexl","gridExtra", "ggrepel", "xtable", "pracma",
          "viridis","remotes","extrafont","gtable","cowplot","grid",
          "RColorBrewer","ggpattern")
LoadInstall(deps)
options(scipen=999)
#remotes::install_version("Rttf2pt1", version = "1.3.8")
#extrafont::font_import()
extrafont::loadfonts(device = "win")
windowsFonts("Arial" = windowsFont("Arial"))

#source("ImportNVDB_Data.R")
source("SurvivalAnalysisData.r", encoding = 'UTF-8')
source("PrepareEngNVDBOutput.r", encoding = 'UTF-8')
source("PrepareHomoSwe.r", encoding = 'UTF-8')
source("ImputeMissingData.r", encoding = 'UTF-8')
source("Age_NVDBData.r", encoding = 'UTF-8')
source("AFTRegression.r", encoding = 'UTF-8')
source("CoxPH.r", encoding = 'UTF-8')
source("CombineBeltyp.r", encoding = 'UTF-8')
source("CalculateRemainingServiceLife.r", encoding = 'UTF-8')
source("CreatePCI.r", encoding = 'UTF-8')
source("DescriptiveStatsFunctions.r", encoding = 'UTF-8')
source("ShiftLegendFunction.r", encoding = 'UTF-8')
source("Tests_Swedenroads_main.r", encoding = 'UTF-8')
source("Update 2022/UpdateDataFunctions.r", encoding = 'UTF-8')
source("Update 2022/Update2022Functions.r", encoding = 'UTF-8')
#source("ImportAndPreparePMSData.r", encoding = 'UTF-8')

datapath <- "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/"
# Lan and kommun
lankom <- fread(paste0(datapath,"LänKommun_TRV.csv"), encoding = 'UTF-8')

# Import 2020 NVDB-data
nvdb_2020 <- st_read(paste0(datapath,"nvdb_surv_sweden_75perc_matdatum.shp"), options = "ENCODING=WINDOWS-1252")
swedenroads_2020 <- st_read(paste0(datapath,"swedenroads_2020_v4.shp"), options = "ENCODING=WINDOWS-1252")
sw22_geo <- swedenroads_2020[,c("ID")]

# Import survival data
lans_dt <- readRDS(paste0(datapath,"lans_dt.rds"))
lan_surv_dt <- readRDS(paste0(datapath,"lan_surv_dt_matning.rds"))

# Import 2022 data (processed in PostGIS and QGIS)
swedenroads_2022 <- st_read(paste0(datapath,"2022/swedenroads_2022.shp"))

# Drop geometry for analytical purposes
sweden22 <- st_drop_geometry(swedenroads_2022)

# Change vagtyp
sweden22 <- sweden22 %>%
  dplyr::mutate(vagtyp_21 = ChangeVagtyp(vagtyp_21))

# Add beläggning
sweden22_comp  <- sweden22 %>% dplyr::mutate(PavementType = ChangeBeltyp(blggnngst)) %>%
  dplyr::mutate(PavementType = as.factor(PavementType)) 

#########################################
# Uppdatera variabler

sw22 <- UpdateVariables22(sweden22)

#########################################
# Beräkna ny livslängd

sw22 <- NewLifetime22(sw22, nvdb_2020, lan_surv_dt)

#########################################
# Beräkna PCI

sw22 <- CalculatePCI22(sw22)






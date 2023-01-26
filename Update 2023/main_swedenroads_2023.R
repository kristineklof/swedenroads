#=================================================================#
#              Swedenroads: create 2023 data
#=================================================================#

source("LoadInstall.R")
deps <- c("sf", "data.table","tidyverse","survival","fasttime","survminer",
          "openxlsx", "writexl","gridExtra", "ggrepel", "xtable", "pracma",
          "viridis","remotes","extrafont","gtable","cowplot","grid",
          "RColorBrewer","ggpattern")
LoadInstall(deps)
options(scipen=999)

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
source("Update 2023/UpdateDataFunctions.r", encoding = 'UTF-8')
source("Update 2023/Update2023Functions.r", encoding = 'UTF-8')

datapath <- "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/"
# Lan and kommun
lankom <- fread(paste0(datapath,"LänKommun_TRV.csv"), encoding = 'UTF-8')
lan <- lankom %>% dplyr::select(Län, Länsnamn)

# Import 2020 NVDB-data
nvdb_2020 <- st_read(paste0(datapath,"nvdb_surv_sweden_75perc_matdatum.shp"), options = "ENCODING=WINDOWS-1252")

# Import survival data
lans_dt <- readRDS(paste0(datapath,"lans_dt.rds"))
lan_surv_dt <- readRDS(paste0(datapath,"lan_surv_dt_matning.rds"))

# Import 2023 data (processed in PostGIS and QGIS)
swedenroads_2023 <- st_read(paste0(datapath,"2023/swedenroads_2023_database_version.shp"))
swedenroads_2020 <- st_read(paste0(datapath,"swedenroads_2020_v4.shp"))

# Drop geometry for analytical purposes
sweden23 <- st_drop_geometry(swedenroads_2023)
sweden20 <- st_drop_geometry(swedenroads_2020)

# Rename some variables for sweden 2020
sw20 <- sweden20 %>% dplyr::rename(PCIClass_2020 = IndxKls)
head(sw20)

# Change vagtyp
sweden23 <- sweden23 %>%
  dplyr::mutate(vagtyp_23 = ChangeVagtyp(vagtyp_23))

# Add region
sweden23 <- sweden23 %>%
  dplyr::mutate(region = AddRegion2023(län_nr))

# Add trafikklass
sweden23 <- TrafficClass2023(sweden23)

# Add beläggning
sweden23_comp  <- sweden23 %>% dplyr::mutate(PavementType = ChangeBeltyp(blggnngst)) %>%
  dplyr::mutate(PavementType = as.factor(PavementType)) 

#########################################
# Uppdatera variabler

sw23 <- UpdateVariables23(sweden23, lans_dt)

#########################################
# Beräkna ny livslängd

sw23 <- NewLifetime23(sw23, nvdb_2020, lan_surv_dt)
str(sw23)

#########################################
# Beräkna PCI

sw23 <- CalculatePCI23(sw23)


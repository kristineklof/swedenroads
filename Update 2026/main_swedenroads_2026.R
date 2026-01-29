#=================================================================#
#              Swedenroads: create 2026 data
#=================================================================#

source("LoadInstall.R")
deps <- c("sf", "data.table","tidyverse","survival","fasttime","survminer",
          "openxlsx", "writexl","gridExtra", "ggrepel", "xtable", "pracma",
          "viridis","remotes","extrafont","gtable","cowplot","grid",
          "RColorBrewer","ggpattern","patchwork")
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
source("Update 2024/Update2024Functions.r", encoding = 'UTF-8')
source("Update 2025/update_2025_functions.r", encoding = 'UTF-8')
source("Update 2026/update_2026_functions.r", encoding = 'UTF-8')

datapath <- "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/"
# Lan and kommun
lankom <- fread(paste0(datapath,"LänKommun_TRV.csv"), encoding = 'UTF-8')
lan <- lankom %>% dplyr::select(Län, Länsnamn)

# Import 2020 NVDB-data
nvdb_2020 <- st_read(paste0(datapath,"nvdb_surv_sweden_75perc_matdatum.shp"), options = "ENCODING=WINDOWS-1252")

# Import survival data
lans_dt <- readRDS(paste0(datapath,"lans_dt.rds"))
lan_surv_dt <- readRDS(paste0(datapath,"lan_surv_dt_matning.rds"))

# Import 2025 data (processed in PostGIS and QGIS)
swedenroads_2026 <- st_read(paste0(datapath,"2026/swedenroads_2025_database_version.shp"))
sw26_geo <- swedenroads_2026[,c("id")]

# Drop geometry for analytical purposes
sweden26 <- st_drop_geometry(swedenroads_2026)
head(sweden26)

# Check beldat & matdatum_2 -- looks OK
sum(is.na(sweden26$beldat_25))/nrow(sweden26) # 8.6% missing 
max(sweden26$beldat_25, na.rm=TRUE)

sum(is.na(sweden26$matdatum_2))/nrow(sweden26) # 7.4 % missing
max(sweden26$matdatum_2, na.rm=TRUE)

### Update maintenance standard
sweden26 <- MaintenanceStandard2026(sweden26)

# Change names
sweden26 <- sweden26 %>% rename(
  beldatum = bldt_25,
  matdatum = mtdtm_2
)

# Add län
sweden26 <- AddLan2026(sweden26, lankom)

# Update belaggning and matdata
sweden26 <- UpdateBelaggningAlderBeltyp26(sweden26, lans_dt, "2025-01-01")
sweden26 <- UpdateMatning26(sweden26)

# Beräkna ny livslängd
sw26 <- NewLifetime26(sweden26, lan_surv_dt, "2025-01-01")

# Beräkna PCI
sw26 <- CalculatePCI26(sw26, "2025-01-01")

sw26 <- PCIClass2026(sw26, "indx_21", "PCIClass_21")
sw26 <- PCIClass2026(sw26, "indx_22", "PCIClass_22")
sw26 <- PCIClass2026(sw26, "indx_23", "PCIClass_23")
sw26 <- PCIClass2026(sw26, "indx_24", "PCIClass_24")
sw26 <- PCIClass2026(sw26, "indx_25", "PCIClass_25")

# Update predicted and remaining service lives
sw26 <- UpdateServiceLife26(sw26)
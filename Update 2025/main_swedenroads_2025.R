#=================================================================#
#              Swedenroads: create 2025 data
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

datapath <- "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/"
# Lan and kommun
lankom <- fread(paste0(datapath,"LänKommun_TRV.csv"), encoding = 'UTF-8')
lan <- lankom %>% dplyr::select(Län, Länsnamn)

# Import 2020 NVDB-data
nvdb_2020 <- st_read(paste0(datapath,"nvdb_surv_sweden_75perc_matdatum.shp"), options = "ENCODING=WINDOWS-1252")

# Import survival data
lans_dt <- readRDS(paste0(datapath,"lans_dt.rds"))
lan_surv_dt <- readRDS(paste0(datapath,"lan_surv_dt_matning.rds"))

# Import 2024 data (processed in PostGIS and QGIS)
swedenroads_2025 <- st_read(paste0(datapath,"2025/swedenroads_2025_database_version.shp"))
sw25_geo <- swedenroads_2025[,c("id")]

# Drop geometry for analytical purposes
sweden25 <- st_drop_geometry(swedenroads_2025)
head(sweden25)

# Drop columns
sweden25 <- sweden25 %>% select(-c(objectid, route_id, from_measu, to_measure, 
                                   klass_181, fpv_k_309, funktionel, farjeled,
                                   slitl_152, vagha_7, vagha_6, vagtr_474,
                                   korfa_52, typ_369, namn_132, vagkl_564))  # Drop multiple columns

# Change names
names(sweden25) <- c("id", "brghtsk", "hastght", "ådt_mtr","ådt_tng","ådt_frd",
                     "dou2017", "vägbrdd","vägktgr",
                     "vägnmmr",  "vägtyp", "kmmn_nr", "längd",
                     "sparm17_25", "sparm15_25", "irih_25", "iriv_25",
                     "kantdjup_2","matdatum_2","beldat_25", "beltyp_25",
                     "tillvm_25", "utlm_25", "tackning","korfalt_25",
                     "index_24", "index_23", "index_22", "index_21")
                     

# Change vagtyp
sweden25 <- sweden25 %>%
  dplyr::mutate(vägtyp = ChangeVagtyp(vägtyp))

# Add län and region
sweden25 = AddLan2025(sweden25,lankom)
sweden25 <- sweden25 %>%
  dplyr::mutate(region = AddRegion(län_nr))

sweden25 %>% summarise(na_count = sum(is.na(län_nr))) # No missing values

# Add trafikklass
sweden25 <- AddAADTIfMissing2025(sweden25)
sweden25 <- TrafficClass(sweden25)

sweden25 %>% summarise(na_count = sum(is.na(trfkkls))) # No missing values

#########################################
# Uppdatera beläggning
sw25 <- UpdateBeltyp25(sweden25, lans_dt)
sw25 <- AddPavementTypeIfMissing2025(sw25)
sw25 %>% summarise(na_count = sum(is.na(PavementType))) # NA count before missing: 19385

#########################################
# Impute missing values
sw25 %>% summarise(na_count = sum(is.na(vägktgr))) # NA count before missing: 9
sw25 %>% summarise(na_count = sum(is.na(vägtyp))) # NA count before missing: 4
sw25 %>% summarise(na_count = sum(is.na(dou2017))) # NA count before missing: 8
sw25 %>% summarise(na_count = sum(is.na(vägbrdd))) # NA count before missing: 98
sw25 %>% summarise(na_count = sum(is.na(hastght))) # NA count before missing: 710
sw25 %>% summarise(na_count = sum(is.na(brghtsk))) # NA count before missing: 0

sw25 <- sw25 %>% mutate(vägtyp = ifelse(is.na(vägtyp),4,vägtyp),
                        vägktgr = ifelse(is.na(vägktgr),4,vägktgr))

sw25 <- AddRoadWidthIfMissing2025(sw25)
sw25 <- AddSpeedLimitIfMissing2025(sw25)
sw25 <- AddDOU2017IfMissing2025(sw25)

#########################################

sw25 <- UpdateAlder25(sw25, "2024-01-01")
sw25 %>% summarise(na_count = sum(is.na(ålder))) # NA count before missing: 19060
sw25 <- AddAlderIfMissing2025(sw25)
sw25 <- UpdateMatning25(sw25)

# Beräkna ny livslängd
sw25 <- NewLifetime25(sw25, lan_surv_dt, "2024-01-01")
str(sw25)

#########################################
# Beräkna PCI
sw25 <- CalculatePCI25(sw25, "2024-01-01")

sw25 <- PCIClass2025(sw25, "index_21", "PCIClass_21")
sw25 <- PCIClass2025(sw25, "index_22", "PCIClass_22")
sw25 <- PCIClass2025(sw25, "index_23", "PCIClass_23")
sw25 <- PCIClass2025(sw25, "index_24", "PCIClass_24")
sw25 <- PCIClass2025(sw25, "index_25", "PCIClass_25")
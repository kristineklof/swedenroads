#=================================================================#
#              Swedenroads: create 2024 data
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
swedenroads_2024 <- st_read(paste0(datapath,"2024/swedenroads_2024_database_version.shp"))
swedenroads_2023 <-  st_read(paste0(datapath,"2023/sweden_2023_validation_file.shp"))
swedenroads_2020 <- st_read(paste0(datapath,"swedenroads_2020_v4.shp"))
sw24_geo <- swedenroads_2020[,c("ID")]

# Drop geometry for analytical purposes
sweden24 <- st_drop_geometry(swedenroads_2024)
sweden23 <- st_drop_geometry(swedenroads_2023)
sweden20 <- st_drop_geometry(swedenroads_2020)

head(sweden24)

# Change names
names(sweden24) <- c("id", "brghtsk", "hastght", "dou2017", 
                     "ådt_frd", "ådt_tng", "ådt_mtr", "vägbrdd",
                     "vägnmmr", "vägktgr", "vägtyp", "längd",
                     "blggnngsd", "blggnngst", "tackning", "spårdjp", "iri",    
                     "mätdatm", "iri_maint", "sp_maint", "län_nr", "kmmn_nr",
                     "ålder", "frvntdl", "åtrstnl", "tllstnl", "indxkls", 
                     "ikls_1", "ikls_2", "ikls_3", "sparm17_24", "sparm15_24",
                     "irih_24", "iriv_24", "kantdjup_2", "matdatum_2", "beldat_24",               
                     "beltyp_24", "tillvm_24", "utlm_24", "tackning_2",
                     "korfalt_24", "barig_24", "dou_24", "adt_24", "adt_tung_2", 
                     "adt_mat_24", "hast_24", "vagtyp_24", "vagbredd_2")

# Rename some variables for sweden 2020
sw20 <- sweden20 %>% dplyr::rename(PCIClass_2020 = IndxKls)
head(sw20)

# Change vagtyp
sweden24 <- sweden24 %>%
  dplyr::mutate(vagtyp_24 = ChangeVagtyp(vagtyp_24))

head(sweden24)

# Add region
sweden24 <- sweden24 %>%
  dplyr::mutate(region = AddRegion(län_nr))

# Add trafikklass
sweden24 <- TrafficClass(sweden24)

# Add beläggning
sweden24_comp  <- sweden24 %>% dplyr::mutate(PavementType = ChangeBeltyp(blggnngst)) %>%
  dplyr::mutate(PavementType = as.factor(PavementType)) 

#########################################
# Uppdatera variabler

sw24 <- UpdateVariables24(sweden24, lans_dt)

#########################################
# Beräkna ny livslängd

sw24 <- NewLifetime24(sw24, nvdb_2020, lan_surv_dt, "2023-01-01")
str(sw24)

#########################################
# Beräkna PCI

sw24 <- CalculatePCI24(sw24, "2023-01-01")

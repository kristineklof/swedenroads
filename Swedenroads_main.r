#=================================================================#
#            Swedenroads: create dataset for analysis 
#=================================================================#
source("LoadInstall.R")
deps <- c("sf", "data.table","tidyverse", "lwgeom","survival","fasttime","survminer",
          "remotes","sp", "rgdal", "rpostgis", "RPostgres", "scales", 
          "openxlsx", "writexl","gridExtra", "ggrepel", "xtable", "pracma")
LoadInstall(deps)
options(scipen=999)

#source("ImportNVDB_Data.R")
source("SurvivalAnalysisData.r", encoding = 'UTF-8')
source("PrepareEngNVDBOutput.r", encoding = 'UTF-8')
source("PrepareHomoSwe.r", encoding = 'UTF-8')
source("ImputeMissingData.r", encoding = 'UTF-8')
source("Age_NVDBData.r", encoding = 'UTF-8')
source("QClassFunction.r", encoding = 'UTF-8')
source("AFTRegression.r", encoding = 'UTF-8')
source("CoxPH.r", encoding = 'UTF-8')
source("CombineBeltyp.r", encoding = 'UTF-8')
source("CreatePCI.r", encoding = 'UTF-8')
source("DescriptiveStatsFunctions.r", encoding = 'UTF-8')
source("Tests_Swedenroads_main.r", encoding = 'UTF-8')
#source("ImportAndPreparePMSData.r", encoding = 'UTF-8')

# Data
datapath <- "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/"

# Import survival data
lans_dt <- readRDS(paste0(datapath,"lans_dt.rds"))
lan_surv_dt <- readRDS(paste0(datapath,"lan_surv_dt_matning.rds"))
itShouldTestSurvivalData(dat = lan_surv_dt)
str(lans_dt, list.len=ncol(lans_dt))
head(lans_dt)

# Lan and kommun
lankom <- fread(paste0(datapath,"LänKommun_TRV.csv"))

# Import NVDB data
nvdb_bel_mat_org <- st_read("C:/Users/winte/Swedenroads_homo_v2/nvdb_surv_sweden_75perc_matdatum.shp")
setDT(nvdb_bel_mat_org)
itShouldTestHomoNVDBdata(dat = nvdb_bel_mat_org)

# If beltyp and beldatum is missing, and survbeltyp exists, replace with survbeltyp
nvdb_bel_mat <- CombineBeltyp(nvdb_bel_mat_org)
itShouldTestBelaggningReplacement(nvdb_bel_mat)

###################################################################################################################
## Select and rename columns
outcols <- c("objectid", "barig_64","f_hogst_22","lever_292","adt_f_117","adt_l_115","adt_a_113","matar_121","slitl_25","bredd_156",
            "kateg_380","huvud_13","lanst_15","vagty_41","kommunnr","shape_leng","beldatum","beltyp","tillvmetod","utlmetod", "omfttnn",
            "sparm17","sparm15","spar17h", "spar17v","irih", "iriv","kantdjup","matdatum","geometry")

names_eng <- c("Objectid","BearingCapacityClass","SpeedLimit","DoU2017","AADT","AADT_heavy","AADT_axle","AADT_measurement_year","SurfaceType","RoadWidth",
                "RoadCategory","Road_Number","County","RoadType","Municipality","Length","TreatmentDate","Treatment","ManufactureMethod","PavingMethod", "Coverage",
                "rut_max17_perc","rut_max15_perc","rut_r_perc","rut_l_perc","IRI_r_perc","IRI_l_perc","EdgeDepth","MeasurementDate","geometry")

names_swe <- c("ID","Bärighetsklass","Hastighet","DoU2017","ÅDT_fordon","ÅDT_tung","ÅDT_axel","ÅDT_mätår","Slitlager","Vägbredd",
                "Vägkategori","Vägnummer","Län_nr","Vägtyp","Kommun_nr","Längd","Beläggningsdatum","Beläggning","TillverkningsMetod","UtläggningsMetod", "Omfattning",
                "Sparmax17","Spar_max15","Spar_h","Spar_l","IRI_h","IRI_v","Kantdjup","Mätdatum","geometry", "Beläggningstyp", "Länsnamn", "Kommunnamn", "Trafikklass",
                "IRI_underhållsstandard", "Spårdjup_underhållsstandard", "Region", "Beläggningsklass", "Ålder", "Förväntad_medianålder", "Förväntad_75p_ålder", "FörväntadLivslängd",
                "ÅterståendeLivslängd","QClass")

# English version
outdat_eng <- PrepareHomoNVDB(outcols = outcols, names_eng = names_eng, indat = nvdb_bel_mat, pmsdat = lans_dt, lankom = lankom)
print(head(outdat_eng))
itShouldTestEnglishOutput(outdat_eng)
#saveRDS(outdat_eng, paste0(datapath,"outdat_eng.rds")

# Swedish version
outdat_swe <- PrepareHomoSwe(outcols = outcols, names_eng = names_eng, names_swe = names_swe, indat = nvdb_bel_mat, pmsdat = lans_dt, survdat=lan_surv_dt, lankom = lankom)
print(head(outdat_swe))
itShouldTestSwedishOutput(outdat_swe)
#saveRDS(outdat_swe, paste0(datapath,"outdat_swe.rds")

###################################################################################################################
## Add predicted service life
#outdat_eng <- readRDS(paste0(datapath,"outdat_eng.rds")

outdat_eng <- CreateServiceLifeData(dat = outdat_eng, survdat=lan_surv_dt, metod = "AFT", 
                                          distribution = "lognormal", percentil_high = 0.5, percentil_low = 0.75,
                                          div = "no", divclass = 5)

predservlife <- outdat_eng %>% group_by(tkl8, PavementType) %>%
  summarise_at(c("PredictedServiceLife"), mean, na.rm = TRUE) %>%
  mutate(PredictedServiceLife = round(PredictedServiceLife, digits=0))

print(predservlife, n=Inf)

itShouldTestConditionMeasurements(outdat_eng)
itShouldTestVariableAge(outdat_eng, lan_surv_dt)

###################################################################################################################
## Export English shapefile

# Rename factor levels
outdat_eng_shape <- copy(outdat_eng)
new_pave_levels <- c("Hot mix asphalt (asphalt concrete)", "Seal coat", "Half warm asphalt", 
                      "Grouted macadam", "Thin asphalt layer", "Hot mix asphalt (stone mastic)", 
                      "Surface dressing on bituminous surface", "Surface dressing on gravel", "Other")
setattr(outdat_eng_life_shape$PavementType,"levels",new_pave_levels)

# Road width to meters
outdat_eng_shape[, RoadWidth := RoadWidth/10]
stopifnot(length(outdat_eng_life_shape$Objectid) == 437189) 

# Export as shapefile
st_write(outdat_eng_shape, paste0(datapath,"sweden_v3_201119.shp"), driver="ESRI Shapefile", append=FALSE) 

###################################################################################################################
## Calculate PCI
swedt <- st_read("C:/Users/winte/Swedenroads_outputs/sweden_v3_201119.shp")
setDT(swedt)

swedt_PCI <- CreatePCI(swedt)
swedt_PCI <- PCIClass(swedt_PCI)
head(swedt_PCI)
itShouldTestPCI(swedt_PCI)

# Export everything as shapefile
st_write(swedt_PCI, "C:/Users/winte/Swedenroads_outputs/sweden_v3_pci201206.shp", driver="ESRI Shapefile", append=FALSE) 

# Check PCI statistics
QualitativeStatsSingleGroup(swedt_PCI, quo(PCIClass), quo(Length))
QualitativeStatsDoubleGroup(swedt_PCI, quo(RoadTyp), quo(PCIClass), quo(Length))
QualitativeStatsDoubleGroup(swedt_PCI, quo(tkl8), quo(PCIClass), quo(Length))
QualitativeStatsDoubleGroup(swedt_PCI, quo(DoU2017), quo(PCIClass), quo(Length))

###################################################################################################################
## Select variables in Swedish dataset
keeps <- c("ID","Bärighetsklass","Hastighet","DoU2017","ÅDT_fordon","ÅDT_tung","ÅDT_mätår","Vägbredd",
                "Vägnummer","Vägkategori","Vägtyp","Längd","Beläggningsdatum","Beläggningstyp", 
                "Spårdjup","IRI","Mätdatum","geometry", "Län_nr", "Kommun_nr", "Trafikklass",
                "IRI_underhållsstandard", "Spårdjup_underhållsstandard", "Region", "Ålder", "FörväntadLivslängd",
                "ÅterståendeLivslängd")

outdat_swe_small <- outdat_swe[, ..keeps]

# Add PCI
outdat_swe_small <- left_join(outdat_swe_small, swedt_PCI[,c("Objectd", "PCI","PCIClass")], by = c("ID" = "Objectd"))

# Byt namn på indexvariabler
names(outdat_swe_small) <- c(keeps,"TillståndsIndex","IndexKlass")

itShouldTestSwedenroadsData(outdat_swe_small)

# Check PCI statistics
QualitativeStatsSingleGroup(outdat_swe_small, quo(IndexKlass), quo(Längd))
QualitativeStatsDoubleGroup(outdat_swe_small, quo(DoU2017), quo(IndexKlass), quo(Längd))
QualitativeStatsDoubleGroup(outdat_swe_small, quo(Region), quo(IndexKlass), quo(Längd))
QualitativeStatsDoubleGroup(outdat_swe_small, quo(Vägtyp), quo(IndexKlass), quo(Längd))

# Save as shapefile
st_write(outdat_swe_small, "C:/Users/winte/Swedenroads_outputs/swedenroads_2020_v2.shp", driver="ESRI Shapefile", append=TRUE) 

# vscode://vscode.github-authentication/did-authenticate?windowid=1&code=931650176fe14d005e9d&state=f946fae0-8930-4a19-a358-43ff4dba1639


#=================================================================#
#            Swedenroads: create dataset for analysis 
#=================================================================#
source("LoadInstall.R")
deps <- c("sf", "data.table","tidyverse", "lwgeom","survival","fasttime","survminer","remotes","sp", "rgdal", "rpostgis", "RPostgres")
LoadInstall(deps)
options(scipen=999)

#source("ImportNVDB_Data.R")
source("SurvivalAnalysisData.r", encoding = 'UTF-8')
source("PrepareEngNVDBOutput.r", encoding = 'UTF-8')
source("ImputeMissingData.r", encoding = 'UTF-8')
source("Age_NVDBData.r", encoding = 'UTF-8')
source("QClassFunction.r", encoding = 'UTF-8')
source("AFTRegression.r", encoding = 'UTF-8')
source("CoxPH.r", encoding = 'UTF-8')
source("CombineBeltyp.r", encoding = 'UTF-8')
#source("ImportAndPreparePMSData.r", encoding = 'UTF-8')

# Import survival data
lans_dt <- readRDS("C:/Users/winte/Swedenroads_outputs/lans_dt.rds")
lan_surv_dt <- readRDS("C:/Users/winte/Swedenroads_outputs/lan_surv_dt_matning.rds")
head(lan_surv_dt)
head(lans_dt)

pmsunique <- lan_surv_dt[, .SD[1], hom_id2]
stopifnot(round(sum(pmsunique$langd)/1000) == 103582) 

# Lan and kommun
lankom <- fread("C:/Users/winte/OneDrive/Documents/salbo.ai/Transportföretagen/Data/LänKommun.csv")

# Import NVDB data
nvdb_bel_mat_org <- st_read("C:/Users/winte/Swedenroads_homo_v2/nvdb_surv_sweden_75perc.shp")
stopifnot(nrow(nvdb_bel_mat_org) == 973518)
stopifnot(length(unique(nvdb_bel_mat_org$objectid)) == 973518)
setDT(nvdb_bel_mat_org)

head(nvdb_bel_mat_org)
str(nvdb_bel_mat_org)
table(nvdb_bel_mat_org$omfttnn)
unique(nvdb_bel_mat_org$omfttnn)
unique(nvdb_bel_mat_org$survbeltyp)
nvdb_bel_mat_org[is.na(atgdt_f)]
stopifnot(round(nrow(nvdb_bel_mat_org[is.na(beldatum)])/(nrow(nvdb_bel_mat_org)),digits=2) == 0.13) # 13 percent missing treatment date

#####################################################################################
# If beltyp and beldatum is missing, and survbeltyp exists, replace with survbeltyp
nrow(nvdb_bel_mat_org[!is.na(survbeltyp) & beltyp != survbeltyp])/nrow(nvdb_bel_mat_org)
nrow(nvdb_bel_mat_org[!is.na(survbeltyp) & beltyp != survbeltyp & !is.na(atgd2_f)])
nrow(nvdb_bel_mat_org[!is.na(atgd2_f) & beldatum != atgd2_f])/nrow(nvdb_bel_mat_org)
nrow(nvdb_bel_mat_org[is.na(atgd2_f)])
nrow(nvdb_bel_mat_org[is.na(beldatum)])

nvdb_bel_mat <- CombineBeltyp(nvdb_bel_mat_org)
stopifnot(round(nrow(nvdb_bel_mat[is.na(beldatum)])/(nrow(nvdb_bel_mat_org)),digits=2) == 0.11)
#nvdb_bel_mat <- nvdb_bel_mat_org
nrow(nvdb_bel_mat[!is.na(atgd2_f) & beldatum != atgd2_f])/nrow(nvdb_bel_mat) 

#####################################################################################
## Select and rename columns
outcols <- c("objectid", "barig_64","f_hogst_22","lever_292","adt_f_117","adt_l_115","adt_a_113","matar_121","slitl_25","bredd_156",
            "kateg_380","huvud_13","lanst_15","vagty_41","kommunnr","shape_leng","beldatum","beltyp","tillvmetod","utlmetod", "omfttnn",
            "sparm17","sparm15","spar17h", "spar17v","irih", "iriv","kantdjup", "geometry")

names_eng <- c("Objectid","BearingCapacityClass","SpeedLimit","DoU2017","AADT","AADT_heavy","AADT_axle","AADT_measurement_year","SurfaceType","RoadWidth",
                "RoadCategory","Road_Number","County","RoadType","Municipality","Length","TreatmentDate","Treatment","ManufactureMethod","PavingMethod", "Coverage",
                "rut_max17_perc","rut_max15_perc","rut_r_perc","rut_l_perc","IRI_r_perc","IRI_l_perc","EdgeDepth","geometry")

names_swe <- c("ID","Bärighetsklass","Hastighet","DoU2017","ÅDT_fordon","ÅDT_tung","ÅDT_axel","ÅDT_mätår","Slitlager","Vägbredd",
                "Vägnummer","Vägtyp","Län_nr","Vägtyp","Kommun_nr","Längd","Beläggningsdatum","Beläggning","TillverkningsMetod","UtläggningsMetod", "Omfattning",
                "Avg_sparmax17","Avg_spar_max15","Avg_spar_h","Avg_spar_l","Avg_IRI_h","Avg_IRI_v","Avg_kantdjup","geometry")

outdat_eng <- PrepareHomoNVDB(outcols = outcols, names_eng = names_eng, indat = nvdb_bel_mat, pmsdat = lans_dt, lankom = lankom)
print(head(outdat_eng))
                 
#####################################################################################
# Handeling of missing data
stopifnot(sum(is.na(outdat_eng$County)) == 0)
stopifnot(sum(is.na(outdat_eng$AADT)) == 0)
stopifnot(sum(is.na(outdat_eng$RoadType)) == 0)
stopifnot(sum(is.na(outdat_eng$RoadCategory)) == 0)
stopifnot(sum(is.na(outdat_eng$tkl8)) == 0)
stopifnot(sum(is.na(outdat_eng$Municipality)) == 0)
stopifnot(sum(is.na(outdat_eng$BearingCapacityClass)) == 0)
stopifnot(sum(is.na(outdat_eng$Length)) == 0)
stopifnot(sum(is.na(outdat_eng$SpeedLimit)) == 0)
stopifnot(sum(is.na(outdat_eng$RoadWidth)) == 0)
stopifnot(sum(is.na(outdat_eng$PavementType)) == 0)
stopifnot(sum(is.na(outdat_eng$DoU2017)) == 0)
stopifnot(sum(is.na(outdat_eng$Age)) == 0)

stopifnot(length(levels(outdat_eng$RoadType)) == 5)
stopifnot(length(unique(outdat_eng$tkl8)) == 8)
stopifnot(length(levels(outdat_eng$SurfaceClass)) == 3)
stopifnot(length(levels(outdat_eng$PavementType)) == 9)
#stopifnot(nrow(outdat_eng) == 437801)
stopifnot(nrow(outdat_eng) == 437189)
stopifnot(nrow(unique(outdat_eng, by=c("Objectid")))==437189)
#saveRDS(outdat_eng, "C:/Users/winte/Swedenroads_outputs/outdat_eng.rds")

#####################################################################################
outdat_eng <- readRDS("C:/Users/winte/Swedenroads_outputs/outdat_eng.rds")

# Add predicted service life
outdat_eng_life <- CreateServiceLifeData(dat = outdat_eng, survdat=lan_surv_dt, metod = "AFT", 
                                          distribution = "lognormal", percentil_high = 0.5, percentil_low = 0.75,
                                          div = "no", divclass = 5)
print(head(outdat_eng_life))
stopifnot(nrow(outdat_eng_life[is.na(QClass)]) == 0)

predservlife <- outdat_eng_life %>% group_by(tkl8, PavementType) %>%
  summarise_at(c("PredictedServiceLife"), mean, na.rm = TRUE) %>%
  mutate(PredictedServiceLife = round(PredictedServiceLife, digits=0))

print(predservlife, n=Inf)

# Save as rds
#saveRDS(outdat_eng_life, "C:/Users/winte/Swedenroads_outputs/outdat_eng_201109.rds")
#outdat_eng <- readRDS("C:/Users/winte/Swedenroads_outputs/outdat_eng.rds")

#####################################################################################
# Check service lives & QClass
sum(outdat_eng_life$Length/1000)
sum(outdat_eng_life$Length[outdat_eng$RemainingServiceLife < 0],na.rm=TRUE)/sum(outdat_eng_life$Length)
sum(outdat_eng_life$RemainingServiceLife < 0, na.rm=TRUE)/nrow(outdat_eng_life)

hist(outdat_eng_life$RemainingServiceLife)

qclasslength <- outdat_eng_life %>%
              group_by(QClass) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen))
print(qclasslength)

qclasslengthtkl <- outdat_eng_life %>%
              group_by(tkl8, QClass) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen))
print(qclasslengthtkl, n=Inf)

qclasslengthdou <- outdat_eng_life %>%
              group_by(DoU2017, QClass) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen))
print(qclasslengthdou, n=Inf)

table(outdat_eng_life$QClass)/nrow(outdat_eng_life)

# Proportions of observations in QClass by traffic class
prop.table(table(outdat_eng_life$QClass, outdat_eng_life$tkl8),margin=2)
rem0 <- outdat_eng_life[RemainingServiceLife < 0]
prop.table(table(rem0$tkl8))

#####################################################################################
# Check network length
mean(outdat_eng_life$Length,na.rm=TRUE)
median(outdat_eng_life$Length,na.rm=TRUE)
max(outdat_eng_life$Length,na.rm=TRUE)

doulength <- outdat_eng_life %>%
              group_by(DoU2017) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen))
print(doulength)

#####################################################################################
# Sections above maintenance standard
sum(outdat_eng_life$rut_max17_perc > outdat_eng_life$SP_maint, na.rm=TRUE)/nrow(outdat_eng_life)
sum(outdat_eng_life$rut_max15_perc > outdat_eng_life$SP_maint, na.rm=TRUE)/nrow(outdat_eng_life)
sum(outdat_eng_life$IRI_l_perc > outdat_eng_life$IRI_maint, na.rm=TRUE)/nrow(outdat_eng_life)
sum(outdat_eng_life$IRI_r_perc > outdat_eng_life$IRI_maint, na.rm=TRUE)/nrow(outdat_eng_life)
sum(outdat_eng_life$rut_max17_perc > outdat_eng_life$SP_maint | outdat_eng_life$rut_max15_perc > outdat_eng_life$SP_maint | outdat_eng_life$IRI_r_perc > outdat_eng_life$IRI_maint | outdat_eng_life$IRI_l_perc > outdat_eng_life$IRI_maint, na.rm=TRUE)/nrow(outdat_eng_life)

maintstandlengthtkl <- outdat_eng_life %>%
              group_by(tkl8) %>%
              summarise(grouplen = sum(Length)/1000,
                        lenabove= sum(Length[rut_max17_perc > SP_maint | rut_max15_perc > SP_maint | IRI_r_perc > IRI_maint | IRI_l_perc > IRI_maint], na.rm = TRUE)/1000/grouplen)
print(maintstandlengthtkl)

maintstandlengthdou <- outdat_eng_life %>%
              group_by(DoU2017) %>%
              summarise(grouplen = sum(Length)/1000,
                        lenabove= sum(Length[rut_max17_perc > SP_maint | rut_max15_perc > SP_maint | IRI_r_perc > IRI_maint | IRI_l_perc > IRI_maint], na.rm = TRUE)/1000/grouplen)
print(maintstandlengthdou)

# Check missing measurements - about 8 percent of road network has missing measurement values
nrow(outdat_eng_life[is.na(rut_max17_perc)])/nrow(outdat_eng_life)
nrow(outdat_eng_life[is.na(rut_max15_perc)])/nrow(outdat_eng_life)
nrow(outdat_eng_life[is.na(IRI_r_perc)])/nrow(outdat_eng_life)
nrow(outdat_eng_life[is.na(IRI_l_perc)])/nrow(outdat_eng_life)

#####################################################################################
# Check variable age

mean(outdat_eng_life$Age, na.rm=TRUE)
max(outdat_eng_life$Age, na.rm=TRUE)
min(outdat_eng_life$Age, na.rm=TRUE)
quantile(outdat_eng_life$Age, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE) # quartile
quantile(outdat_eng_life$RemainingServiceLife, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE) # quartile

# Compare with survival data
survdat <- lan_surv_dt[is.na(atgdatne_Fikeff) & order(hom_id2,-langd)]
survdat <- survdat[, .SD[1], hom_id2]
stopifnot(round(sum(survdat$langd)/1000, digits=0) == 98467)
stopifnot(round(mean(survdat$age_non0, na.rm=TRUE), digits = 1) == 13.3)
quantile(survdat$age_non0, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE) # quartile

#####################################################################################
# Rename PavementType levels for English output
tab1 <- table(outdat_eng_life$SurfaceClass, outdat_eng_life$PavementType)
tab2 <- table(outdat_eng_life$SurfaceClass)
tab3 <- table(outdat_eng_life$PavementType)
tab4 <- table(outdat_eng$RoadType, outdat_eng$DoU2017)

prop.table(tab1)
prop.table(tab2)
prop.table(tab3)

# Rename factor levels
outdat_eng_life_shape <- copy(outdat_eng_life)
new_pave_levels <- c("Hot mix asphalt (asphalt concrete)", "Seal coat", "Half warm asphalt", "Grouted macadam", "Thin asphalt layer", "Hot mix asphalt (stone mastic)", "Surface dressing on bituminous surface", "Surface dressing on gravel", "Other")
setattr(outdat_eng_life_shape$PavementType,"levels",new_pave_levels)

# Road width to meters
outdat_eng_life_shape[, RoadWidth := RoadWidth/10]
stopifnot(length(outdat_eng_life_shape$Objectid) == 437189) 
print(head(outdat_eng_life_shape))

# Export as shapefile
st_write(outdat_eng_life_shape, "C:/Users/winte/Swedenroads_outputs/sweden_v3_201116.shp", driver="ESRI Shapefile", append=FALSE) 

# Compare last output
firstversion <- st_read("C:/Users/winte/Swedenroads_outputs/sweden_v3_201109.shp")
head(firstversion)
setDT(firstversion)
quantile(firstversion$Age, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE) # quartile
quantile(firstversion$RmnngSL, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE) # quartile

qclasslength_first <- firstversion %>%
              group_by(QClass) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen))
print(qclasslength_first)

qclasslengthtkl_first <- firstversion %>%
              group_by(tkl8, QClass) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen))
print(qclasslengthtkl_first, n=Inf)


qclasslength <- outdat_eng_life %>%
              group_by(QClass) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen))
print(qclasslength)

qclasslengthtkl <- outdat_eng_life %>%
              group_by(tkl8, QClass) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen))
print(qclasslengthtkl, n=Inf)

qclasslength$comp <- (qclasslength$prop - qclasslength_first$prop)
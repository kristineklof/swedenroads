#=================================================================#
#            Swedenroads: create dataset for analysis 
#=================================================================#
source("LoadInstall.R")
deps <- c("sf", "data.table","tidyverse", "lwgeom","survival","fasttime")
LoadInstall(deps)
options(scipen=999)

#source("ImportNVDB_Data.R")
source("SurvivalAnalysisData.r", encoding = 'UTF-8')
source("PrepareEngNVDBOutput.r", encoding = 'UTF-8')
source("ImputeMissingData.r", encoding = 'UTF-8')

nvdb_bel_mat <- st_read("C:/Users/winte/Swedenroads_homo_v2/nvdb_sweden_bel_mat_201024.shp")
head(nvdb_bel_mat)
nvdb_bel_mat[70000:70010,]
nrow(nvdb_bel_mat)
str(nvdb_bel_mat)
missinglan <- nvdb_bel_mat[is.na(lanst_15),]
nrow(missinglan)
sort(unique(missinglan$kommunnr))
length(sort(unique(missinglan$kommunnr)))
sort(unique(nvdb_bel_mat$lanst_15))

# PMS data for survival analysis
vast <- fread("C:/Users/winte/Överlevnadsanalys/Västmanland.csv", encoding = 'Latin-1')
unique(vast$Atgard02)
unique(vast$Beltyp)

# Lan and kommun
lankom <- fread("C:/Users/winte/OneDrive/Documents/salbo.ai/Transportföretagen/Data/LänKommun.csv")

##############################################
## Select and rename columns
outcols <- c("objectid", "barig_64","f_hogst_22","lever_292","adt_f_117","adt_l_115","adt_a_113","matar_121","slitl_25","bredd_156",
            "kateg_380","huvud_13","lanst_15","vagty_41","kommunnr","shape_leng","beldatum","beltyp","tillvmetod","utlmetod",
            "mean_sparm","mean_spa_1","mean_spar1", "mean_spa_2","mean_irih", "mean_iriv","mean_kantd", "geometry")

names_eng <- c("Objectid","BearingCapacityClass","SpeedLimit","DoU2017","AADT","AADT_heavy","AADT_axle","AADT_measurement_year","SurfaceType","RoadWidth",
                "RoadCategory","Road_Number","County","RoadType","Municipality","Length","TreatmentDate","Treatment","ManufactureMethod","PavingMethod",
                "Mean_rut_max17","Mean_rut_max15","Mean_rut_r","Mean_rut_l","Mean_IRI_r","Mean_IRI_l","EdgeDepth","geometry")

names_swe <- c("ID","Bärighetsklass","Hastighet","DoU2017","ÅDT_fordon","ÅDT_tung","ÅDT_axel","ÅDT_mätår","Slitlager","Vägbredd",
                "Vägnummer","Vägtyp","Län_nr","Vägtyp","Kommun_nr","Längd","Beläggningsdatum","Beläggning","TillverkningsMetod","UtläggningsMetod",
                "Avg_sparmax17","Avg_spar_max15","Avg_spar_h","Avg_spar_l","Avg_IRI_h","Avg_IRI_v","Avg_kantdjup","geometry")


outdat_eng <- PrepareHomoNVDB(outcols = outcols, names_eng = names_eng, indat = nvdb_bel_mat, pmsdat = vast, lankom = lankom)
#saveRDS(outdat_eng, "C:/Users/winte/Swedenroads_outputs/outdat_eng.rds")
#outdat_eng <- readRDS("C:/Users/winte/Swedenroads_outputs/outdat_eng.rds")

print(head(outdat_eng))
                 
tab1 <- table(outdat_eng$SurfaceClass, outdat_eng$PavementType)
tab2 <- table(outdat_eng$SurfaceClass)
tab3 <- table(outdat_eng$PavementType)
prop.table(tab1)
prop.table(tab2)
prop.table(tab3)

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

stopifnot(length(levels(outdat_eng$RoadType)) == 5)
stopifnot(length(unique(outdat_eng$tkl8)) == 8)
stopifnot(length(levels(outdat_eng$SurfaceClass)) == 3)
stopifnot(length(levels(outdat_eng$PavementType)) == 9)

#####################################################################################
# Add variable age
CalculateAge <- function(TreatmentDate){
    TreatmentDate <- as.character(TreatmentDate)
    Age <- 2020-as.numeric(substring(TreatmentDate, 1, 4))
    return(Age)
}

itShouldCalculateAge <- function(){
    TreatmentDate <- c(19850912, 19881110, 20190506)
    res <- CalculateAge(TreatmentDate)
    gold <- c(35,32,1)
    stopifnot(res == gold)
}
itShouldCalculateAge()

nrow(outdat_eng[is.na(TreatmentDate)])/(nrow(outdat_eng)) # 13 percent missing treatment date

# Add age
outdat_eng[ , Age := lapply(.SD, CalculateAge), .SDcols = "TreatmentDate"]

mean(outdat_eng$Age, na.rm=TRUE)
max(outdat_eng$Age, na.rm=TRUE)
min(outdat_eng$Age, na.rm=TRUE)
quantile(outdat_eng$Age, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE) # quartile

AddIfAgeMissing <- function(dat){
    # Impute missing age
    dat <- dat %>% group_by(Municipality,tkl8,RoadCategory,RoadType) %>% 
              mutate(Age_imp = round(mean(Age, na.rm = TRUE), digits=0)) %>%
              mutate(Age = coalesce(Age,Age_imp)) %>%
              select(-Age_imp) %>%
              ungroup()
    
    # If still missing, set to median age
    setDT(dat)
    dat[, Age := ifelse(is.na(Age), 9, Age)]

    return(setDT(dat))
}

# Add age
outdat_eng <- AddIfAgeMissing(dat = outdat_eng)

nrow(test[is.na(Age)])/(nrow(test)) # 13 percent missing age

mean(test$Age, na.rm=TRUE)
max(outdat_eng$Age, na.rm=TRUE)
min(outdat_eng$Age, na.rm=TRUE)
quantile(test$Age, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE) # quartile


#####################################################################################
# Rename PavementType levels for English output
levels(outdat_eng$PavementType)
# Rename factor levels
new_pave_levels <- c("Hot mix asphalt (asphalt concrete)", "Seal coat", "Half warm asphalt", "Grouted macadam", "Micro surfacing", "Hot mix asphalt (stone mastic)", "Surface dressing on bituminous surface", "Surface dressing on gravel", "Other")
setattr(outdat_eng$PavementType,"levels",new_pave_levels)




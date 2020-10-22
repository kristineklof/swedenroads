#=================================================================#
#            This file imports homogenous NVDB data
#               and extracts statlig, belagd väg
#=================================================================#

##############################################
## Import NVDB Shapefile
nvdb_homo <- st_read("C:/Users/winte/Swedenroads_homo_v2/Swedenroads_homo_v2_Sverige_shape/Swedenroads_homo_v2_Sverige.shp")
head(nvdb_homo)
nrow(nvdb_homo)
str(nvdb_homo)

itShouldOnlyIncludeUniqueSectionsHomo <- function(nvdb_homo){
    stopifnot(length(unique(nvdb_homo$OBJECTID)) == nrow(nvdb_homo))
    print("OK")
}
itShouldOnlyIncludeUniqueSectionsHomo(nvdb_homo)

##############################################
## Filter väghållare & färjeled & belagd väg & riktning
# Vagha_6: 1: statlig, 2: kommunal, 3: enskild
# Farjeled: 1: färjeled, NA: ej färjeled
# Slitl_152: 1 belagd, 2: grus
setDT(nvdb_homo)
nvdb_nat <- nvdb_homo[Vagha_6 == 1 & Slitl_152 == 1]

##############################################
## Filter väghållare & färjeled & belagd väg
# Vagha_6: 1: statlig, 2: kommunal, 3: enskild
# Farjeled: 1: färjeled, NA: ej färjeled
# Slitl_152: 1 belagd, 2: grus
setDT(nvdb_homo)
nvdb_nat <- nvdb_homo[Vagha_6 == 1 & Slitl_152 == 1]

itShouldOnlyIncludeStatligBelagdEjFärja <- function(nvdb_nat){
    stopifnot(length(nvdb_nat$Farjeled) == 1035039)
    stopifnot(length(nvdb_nat$Vagha_6) == 1035039)
    stopifnot(length(nvdb_nat$Slitl_152) == 1035039)
    print("OK")
}
itShouldOnlyIncludeStatligBelagdEjFärja(nvdb_nat)

itShouldSumToEntireNetworkLength <- function(nvdbnat){
    stopifnot(round(sum(nvdb_nat$Shape_Leng/1000),2) == 90673.76)
    print("OK") 
}
itShouldSumToEntireNetworkLength(nvdb_nat)

itShouldOnlyIncludeUniqueSections <- function(nvdb_nat){
    stopifnot(length(nvdb_nat$OBJECTID) == nrow(nvdb_nat))
    print("OK")
}
itShouldOnlyIncludeUniqueSections(nvdb_nat)

# Export to test shapefile
nrow(nvdb_nat)
st_write(nvdb_nat, "C:/Users/winte/Swedenroads_outputs/nvdb_nat_alla_str_201021.shp", driver="ESRI Shapefile") 

# Extract Norrbotten and export to shapefile
nvdb_norrbotten <- nvdb_nat[Lanst_15 == 25]
nrow(nvdb_norrbotten)
sum(nvdb_norrbotten$Shape_Leng)/1000
st_write(nvdb_norrbotten, "C:/Users/winte/Swedenroads_outputs/nvdb_norrbotten_201018.shp", driver="ESRI Shapefile") 

##############################################
## Extract sections longer than 50 m
nvdb_nat <- nvdb_nat[Shape_Leng >= 50]

itShouldSummarizePartialNetwork<- function(nvdbnat){
    stopifnot(round(sum(nvdb_nat$Shape_Leng/1000),2) == 83840.95)
    stopifnot(nrow(nvdb_nat) == 376025)
    stopifnot(round(mean(nvdb_nat$Shape_Leng),1) == 223)
    stopifnot(round(max(nvdb_nat$Shape_Leng),1) == 10836.9)
    stopifnot(round(min(nvdb_nat$Shape_Leng),1) == 50)
    stopifnot(round(sd(nvdb_nat$Shape_Leng),1) == 231.5)
    print("OK") 
}
itShouldSummarizePartialNetwork(nvdb_nat)

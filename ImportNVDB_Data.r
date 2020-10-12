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

head(nvdb_nat)
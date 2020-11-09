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
ncol(nvdb_homo)
sum(nvdb_homo$Shape_Leng)/1000

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
# Leveranskvalitet not NA & vagkategori not NA & Europavag not NA (icke europavägar har 0, cykelvägar troligtvis NA)
setDT(nvdb_homo)
nvdb_nat <- nvdb_homo[Vagha_6 == 1 & Slitl_152 == 1 & !is.na(Lever_292) &  !is.na(Kateg_380) !is.na(Europ_16)]
sum(nvdb_nat$Shape_Leng)/1000
head(nvdb_nat)

#Filter
dat_fil <- nvdb_nat[Shape_Leng >= 30]
sum(dat_fil$Shape_Leng)/1000
mean(dat_fil$Shape_Leng)
max(dat_fil$Shape_Leng)
sd(dat_fil$Shape_Leng)
nrow(dat_fil)

itShouldOnlyIncludeStatligBelagdEjFärja <- function(nvdb_nat){
    stopifnot(length(nvdb_nat$Farjeled) == 973518)
    stopifnot(length(nvdb_nat$Vagha_6) == 973518)
    stopifnot(length(nvdb_nat$Slitl_152) == 973518)
    print("OK")
}
itShouldOnlyIncludeStatligBelagdEjFärja(nvdb_nat)

itShouldSumToEntireNetworkLength <- function(nvdbnat){
    stopifnot(round(sum(nvdb_nat$Shape_Leng/1000),2) == 87414.92)
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
st_write(nvdb_nat, "C:/Users/winte/Swedenroads_outputs/nvdb_nat_201104.shp", driver="ESRI Shapefile") 

# Extract Norrbotten and export to shapefile
nvdb_norrbotten <- nvdb_nat[Lanst_15 == 25]
nrow(nvdb_norrbotten)
sum(nvdb_norrbotten$Shape_Leng)/1000
st_write(nvdb_norrbotten, "C:/Users/winte/Swedenroads_outputs/nvdb_norrbotten_201018.shp", driver="ESRI Shapefile") 

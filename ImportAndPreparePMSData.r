#=================================================================#
#            This file creates the data for survival 
#                       survival analysis
#=================================================================#

lans <- list()
lannamn <- c("AB","AC","BD","C","D","E","F",
            "G","H","I","K","M","N","O","S","T",
            "U","W","X","Y","Z")
length(lannamn)

for(i in seq_along(lannamn)){
    lans[[i]] <- fread(paste0("C:/Users/winte/Överlevnadsanalys/MedMatning/atg_int_KE_",lannamn[i],".csv"), encoding="Latin-1")
    print(nrow(lans[[i]]))
}

lan_surv <- list()

for(i in seq_along(lans)){
    lan_surv[[i]] <- CreateSurvivalData2019(lans[[i]])
    lan_surv[[i]] <- CreateHomoData(lan_surv[[i]])
    lan_surv[[i]] <- MaintenanceStandard(lan_surv[[i]])
    lan_surv[[i]] <- CreateAgeEvent(lan_surv[[i]])
    print(nrow(lan_surv[[i]]))
}

lan_surv_dt <- rbindlist(lan_surv)
head(lan_surv_dt)
stopifnot(nrow(lan_surv_dt) == 491515)
stopifnot(sum(lan_surv_dt$d-lan_surv_dt$d_uh,na.rm=TRUE) == -9218) 
pmsunique <- lan_surv_dt[, .SD[1], hom_id2]
stopifnot(round(sum(pmsunique$langd)/1000, digits=1) == 103581.7) 
#saveRDS(lan_surv_dt, "C:/Users/winte/Swedenroads_outputs/lan_surv_dt_matning.rds")
lan_surv_dt <- readRDS("C:/Users/winte/Swedenroads_outputs/lan_surv_dt_matning.rds")
head(lan_surv_dt)

# Fiktiv åtgärd
lan_surv_dt[hom_id2 == "25.01382"]
lansdt <- lans_dt[hom_id2 == "25.01382" & atgdat2e_Fikeff == as.Date("2017-01-10")]
nrow(lansdt)

# Fiktiv åtgärd
lan_surv_dt[hom_id2 == "24.00145"]
lansdt <- lans_dt[hom_id2 == "24.00145" & atgdat2e_Fikeff == as.Date("2018-12-27")]
nrow(lansdt)

# Fiktiv åtgärd
lan_surv_dt[hom_id2 == "22.000858"]
nrow(lansdt)

# Select only the last longest section
survdat <- lan_surv_dt[is.na(atgdatne_Fikeff) & order(hom_id2,-langd)]
survdat <- survdat[, .SD[1], hom_id2]
head(survdat)
stopifnot(round(sum(survdat$langd)/1000, digits=2) == 98467.05) 
mean(survdat$langd)
quantile(survdat$langd)
nrow(survdat)

# Save original PMS data
lans_dt <- rbindlist(lans)
head(lans_dt)
stopifnot(nrow(lans_dt) == 3288952)
#saveRDS(lans_dt, "C:/Users/winte/Swedenroads_outputs/lans_dt.rds")

atgdatne <- fasttime::fastPOSIXct(lans_dt$atgdatne_Fikeff)
stopifnot(max(atgdatne,na.rm=TRUE) == "2020-09-02 02:00:00 CEST")
stopifnot(min(atgdatne,na.rm=TRUE) == "1971-06-07 01:00:00 CET")

#############################################################
# Coordinates
lans_cord <- list()

for(i in seq_along(lannamn)){
    lans_cord[[i]] <- fread(paste0("C:/Users/winte/Överlevnadsanalys/Koordinater/koord_",lannamn[i],"_tot.csv"))
    print(nrow(lans_cord[[i]]))
}

lans_cord_dt <- rbindlist(lans_cord)
lans_cord_dt[, hom_id2 := as.character(hom_id2)]
str(lans_cord_dt)

# Survival data with coordinates
surv_with_cords <- left_join(survdat, lans_cord_dt, by = "hom_id2")
setDT(surv_with_cords)
nrow(surv_with_cords)
head(surv_with_cords)

# Add coordinates and projection for ESRI Shapefile
surv_with_cords <- na.omit(surv_with_cords, cols=c("X","Y"))
nrow(surv_with_cords[is.na(X)])
surv_with_cords = st_as_sf(surv_with_cords, coords = c("X", "Y"), crs = 3006)
ncol(surv_with_cords)
nrow(surv_with_cords)

#############################################################
# Export as shapefile - in pieces because of large files
bits <- c(ceiling(nrow(surv_with_cords)/10),rep(NA,8), nrow(surv_with_cords))

for(i in 2:9){
    bits[i] <- bits[i-1] + ceiling(nrow(surv_with_cords)/10)
}

st_write(surv_with_cords[1:bits[1],], "C:/Users/winte/Swedenroads_outputs/survdat_1.shp", driver="ESRI Shapefile", append=FALSE) 

for(i in 1:(length(bits)-1)){
    filename <- paste0("C:/Users/winte/Swedenroads_outputs/survdat_",i+1,".shp")
    st_write(surv_with_cords[(bits[i]+1):bits[i+1],], filename, driver="ESRI Shapefile", append=FALSE) 
}

# Export to PostGIS
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = 'localhost',
  port = 5432,
  user = 'postgres',
  dbname = "swedenroads",
  password = 'strodie',
  bigint = "numeric")

coordinates(surv_with_cords) <- ~X+Y
proj4string(surv_with_cords) = CRS("+init=epsg:3006")

pgInsert(conn, "survdat_201110", surv_with_cords, new.id = "gid")


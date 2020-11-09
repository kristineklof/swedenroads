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
    lans[[i]] <- fread(paste0("C:/Users/winte/Överlevnadsanalys/MedMatning/atg_int_KE_",lannamn[i],".csv"))
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
stopifnot(sum(lan_surv_dt$d-lan_surv_dt$d_uh,na.rm=TRUE) == -41601) 
pmsunique <- lan_surv_dt[, .SD[1], hom_id2]
stopifnot(round(sum(pmsunique$langd)/1000, digits=2) == 103581.7) 

#saveRDS(lan_surv_dt, "C:/Users/winte/Swedenroads_outputs/lan_surv_dt_matning.rds")

# Select only the last section
survdat <- lan_surv_dt[order(hom_id2,-atgdat2e_Fikeff)]
test <- survdat[, .SD[1], hom_id2]
stopifnot(round(sum(test$langd)/1000, digits=2) == 98470.69) 

# Save original PMS data
lans_dt <- rbindlist(lans)
head(lans_dt)
stopifnot(nrow(lans_dt) == 3288952)
#saveRDS(lans_dt, "C:/Users/winte/Swedenroads_outputs/lans_dt.rds")

atgdatne <- fasttime::fastPOSIXct(lans_dt$atgdatne_Fikeff)
stopifnot(max(atgdatne,na.rm=TRUE) == "2020-09-02 02:00:00 CEST")
stopifnot(min(atgdatne,na.rm=TRUE) == "1971-06-07 01:00:00 CET")

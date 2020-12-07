#=================================================================#
#                 Tests of outputs in mainfile
#=================================================================#

# Survival data
itShouldTestSurvivalData <- function(dat){
    pmsunique <- dat[, .SD[1], hom_id2]
    stopifnot(round(sum(pmsunique$langd)/1000) == 103582) 
}

# Homogeneous NVDB data
itShouldTestHomoNVDBdata <- function(dat){
    stopifnot(nrow(dat) == 973518)
    stopifnot(length(unique(dat$objectid)) == 973518)
    stopifnot(round(nrow(dat[is.na(beldatum)])/(nrow(dat)),digits=2) == 0.13) # 13 percent missing treatment date
}

itShouldTestBelaggningReplacement <- function(dat){
    stopifnot(round(nrow(dat[is.na(beldatum)])/(nrow(dat)),digits=2) == 0.11)
}

# English output
itShouldTestEnglishOutput <- function(dat){
    # Missing data
    stopifnot(sum(is.na(dat$County)) == 0)
    stopifnot(sum(is.na(dat$AADT)) == 0)
    stopifnot(sum(is.na(dat$RoadType)) == 0)
    stopifnot(sum(is.na(dat$RoadCategory)) == 0)
    stopifnot(sum(is.na(dat$tkl8)) == 0)
    stopifnot(sum(is.na(dat$Municipality)) == 0)
    stopifnot(sum(is.na(dat$BearingCapacityClass)) == 0)
    stopifnot(sum(is.na(dat$Length)) == 0)
    stopifnot(sum(is.na(dat$SpeedLimit)) == 0)
    stopifnot(sum(is.na(dat$RoadWidth)) == 0)
    stopifnot(sum(is.na(dat$PavementType)) == 0)
    stopifnot(sum(is.na(dat$DoU2017)) == 0)
    stopifnot(sum(is.na(dat$Age)) == 0)

    # Classes/levels/observations
    stopifnot(length(levels(dat$RoadType)) == 5)
    stopifnot(length(unique(dat$tkl8)) == 8)
    stopifnot(length(levels(dat$SurfaceClass)) == 3)
    stopifnot(length(levels(dat$PavementType)) == 9)
    stopifnot(nrow(dat) == 437189)
    stopifnot(nrow(unique(dat, by=c("Objectid")))==437189)

    # Network length
    stopifnot(round(sum(dat$Length,na.rm=TRUE)/1000,0) == 84021)
    stopifnot(round(mean(dat$Length,na.rm=TRUE),0) == 192)
    stopifnot(round(median(dat$Length,na.rm=TRUE),0) == 115)
}

# Swedish output
itShouldTestSwedishOutput <- function(dat){
    stopifnot(sum(is.na(dat$Län_nr)) == 0)
    stopifnot(sum(is.na(dat$ÅDT_fordon)) == 42)
    stopifnot(sum(is.na(dat$Vägtyp)) == 66)
    stopifnot(sum(is.na(dat$Ålder)) == 34629)
    stopifnot(sum(is.na(dat$Beläggning)) == 34629)
    stopifnot(sum(is.na(dat$Bärighetsklass)) == 491)
    stopifnot(sum(is.na(dat$FörväntadLivslängd)) == 36284)
    stopifnot(sum(is.na(dat$Hastighet)) == 535)
    stopifnot(sum(is.na(dat$Vägbredd)) == 55)
    stopifnot(round(sum(dat$Längd[is.na(dat$Beläggning)])/sum(dat$Längd),digits=2) == 0.06)
}

# Condition measurements
itShouldTestConditionMeasurements <- function(dat){
    # Total number of sections above maintenance standard
    stopifnot(round(sum(dat$rut_max17_perc > dat$SP_maint | dat$rut_max15_perc > dat$SP_maint | dat$IRI_r_perc > dat$IRI_maint, na.rm=TRUE)/nrow(dat),4) == 0.0489)

    # Check missing measurements - about 8 percent of road network has missing measurement values
    stopifnot(round(nrow(dat[is.na(rut_max17_perc)])/nrow(dat),3) == 0.082)
    stopifnot(round(nrow(dat[is.na(rut_max15_perc)])/nrow(dat),3) == 0.082)
    stopifnot(round(nrow(dat[is.na(IRI_r_perc)])/nrow(dat),3) == 0.082)

    # Check dates
    stopifnot(as.Date(quantile(unclass(dat$MeasurementDate), probs = c(0,0.05,0.25, 0.5, 0.75, 0.95), na.rm=TRUE), origin = "1970-01-01") == c("1987-05-22","2016-07-01","2018-05-08","2018-08-03","2019-06-26","2019-08-14"))
}

itShouldTestVariableAge <- function(dat, survdat){
    stopifnot(round(mean(dat$Age, na.rm=TRUE),2) == 11.46)
    stopifnot(quantile(dat$Age, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE) == c(0, 4, 9, 15, 89))
    stopifnot(quantile(dat$RemainingServiceLife, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE) == c(-64, 7, 14, 19, 47))

    # Compare with survival data
    survd <- survdat[is.na(atgdatne_Fikeff) & order(hom_id2,-langd)]
    survd <- survd[, .SD[1], hom_id2]
    stopifnot(round(sum(survd$langd)/1000, digits=0) == 98467)
    stopifnot(round(mean(survd$age_non0, na.rm=TRUE), digits = 1) == 13.3)
    stopifnot(quantile(survd$age_non0, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE) == c(1,5,11,19,51))
}

itShouldTestPCI <- function(dat){
    stopifnot(round(mean(dat$PCI),2) == 52.72)
    stopifnot(round(mean(dat$IRI_Index, na.rm=TRUE),2) == 64.73)
    stopifnot(round(mean(dat$Rut_Index, na.rm=TRUE),2) == 60.68)
    stopifnot(round(mean(dat$RMS_Index, na.rm=TRUE),2) == 52.69)
    stopifnot(round(sum(dat$Length)/1000,0) == 84021)

    # Km treated after 2019-01-01
    stopifnot(round(sum(dat[TrtmntD > as.Date("2019-01-01")]$Length)/1000,0) == 5919)

    # IRI Index after 2018
    stopifnot(round(mean(dat[TrtmntD > as.Date("2018-01-01")]$IRI_Index, na.rm=TRUE),1) == 69.2)

    # Rut index after 2018
    stopifnot(round(mean(dat[TrtmntD > as.Date("2018-01-01")]$Rut_Index, na.rm=TRUE),1) == 66)

    # Treatments after measurement date
    stopifnot(round(nrow(dat[TrtmntD > MsrmntD])/nrow(dat),3) == 0.019)

    # No missing PCI
    stopifnot(nrow(dat[is.na(PCI)]) == 0)

    # Quantiles
    stopifnot(quantile(dat$PCI, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE) == c(11,36,54,71,91))
    stopifnot(quantile(dat$IRI_Index, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE) == c(23,47,66,84,100))
    stopifnot(quantile(dat$Rut_Index, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE) == c(28,47,63,76,87))
    stopifnot(quantile(dat$RMS_Index, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE) == c(11,32,53,74,92))
}

itShouldTestSwedenroads <- function(dat){
    stopifnot(round(sum(dat$Längd)/1000,0) == 84022)
    stopifnot(round(mean(dat$Ålder, na.rm=TRUE),1) == 11.5)
    stopifnot(quantile(dat$Ålder, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE) == c(0,4,8,15,89))
    stopifnot(quantile(dat$ÅterståendeLivslängd, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE) == c(-49, 9, 17, 22, 52))
    stopifnot(quantile(dat$TillståndsIndex, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE) == c(11,36,54,71,91))
}






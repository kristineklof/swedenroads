#=================================================================#
#            Prepare homogeneous NVDB data
#=================================================================#

PrepareHomoNVDB <- function(outcols, names_eng, indat, pmsdat, lankom){
  # Extract sections longer than 50 m
  setDT(indat)
  dat_fil <- indat[shape_leng >= 30]

    itShouldSummarizePartialNetwork<- function(nvdbnat){
      stopifnot(round(sum(nvdbnat$shape_leng/1000),2) == 84020.81)
      stopifnot(nrow(nvdbnat) == 437189)
      stopifnot(round(mean(nvdbnat$shape_leng),1) == 192.2)
      stopifnot(round(max(nvdbnat$shape_leng),1) == 10836.9)
      stopifnot(round(min(nvdbnat$shape_leng),1) == 30)
      stopifnot(round(sd(nvdbnat$shape_leng),1) == 223.3)
    }
    itShouldSummarizePartialNetwork(nvdbnat = dat_fil)

  dat <- dat_fil[, ..outcols]
  names(dat) <- names_eng
  setDT(dat)
  print(nrow(dat))

  dat[, MeasurementDate := as.Date(as.character(MeasurementDate), format="%Y%m%d")]

  dat <- AddPavementType(dat = dat, pmsdat = pmsdat)
  print(nrow(dat))

  # Add County
  dat <- AddCountyIfMissing(dat = dat, lankom = lankom)
  print(nrow(dat))

  # Add RoadType
  dat <- AddRoadType(dat = dat)
  print(nrow(dat))

  # If RoadType is missing, impute "ordinary road"
  dat[, RoadType := ifelse(is.na(RoadType), "Ordinary road", RoadType)]
  dat[, RoadType := as.factor(RoadType)]
  dat[, RoadType := relevel(RoadType, "Ordinary road")]
  print(nrow(dat))

  # If RoadCategory is missing, impute "ordinary road"
  dat[, RoadCategory := ifelse(is.na(RoadCategory), 4, RoadCategory)]
  print(nrow(dat))

  # If AADT is missing, impute mean for the municipality & roadtype & roadcategory
  dat <- AddAADTIfMissing(dat = dat)
  print(nrow(dat))

  # Add traffic class, region, and maintenance standard
  dat <- TrafficClass(dat)
  print(nrow(dat))

  # If PavementType is missing, impute the most common class for tkl8, RoadType, RoadCategory
  dat <- AddPavementTypeIfMissing(dat)
  print(nrow(dat))

  # If BearingCapacityClass impute 1
  dat[, BearingCapacityClass := ifelse(is.na(BearingCapacityClass),1, BearingCapacityClass)]
  dat[, BearingCapacityClass := as.factor(BearingCapacityClass)]
  print(nrow(dat))

  # If RoadWidth is missing, impute mean for the municipality & roadtype & roadcategory
  dat <- AddRoadWidthIfMissing(dat = dat) 
  dat[, RoadWidth := RoadWidth*10]
  print(nrow(dat))

  # If SpeedLimit is missing, impute mean for the municipality & roadtype & roadcategory
  dat <- AddSpeedLimitIfMissing(dat = dat) 
  print(nrow(dat))

  # Add maintenance standard
  dat <- MaintenanceStandard(dat)
  print(nrow(dat))

  # Add region
  dat <- AddRegion(dat)
  print(nrow(dat))

  # Add pavement type and surface class
  dat <- AddSurface(dat)
  print(nrow(dat))

  # If SurfaceClass is missing, impute "Hot mix asphalt" if SurfaceType == 1, else "Surface treated"
  dat[, SurfaceClass := as.character(SurfaceClass)]
  dat[, SurfaceClass := ifelse(is.na(SurfaceClass) & SurfaceType == 1, "Hot mix asphalt", SurfaceClass)]
  dat[, SurfaceClass := ifelse(is.na(SurfaceClass), "Surface treated", SurfaceClass)]
  dat[, SurfaceClass := as.factor(SurfaceClass)]
  print(nrow(dat))

  # Add DoU if missing based on County, tkl8, RoadType, RoadCategory
  dat <- AddDOU2017IfMissing(dat)
  print(nrow(dat))

  # Add CZON
  dat[, CZON := ifelse(County == 17 | County >=20, "Central", "South")]
  dat[, CZON := ifelse(County >=23, "North", CZON)]
  print(nrow(dat))

  # Add Age
  dat[ , Age := lapply(.SD, CalculateAge), .SDcols = "TreatmentDate"]
  dat <- AddIfAgeMissing(dat = dat)
  print(nrow(dat))

  dat <- unique(dat, by=c("Objectid"))

  return(setDT(dat))
}

AddRoadType <- function(dat){
  dat[, RoadType := as.character(RoadType)][RoadType == "1", RoadType:= "Motorway"]
  dat[, RoadType := as.character(RoadType)][RoadType == "2", RoadType:= "Undivided motorway"]
  dat[, RoadType := as.character(RoadType)][RoadType == "3", RoadType:= "2+1 road"]
  dat[, RoadType := as.character(RoadType)][RoadType == "4", RoadType:= "4-lane road"]
  dat[, RoadType := as.character(RoadType)][RoadType == "5", RoadType:= "Ordinary road"]
  dat[, RoadType := as.character(RoadType)][RoadType == "6", RoadType:= "2+1 road"]

  return(setDT(dat))
}

# Add region
AddRegion <- function(dat){
  setDT(dat)
  # Create region variable
  dat[, Region := ifelse(County == 24 | County == 25, "Nord", NA)]
  dat[, Region := ifelse(County == 20 | County == 21 | County == 22 | County == 23, "Mitt", Region)]
  dat[, Region := ifelse(County == 1 | County == 9, "Sthlm", Region)]
  dat[, Region := ifelse(County == 3 | County == 4 | County == 5 | County == 18 | County == 19, "Ost", Region)]
  dat[, Region := ifelse(County == 13 | County == 14 | County == 15 | County == 16 | County == 17, "Vast", Region)]
  dat[, Region := ifelse(County == 6 | County == 7 | County == 8 | County == 10 | County == 12, "Syd", Region)]

  return(setDT(dat))
}

AddPavementType <- function(dat, pmsdat){
  # Add Pavement types
  pmsdat <- CreatePavementType(pmsdat)

  #Extract beltyp & pavement type
  agg_atg <- as.data.frame(table(pmsdat$Beltyp, pmsdat$PavementType))
  names(agg_atg) <- c("Treatment","PavementType","Freq")
  agg_atg <- agg_atg %>% group_by(Treatment) %>%
                         top_n(1, Freq)
    
  dat <- left_join(dat, agg_atg[,c("Treatment","PavementType")], by = "Treatment")

  return(setDT(dat))
}

AddSurface <- function(dat){
  setDT(dat)
  # Add surface
  dat[, SurfaceClass := ifelse(PavementType == "Varm" | PavementType == "Varm stenrik" | PavementType == "Ytbehandling på bituminöst underlag" | 
                          PavementType == "Tunnskikt" | PavementType == "Halvvarm" , "Hot mix asphalt", NA)]
  dat[, SurfaceClass  := ifelse(PavementType == "Indränkt makadam" | 
                          PavementType == "Ytbehandling på grus" | PavementType == "Övrigt" | 
                          PavementType == "Försegling", "Surface treated", SurfaceClass)]
  dat[, SurfaceClass  := ifelse(SurfaceType == 5, "Concrete", SurfaceClass)]
  dat[, SurfaceClass := as.factor(SurfaceClass)]
  print(levels(dat$SurfaceClass))

  return(setDT(dat))
}

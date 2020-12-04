#=================================================================#
#            Prepare output for Swedenroads
#=================================================================#

PrepareHomoSwe <- function(outcols, names_eng, names_swe, indat, pmsdat, survdat, lankom){
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

  # BearingCapacityClass
  dat[, BearingCapacityClass := as.factor(BearingCapacityClass)]

  # Add traffic class, region, and maintenance standard
  dat <- TrafficClass(dat)
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

  # Add Age
  dat[ , Age := lapply(.SD, CalculateAge), .SDcols = "TreatmentDate"]
  print(nrow(dat))

  dat <- unique(dat, by=c("Objectid"))

  # Add service life
  dat <- CreateServiceLifeData(dat = dat, survdat=survdat, metod = "AFT", 
                                          distribution = "lognormal", percentil_high = 0.5, 
                                          percentil_low = 0.75,
                                          div = "no", divclass = 5)

  dat[, IRI_mnt := IRI_maint]
  dat[, SP_mant := SP_maint]
  dat[, IRI_r_p := IRI_r_perc]
  dat[, rt_m17_ := rut_max17_perc]
  dat[, rt_m15_ := rut_max15_perc]
  dat[, PrdctSL := PredictedServiceLife]
  dat[, RmnngSL := RemainingServiceLife]
  dat[, RodWdth := RoadWidth/10]
  dat[, RoadTyp := RoadType]
  dat[, TrtmntD := TreatmentDate]
  dat[, MsrmntD := MeasurementDate]

  # Add PCI
  dat <- CreatePCI(dat)
  dat <- PCIClass(dat)

  # Remove columns
  dat[, c("IRI_mnt","SP_mant","IRI_r_p","rt_m17_","rt_m15_","PrdctSL","RmnngSL","RodWdth","RoadTyp","TrtmntD","MsrmntD","IRI_r_p_ceil", "Rut_17_Index", "Rut_15_Index"):=NULL] 

  names(dat) <- names_swe

  # Add and change columns for website
  dat[, Spårdjup := ifelse(Vägbredd > 6, Sparmax17, Spar_max15)]
  dat[, IRI := IRI_h]
  dat[, ÅDT_mätår := substring(as.character(ÅDT_mätår),1,4)]
  dat[, ÅDT_mätår := as.integer(ÅDT_mätår)]
  dat[, Längd := round(Längd, digits = 0)]

    # Change to numeric levels
  dat[, Vägtyp := as.factor(Vägtyp)]
  new_vag_levels <- c("1", "2", "3", "4", "5")
  setattr(dat$Vägtyp,"levels",new_vag_levels)
  dat[, Vägtyp := as.integer(Vägtyp)]

  new_bel_levels <- c("1", "2", "3", "4", "5", "6" , "7", "8", "9")
  setattr(dat$Beläggningstyp,"levels",new_bel_levels)
  dat[, Beläggningstyp := as.integer(Beläggningstyp)]

  dat[, Region := as.factor(Region)]
  new_reg_levels <- c("1", "2", "3", "4", "5", "6")
  setattr(dat$Region,"levels",new_reg_levels)
  dat[, Region := as.integer(Region)]

  return(setDT(dat))
}



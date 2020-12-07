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



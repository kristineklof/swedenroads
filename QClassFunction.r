#=================================================================#
#              This file  creates five quality classes 
#                 based on survival curves percentiles
#=================================================================#

if_else_na <- function(a,b,c){
  a[is.na(a)] <- FALSE
  return(ifelse(a,b,c))
}

SetClasses <- function(dat){
  setDT(dat)

  dat[, QClass := if_else_na(RemainingServiceLife < ceiling(0.25*PredictedServiceLife), 2, NA)]
  dat[, QClass := if_else_na(RemainingServiceLife >= ceiling(0.25*PredictedServiceLife), 3, QClass)]
  dat[, QClass := if_else_na(RemainingServiceLife >= ceiling(0.5*PredictedServiceLife), 4, QClass)]
  dat[, QClass := if_else_na(RemainingServiceLife >= ceiling(0.75*PredictedServiceLife), 5, QClass)]
  dat[, QClass := if_else_na(RemainingServiceLife < 0, 1, QClass)]
  dat[, QClass := if_else_na((RoadType == "Motorway" & tkl8 >= 7 & QClass <3) | (tkl8 >= 7 & QClass <3), 3, QClass)]
  dat[, QClass := if_else_na((RoadWidth > 60 & rut_max17_perc > SP_maint) | (RoadWidth <= 60 & rut_max15_perc > SP_maint) | 
                              IRI_r_perc > IRI_maint | IRI_l_perc > IRI_maint, 1, QClass)]

  return(dat)
}

itShouldSetClasses <- function(){
  testdat <- data.frame(rut_max17_perc = c(NA,1,1,10,1,1,1),
                        rut_max15_perc = c(NA,1,1,5,1,1,1),
                        SP_maint = c(5,5,5,5,5,5,5),
                        IRI_r_perc = c(10,NA,1,2,4,1,1),
                        IRI_l_perc = c(8,NA,1,1,3,1,1),
                        IRI_maint = c(5,5,5,5,5,5,5),
                        RemainingServiceLife = c(7,-3,12,2,11,6,0),
                        RoadWidth = c(60,65,60,70,60,60,60),
                        PredictedServiceLife = c(11,22,15,12,17,22,24),
                        RoadType = c("Ordinary road", "Motorway", "Ordinary road", "Ordinary road", "Ordinary road", "Ordinary road","Ordinary road"),
                        tkl8 = c(1,7,1,8,1,1,1))
  
  res <- SetClasses(testdat)
  #print(res)
  gold <- c(1,3,5,1,4,3,2)
  stopifnot(res$QClass == gold)
}

itShouldSetClasses()

CreateServiceLifeData <- function(dat, survdat, metod = "AFT", distribution, percentil_high, percentil_low, div = "yes", divclass = 5){
  setDT(dat)
  myVector <- c("tkl8", "AADT", "AADT_heavy", "PavementType", "Region", 
                "BearingCapacityClass", "RoadType", "RoadWidth", "SpeedLimit")
  preddat <- dat[, ..myVector]

  if(metod == "Cox"){
      cox_reg <- CoxPH(survdat)

      forecast <- coxed(cox_reg, newdata=preddat, method="npsf")
       # Predict Cox lifetimes
      dat[, PredictedServiceLife := round(forecast$exp.dur, digits=0)]
      dat[, RemainingServiceLife := PredictedServiceLife-Age]
  } else {
    reg_d_uh <- AFT_Regression(survdat,distribution)

    dat[, Pred_50perc := round(predict(reg_d_uh, preddat, type="quantile", p=percentil_high), digits=0)]
    dat[, Pred_75perc := round(predict(reg_d_uh, preddat, type="quantile", p=percentil_low), digits=0)]

    # Predict  lifetimes
    if(div == "yes"){
        dat[, PredictedServiceLife := ifelse(tkl8 <= divclass, Pred_75perc, Pred_50perc)]
        dat[, RemainingServiceLife := PredictedServiceLife-Age]
    } else if(div == "median") {
        dat[, PredictedServiceLife := Pred_50perc]
        dat[, RemainingServiceLife := PredictedServiceLife-Age]
    } else {
        dat[, PredictedServiceLife := Pred_75perc]
        dat[, RemainingServiceLife := PredictedServiceLife-Age]      
    }
  }

  # If PavementType == "Försegling" subtract 8 years to remaining service life, 
  # "Indränkt makadam", "Tunnskikt", or "Övrigt", subtract 5 years to remaining service life, 
  dat <- CoverageServiceLifes(dat)

  # Set classes
  dat <- SetClasses(dat)
  
  return(setDT(dat))
}

CoverageServiceLifes <- function(dat){
  setDT(dat)
 
  dat[, RemainingServiceLife := ifelse(Coverage %in% c("Fläckvis", "Spårlagning", "Fläckvis <20%", "Fläckvis >20%", "Kanthäng", "Fläckvis spårlagning"), round(RemainingServiceLife*0.3,digits=0), RemainingServiceLife)]
  #dat[, RemainingServiceLife:= ifelse(PavementType == "Försegling" & (Coverage %in% c("Fläckvis", "Spårlagning", "Fläckvis <20%", "Fläckvis >20%", "Kanthäng", "Fläckvis spårlagning")), 5, RemainingServiceLife)]
  #dat[, RemainingServiceLife := ifelse(PavementType == "Indränkt makadam" & (Coverage %in% c("Fläckvis", "Spårlagning", "Fläckvis <20%", "Fläckvis >20%", "Kanthäng", "Fläckvis spårlagning")), 5, RemainingServiceLife)]
  #dat[, RemainingServiceLife := ifelse(PavementType == "Övrigt" & (Coverage %in% c("Fläckvis", "Spårlagning", "Fläckvis <20%", "Fläckvis >20%", "Kanthäng", "Fläckvis spårlagning")), 5, RemainingServiceLife)]
  #dat[, RemainingServiceLife := ifelse(PavementType == "Tunnskikt" & (Coverage %in% c("Fläckvis", "Spårlagning", "Fläckvis <20%", "Fläckvis >20%", "Kanthäng", "Fläckvis spårlagning")), 7, RemainingServiceLife)]
  #dat[, RemainingServiceLife := ifelse(PavementType == "Ytbehandling på bituminöst underlag" & (Coverage %in% c("Fläckvis", "Spårlagning", "Fläckvis <20%", "Fläckvis >20%", "Kanthäng", "Fläckvis spårlagning")), 7, RemainingServiceLife)]
  #dat[, RemainingServiceLife := ifelse(PavementType == "Ytbehandling på grus" & (Coverage %in% c("Fläckvis", "Spårlagning", "Fläckvis <20%", "Fläckvis >20%", "Kanthäng", "Fläckvis spårlagning")), 7, RemainingServiceLife)]
  #dat[, RemainingServiceLife := ifelse(PavementType == "Halvvarm" & (Coverage %in% c("Fläckvis", "Spårlagning", "Fläckvis <20%", "Fläckvis >20%", "Kanthäng", "Fläckvis spårlagning")), 7, RemainingServiceLife)]
  #dat[, RemainingServiceLife := ifelse(PavementType == "Varm" & (Coverage %in% c("Fläckvis", "Spårlagning", "Fläckvis <20%", "Fläckvis >20%", "Kanthäng", "Fläckvis spårlagning")), 7, RemainingServiceLife)]
  #dat[, RemainingServiceLife := ifelse(PavementType == "Varm stenrik" & (Coverage %in% c("Fläckvis", "Spårlagning", "Fläckvis <20%", "Fläckvis >20%", "Kanthäng", "Fläckvis spårlagning")), 7, RemainingServiceLife)]

  return(dat)
}

itShouldSetServiceLivesForFläckvisCoverage <- function(){
  testdat <- data.frame(PavementType = c("Försegling","Försegling","Försegling","Tunnskikt","Tunnskikt"),
                        Coverage = c("Fläckvis",NA,"Heltäckande","Spårlagning","Fläckvis >20%"),
                        tkl8 = c(1,1,1,1,7),
                        RemainingServiceLife = c(22,10,10,10,10))
  
  res <- CoverageServiceLifes(testdat)
  #print(res)
  gold <- c(7,10,10,3,3)
  stopifnot(res$RemainingServiceLife == gold)
}

itShouldSetServiceLivesForFläckvisCoverage()




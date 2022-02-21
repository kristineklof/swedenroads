#=================================================================#
#              This file calculates predicted and
#                 remaining service life
#=================================================================#


if_else_na <- function(a,b,c){
  a[is.na(a)] <- FALSE
  return(ifelse(a,b,c))
}

CreateServiceLifeData <- function(dat, survdat, metod = "AFT", 
                                  distribution, percentil_high, 
                                  percentil_low, div = "yes", 
                                  divclass = 5, vars = c("tkl8", "AADT", "AADT_heavy", "PavementType", "Region", 
                                                         "BearingCapacityClass", "RoadType", "RoadWidth", "SpeedLimit")){
  setDT(dat)
  preddat <- dat[, ..vars]
  
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
  #dat <- SetClasses(dat)
  
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




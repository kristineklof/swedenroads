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
                              IRI_r_perc > IRI_maint, 1, QClass)]

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

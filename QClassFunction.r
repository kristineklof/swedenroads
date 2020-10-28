
#=================================================================#
#              This file  creates five quality classes 
#                 based on survival curves percentiles
#=================================================================#

if_else_na <- function(a,b,c){
  a[is.na(a)] <- FALSE
  return(ifelse(a,b,c))
}

SetClasses <- function(dat){
  dat <- as.data.table(dat)
  
  dat[, QClass := if_else_na(RemainingServiceLife >= 0, 2, NA)]
  dat[, QClass := if_else_na(RemainingServiceLife>= 5, 3, QClass)]
  dat[, QClass := if_else_na(RemainingServiceLife >= 10, 4, QClass)]
  dat[, QClass := if_else_na(RemainingServiceLife >= 15, 5, QClass)]
  dat[, QClass := if_else_na(SP_MEAN > SP_maint | IR_MEAN > IRI_maint | RemainingServiceLife < 0, 1, QClass)]
  
  return(dat)
}

itShouldSetClasses <- function(){
  testdat <- data.frame(SP_MEAN = c(NA,1,1,0,10),
                        SP_maint = c(5,5,5,5,5),
                        IR_MEAN = c(10,NA,1,2,4),
                        IRI_maint = c(5,5,5,5,5),
                        RemainingServiceLife = c(3,25,3,10,5),
                        PredictedMedianLife = c(10,))
  
  res <- SetClasses(testdat)
  #print(res)
  gold <- c(1,5,2,4,1)
  stopifnot(res$QClass == gold)
}

itShouldSetClasses()
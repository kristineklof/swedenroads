#=================================================================#
#                      This file creates the 
#          Cox survival regression to predict service lives
#=================================================================#

CoxPH <- function(dat){
    cox_reg <- coxph(Surv(age_non0,d_uh) ~ strata(tkl8) +
                        #AADT +
                        #AADT_heavy +
                        PavementType + 
                        Region +
                        #StoneSize +
                        BearingCapacityClass +
                        RoadType  +
                        RoadWidth +
                        SpeedLimit,
                      data=dat)

    return(cox_reg)
}

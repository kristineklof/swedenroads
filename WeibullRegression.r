#=================================================================#
#                      This file creates the 
#           Weibull survival regression to predict service lives
#=================================================================#

WeibullRegression <- function(dat){
    wei_reg_d_uh <- survreg(Surv(age_non0,d_uh) ~ strata(tkl8) +
                        AADT +
                        AADT_heavy +
                        PavementType + 
                        Region +
                        #StoneSize +
                        BearingCapacityClass +
                        RoadType  +
                        RoadWidth +
                        SpeedLimit,
                      data=dat,
                      dist="weibull")

    return(wei_reg_d_uh)
}

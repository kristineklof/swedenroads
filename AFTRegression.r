#=================================================================#
#                      This file creates the 
#           AFT survival regression to predict service lives
#=================================================================#

AFT_Regression <- function(dat,distribution){
    reg_d_uh <- survreg(Surv(age_non0,d_uh) ~ strata(tkl8) +
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
                      dist=distribution)

    return(reg_d_uh)
}

#=================================================================#
#                      This file is a playground for
#                Survival Analysis with PMS-data
#=================================================================#

# Load data
vast <- fread("C:/Users/winte/Överlevnadsanalys/Västmanland.csv")
dat_2011 <- fread("C:/Users/winte/Google Drive/Kristin & Johan/Kristins HDa/Masteruppsats/SAS-kod Masteruppsats/Dataset/bel_140507US", header=TRUE, 
                           sep=",")
head(vast)
head(dat_2011)
nrow(vast)
length(unique(vast$hom_id2))
vast[order(atgdat2e_Fikeff), head(.SD, 1L)]
vast_homo <- unique(vast, by=c("hom_id2"))
head(vast_homo)

# 2011 and before
survdat_2011 <- CreateSurvivalData2011(dat = dat_2011)
survdat_2011 <- MaintenanceStandard(dat = survdat_2011)
head(survdat_2011)
nrow(survdat_2011)

# Vastmanland
vast_surv <- CreateSurvivalData2019(vast)
vast_surv <- CreateHomoData(vast_surv)
vast_surv <- MaintenanceStandard(vast_surv)
vast_surv <- CreateAgeEvent(vast_surv)
print(vast_surv[1:10,])
nrow(vast_surv)

# AB lan
lan_ab <- fread("C:/Users/winte/Överlevnadsanalys/Sweden/atg_int_AB.csv")
ab_surv <- CreateSurvivalData2019(lan_ab)
ab_surv <- CreateHomoData(ab_surv)
ab_surv <- MaintenanceStandard(ab_surv)
ab_surv <- CreateAgeEvent(ab_surv)
head(ab_surv)

# Weibull regression with Vastmanland
wei_reg <- survreg(Surv(age_non0 ,d) ~ strata(tkl8) +
                        AADT +
                        AADT_heavy +
                        PavementType + 
                        #CZON +
                        StoneSize +
                        BARIGHET +
                        Roadtype  +
                        VAGBR +
                        SpeedLimit,
                      data=vast_surv,
                      dist="weibull")
summary(wei_reg)
exp(wei_reg$coefficients)

# Weibull regression with 2011
wei_reg_2011 <- survreg(Surv(age_non0 ,d) ~ strata(tkl8) +
                        AADT +
                        AADT_heavy +
                        PavementType + 
                        CZON +
                        StoneSize +
                        BARIGHET +
                        Roadtype  +
                        VAGBR +
                        SpeedLimit,
                      data=survdat_2011,
                      dist="weibull")
summary(wei_reg_2011)
exp(wei_reg_2011$coefficients)

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
head(lan_ab)
table(lan_ab$Bindemedel)

ab_surv <- CreateSurvivalData2019(lan_ab)
ab_surv <- CreateHomoData(ab_surv)
ab_surv <- MaintenanceStandard(ab_surv)
ab_surv <- CreateAgeEvent(ab_surv)
head(ab_surv)
nrow(ab_surv)

# Weibull regression test
head(lan_surv_dt)
print(unique(lan_surv_dt$age_non0))
nrow(lan_surv_dt)

# Antal sträckor som överskrider UH standard
over_uh <- lan_surv_dt[((SP_perc > SP_maint | IR_perc > IRI_maint) & d != 1)]
over_uh_sp <- lan_surv_dt[((SP_perc > SP_maint) & d != 1)]
over_uh_iri <- lan_surv_dt[((IR_perc > IRI_maint) & d != 1)]

over_uh_langd <- sum(over_uh$langd)/1000
over_uh_sp_langd <- sum(over_uh_sp$langd)/1000
over_uh_iri_langd <- sum(over_uh_iri$langd)/1000
tot_langd <- sum(lan_surv_dt$langd)/1000
over_uh_langd/tot_langd
over_uh_sp_langd/tot_langd
over_uh_iri_langd/tot_langd

# Without UH standard
wei_reg_d <- survreg(Surv(age_non0 ,d) ~ strata(tkl8) +
                        AADT +
                        AADT_heavy +
                        PavementType + 
                        CZON +
                        #StoneSize +
                        BearingCapacityClass +
                        RoadType  +
                        RoadWidth +
                        SpeedLimit,
                      data=lan_surv_dt,
                      dist="weibull")
summary(wei_reg_d)
exp(wei_reg_d$coefficients)

# With UH standard
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
                      data=lan_surv_dt,
                      dist="weibull")
summary(wei_reg_d_uh)
exp(wei_reg_d_uh$coefficients)
(exp(wei_reg_d_uh$coefficients)-exp(wei_reg_d$coefficients))

# Cox proportional hazards model
cox_reg <- coxph(Surv(age_non0,d_uh) ~ strata(tkl8) +
                        #AADT +
                        #AADT_heavy +
                        PavementType + 
                        CZON +
                        StoneSize +
                        BearingCapacityClass +
                        RoadType  +
                        RoadWidth +
                        SpeedLimit,
                      data=lan_surv_dt)
summary(cox_reg)
exp(cox_reg$coefficients)

# Survival curves by tkl8
fit <- survfit(Surv(age_non0,d_uh) ~ tkl8, data = lan_surv_dt)
ggsurv <- ggsurvplot(fit, data = lan_surv_dt, 
                      surv.median.line = "hv",
                      risk.table = TRUE,
                      risk.table.height = 0.35)
ggsurv$plot <- ggpar(
  ggsurv$plot,
  font.title    = c(10, "bold", "black"),
  font.subtitle = c(10, "bold", "black"),
  font.caption  = c(10, "plain", "black"),
  font.x        = c(8, "plain", "black"),
  font.y        = c(8, "plain", "black"),
  font.xtickslab = c(8, "plain", "black"),
  legend = "right"
)
ggsurv$table <- ggpar(
  ggsurv$table,
  font.title    = c(10, "bold", "black"),
  font.subtitle = c(10, "bold", "black"),
  font.caption  = c(10, "plain", "black"),
  font.x        = c(8, "plain", "black"),
  font.y        = c(8, "plain", "black"),
  font.xtickslab = c(8, "plain", "black"),
  font.table = c(8, "plain", "black")
)
print(ggsurv)



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

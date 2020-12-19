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
                        Region +
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

# With UH standard & coverage
surv_cover <- copy(lan_surv_dt)
surv_cover[, Kat_omf := ifelse(omfattning %in% c("Fläckvis", "Spårlagning", "Fläckvis <20%", "Fläckvis >20%", "Kanthäng", "Fläckvis spårlagning"), "Fläckvis", "Heltäckande")]
surv_cover[, Kat_omf := ifelse(is.na(omfattning) | omfattning == "NULL", NA, Kat_omf)]
surv_cover[, Kat_omf := as.factor(Kat_omf)]
surv_cover[, Kat_omf  := relevel(Kat_omf, "Heltäckande")]
table(surv_cover$Kat_omf, surv_cover$d_uh)

wei_reg_d_uh_omf <- survreg(Surv(age_non0,d_uh) ~ strata(tkl8) +
                        AADT +
                        AADT_heavy +
                        PavementType +
                        Kat_omf + 
                        Region +
                        #StoneSize +
                        BearingCapacityClass +
                        RoadType  +
                        RoadWidth +
                        SpeedLimit,
                      data=surv_cover,
                      dist="weibull")
summary(wei_reg_d_uh_omf)
exp(wei_reg_d_uh_omf$coefficients)


loglog_reg_d_uh <- survreg(Surv(age_non0,d_uh) ~ strata(tkl8) +
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
                      dist="loglogistic")
summary(loglog_reg_d_uh)
exp(loglog_reg_d_uh$coefficients)


log_reg_d_uh <- survreg(Surv(age_non0,d_uh) ~ strata(tkl8) +
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
                      dist="lognormal")
summary(log_reg_d_uh)
exp(log_reg_d_uh$coefficients)

# Compare AIC
extractAIC(loglog_reg_d_uh)
extractAIC(wei_reg_d_uh)
extractAIC(log_reg_d_uh) # lognormal has the lowest AIC

# 2*traffic for motorways, 4-lane roads and 1.5*traffic for 2+1 roads
surv_double_traf <- copy(lan_surv_dt)

MultiplyTraffic <- function(dat){
  dat[, AADT := ifelse(RoadType == "Motorway" | RoadType == "4-lane road", round(AADT*2, digits=0), round(AADT, digits=0))]
  #dat[, AADT := ifelse(RoadType == "2+1 road", round(AADT*1.5, digits=0), round(AADT, digits=0))]
  dat[, AADT_heavy := ifelse(RoadType == "Motorway" | RoadType == "4-lane road", round(AADT_heavy*2, digits=0), round(AADT_heavy, digits=0))]
  #dat[, AADT_heavy := ifelse(RoadType == "2+1 road", round(AADT_heavy*1.5, digits=0), round(AADT_heavy, digits=0))]

  # Traffic class variable
  dat[, tkl8 := ifelse(AADT <250, 1, tkl8)]
  dat[, tkl8 := ifelse(AADT >=250, 2, tkl8)]
  dat[, tkl8 := ifelse(AADT >499, 3, tkl8)]
  dat[, tkl8 := ifelse(AADT >999, 4, tkl8)]
  dat[, tkl8 := ifelse(AADT >1999, 5, tkl8)]
  dat[, tkl8 := ifelse(AADT >3999, 6, tkl8)]
  dat[, tkl8 := ifelse(AADT >7999, 7, tkl8)]
  dat[, tkl8 := ifelse(AADT >11999, 8, tkl8)]

  print(table(dat$BearingCapacityClass))

  dat[, BearingCapacityClass := as.numeric(BearingCapacityClass)]
  dat[, BearingCapacityClass := ifelse(BearingCapacityClass == 4 | BearingCapacityClass == 5, 1, BearingCapacityClass)]
  dat[, BearingCapacityClass := as.factor(BearingCapacityClass)]

  return(dat)
}

surv_double_traf <- MultiplyTraffic(surv_double_traf)
table(surv_double_traf$BearingCapacityClass)

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
                      data=surv_double_traf)
summary(cox_reg)
exp(cox_reg$coefficients)

ed1 <- coxed(cox_reg, method="npsf")
head(ed1$exp.dur)

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


# Weibull survival curves
# Remove pavement type NA
survdata <- readRDS("C:/Users/winte/OneDrive - Salbo Konsult AB/salbo.ai/PEAB Asfalt/lan_surv_dt_matning_na_fix.rds")
head(survdata)
survdata$tkl8 <- as.factor(survdata$tkl8)
survdata$tkl8 <- recode(survdata$tkl8, "1" = "<250", 
                              "2" = "250-499", 
                              "3" = "500-999", 
                              "4" = "1000-1999", 
                              "5" = "2000-3999",
                              "6" = "4000-7999",
                              "7" = "8000-11999",
                              "8" = ">12000")

s <- with(survdata, Surv(age_non0,d_uh) )
fKM <- survfit(s ~ tkl8, data=survdata)
sPar <- survreg(s ~ strata(tkl8) + tkl8, dist='lognormal', data=survdata)

seql <- length(seq(.01,.99,by=.01))
df <- data.frame(y = rep(rev(seq(.01,.99,by=.01)),8),
                 tkl8 = c(rep("<250",seql),
                        rep("250-499",seql),
                        rep("500-999",seql),
                        rep("1000-1999",seql),
                        rep("2000-3999",seql),
                        rep("4000-7999",seql),
                        rep("8000-11999",seql),
                        rep(">12000",seql)),
                time = rep(NA,seql*8))

predlist <- list()
tkl8list <- c("<250", "250-499", "500-999", "1000-1999", "2000-3999", "4000-7999", "8000-11999", ">12000")
for(i in 1:8){
  predlist[[i]] = predict(sPar, newdata=list(tkl8=tkl8list[i]),type="quantile",p=seq(.01,.99,by=.01))
}

df$time <- unlist(predlist)
nrow(df)
head(df)

theme <- theme(axis.line = element_line(colour = "black"),
             panel.grid.major = element_line(colour = "grey90"),
             panel.grid.minor = element_line(colour = "grey90"),
             panel.border = element_blank(),
             panel.background = element_blank(),
            legend.title = element_text(size=16),
             legend.text = element_text(size=16),
             axis.title.y = element_text(size=16),
             axis.text.y = element_text(size=16),
             axis.title.x = element_text(size=16),
             axis.text.x = element_text(size=16)) 

p = ggsurvplot(fKM, data = survdata, 
              censor = FALSE,
              color = "strata", 
              linetype = "solid", 
              risk.table = FALSE,
              break.x.by = 5, 
              ggtheme=theme,
              risk.table.title = "Antal vägsträckor som ännu ej fått en underhållsåtgärd",
              legend.labs = c("<250", "250-499", "500-999", "1000-1999", "2000-3999", "4000-7999", "8000-11999", ">12000"),
              legend = c("right"),
              legend.title = "Trafikmängd (fordon/dygn)",
               xlab = "Ålder (år)",
               ylab = "Sannolikhet att ej behöva underhåll") 
print(p)
p$plot <- p$plot + geom_hline(yintercept=0.25, linetype="solid", size = 2) + geom_hline(yintercept=0.5, linetype="dashed", size = 2)
#p$plot = p$plot + geom_line(data=df, aes(x=time, y=y, group=tkl8))
print(p) 

# Expected lifetimes for 11 family classes
s_exp <- survreg(s ~ strata(tkl8) + tkl8 + PavementType + RoadType, dist='lognormal', data=survdata)

newdata <- data.frame(tkl8 = c("<250",
                                "1000-1999",
                                "1000-1999",
                                "2000-3999",
                                "2000-3999",
                                "8000-11999",
                                "4000-7999",
                                ">12000",
                                "4000-7999",
                                "8000-11999",
                                ">12000"),
                     PavementType = c("Ytbehandling på bituminöst underlag",
                                      "Ytbehandling på bituminöst underlag",
                                      "Halvvarm",
                                      "Varm", 
                                      "Varm",
                                      "Varm",
                                      "Varm",
                                      "Varm",
                                      "Varm stenrik",
                                       "Varm stenrik",
                                        "Varm stenrik"),
                     RoadType = c("Ordinary road",
                                  "Ordinary road",
                                  "Ordinary road",
                                  "Ordinary road",
                                  "2+1 road",
                                  "2+1 road",
                                  "Motorway",
                                  "Motorway",
                                  "Ordinary road", 
                                  "2+1 road",
                                  "Motorway"))

unique(survdata$PavementType)
EstimatedServiceLife <- c()

EstimatedServiceLife <- predict(s_exp, newdata=newdata,type="quantile",p=0.75)
familyclasses <- cbind(newdata,round(EstimatedServiceLife,0))


#Plot survival for both sexes and show exponential hazard estimates
f <- npsurv(s ~ tkl8, data= lan_surv_dt)
survplot(f, aehaz=TRUE)
#Check for log-normal and log-logistic fits
survplot(f, fun=qnorm, ylab="Inverse Normal Transform")
survplot(f, fun=function(y)log(y/(1-y)), ylab="Logit S(t)")
survplot(f, logt=TRUE, loglog=TRUE, data=) 
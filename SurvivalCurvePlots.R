#=================================================================#
#           This file produces survival curve plots
#         and expected lifetimes for family classes
#=================================================================#

# Lognormal survival curves
# Remove pavement type NA
survdata <- readRDS("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/PEAB Asfalt/lan_surv_dt_matning_na_fix.rds")
head(survdata)
table(survdata$omfattning)
max(survdata$atgdatne_Fikeff, na.rm=TRUE)

#survdata <- survdata %>% dplyr::filter(omfattning == "Heltäckande")

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
               legend.title = element_text(size=14),
               legend.text = element_text(size=14),
               axis.title.y = element_text(size=14),
               axis.text.y = element_text(size=14),
               axis.title.x = element_text(size=14),
               axis.text.x = element_text(size=14)) 

p = ggsurvplot(fKM, data = survdata, 
               censor = FALSE,
               color = "strata",
               linetype = "solid", 
               risk.table = TRUE,
               fontsize = 4,
               break.x.by = 5, 
               ggtheme=theme,
               risk.table.title = "Antal vägsträckor som ännu ej fått en underhållsåtgärd",
               legend.labs = c("250", "250-499", "500-999", "1000-1999", "2000-3999", "4000-7999", "8000-11999", "12000"),
               legend = c("none"),
               legend.title = "Trafikmängd (fordon/dygn)",
               xlab = "Ålder (år)",
               ylab = "Sannolikhet att ej behöva underhåll") 

p = ggsurvplot(fKM, data = survdata, 
               censor = FALSE,
               color = "strata", 
               linetype = "solid", 
               risk.table = FALSE,
               break.x.by = 5, 
               ggtheme=theme,
               legend.labs = c("<250", "250-499", "500-999", "1000-1999", "2000-3999", "4000-7999", "8000-11999", ">12000"),
               legend = c("right"),
               legend.title = "AADT (traffic/day)",
               xlab = "Age (years)",
               ylab = "Probability of not yet recieved maintenance") 
print(p)
p$plot <- p$plot + 
  geom_hline(yintercept=0.25, linetype="solid", size = 2) +
  geom_hline(yintercept=0.5, linetype="dashed", size = 2)
p$plot = p$plot + 
  geom_line(data=df, 
            aes(x=time, y=y, 
                color=tkl8), size=1)
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

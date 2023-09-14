#=================================================================#
#                 Descriptove statistics for the report
#=================================================================#

swedt_PCI <- st_read(paste0(datapath,"sweden_v3_pci201206.shp")) 
setDT(swedt_PCI)

swedt_PCI <- PCIClass(swedt_PCI)
swedt_PCI[, PCIClass := as.factor(PCIClass)]
str(swedt_PCI)

#############################################################################
#  Förväntad livslängd för olika beläggningar, ÅDT samt andel ÅDT tung
age_bel_adt <- swedt_PCI %>%
  mutate(Andel_tungtr = ifelse(AADT_hv/AADT >= 0.15, ">= 15 %", "< 15 %")) %>%
  mutate(tkl8 = factor(tkl8, levels = c("1","2","3","4","5", "6", "7", "8"))) %>%
  mutate(tkl8 = recode(tkl8, 
                       "1" ="<250", 
                       "2" = "250-499", 
                       "3" = "500-999", 
                       "4" = "1000-1999", 
                       "5" = "2000-3999",
                       "6" = "4000-7999",
                       "7" = "8000-11999",
                       "8" = ">=12000")) %>%
  mutate(PvmntTy = factor(PvmntTy)) %>%
  mutate(PvmntTy = recode(PvmntTy, 
                       "Grouted macadam" ="Indränkt makadam", 
                       "Half warm asphalt" = "Mjukbitumenbundet grus (MJOG)", 
                       "Hot mix asphalt (asphalt concrete)" = "Asfaltsbetong (ABT)", 
                       "Hot mix asphalt (stone mastic)" = "Asfaltsbetong, stenrik (ABS)", 
                       "Seal coat" = "Försegling",
                       "Other" = "Övrigt",
                       "Thin asphalt layer" = "Tunnskiktsbeläggning",
                       "Surface dressing on bituminous surface" = "Ytbehandling, bituminös",
                       "Surface dressing on gravel" = "Ytbehandling, grus")) %>%
  group_by(tkl8, Andel_tungtr, PvmntTy) %>%
  summarise(forv = round(mean(PrdctSL, na.rm=TRUE),0),
            medel = round(mean(Prd_50p, na.rm=TRUE),0))
  
print(age_bel_adt, n=Inf)
write.xlsx(age_bel_adt, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Kommunworkshop/alder_adt_bel.xlsx",
           overwrite = TRUE)

#############################################################################
#  Studera sträckor med index under 20 som inte överskrider underhållstandard
# eller åldersgränsen

low <- swedt_PCI[PCI <= 20 & !(RMS_Ind <= 20 | IRI_Ind  <= 20 | Rt_Indx <= 20)]
low <- swedt_PCI[PCI <= 20 & !(Age >= PrdctSL  | IRI_r_p  >= IRI_mnt | rt_m17_ >= SP_mant)]
nrow(low)
head(low)
tail(low)

#############################################################################
#  Beräkna medelindex för alla vägar över 20
mean(swedt_PCI[PCI > 20]$PCI)

#############################################################################
#  Vägytemätningsdatum
as.Date(quantile(unclass(swedt_PCI$MsrmntD), probs = c(0.02, 0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE), origin = "1970-01-01")

#############################################################################
# Kolla utfall av underhållsåtgärder 2019
uh_atg2019 <- QualitativeStatsSingleGroup(swedt_PCI[TrtmntD > "2019-01-01"], quo(Tretmnt), quo(Length))
uh_atg2019 <- uh_atg2019 %>% arrange(desc(prop)) %>% top_n(n=10) 

#############################################################################
# Fördelning trafikklass mellan olika index
index_below20 <- swedt_PCI %>%
              group_by(tkl8) %>%
              summarise(grouplen = sum(Length)/1000,
                        IRI_Index_below = sum(Length[IRI_Ind <= 20], na.rm = TRUE)/1000/grouplen,
                        Rut_Index_below = sum(Length[Rt_Indx <= 20], na.rm = TRUE)/1000/grouplen,
                        RMS_Index_below = sum(Length[RMS_Ind <= 20 & RoadTyp == "Ordinary road"], na.rm = TRUE)/1000/grouplen,
                        Tot_Index_below = sum(Length[PCI <= 20], na.rm = TRUE)/1000/grouplen,
                        IRI_lenbelow = IRI_Index_below*grouplen,
                        Rut_lenbelow = Rut_Index_below*grouplen,
                        RMS_lenabove = RMS_Index_below*grouplen)

print(index_below20, n=Inf)

#############################################################################
# Correlation between IRI and spårdjup
inds <- c("Rt_Indx","IRI_Ind")
cor(swedt_PCI[RoadTyp == "Ordinary road",..inds], use="complete.obs", method="pearson")
cor(swedt_PCI[RoadTyp != "Ordinary road",..inds], use="complete.obs", method="pearson")

values <- c("rt_m17_","rt_m15_","IRI_r_p")
cor(swedt_PCI[RoadTyp == "Ordinary road",..values], use="complete.obs", method="pearson")
cor(swedt_PCI[RoadTyp != "Ordinary road",..values], use="complete.obs", method="pearson")

# Hur många lågtrafikerade vägar överskrider spårdjup resp IRI uh standard?
rut_tkl <- QualitativeStatsSingleGroup(swedt_PCI[Rt_Indx <= 20], quo(tkl8), quo(Length))
iri_tkl <- QualitativeStatsSingleGroup(swedt_PCI[IRI_Ind <= 20], quo(tkl8), quo(Length))

#############################################################################
# Quantative descriptive statistics for report
DescriptiveStats(swedt_PCI$Length)
DescriptiveStats(swedt_PCI$Age)
DescriptiveStats(swedt_PCI$AADT_hv)
DescriptiveStats(swedt_PCI$AADT)

# Qualitative descriptive statistics for report
QualitativeStatsSingleGroup(swedt_PCI, quo(tkl8), quo(Length))
QualitativeStatsSingleGroup(swedt_PCI, quo(PvmntTy), quo(Length))
QualitativeStatsSingleGroup(swedt_PCI, quo(DoU2017), quo(Length))
QualitativeStatsSingleGroup(swedt_PCI, quo(RoadTyp), quo(Length))
QualitativeStatsSingleGroup(swedt_PCI, quo(RdCtgry), quo(Length))
QualitativeStatsSingleGroup(swedt_PCI, quo(BrngCpC), quo(Length))

# Med trafikarbete
QualitativeStatsSingleGroupTA(swedt_PCI,quo(DoU2017),quo(Length),quo(AADT))

# Above maintenance standard
maintstandlengthdou <- swedt_PCI %>%
              group_by(DoU2017) %>%
              summarise(grouplen = sum(Length)/1000,
                        percabove= sum(Length[(rt_m17_ > SP_mant & RodWdth > 6) | (rt_m15_ > SP_mant & RodWdth <= 6)| IRI_r_p > IRI_mnt], na.rm = TRUE)/1000/grouplen,
                        lenabove= percabove*grouplen)
print(maintstandlengthdou)
sum(maintstandlengthdou$lenabove)/sum(maintstandlengthdou$grouplen)

# Older than expected lifetime
age_above <- swedt_PCI %>%
              group_by(tkl8) %>%
              summarise(mean_PredSL = round(mean(PrdctSL,na.rm=TRUE),0),
                        grouplen = sum(Length)/1000,
                        percabove= sum(Length[Age > PrdctSL & RoadTyp == "Ordinary road"], na.rm = TRUE)/1000/grouplen,
                        lenabove= percabove*grouplen)
print(age_above)

#################################################################
# PCI statistics for report
quantile(swedt_PCI$PCI, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE) # quartile

DescriptiveStats(swedt_PCI$PCI)

pclasslength <- swedt_PCI %>%
              group_by(PCIClass) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen))
print(pclasslength)

# PCI barchart region
fill <- c("#20AC65", "#71C94B","#FABF20","#F2203E","#C40A3B")

cond_p <- swedt_PCI %>%
  mutate(Region = recode(Region, Ost="Öst", Vast="Väst")) %>%
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  group_by(Region, PCIClass) %>%
  summarise(grouplen = sum(Length)/1000) %>%
  mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = Region, y = percentage, fill = PCIClass, label = paste0(round(100*percentage,digits=0)," %"))) +
    geom_bar(position = 'fill', stat = 'identity') +
    scale_fill_manual(values=fill) +
    labs(y="", x = "") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position="right", legend.direction="vertical",
                   legend.title = element_blank(), 
                   legend.text=element_text(size=16), 
                   axis.text=element_text(size=16)) +
    geom_text(size = 3, position = position_stack(vjust = 0.5))

print(cond_p)

# PCI barchart vägtyp
cond_v <- swedt_PCI %>%
  mutate(RoadTyp = as.factor(RoadTyp)) %>%
  mutate(RoadTyp = recode(RoadTyp, "Ordinary road" = "Vanlig väg", "2+1 road" = "2+1 väg", "Undivided motorway" = "Motortrafikled", "Motorway" = "Motorväg", "4-lane road" = "4-fälts väg")) %>%
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  group_by(RoadTyp, PCIClass) %>%
  summarise(grouplen = sum(Length)/1000) %>%
  mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = RoadTyp, y = percentage, fill = PCIClass, label = paste0(round(100*percentage,digits=0)," %"))) +
    geom_bar(position = 'fill', stat = 'identity') +
    scale_fill_manual(values=fill) +
    labs(y="", x = "") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position="right", legend.direction="vertical",
                   legend.title = element_blank(), 
                   legend.text=element_text(size=16), 
                   axis.text=element_text(size=16)) +
    geom_text(size = 3, position = position_stack(vjust = 0.5))

print(cond_v)

#####################################################
# Plot index curve
PlotIndexCurve <- function(cutoff, actual, x_lab){
    df <- data.frame(x = (cutoff - actual)/cutoff)
    index <- function(x) 100*exp(--log(0.2)*x)

  p <- ggplot(data = df, mapping = aes(x = x)) + 
                stat_function(fun = index, linewidth = 1) +
                geom_hline(yintercept=20, linetype="dashed", color = "red", size = 2) +
                theme(axis.text.x = element_text(size=16),
                      axis.text.y = element_text(size=16),
                      axis.title = element_text(size=14)) +
                scale_x_continuous(name=x_lab, limits=c(0, 4)) +
                scale_y_continuous(name="IRI-index", breaks=seq(0,100,20))


  return(p)              
}

# IRI
cutoff <- (swedt_PCI$IRI_mnt)
IRI_ceil <- if_else_na(swedt_PCI$IRI_r_p < 1, ceiling(swedt_PCI$IRI_r_p), swedt_PCI$IRI_r_p)
actual <- swedt_PCI$IRI_mnt - IRI_ceil

p <- PlotIndexCurve(cutoff, actual, x_lab = "Relativt IRI-värde")
print(p)

# IRI
cutoff <- (swedt_PCI$IRI_mnt - 1)
IRI_ceil <- if_else_na(swedt_PCI$IRI_r_p < 1, ceiling(swedt_PCI$IRI_r_p), swedt_PCI$IRI_r_p)
actual <- swedt_PCI$IRI_mnt - IRI_ceil

p <- PlotIndexCurve(cutoff, actual, x_lab = "Mätvärde/Mätvärde underhållsstandard")
print(p)

# Index scatterplot
PlotIndexScatter <- function(df, by_index = FALSE, x_lab){
 if(by_index){
    #df <- df[sample(nrow(df), 150000),]
   df <- df[order(-df$PCI),]
   df$VägID <- 1:nrow(df)

    p <- ggplot(df, aes(x = VägID, y = PCI)) +
            geom_point(size = 1) +
            geom_hline(yintercept=20, linetype="dashed", color = "red", size = 2) +
            theme(axis.text.x = element_text(size=16),
                      axis.text.y = element_text(size=16),
                      axis.title = element_text(size=14)) +
            scale_x_continuous(name=x_lab) +
            scale_y_continuous(name="Indexvärde", breaks=seq(0,100,20))
 } else {
   df <- df[sample(nrow(df), 20000),]
  df <- df %>% mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
               mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig"))

    p <- ggplot(df, aes(x = Age, y = PCI)) +
            geom_point(aes(colour=PCIClass), size=1) + 
            geom_hline(yintercept=20, linetype="solid", color = "black", size = 2) +
            theme(axis.text.x = element_text(size=16),
                      axis.text.y = element_text(size=16),
                      axis.title = element_text(size=16),
                      legend.text=element_text(size=16),
                      legend.title = element_blank()) +
            scale_color_manual(values=c("#20AC65", "#71C94B","#FABF20","#F2203E","#C40A3B")) +
            scale_x_continuous(name=x_lab, limits=c(0, 50)) +
            scale_y_continuous(name="Indexvärde", breaks=seq(0,100,20)) 
            #geom_hline(yintercept=20, linetype="solid", size = 1.5, color="#F2203E") +
            #geom_hline(yintercept=40, linetype="solid", size = 1.5, color="#FABF20") +
            #geom_hline(yintercept=60, linetype="solid", size = 1.5, color="#71C94B") +
            #geom_hline(yintercept=80, linetype="solid", size = 1.5, color="#20AC65")
 }
  return(p)              
}

p <- PlotIndexScatter(df = swedt_PCI, x_lab = "Ålder")
print(p)

p <- PlotIndexScatter(df = swedt_PCI, by_index=TRUE, x_lab = "VägID")
print(p)

# Look at sample
samp_swedt <- swedt_PCI[sample(nrow(swedt_PCI), 20000),]
setDT(samp_swedt)
head(samp_swedt)
samp_swedt[Age == 13 & PCI > 70]

##############################################################
# Plot degradation curve
swedt_PCI[60000,]
swedt_PCI[Objectd == 781887]
age <- c(35,25,0)
age_l <- c(25,19,0)
age2 <- c(8,5,0)
age2_l <- c(5,3.5,0)
ind <- c(0,20,100)

PlotDegradationCurve <- function(x, y){
    df <- as.data.frame(x = x, y = y)
    pol <- polyfit(x, y, 2)
    index <- function(x) pol[1]*x^2 + pol[2]*x + pol[3]

  p <- ggplot(data = df, mapping = aes(x = x)) + 
                stat_function(fun = index, size = 1) +
                #geom_hline(yintercept=20, linetype="dashed", color = "red", size = 2) +
                theme(axis.text.x = element_text(size=16),
                      axis.text.y = element_text(size=16),
                      axis.title = element_text(size=14)) +
                scale_x_continuous(name="Ålder") +
                scale_y_continuous(name="Tillståndsindex", breaks=seq(0,100,20))

  return(p)              
}

d1 <- PlotDegradationCurve(x=age, y=ind)
d2 <- PlotDegradationCurve(x=age2, y=ind)
grid.arrange(d1, d2, ncol=2)

d1_l <- PlotDegradationCurve(x=age_l, y=ind)
d2_l <- PlotDegradationCurve(x=age2_l, y=ind)
grid.arrange(d1_l, d2_l, ncol=2)

##############################################################
# Sammanställning missing data
outdat_swe <- readRDS("C:/Users/winte/Swedenroads_outputs/outdat_swe.rds")
setDT(outdat_swe)

sum(outdat_swe$Längd[is.na(outdat_swe$Beläggning)])/sum(outdat_swe$Längd)
sum(outdat_swe$Längd[is.na(outdat_swe$Beläggning)])/1000

sum(outdat_swe$Längd[is.na(outdat_swe$ÅDT_fordon)])/sum(outdat_swe$Längd)
sum(outdat_swe$Längd[is.na(outdat_swe$ÅDT_fordon)])/1000

sum(outdat_swe$Längd[is.na(outdat_swe$ÅDT_tung)])/sum(outdat_swe$Längd)
sum(outdat_swe$Längd[is.na(outdat_swe$ÅDT_tung)])/1000

sum(outdat_swe$Längd[is.na(outdat_swe$Vägtyp)])/sum(outdat_swe$Längd)
sum(outdat_swe$Längd[is.na(outdat_swe$Vägtyp)])/1000

sum(outdat_swe$Längd[is.na(outdat_swe$Bärighetsklass)])/sum(outdat_swe$Längd)
sum(outdat_swe$Längd[is.na(outdat_swe$Bärighetsklass)])/1000

sum(outdat_swe$Längd[is.na(outdat_swe$Hastighet)])/sum(outdat_swe$Längd)
sum(outdat_swe$Längd[is.na(outdat_swe$Hastighet)])/1000

sum(outdat_swe$Längd[is.na(outdat_swe$Vägbredd)])/sum(outdat_swe$Längd)
sum(outdat_swe$Längd[is.na(outdat_swe$Vägbredd)])/1000

sum(outdat_swe$Längd[is.na(outdat_swe$DoU2017)])/sum(outdat_swe$Längd)
sum(outdat_swe$Längd[is.na(outdat_swe$DoU2017)])/1000

sum(outdat_swe$Längd[is.na(outdat_swe$IRI)])/sum(outdat_swe$Längd)
sum(outdat_swe$Längd[is.na(outdat_swe$IRI)])/1000

sum(outdat_swe$Längd[is.na(outdat_swe$Spårdjup)])/sum(outdat_swe$Längd)
sum(outdat_swe$Längd[is.na(outdat_swe$Spårdjup)])/1000

##################################################################################
# Sections above maintenance standard

outdat_eng <- readRDS("C:/Users/winte/Swedenroads_outputs/outdat_eng.rds")

maintstandlengthtkl <- outdat_eng %>%
              group_by(tkl8) %>%
              summarise(grouplen = sum(Length)/1000,
                        lenabove= sum(Length[(rut_max17_perc > SP_maint & RoadWidth > 60) | (rut_max15_perc > SP_maint & RoadWidth <= 60) | IRI_r_perc > IRI_maint], na.rm = TRUE)/1000/grouplen)
print(maintstandlengthtkl)

maintstandlengthdou <- outdat_eng %>%
              group_by(DoU2017) %>%
              summarise(grouplen = sum(Length)/1000,
                        lenabove= sum(Length[(rut_max17_perc > SP_maint & RoadWidth > 60) | (rut_max15_perc > SP_maint & RoadWidth <= 60)| IRI_r_perc > IRI_maint], na.rm = TRUE)/1000/grouplen)
print(maintstandlengthdou)









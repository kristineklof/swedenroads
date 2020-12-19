#=================================================================#
#                 Descriptove statistics for the report
#=================================================================#

swedt_PCI <- st_read( "C:/Users/winte/Swedenroads_outputs/sweden_v3_pci201206.shp") 
setDT(swedt_PCI)
pci2030 <- read.xlsx("C:/Users/winte/Swedenroads_outputs/2030_PCI_20201218.xlsx")
head(pci2030)
names(pci2030) <- c("Objectd","Year","PCI")
setDT(pci2030)

swedt_PCI <- PCIClass(swedt_PCI)
swedt_PCI[, PCIClass := as.factor(PCIClass)]
str(swedt_PCI)

pci2030 <- PCIClass(pci2030)
pci2030[, PCIClass_2030 := as.factor(PCIClass)]
pci2030[, PCIClass := NULL]
str(pci2030)
head(pci2030)

cols <- c("Objectd","Length","PCIClass","AADT","RoadTyp")
pci2030 <- pci2030[swedt_PCI[, ..cols], on = 'Objectd']

#############################################################################
# Correlation between IRI and spårdjup
inds <- c("Rt_Indx","IRI_Ind")
cor(swedt_PCI[RoadTyp == "Ordinary road",..inds], use="complete.obs", method="pearson")
cor(swedt_PCI[RoadTyp != "Ordinary road",..inds], use="complete.obs", method="pearson")

values <- c("rt_m17_","rt_m15_","IRI_r_p")
cor(swedt_PCI[RoadTyp == "Ordinary road",..values], use="complete.obs", method="pearson")
cor(swedt_PCI[RoadTyp != "Ordinary road",..values], use="complete.obs", method="pearson")

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

# Roadlength under PCI 5 2020 vs 2030
pci_5_2020 <- swedt_PCI %>%
              group_by(RoadTyp) %>%
              summarise(grouplen = sum(Length)/1000,
                        percbelow= sum(Length[PCI <= 5], na.rm = TRUE)/1000/grouplen,
                        lenbelow= percbelow*grouplen)
print(pci_5_2020)
sum(pci_5_2020$lenbelow)/sum(pci_5_2020$grouplen)

# Roadlength under PCI 5 2030
pci_5_2030 <- pci2030 %>%
              group_by(RoadTyp) %>%
              summarise(grouplen = sum(Length)/1000,
                        percbelow= sum(Length[PCI <= 5])/1000/grouplen,
                        lenbelow= percbelow*grouplen)
print(pci_5_2030)
sum(pci_5_2030$lenbelow)/sum(pci_5_2030$grouplen)

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

###############################################################################
# PCI 2020 vs 2030 barchart traffic
df_long <- melt(data = pci2030, 
                id.vars = c("Objectd","AADT","Length"),
                measure.vars = c("PCIClass_2030", "PCIClass"),
                variable.name = "Y",
                value.name = "PCIClass")
head(df_long)

# Traffic class variable
df_long[, Trafikklass := ifelse(AADT <500, "<500", NA)]
df_long[, Trafikklass := ifelse(AADT >1000, "500-3999", NA)]
df_long[, Trafikklass := ifelse(AADT >4000, "4000-7999", NA)]
df_long[, Trafikklass := ifelse(AADT <8000, "<500", NA)]

# Trafikarbete
cond_y <- df_long %>%
  mutate(Y = as.factor(Y)) %>%
  mutate(Y = recode(Y, "PCIClass" = "2020", "PCIClass_2030" = "2030")) %>%
  mutate(Y = factor(Y, levels = c('2020', '2030'))) %>%
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  group_by(Y, PCIClass) %>%
  summarise(grouplen = sum(Length*AADT)/1000) %>%
  mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = Y, y = percentage, fill = PCIClass, label = paste0(round(100*percentage,digits=0)," %"))) +
    geom_bar(position = 'fill', stat = 'identity') +
    scale_fill_manual(values=fill) +
    labs(y="", x = "") +
    #ggtitle("Trafikarbete") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position="none", 
          axis.text=element_text(size=16),
          plot.title = element_text(size = 28, face = "bold")) +
    geom_text(size = 3, position = position_stack(vjust = 0.5))

# Väglängd
cond_yv <- df_long %>%
  mutate(Y = as.factor(Y)) %>%
  mutate(Y = recode(Y, "PCIClass" = "2020", "PCIClass_2030" = "2030")) %>%
  mutate(Y = factor(Y, levels = c('2020', '2030'))) %>%
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  group_by(Y, PCIClass) %>%
  summarise(grouplen = sum(Length)/1000) %>%
  mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = Y, y = percentage, fill = PCIClass, label = paste0(round(100*percentage,digits=0)," %"))) +
    geom_bar(position = 'fill', stat = 'identity') +
    scale_fill_manual(values=fill) +
    labs(y="", x = "") +
    #ggtitle("Väglängd") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "none",
          axis.text=element_text(size=16),
          plot.title = element_text(size = 28, face = "bold")) +
    geom_text(size = 3, position = position_stack(vjust = 0.5))

# Length vs trafikarbete
grid.arrange(cond_yv, cond_y, ncol=2)

#####################################################
# Plot index curve
PlotIndexCurve <- function(cutoff, actual, x_lab){
    df <- data.frame(x = (cutoff - actual)/cutoff)
    index <- function(x) 100*exp(--log(0.2)*x)

  p <- ggplot(data = df, mapping = aes(x = x)) + 
                stat_function(fun = index, size = 1) +
                geom_hline(yintercept=20, linetype="dashed", color = "red", size = 2) +
                theme(axis.text.x = element_text(size=16),
                      axis.text.y = element_text(size=16),
                      axis.title = element_text(size=14)) +
                scale_x_continuous(name=x_lab, limits=c(0, 4)) +
                scale_y_continuous(name="Indexvärde", breaks=seq(0,100,20))


  return(p)              
}

# IRI
cutoff <- (swedt_PCI$IRI_mnt - 1)
IRI_ceil <- if_else_na(swedt_PCI$IRI_r_p < 1, ceiling(swedt_PCI$IRI_r_p), swedt_PCI$IRI_r_p)
actual <- swedt_PCI$IRI_mnt - IRI_ceil

p <- PlotIndexCurve(cutoff, actual, x_lab = "(Mätvärde underhållsstandard + Mätvärde)/Mätvärde underhållsstandard")
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


#####################################################
# Beläggning topp 10 i Sverige
swebel <- swedt_PCI %>%
              filter(!is.na(Tretmnt)) %>%
              group_by(Tretmnt) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              top_n(n=10) %>%
              arrange(desc(prop))
print(swebel, n=Inf)
sum(swebel$grouplen)/(sum(swedt_PCI$Length)/1000)
sum(swedt_PCI[is.na(Tretmnt)]$Length)/sum(swedt_PCI$Length)

# Beläggningsåtgärder per region
regbel <- swedt_PCI %>%
              filter(!is.na(Tretmnt)) %>%
              group_by(Region,Tretmnt) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              top_n(n=10) %>%
              arrange(Region, desc(prop))
print(regbel, n=Inf)

write_xlsx(regbel,"C:/Users/winte/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Data/regbel.xlsx")

# Klassificerad
swebel_klass <- swedt_PCI %>%
              filter(!is.na(PvmntTy)) %>%
              group_by(PvmntTy) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              top_n(n=10) %>%
              arrange(desc(prop))
print(swebel_klass, n=Inf)

# Metod + vägtyp
klass_manf <- swedt_PCI %>%
              filter(!is.na(PvngMth)) %>%
              group_by(RoadTyp, PvngMth) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              top_n(n=5) %>%
              arrange(RoadTyp, desc(prop))
print(klass_manf, n=Inf)

# Metod + region
klass_reg <- swedt_PCI %>%
              filter(!is.na(PvngMth)) %>%
              group_by(Region, PvngMth) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              top_n(n=5) %>%
              arrange(Region, desc(prop))
print(klass_reg, n=Inf)

# Klass + trafikmängd
swebel_klass_tkl <- swedt_PCI %>%
              filter(!is.na(Tretmnt)) %>%
              group_by(tkl8, Tretmnt) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              top_n(n=5) %>%
              arrange(tkl8, desc(prop))
print(swebel_klass_tkl, n=Inf)

# Klass + vägtyp
swebel_klass_rt <- swedt_PCI %>%
              filter(!is.na(Tretmnt)) %>%
              group_by(RoadTyp, Tretmnt) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              top_n(n=8) %>%
              arrange(RoadTyp, desc(prop))
print(swebel_klass_rt, n=Inf)


# Klassificerad historisk (PMS)
head(lans_dt)

historisk_bel <- lans_dt %>%
              filter(!is.na(Atgard1)) %>%
              group_by(Atgard1) %>%
              summarise(grouplen = sum(langd)/1000) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              #top_n(n=10) %>%
              arrange(desc(prop))
print(historisk_bel, n=Inf)

# Join with gruppering
swebel_kat <- left_join(swebel, historisk_bel[,1:2], by = c("Tretmnt" = "Beltyp"))
swebel_kat$Atgard2 <- substring(swebel_kat$Atgard2,5)
print(swebel_kat, n=Inf)

write_xlsx(swebel_kat,"C:/Users/winte/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Data/beläggningsåtgärder_grupperad.xlsx")

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









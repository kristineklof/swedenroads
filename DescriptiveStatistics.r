#=================================================================#
#                 Descriptove statistics for the report
#=================================================================#

swedt_PCI <- st_read( "C:/Users/winte/Swedenroads_outputs/sweden_v3_pci201119.shp") 

head(swedt_PCI)
setDT(swedt_PCI)
swedt_PCI <- PCIClass(setDT(swedt_PCI))

DescriptiveStats(swedt_PCI$Length)
DescriptiveStats(swedt_PCI$Age)
DescriptiveStats(swedt_PCI$AADT_hv)
DescriptiveStats(swedt_PCI$AADT)
           
DescriptiveStats <- function(var){
    dstats <- data.frame(Mean = round(mean(var, na.rm=TRUE), digits = 0),
                SD = round(sd(var, na.rm=TRUE), digits = 0),
               Min = round(min(var, na.rm=TRUE), digits = 0),
               Q1 = round(quantile(var, probs = 0.25, na.rm=TRUE), digits=0),
               Median = round(median(var, na.rm=TRUE), digits = 0),
               Q3 = round(quantile(var, probs = 0.75, na.rm=TRUE), digits=0),
               Max = round(max(var, na.rm=TRUE), digits = 0))

    return(dstats)
}

QualitativeStats(swedt_PCI, quo(tkl8), quo(Length))
QualitativeStats(swedt_PCI, quo(PvmntTy), quo(Length))
QualitativeStats(swedt_PCI, quo(DoU2017), quo(Length))

maintstandlengthdou <- swedt_PCI %>%
              group_by(DoU2017) %>%
              summarise(grouplen = sum(Length)/1000,
                        percabove= sum(Length[(rt_m17_ > SP_mant & RodWdth > 6) | (rt_m15_ > SP_mant & RodWdth <= 6)| IRI_r_p > IRI_mnt], na.rm = TRUE)/1000/grouplen,
                        lenabove= percabove*grouplen)
print(maintstandlengthdou)
sum(maintstandlengthdou$lenabove)/sum(maintstandlengthdou$grouplen)

QualitativeStats <- function(df, grp.var, uniq.var){
    classlengtht <- df %>%
              group_by(!!grp.var) %>%
              summarise(grouplen = sum(!!uniq.var)/1000) %>%
              mutate(prop = grouplen/sum(grouplen))

    return(classlengtht)
}

quantile(swedt_PCI$PCI, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE) # quartile

DescriptiveStats(swedt_PCI$PCI)

pclasslength <- swedt_PCI %>%
              group_by(PCIClss) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen))
print(pclasslength)

#####################################################
# Plot index curve

PlotIndexCurve <- function(cutoff, actual){
    df <- data.frame(x = (cutoff - actual)/cutoff)
    index <- function(x) 100*exp(--log(0.2)*x)

    p <- ggplot(data = df, mapping = aes(x = x)) + 
                stat_function(fun = index, size = 1) +
                geom_hline(yintercept=20, linetype="dashed", color = "red") +
                theme(axis.text.x = element_text(face="bold", size=12),
                     axis.text.y = element_text(face="bold", size=12)) +
                scale_x_continuous(name="(IRI underhållsstandard - IRI mätvärde)/IRI underhållsstandard", limits=c(0, 5)) +
                scale_y_continuous(name="Indexvärde", breaks=seq(0,100,20))

  return(p)              
}

cutoff <- (swedt_PCI$IRI_mnt - 1)
IRI_ceil <- if_else_na(swedt_PCI$IRI_r_p < 1, ceiling(swedt_PCI$IRI_r_p), swedt_PCI$IRI_r_p)
actual <- swedt_PCI$IRI_mnt - IRI_ceil

p <- PlotIndexCurve(cutoff, actual)
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







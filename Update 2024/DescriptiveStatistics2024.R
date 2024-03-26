#=================================================================#
#       Swedenroads: Descriptive stats for report 2024
#=================================================================#

head(sw24)
head(sweden24)

DescriptiveStats(sw24$längd)
DescriptiveStats(sw24$ålder)
DescriptiveStats(sw24$ådt_tng)
DescriptiveStats(sw24$ådt_frd)

# Qualitative descriptive statistics for report
QualitativeStatsSingleGroup(sweden24, quo(trfkkls), quo(längd))
QualitativeStatsSingleGroup(sw24, quo(PavementType), quo(längd))
QualitativeStatsSingleGroup(sw24, quo(dou2017), quo(längd))
QualitativeStatsSingleGroup(sw24, quo(vägtyp), quo(längd))
QualitativeStatsSingleGroup(sw24, quo(vägktgr), quo(längd))
QualitativeStatsSingleGroup(sw24, quo(brghtsk), quo(längd))

# Above mainteance standard
maintstandlengthdou <- sw24 %>%
  group_by(dou2017) %>%
  summarise(grouplen = sum(längd)/1000,
            percabove= sum(längd[(sparm17_24 > SP_maint & vägbrdd > 6) | (sparm15_24 > SP_maint & vägbrdd <= 6)| irih_24 > IRI_maint], na.rm = TRUE)/1000/grouplen,
            lenabove= percabove*grouplen)
print(maintstandlengthdou)
sum(maintstandlengthdou$lenabove)/sum(maintstandlengthdou$grouplen)

# Older than expected lifetime
sw24_trfkkls <- TrafficClass(sw24)
head(sw24_trfkkls)

age_above <- sw24_trfkkls %>%
  group_by(trfkkls) %>%
  summarise(mean_PredSL = round(mean(PredictedServiceLife,na.rm=TRUE),0),
            grouplen = sum(längd)/1000,
            percabove= sum(längd[ålder > PredictedServiceLife & vägtyp == 4], na.rm = TRUE)/1000/grouplen,
            lenabove= percabove*grouplen)
print(age_above)

# Reconstruction
sw24_trfkkls <- TrafficClass(sw24)

recon <- sw24_trfkkls %>%
  dplyr::mutate(rekon = if_else(PCI_24 <= 5, "Ja","Nej"))

QualitativeStatsDoubleGroup(recon, quo(trfkkls),quo(rekon),quo(längd))

tot <- recon %>% group_by(rekon) %>%
  summarise(längd = sum(längd/1000))
tot$längd[tot$rekon == "Ja"]/sum(tot$längd)

#####################################
# Mätdatum
as.Date(quantile(unclass(sw24$mätdatm), probs = c(0.02, 0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE), origin = "1970-01-01")

#####################################
# Tillstånd 2024
QualitativeStatsSingleGroup(sw24, quo(PCIClass_24), quo(längd))
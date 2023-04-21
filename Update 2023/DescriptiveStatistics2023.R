#=================================================================#
#       Swedenroads: Descriptive stats for report 2023
#=================================================================#

head(sw23)
head(sweden23)

DescriptiveStats(sw23$längd)
DescriptiveStats(sw23$ålder)
DescriptiveStats(sw23$ådt_tng)
DescriptiveStats(sw23$ådt_frd)

# Qualitative descriptive statistics for report
QualitativeStatsSingleGroup(sweden23, quo(trfkkls), quo(längd))
QualitativeStatsSingleGroup(sw23, quo(PavementType), quo(längd))
QualitativeStatsSingleGroup(sw23, quo(dou2017), quo(längd))
QualitativeStatsSingleGroup(sw23, quo(vägtyp), quo(längd))
QualitativeStatsSingleGroup(sw23, quo(vägktgr), quo(längd))
QualitativeStatsSingleGroup(sw23, quo(brghtsk), quo(längd))

# Above mainteance standard
maintstandlengthdou <- sw23 %>%
  group_by(dou2017) %>%
  summarise(grouplen = sum(längd)/1000,
            percabove= sum(längd[(sparm17_23 > SP_maint & vägbrdd > 6) | (sparm15_23 > SP_maint & vägbrdd <= 6)| irih_23 > IRI_maint], na.rm = TRUE)/1000/grouplen,
            lenabove= percabove*grouplen)
print(maintstandlengthdou)
sum(maintstandlengthdou$lenabove)/sum(maintstandlengthdou$grouplen)

# Older than expected lifetime
sw23_trfkkls <- TrafficClass2023(sw23)
head(sw23_trfkkls)

age_above <- sw23_trfkkls %>%
  group_by(trfkkls) %>%
  summarise(mean_PredSL = round(mean(PredictedServiceLife,na.rm=TRUE),0),
            grouplen = sum(längd)/1000,
            percabove= sum(längd[ålder > PredictedServiceLife & vägtyp == 4], na.rm = TRUE)/1000/grouplen,
            lenabove= percabove*grouplen)
print(age_above)

# Reconstruction
sw23_trfkkls <- TrafficClass2023(sw23)
head(sw23_trfkkls)

recon <- sw23_trfkkls %>%
  dplyr::mutate(rekon = if_else(PCI_23 <= 5, "Ja","Nej"))

QualitativeStatsDoubleGroup(recon, quo(trfkkls),quo(rekon),quo(längd))

tot <- recon %>% group_by(rekon) %>%
  summarise(längd = sum(längd/1000))
tot$längd[tot$rekon == "Ja"]/sum(tot$längd)

#####################################
# Mätdatum
as.Date(quantile(unclass(sw23$mätdatm), probs = c(0.02, 0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE), origin = "1970-01-01")

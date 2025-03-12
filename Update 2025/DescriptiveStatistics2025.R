#=================================================================#
#       Swedenroads: Descriptive stats for report 2025
#=================================================================#

head(sw25)
head(sweden25)

DescriptiveStats(sw25$längd)
DescriptiveStats(sw25$ålder)
DescriptiveStats(sw25$ådt_tng)
DescriptiveStats(sw25$ådt_frd)
sum(sweden25$längd)/1000
nrow(sweden25)

# Qualitative descriptive statistics for report
QualitativeStatsSingleGroup(sweden25, quo(trfkkls), quo(längd))
QualitativeStatsSingleGroup(sw25, quo(PavementType), quo(längd))
QualitativeStatsSingleGroup(sweden25, quo(dou2017), quo(längd))
QualitativeStatsSingleGroup(sweden25, quo(vägtyp), quo(längd))
QualitativeStatsSingleGroup(sweden25, quo(vägktgr), quo(längd))
QualitativeStatsSingleGroup(sweden25, quo(brghtsk), quo(längd))

# Missing data for report
missing_bel <- sweden25 %>% missing_length_summary(beltyp_25, längd) 
missing_adt <- sweden25 %>%  missing_length_summary(ådt_frd, längd) 
missing_typ <- sweden25 %>%  missing_length_summary(vägtyp, längd) 
missing_bh <- sweden25 %>%  missing_length_summary(brghtsk, längd)
missing_ht <- sweden25 %>%  missing_length_summary(hastght, längd)
missing_vb <- sweden25 %>%  missing_length_summary(vägbrdd, längd)  
missing_iri <- sweden25 %>%  missing_length_summary(irih_25, längd)  
missing_sp <- sweden25 %>%  missing_length_summary(sparm17_25, längd) 
  

mean_length <- aggregate(längd ~ vägtyp, data = sweden25, FUN = mean)

# Above mainteance standard
maintstandlengthdou <- sw25 %>%
  group_by(dou2017) %>%
  summarise(grouplen = sum(längd)/1000,
            groupperc = grouplen/85750.23,
            percabove= sum(längd[(sparm17_25 > SP_maint & vägbrdd > 6) | (sparm15_25 > SP_maint & vägbrdd <= 6)| irih_25 > IRI_maint], na.rm = TRUE)/1000/grouplen,
            lenabove = percabove*grouplen)
print(maintstandlengthdou)
sum(maintstandlengthdou$lenabove)/sum(maintstandlengthdou$grouplen)

# Older than expected lifetime
age_above <- sw25 %>%
  group_by(trfkkls) %>%
  summarise(mean_PredSL = round(mean(PredictedServiceLife,na.rm=TRUE),0),
            grouplen = sum(längd)/1000,
            percabove= sum(längd[ålder > PredictedServiceLife & vägtyp == 4], na.rm = TRUE)/1000/grouplen,
            lenabove= percabove*grouplen)
print(age_above)

# Reconstruction
recon <- sw25 %>%
  dplyr::mutate(rekon = if_else(index_25 <= 5, "Ja","Nej"))

QualitativeStatsDoubleGroup(recon, quo(trfkkls),quo(rekon),quo(längd))

tot <- recon %>% group_by(rekon) %>%
  summarise(längd = sum(längd/1000))
tot$längd[tot$rekon == "Ja"]/sum(tot$längd)

#####################################
# Mätdatum
as.Date(quantile(unclass(sw25$matdatum_2), probs = c(0.02, 0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE), origin = "1970-01-01")

#####################################
# Saknar mätning
sum(sw25$längd[is.na(sw25$matdatum_2)])/sum(sw25$längd)

#####################################
# Tillstånd 2025
QualitativeStatsSingleGroup(sw25, quo(PCIClass_25), quo(längd))
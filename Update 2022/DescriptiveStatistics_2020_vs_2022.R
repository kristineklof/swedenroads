#=================================================================#
#           Swedenroads: Comparison of 2020 with 2022
#=================================================================#

# How many roads have been maintained since 2019?
sweden22 <- sweden22 %>% 
  dplyr::mutate(ny_bel = if_else_na(beldat_22 > blggnngsd, "Ja", "Nej"))
nybel_2019_2021 <- QualitativeStatsSingleGroup(sweden22, quo(ny_bel), quo(längd))

# Latest date for ny belaggning
max(sweden22$beldat_22,na.rm=TRUE)

#################################################
# Hur många sträckor har ändrats sen 2022?
sum(sweden22$brghtsk != sweden22$barig_21, na.rm=TRUE)
sum(sweden22$hastght != sweden22$hast_21, na.rm=TRUE)
sum(sweden22$Ådt_frd != sweden22$adt_21, na.rm=TRUE)
sum(sweden22$dou2017 != sweden22$dou_21, na.rm=TRUE)
sum(sweden22$vägbrdd != sweden22$bredd_21, na.rm=TRUE)
sum(sweden22$vägktgr != sweden22$vagkategor, na.rm=TRUE)
sum(sweden22$vägtyp != sweden22$vagtyp_21, na.rm=TRUE)

QualitativeStatsSingleGroup(sweden22, quo(brghtsk), quo(längd))
QualitativeStatsSingleGroup(sweden22, quo(barig_21), quo(längd))

QualitativeStatsSingleGroup(sweden22, quo(hastght), quo(längd))
QualitativeStatsSingleGroup(sweden22, quo(hast_21), quo(längd))

QualitativeStatsSingleGroup(sweden22, quo(dou2017), quo(längd))
QualitativeStatsSingleGroup(sweden22, quo(dou_21), quo(längd))

QualitativeStatsSingleGroup(sweden22, quo(vägtyp), quo(längd))
QualitativeStatsSingleGroup(sweden22, quo(vagtyp_21), quo(längd))

QualitativeStatsSingleGroup(sweden22, quo(trfkkls), quo(längd))


#################################################
# Jämför PCI
QualitativeStatsSingleGroup(sw22, quo(PCIClass_22), quo(längd))
QualitativeStatsSingleGroup(sw22, quo(indxkls), quo(längd))

QualitativeStatsDoubleGroup(sw22, quo(hastght), quo(PCIClass_22), quo(längd))
QualitativeStatsDoubleGroup(sw22, quo(hastght), quo(indxkls), quo(längd))

QualitativeStatsDoubleGroup(sw22, quo(dou2017), quo(PCIClass_22), quo(längd))
QualitativeStatsDoubleGroup(sw22, quo(dou2017), quo(indxkls), quo(längd))

head(sw22)


###############################################################
# Above maintenance standard
maintstandlengthdou <- sw22 %>%
  group_by(dou2017) %>%
  summarise(grouplen = sum(längd)/1000,
            percabove= sum(längd[(sparm17_22 > SP_maint & vägbrdd > 6) | (sparm15_22 > SP_maint & vägbrdd <= 6)| irih_22 > IRI_maint], na.rm = TRUE)/1000/grouplen,
            lenabove= percabove*grouplen)
print(maintstandlengthdou)
sum(maintstandlengthdou$lenabove)/sum(maintstandlengthdou$grouplen)
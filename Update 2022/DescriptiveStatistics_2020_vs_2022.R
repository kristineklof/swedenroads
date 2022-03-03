#=================================================================#
#           Swedenroads: Comparison of 2020 with 2022
#=================================================================#

# How many roads have been maintained since 2019?
sweden22 <- sweden22 %>% 
  dplyr::mutate(ny_bel = if_else_na(beldat_22 > blggnngsd, "Ja", "Nej"))
nybel_2019_2021 <- QualitativeStatsSingleGroup(sweden22, quo(ny_bel), quo(längd))
nybel_2019_2021

# Latest date for ny belaggning
max(sweden22$beldat_22,na.rm=TRUE)

#################################################
# Hur många sträckor har ändrats sen 2022?
sum(sweden22$brghtsk != sweden22$barig_21, na.rm=TRUE)
sum(sweden22$Ådt_frd != sweden22$adt_21, na.rm=TRUE)
sum(sweden22$dou2017 != sweden22$dou_21, na.rm=TRUE)
sum(sweden22$vägbrdd != sweden22$bredd_21, na.rm=TRUE)
sum(sweden22$vägktgr != sweden22$vagkategor, na.rm=TRUE)
sum(sweden22$vägtyp != sweden22$vagtyp_21, na.rm=TRUE)

bärighet <- ComparisonBetweenYears(sweden22,sw22, quo(brghtsk),"brghtsk")
print(bärighet, n=Inf)

dou2017 <- ComparisonBetweenYears(sweden22,sw22, quo(dou2017),"dou2017")
print(dou2017, n=Inf)

vägtyp <- ComparisonBetweenYears(sweden22,sw22, quo(vägtyp),"vägtyp")
print(vägtyp, n=Inf)

trfkkls <- ComparisonBetweenYears(sweden22,sw22, quo(trfkkls),"trfkkls")
print(trfkkls, n=Inf)

beltyp <- ComparisonBetweenYears(sweden22,sw22, quo(trfkkls),"trfkkls")
print(trfkkls, n=Inf)

#################################################
# Jämför PCI
pci_2022_vs_2020 <- ConditionComparisonBetweenYears(sw22,sw22, NA,NA,single=TRUE)
print(pci_2022_vs_2020, n=Inf)

pci_hast_2022_vs_2020 <- ConditionComparisonBetweenYears(sw22,sw22, quo(hastght),"hastght")
print(pci_hast_2022_vs_2020, n=Inf)

pci_dou_2022_vs_2020 <- ConditionComparisonBetweenYears(sw22,sw22, quo(dou2017),"dou2017")
print(pci_dou_2022_vs_2020, n=Inf)

###############################################################
# Länsvis
sw22_lankom <- dplyr::left_join(sw22,lankom, by = c("län_nr" = "Län", "kmmn_nr" = "Kommunnr"))

pci_lan_2022_vs_2020 <- ConditionComparisonBetweenYears(sw22_lankom,sw22_lankom, quo(Länsnamn),"Länsnamn")
print(pci_lan_2022_vs_2020, n=Inf)

###############################################################
# Above maintenance standard
maintstandlengthdou <- sw22 %>%
  group_by(dou2017) %>%
  summarise(grouplen = sum(längd)/1000,
            percabove= sum(längd[(sparm17_22 > SP_maint & vägbrdd > 6) | (sparm15_22 > SP_maint & vägbrdd <= 6)| irih_22 > IRI_maint], na.rm = TRUE)/1000/grouplen,
            lenabove= percabove*grouplen)
print(maintstandlengthdou)
sum(maintstandlengthdou$lenabove)/sum(maintstandlengthdou$grouplen)




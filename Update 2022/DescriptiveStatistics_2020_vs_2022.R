#=================================================================#
#           Swedenroads: Comparison of 2020 with 2022
#=================================================================#

fill <- c("#20AC65", "#71C94B","#FABF20","#F2203E","#C40A3B")

# How many roads have been maintained since 2019?
sweden22 <- sweden22 %>% 
  dplyr::mutate(ny_bel = if_else_na(beldat_22 > blggnngsd, "Ja", "Nej"))
nybel_2019_2021 <- QualitativeStatsSingleGroup(sweden22, quo(ny_bel), quo(längd))
nybel_2019_2021

# Beläggning 2020-2022 per tillståndsklass och trafikmängd
# procent
nybel_age <- sweden22 %>% drop_na(trfkkls) %>%
  dplyr::mutate(trfkkls = factor(trfkkls)) %>%
  dplyr::mutate(trfkkls  = recode(trfkkls , 
                                  "1" ="<250 fordon/dygn", 
                                  "2" = "250-499",
                                  "3" = "500-999", 
                                  "4" = "1000-1999", 
                                  "5" = "2000-3999",
                                  "6" = "4000-7999",
                                  "7" = "8000-11999",
                                  "8" = ">12000")) %>%
  dplyr::mutate(PCIClass = factor(indxkls, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                  "5" ="Mycket bra", 
                                  "4" = "Bra", 
                                  "3" = "Tillfredsställande", 
                                  "2" = "Dålig", 
                                  "1" = "Mycket dålig")) %>%
  group_by(trfkkls, PCIClass, ny_bel) %>%
  summarise(grouplen = sum(längd)/1000) %>%
  dplyr::mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = PCIClass, y = percentage, 
             fill = ny_bel, col = PCIClass)) +
  geom_bar(position = 'stack', stat = 'identity', size = 1) +
  facet_wrap(~trfkkls, scales='free') +
  scale_color_manual(values=fill) +
  scale_fill_manual(values = c("gray37","gray68")) +
  labs(y="", x = "") +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title=element_text(size=14),
        legend.text=element_text(size=12), 
        axis.text=element_text(size=10),
        strip.text.x = element_text(size=14)) +
  guides(fill=guide_legend(title="Ny beläggning 2020-2021",
                           title.position = "top",
                           label.position = "bottom",
                           nrow = 1),
         color = "none") +
  scale_y_continuous(labels = scales::percent) 
grid::grid.draw(ShiftLegend(nybel_age))

# Beläggning 2020-2022 per tillståndsklass och trafikmängd
# kilometer
nybel_age_km <- sweden22 %>% drop_na(trfkkls) %>%
  dplyr::mutate(trfkkls = factor(trfkkls)) %>%
  dplyr::mutate(trfkkls  = recode(trfkkls , 
                                  "1" ="<250 fordon/dygn", 
                                  "2" = "250-499",
                                  "3" = "500-999", 
                                  "4" = "1000-1999", 
                                  "5" = "2000-3999",
                                  "6" = "4000-7999",
                                  "7" = "8000-11999",
                                  "8" = ">12000")) %>%
  dplyr::mutate(PCIClass = factor(indxkls, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                  "5" ="Mycket bra", 
                                  "4" = "Bra", 
                                  "3" = "Tillfredsställande", 
                                  "2" = "Dålig", 
                                  "1" = "Mycket dålig")) %>%
  group_by(trfkkls, PCIClass, ny_bel) %>%
  summarise(grouplen = sum(längd)/1000) %>%
  ggplot(aes(x = PCIClass, y = grouplen, 
             fill = ny_bel, col = PCIClass)) +
  geom_bar(position = 'stack', stat = 'identity', size = 1) +
  facet_wrap(~trfkkls, scales='free') +
  scale_color_manual(values=fill) +
  scale_fill_manual(values = c("gray37","gray68")) +
  labs(y="", x = "") +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title=element_text(size=14),
        legend.text=element_text(size=12), 
        axis.text=element_text(size=10),
        strip.text.x = element_text(size=14)) +
  guides(fill=guide_legend(title="Ny beläggning 2020-2021",
                           title.position = "top",
                           label.position = "bottom",
                           nrow = 1),
         color = "none") +
grid::grid.draw(ShiftLegend(nybel_age_km))

# Beläggning 2020-2022 per tillståndsklass och trafikmängd
# English
nybel_age_eng <- sweden22 %>% drop_na(trfkkls) %>%
  dplyr::mutate(trfkkls = factor(trfkkls)) %>%
  dplyr::mutate(trfkkls  = recode(trfkkls , 
                                  "1" ="<250 vehicles/day", 
                                  "2" = "250-499",
                                  "3" = "500-999", 
                                  "4" = "1000-1999", 
                                  "5" = "2000-3999",
                                  "6" = "4000-7999",
                                  "7" = "8000-11999",
                                  "8" = ">12000")) %>%
  dplyr::mutate(PCIClass = factor(indxkls, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                  "5" ="Excellent", 
                                  "4" = "Good", 
                                  "3" = "Fair", 
                                  "2" = "Poor", 
                                  "1" = "Very poor")) %>%
  dplyr::mutate(ny_bel = as.factor(ny_bel)) %>%
  dplyr::mutate(ny_bel = recode(ny_bel, 
                                  "Ja" ="Yes", 
                                  "Nej" = "No")) %>%
  group_by(trfkkls, PCIClass, ny_bel) %>%
  summarise(grouplen = sum(längd)/1000) %>%
  dplyr::mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = PCIClass, y = percentage, 
             fill = ny_bel, col = PCIClass)) +
  geom_bar(position = 'stack', stat = 'identity', size = 1) +
  facet_wrap(~trfkkls, scales='free') +
  scale_color_manual(values=fill) +
  scale_fill_manual(values = c("gray37","gray68")) +
  labs(y="", x = "") +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title=element_text(size=14),
        legend.text=element_text(size=12), 
        axis.text=element_text(size=10),
        strip.text.x = element_text(size=14)) +
  guides(fill=guide_legend(title="Maintenance treatment 2020-2021",
                           title.position = "top",
                           label.position = "bottom",
                           nrow = 1),
         color = "none") +
  scale_y_continuous(labels = scales::percent) 
grid::grid.draw(ShiftLegend(nybel_age_eng))

# Täckning 2020-2022 per tillståndsklass och trafikmängd
nybel_tackning <- sweden22 %>% dplyr::filter(ny_bel == "Ja") %>%
  drop_na(trfkkls) %>%
  drop_na(tackning_2) %>%
  dplyr::mutate(trfkkls = factor(trfkkls)) %>%
  dplyr::mutate(trfkkls  = recode(trfkkls , 
                                  "1" ="<250 fordon/dygn", 
                                  "2" = "250-499",
                                  "3" = "500-999", 
                                  "4" = "1000-1999", 
                                  "5" = "2000-3999",
                                  "6" = "4000-7999",
                                  "7" = "8000-11999",
                                  "8" = ">12000")) %>%
  dplyr::mutate(PCIClass = factor(indxkls, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                  "5" ="Mycket bra", 
                                  "4" = "Bra", 
                                  "3" = "Tillfredsställande", 
                                  "2" = "Dålig", 
                                  "1" = "Mycket dålig")) %>%
  group_by(trfkkls, PCIClass, tackning_2) %>%
  summarise(grouplen = sum(längd)/1000) %>%
  dplyr::mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = PCIClass, y = percentage, 
             fill = tackning_2, col = PCIClass)) +
  geom_bar(position = 'stack', stat = 'identity', size = 1) +
  facet_wrap(~trfkkls, scales="free") +
  scale_color_manual(values=fill) +
  scale_fill_manual(values = c("gray88","gray68","gray38")) +
  labs(y="", x = "") +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12), 
        axis.text=element_text(size=10),
        strip.text.x = element_text(size=14)) +
  guides(fill=guide_legend(title="Täckningsgrad beläggningsåtgärder 2020-2021",
                           title.position = "top",
                           label.position = "bottom",
                           nrow = 1),
         color = "none") +
  scale_y_continuous(labels = scales::percent)
grid::grid.draw(ShiftLegend(nybel_tackning))

# Täckning 2020-2022 per tillståndsklass och trafikmängd
# English
nybel_tackning_eng <- sweden22 %>% dplyr::filter(ny_bel == "Ja") %>%
  drop_na(trfkkls) %>%
  drop_na(tackning_2) %>%
  dplyr::mutate(trfkkls = factor(trfkkls)) %>%
  dplyr::mutate(trfkkls  = recode(trfkkls , 
                                  "1" ="<250 vehicles/day", 
                                  "2" = "250-499",
                                  "3" = "500-999", 
                                  "4" = "1000-1999", 
                                  "5" = "2000-3999",
                                  "6" = "4000-7999",
                                  "7" = "8000-11999",
                                  "8" = ">12000")) %>%
  dplyr::mutate(PCIClass = factor(indxkls, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                  "5" ="Excellent", 
                                  "4" = "Good", 
                                  "3" = "Fair", 
                                  "2" = "Poor", 
                                  "1" = "Very Poor")) %>%
  dplyr::mutate(tackning_2 = factor(tackning_2)) %>%
  dplyr::mutate(tackning_2 = recode(tackning_2, 
                                  "Heltäckande" = "Full          ", 
                                  "Fläckvis <20%" = "Partial <20%", 
                                  "Fläckvis >20%" = "Partial >20%")) %>%
  group_by(trfkkls, PCIClass, tackning_2) %>%
  summarise(grouplen = sum(längd)/1000) %>%
  dplyr::mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = PCIClass, y = percentage, 
             fill = tackning_2, col = PCIClass)) +
  geom_bar(position = 'stack', stat = 'identity', size = 1) +
  facet_wrap(~trfkkls, scales="free") +
  scale_color_manual(values=fill) +
  scale_fill_manual(values = c("gray88","gray68","gray38")) +
  labs(y="", x = "") +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12), 
        axis.text=element_text(size=10),
        strip.text.x = element_text(size=14)) +
  guides(fill=guide_legend(title="Coverage maintenance treatments 2020-2021",
                           title.position = "top",
                           label.position = "bottom",
                           nrow = 1),
         color = "none") +
  scale_y_continuous(labels = scales::percent)
grid::grid.draw(ShiftLegend(nybel_tackning_eng))

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

beltyp <- ComparisonBetweenYears(sweden22_comp,sw22, quo(PavementType),"PavementType")
print(beltyp, n=Inf)

###################################################
# Check PCI

min(sw22$PCI_22)
max(sw22$PCI_22)

# Check roads with value 100 and beldat not 2021
sw22[sw22$PCI_22 == 100 & sw22$blggnngsd < as.Date("2021-01-01"),]

# Check roads with > 80
sw22[sw22$PCI_22 > 80 & sw22$blggnngsd < as.Date("2020-01-01"),]

#  Studera sträckor med index under 20 som inte överskrider underhållstandard
# eller åldersgränsen

low <- sw22[PCI_22 <= 20 & !(RMS_Index_22 <= 20 | IRI_Index_22  <= 20 | Rut_Index_22 <= 20)]
low <- sw22[PCI_22 <= 20 & !(Ålder >= PredictedServiceLife  | iri  >= IRI_maint | spårdjp >= SP_maint)]
nrow(low)
head(low)
tail(low)

#################################################
# Jämför PCI
mean(sw22$tllstni)
mean(sw22$PCI_22)

pci_2022_vs_2020 <- ConditionComparisonBetweenYears(sw22,sw22, NA,NA,single=TRUE)
print(pci_2022_vs_2020, n=Inf)

pci_hast_2022_vs_2020 <- ConditionComparisonBetweenYears(sw22,sw22, quo(hastght),"hastght")
print(pci_hast_2022_vs_2020, n=Inf)

pci_dou_2022_vs_2020 <- ConditionComparisonBetweenYears(sw22,sw22, quo(dou2017),"dou2017")
print(pci_dou_2022_vs_2020, n=Inf)

head(sw22)

# PCI 
cond_eng <- sw22 %>%
  dplyr::select(indxkls, PCIClass_22, längd) %>%
  dplyr::rename(PCIClass_2022 = PCIClass_22) %>%
  dplyr::rename(PCIClass_2020 = indxkls) %>%
  tidyr::pivot_longer(cols = PCIClass_2020:PCIClass_2022, 
                      names_to = "Year", 
                      values_to = "PCIClass") %>%
  dplyr::mutate(Year = factor(Year)) %>%
  dplyr::mutate(Year = recode(Year, 
                              "PCIClass_2020" = "2020", 
                              "PCIClass_2022" = "2022")) %>% 
  dplyr::mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                  "5" ="Excellent", 
                                  "4" = "Good", 
                                  "3" = "Fair", 
                                  "2" = "Poor", 
                                  "1" = "Very poor")) %>%
  group_by(Year, PCIClass) %>%
  summarise(grouplen = sum(längd)/1000) %>%
  dplyr::mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = Year, y = percentage, fill = PCIClass, label = paste0(round(100*percentage,digits=0)," %"))) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_fill_manual(values=fill) +
  labs(y="", x = "") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="bottom", legend.direction="vertical",
        legend.title = element_blank(), 
        legend.text=element_text(size=16), 
        axis.text=element_text(size=14),
        strip.text.x = element_text(size=16)) +
  guides(fill=guide_legend(label.position = "bottom",
                           nrow = 1)) +
  geom_text(size = 5, position = position_stack(vjust = 0.5))
print(cond_eng)

# PCI barchart region
cond_p <- sw22 %>%
  dplyr::select(region, indxkls, PCIClass_22, längd) %>%
  dplyr::rename(PCIClass_2022 = PCIClass_22) %>%
  dplyr::rename(PCIClass_2020 = indxkls) %>%
  tidyr::pivot_longer(cols = PCIClass_2020:PCIClass_2022, 
               names_to = "Year", 
               values_to = "PCIClass") %>%
  dplyr::mutate(Year = factor(Year)) %>%
  dplyr::mutate(Year = recode(Year, 
                                  "PCIClass_2020" = "2020", 
                                  "PCIClass_2022" = "2022")) %>% 
  dplyr::mutate(Region = ChangeRegion(region)) %>%
  dplyr::mutate(Region = factor(Region)) %>%
  dplyr::mutate(Region = recode(Region, Ost="Öst", Vast="Väst")) %>%
  dplyr::mutate(Region = factor(Region, levels = c("Mitt","Nord","Sthlm","Syd","Väst","Öst"))) %>%
  dplyr::mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                  "5" ="Mycket bra", 
                                  "4" = "Bra", 
                                  "3" = "Tillfredsställande", 
                                  "2" = "Dålig", 
                                  "1" = "Mycket dålig")) %>%
  group_by(Region, Year, PCIClass) %>%
  summarise(grouplen = sum(längd)/1000) %>%
  dplyr::mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = Year, y = percentage, fill = PCIClass, label = paste0(round(100*percentage,digits=0)," %"))) +
  geom_bar(position = 'stack', stat = 'identity') +
  facet_wrap(~Region, nrow = 1) +
  scale_fill_manual(values=fill) +
  labs(y="", x = "") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="bottom", legend.direction="vertical",
        legend.title = element_blank(), 
        legend.text=element_text(size=16), 
        axis.text=element_text(size=14),
        strip.text.x = element_text(size=16)) +
  guides(fill=guide_legend(label.position = "bottom",
                           nrow = 1)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

print(cond_p)

# PCI barchart region
cond_p_eng <- sw22 %>%
  dplyr::select(region, indxkls, PCIClass_22, längd) %>%
  dplyr::rename(PCIClass_2022 = PCIClass_22) %>%
  dplyr::rename(PCIClass_2020 = indxkls) %>%
  tidyr::pivot_longer(cols = PCIClass_2020:PCIClass_2022, 
                      names_to = "Year", 
                      values_to = "PCIClass") %>%
  dplyr::mutate(Year = factor(Year)) %>%
  dplyr::mutate(Year = recode(Year, 
                              "PCIClass_2020" = "2020", 
                              "PCIClass_2022" = "2022")) %>% 
  dplyr::mutate(Region = ChangeRegion(region)) %>%
  dplyr::mutate(Region = factor(Region)) %>%
  dplyr::mutate(Region = recode(Region, Ost="Öst", Vast="Väst")) %>%
  dplyr::mutate(Region = factor(Region, levels = c("Mitt","Nord","Sthlm","Syd","Väst","Öst"))) %>%
  dplyr::mutate(Region = recode(Region, 
                                  "Mitt" ="Middle", 
                                  "Nord" = "North", 
                                  "Sthlm" = "Sthlm", 
                                  "Syd" = "South", 
                                  "Väst" = "West",
                                  "Öst" = "East")) %>%
  dplyr::mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                  "5" ="Excellent", 
                                  "4" = "Good", 
                                  "3" = "Fair", 
                                  "2" = "Poor", 
                                  "1" = "Very poor")) %>%
  group_by(Region, Year, PCIClass) %>%
  summarise(grouplen = sum(längd)/1000) %>%
  dplyr::mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = Year, y = percentage, fill = PCIClass, label = paste0(round(100*percentage,digits=0)," %"))) +
  geom_bar(position = 'stack', stat = 'identity') +
  facet_wrap(~Region, nrow = 1) +
  scale_fill_manual(values=fill) +
  labs(y="", x = "") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="bottom", legend.direction="vertical",
        legend.title = element_blank(), 
        legend.text=element_text(size=16), 
        axis.text=element_text(size=14),
        strip.text.x = element_text(size=16)) +
  guides(fill=guide_legend(label.position = "bottom",
                           nrow = 1)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

print(cond_p_eng)

# PCI barchart vägtyp
cond_v <- sw22 %>%
  dplyr::select(RoadType, indxkls, PCIClass_22, längd) %>%
  dplyr::rename(PCIClass_2022 = PCIClass_22) %>%
  dplyr::rename(PCIClass_2020 = indxkls) %>%
  tidyr::pivot_longer(cols = PCIClass_2020:PCIClass_2022, 
                      names_to = "Year", 
                      values_to = "PCIClass") %>%
  dplyr::mutate(Year = factor(Year)) %>%
  dplyr::mutate(Year = recode(Year, 
                              "PCIClass_2020" = "2020", 
                              "PCIClass_2022" = "2022")) %>% 
  drop_na(RoadType) %>%
  dplyr::mutate(RoadType = as.factor(RoadType)) %>%
  dplyr::mutate(RoadType = recode(RoadType, "Ordinary road" = "Vanlig väg", 
                           "2+1 road" = "2+1 väg", 
                           "Undivided motorway" = "Motortrafikled", 
                           "Motorway" = "Motorväg", 
                           "4-lane road" = "4-fälts väg")) %>%
  dplyr::mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                     "5" ="Mycket bra", 
                                     "4" = "Bra", 
                                     "3" = "Tillfredsställande", 
                                     "2" = "Dålig", 
                                     "1" = "Mycket dålig")) %>%
  group_by(RoadType, Year, PCIClass) %>%
  summarise(grouplen = sum(längd)/1000) %>%
  mutate(percentage = grouplen/sum(grouplen)) %>%
  mutate(percentage = ifelse(RoadType == "2+1 väg" & PCIClass == "Mycket dålig" & Year == 2022, 
                             0.0655, percentage)) %>%
  ggplot(aes(x = Year, y = percentage, fill = PCIClass, label = paste0(round(100*percentage,0)," %"))) +
  geom_bar(position = 'fill', stat = 'identity') +
  facet_wrap(~ RoadType, nrow = 1) +
  scale_fill_manual(values=fill) +
  labs(y="", x = "") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="bottom", legend.direction="vertical",
        legend.title = element_blank(), 
        legend.text=element_text(size=16), 
        axis.text=element_text(size=14),
        strip.text.x = element_text(size=16)) +
  guides(fill=guide_legend(label.position = "bottom",
                           nrow = 1)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

print(cond_v)

# PCI barchart vägtyp
cond_v_eng <- sw22 %>%
  dplyr::select(RoadType, indxkls, PCIClass_22, längd) %>%
  dplyr::rename(PCIClass_2022 = PCIClass_22) %>%
  dplyr::rename(PCIClass_2020 = indxkls) %>%
  tidyr::pivot_longer(cols = PCIClass_2020:PCIClass_2022, 
                      names_to = "Year", 
                      values_to = "PCIClass") %>%
  dplyr::mutate(Year = factor(Year)) %>%
  dplyr::mutate(Year = recode(Year, 
                              "PCIClass_2020" = "2020", 
                              "PCIClass_2022" = "2022")) %>% 
  drop_na(RoadType) %>%
  dplyr::mutate(RoadType = as.factor(RoadType)) %>%
  dplyr::mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                  "5" ="Excellent", 
                                  "4" = "Good", 
                                  "3" = "Fair", 
                                  "2" = "Poor", 
                                  "1" = "Very poor")) %>%
  group_by(RoadType, Year, PCIClass) %>%
  summarise(grouplen = sum(längd)/1000) %>%
  mutate(percentage = grouplen/sum(grouplen)) %>%
  mutate(percentage = ifelse(RoadType == "2+1 road" & PCIClass == "Very poor" & Year == 2022, 
                             0.0655, percentage)) %>%
  ggplot(aes(x = Year, y = percentage, fill = PCIClass, label = paste0(round(100*percentage,0)," %"))) +
  geom_bar(position = 'fill', stat = 'identity') +
  facet_wrap(~ RoadType, nrow = 1) +
  scale_fill_manual(values=fill) +
  labs(y="", x = "") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="bottom", legend.direction="vertical",
        legend.title = element_blank(), 
        legend.text=element_text(size=16), 
        axis.text=element_text(size=14),
        strip.text.x = element_text(size=16)) +
  guides(fill=guide_legend(label.position = "bottom",
                           nrow = 1)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

print(cond_v_eng)

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
sum(maintstandlengthdou$lenabove)

print(xtable(maintstandlengthdou), include.rownames = FALSE)

# Older than expected lifetime
age_above <- sw22 %>%
  group_by(trfkkls) %>%
  summarise(mean_PredSL = round(mean(PredictedServiceLife,na.rm=TRUE),0),
            grouplen = sum(längd)/1000,
            percabove= sum(längd[Ålder > PredictedServiceLife & RoadType == "Ordinary road"], na.rm = TRUE)/1000/grouplen,
            lenabove= percabove*grouplen)
print(age_above)

###############################################################
# Jämför teckningsgrad

sw22 <- sw22 %>% dplyr::mutate(bel_year = format(blggnngsd,"%Y")) %>%
  dplyr::mutate(tackning = if_else_na(tackning == "NULL",NA,tackning))

tackning_per_year <- QualitativeStatsDoubleGroup(sw22, quo(bel_year),quo(tackning), quo(längd))
tackning_per_year <- tackning_per_year %>% dplyr::filter(bel_year > 2016) %>% 
  dplyr::mutate(prop = round(prop*100,1)) %>%
  dplyr::mutate(grouplen = round(grouplen,0)) %>%
  dplyr::mutate(tackning = if_else_na(is.na(tackning),"Okänd",tackning))
print(tackning_per_year, n=Inf)

print(xtable(tackning_per_year), include.rownames = FALSE)

sw22_2020 <- sw22 %>% dplyr::filter(bel_year >= 2020)
tackning_per_trafikklass <- QualitativeStatsDoubleGroup(sw22_2020, quo(trfkkls), quo(tackning), quo(längd))
tackning_per_trafikklass <- tackning_per_trafikklass %>% dplyr::mutate(prop = round(prop*100,1)) %>%
  dplyr::mutate(grouplen = round(grouplen,0)) %>%
  dplyr::mutate(tackning = if_else_na(is.na(tackning),"Okänd",tackning)) %>%
  dplyr::filter(tackning != "Okänd")
tackning_per_trafikklass <- na.omit(tackning_per_trafikklass)
print(tackning_per_trafikklass, n=Inf)

print(xtable(tackning_per_trafikklass), include.rownames = FALSE)

trafikklass_tackning <- QualitativeStatsDoubleGroup(sw22_2020, quo(tackning), quo(trfkkls), quo(längd))
trafikklass_tackning <- dplyr::mutate(prop = round(prop*100,1)) %>%
  dplyr::mutate(grouplen = round(grouplen,0)) %>%
  dplyr::mutate(tackning = if_else_na(is.na(tackning),"Okänd",tackning))
trafikklass_tackning <- na.omit(trafikklass_tackning)
print(trafikklass_tackning, n=Inf)

print(xtable(trafikklass_tackning), include.rownames = FALSE)

###############################################################
# Jämför rekonstruktionsbehov

sw22_pci_below_5_2020 <- sw22 %>% dplyr::filter(tllstni <= 5)
sw22_pci_below_5_2022 <- sw22 %>% dplyr::filter(PCI_22 <= 5)

rek_2020 <- QualitativeStatsSingleGroup(sw22_pci_below_5_2020, quo(trfkkls), quo(längd))
rek_2020 <- na.omit(rek_2020)
rek_2022 <- QualitativeStatsSingleGroup(sw22_pci_below_5_2022, quo(trfkkls), quo(längd))
rek_2022 <- na.omit(rek_2022)

tot_langd <- QualitativeStatsSingleGroup(sw22, quo(trfkkls), quo(längd))
tot_langd <- na.omit(tot_langd)
print(tot_langd)

rek_2020_2022 <- left_join(rek_2020,rek_2022, by=c("trfkkls"))
names(rek_2020_2022) <- c("trfkkls","grouplen_2020","prop_2020","grouplen_2022","prop_2022")
rek_2020_2022 <- left_join(rek_2020_2022,tot_langd, by=c("trfkkls"))
rek_2020_2022 <- rek_2020_2022 %>% 
  dplyr::mutate(prop_2020 = grouplen_2020/grouplen) %>%
  dplyr::mutate(prop_2022 = grouplen_2022/grouplen)
print(rek_2020_2022)
sum(rek_2020_2022$grouplen_2022)

print(xtable(rek_2020_2022), include.rownames = FALSE)



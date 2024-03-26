#=================================================================#
#           Swedenroads: Comparison of 2023 with 2024
#=================================================================#

fill <- c("#20AC65", "#71C94B","#FABF20","#F2303E","#C40A3B")

# How many roads have been maintained since 2022?
sweden24 <- sweden24 %>% 
  dplyr::mutate(ny_bel = if_else_na(beldat_24 > blggnngsd, "Ja", "Nej"))
nybel_2022_2023 <- QualitativeStatsSingleGroup(sweden24, quo(ny_bel), quo(längd))
nybel_2022_2023

# Beläggning 2022-2023 per tillståndsklass och trafikmängd
# procent
nybel_age <- sweden24 %>% drop_na(trfkkls) %>%
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
  geom_bar(position = 'stack', stat = 'identity', linewidth = 1) +
  facet_wrap(~trfkkls, scales='free') +
  scale_color_manual(values=fill) +
  scale_fill_manual(values = c("gray37","gray68")) +
  labs(y="", x = "") +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title=element_text(size=14),
        legend.text=element_text(size=12), 
        axis.text=element_text(size=8),
        strip.text.x = element_text(size=14)) +
  guides(fill=guide_legend(title="Ny beläggning 2023",
                           title.position = "top",
                           label.position = "bottom",
                           nrow = 1),
         color = "none") +
  scale_y_continuous(labels = scales::percent) 
grid::grid.draw(ShiftLegend(nybel_age))

# Beläggning 2020-2023 per tillståndsklass och trafikmängd
# kilometer
nybel_age_km <- sweden24 %>% drop_na(trfkkls) %>%
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
  geom_bar(position = 'stack', stat = 'identity', linewidth = 1) +
  facet_wrap(~trfkkls, scales='free') +
  scale_color_manual(values=fill) +
  scale_fill_manual(values = c("gray37","gray68")) +
  labs(y="", x = "") +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title=element_text(size=14),
        legend.text=element_text(size=12), 
        axis.text=element_text(size=8),
        strip.text.x = element_text(size=14)) +
  guides(fill=guide_legend(title="Ny beläggning 2023",
                           title.position = "top",
                           label.position = "bottom",
                           nrow = 1),
         color = "none")
grid::grid.draw(ShiftLegend(nybel_age_km))

# Beläggning 2020-2023 per tillståndsklass och trafikmängd
# English
nybel_age_eng <- sweden23 %>% drop_na(trfkkls) %>%
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
  geom_bar(position = 'stack', stat = 'identity', linewidth = 1) +
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

# Täckning 2022-2023 per tillståndsklass och trafikmängd
nybel_tackning <- sweden24 %>% dplyr::filter(ny_bel == "Ja") %>%
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
  geom_bar(position = 'stack', stat = 'identity', linewidth = 1) +
  facet_wrap(~trfkkls, scales="free") +
  scale_color_manual(values=fill) +
  scale_fill_manual(values = c("gray78","gray68","gray38","gray20","gray88")) +
  labs(y="", x = "") +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12), 
        axis.text=element_text(size=8),
        strip.text.x = element_text(size=14)) +
  guides(fill=guide_legend(title="Täckningsgrad beläggningsåtgärder 20202",
                           title.position = "top",
                           label.position = "bottom",
                           nrow = 2),
         color = "none") +
  scale_y_continuous(labels = scales::percent)
grid::grid.draw(ShiftLegend(nybel_tackning))

# Täckning 2020-2023 per tillståndsklass och trafikmängd
# English
nybel_tackning_eng <- sweden23 %>% dplyr::filter(ny_bel == "Ja") %>%
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
  geom_bar(position = 'stack', stat = 'identity', linewidth = 1) +
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
max(sweden23$beldat_23,na.rm=TRUE)

#################################################
# Hur många sträckor har ändrats sen 2022?
sum(sweden24$brghtsk != sweden24$barig_24, na.rm=TRUE)
sum(sweden24$ådt_frd != sweden24$adt_24, na.rm=TRUE)
sum(sweden24$dou2017 != sweden24$dou_24, na.rm=TRUE)
sum(sweden24$vägbrdd != sweden24$vagbredd_2, na.rm=TRUE)
sum(sweden24$vägtyp != sweden24$vagtyp_24, na.rm=TRUE)

bärighet <- ComparisonBetweenYears(sweden24,sw24, quo(brghtsk),"brghtsk")
print(bärighet, n=Inf)

dou2017 <- ComparisonBetweenYears(sweden24,sw24, quo(dou2017),"dou2017")
print(dou2017, n=Inf)

vägtyp <- ComparisonBetweenYears(sweden24,sw24, quo(vägtyp),"vägtyp")
print(vägtyp, n=Inf)

trfkkls <- ComparisonBetweenYears(sweden24,sw24, quo(trfkkls),"trfkkls")
print(trfkkls, n=Inf)

beltyp <- ComparisonBetweenYears(sweden24_comp,sw24, quo(PavementType),"PavementType")
print(beltyp, n=Inf)

###################################################
# Check PCI
min(sw24$PCI_24)
max(sw24$PCI_24)

# Check roads with value 100 and beldat not 2021
sw24[sw24$PCI_24 == 100 & sw24$blggnngsd < as.Date("2023-01-01"),]

# Check roads with > 80
sw24[sw24$PCI_24 > 80 & sw24$blggnngsd < as.Date("2023-01-01"),]

#  Studera sträckor med index under 20 som inte överskrider underhållstandard
# eller åldersgränsen

low <- sw24[PCI_24 <= 20 & !(RMS_Index_24 <= 20 | IRI_Index_24  <= 20 | Rut_Index_24 <= 20)]
low <- sw24[PCI_24 <= 20 & !(ålder >= PredictedServiceLife  | iri  >= IRI_maint | spårdjp >= SP_maint)]
nrow(low)
head(low)
tail(low)

#################################################
# Jämför PCI
head(sw24)
mean(sw24$tllstnl)
mean(sw24$PCI_24)

pci_2024_vs_2023 <- ConditionComparisonBetweenYears24(sw24,sw24, NA,NA,single=TRUE)
print(pci_2024_vs_2023, n=Inf)

pci_hast_2024_vs_2023 <- ConditionComparisonBetweenYears24(sw24,sw24, quo(hastght),"hastght")
print(pci_hast_2024_vs_2023, n=Inf)

pci_dou_2024_vs_2023 <- ConditionComparisonBetweenYears24(sw24,sw24, quo(dou2017),"dou2017")
print(pci_dou_2024_vs_2023, n=Inf)

# PCI 
cond_eng <- sw23 %>%
  dplyr::select(indxkls, PCIClass_23, längd) %>%
  dplyr::rename(PCIClass_2023 = PCIClass_23) %>%
  dplyr::rename(PCIClass_2020 = indxkls) %>%
  tidyr::pivot_longer(cols = PCIClass_2020:PCIClass_2023, 
                      names_to = "Year", 
                      values_to = "PCIClass") %>%
  dplyr::mutate(Year = factor(Year)) %>%
  dplyr::mutate(Year = recode(Year, 
                              "PCIClass_2020" = "2020", 
                              "PCIClass_2023" = "2023")) %>% 
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
print(cond)

# PCI 
sw20_PCI <- sw20 %>% dplyr::select(ID, PCIClass_2020)
sw23_PCI <- sweden23 %>% dplyr::select(id, indxkls, PCIC_23)
sw24_comp <- dplyr::left_join(sw24, sw20_PCI, by = c("id" = "ID"))
sw24_comp <- dplyr::left_join(sw24_comp, sw23_PCI, by = "id")

head(sw24_comp)

cond <- sw24_comp %>%
  dplyr::select(PCIClass_2020, indxkls.y, PCIC_23, PCIClass_24, längd) %>%
  dplyr::rename(PCIClass_2021 = indxkls.y) %>%
  dplyr::rename(PCIClass_2022 = PCIC_23) %>%
  dplyr::rename(PCIClass_2023 = PCIClass_24) %>%
  tidyr::pivot_longer(cols = PCIClass_2020:PCIClass_2023, 
                      names_to = "Year", 
                      values_to = "PCIClass") %>%
  dplyr::mutate(Year = factor(Year)) %>%
  dplyr::mutate(Year = recode(Year, 
                              "PCIClass_2021" = "2021", 
                              "PCIClass_2022" = "2022",
                              "PCIClass_2023" = "2023",
                              "PCIClass_2020" = "2020")) %>% 
  dplyr::mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                  "5" ="Mycket bra", 
                                  "4" = "Bra", 
                                  "3" = "Tillfredsställande", 
                                  "2" = "Dålig", 
                                  "1" = "Mycket dålig")) %>%
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
print(cond)

head(cond)
# Export to Excel
tillstand <- createWorkbook()
addWorksheet(tillstand, "Tillstånd 2020-2023")
writeData(tillstand, sheet = 1, cond)
#Save Workbook
saveWorkbook(tillstand, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Uppdatering 2024/Skickat/Tillstånd_2020_2021_2023.xlsx", overwrite = TRUE)


# PCI barchart region
cond_p <- sw24_comp %>%
  dplyr::select(region, PCIClass_2020, indxkls.y, PCIC_23, PCIClass_24, längd) %>%
  dplyr::rename(PCIClass_2021 = indxkls.y) %>%
  dplyr::rename(PCIClass_2022 = PCIC_23) %>%
  dplyr::rename(PCIClass_2023 = PCIClass_24) %>%
  tidyr::pivot_longer(cols = PCIClass_2023:PCIClass_2020, 
                      names_to = "Year", 
                      values_to = "PCIClass") %>%
  dplyr::mutate(Year = str_replace(Year, "PCIClass_", "")) %>%
  dplyr::mutate(Year = as.numeric(Year)) %>%
  dplyr::filter(Year > 2021) %>%
  dplyr::mutate(Year = factor(Year)) %>%
  dplyr::mutate(Region = region) %>%
  dplyr::mutate(Region = factor(Region)) %>%
  dplyr::mutate(Region = recode(Region, Ost="Öst", Vast="Väst", Gotland="Sthlm")) %>%
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
        axis.text=element_text(size=10),
        strip.text.x = element_text(size=16)) +
  guides(fill=guide_legend(label.position = "bottom",
                           nrow = 1)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

print(cond_p)

# PCI barchart region
cond_p_eng <- sw24 %>%
  dplyr::select(region, indxkls, PCIClass_24, längd) %>%
  dplyr::rename(PCIClass_2023 = PCIClass_24) %>%
  dplyr::rename(PCIClass_2021 = indxkls) %>%
  tidyr::pivot_longer(cols = PCIClass_2021:PCIClass_2023, 
                      names_to = "Year", 
                      values_to = "PCIClass") %>%
  dplyr::mutate(Year = factor(Year)) %>%
  dplyr::mutate(Year = recode(Year, 
                              "PCIClass_2021" = "2021", 
                              "PCIClass_2023" = "2023")) %>% 
  dplyr::mutate(Region = region) %>%
  dplyr::mutate(Region = factor(Region)) %>%
  dplyr::mutate(Region = recode(Region, Ost="Öst", Vast="Väst", Gotland="Sthlm")) %>%
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

unique(sw24_comp$RoadType)

# PCI barchart vägtyp
cond_v <- sw24_comp %>%
  dplyr::select(RoadType, PCIClass_2020, indxkls.y, PCIC_23, PCIClass_24, längd) %>%
  dplyr::rename(PCIClass_2021 = indxkls.y) %>%
  dplyr::rename(PCIClass_2022 = PCIC_23) %>%
  dplyr::rename(PCIClass_2023 = PCIClass_24) %>%
  tidyr::pivot_longer(cols = PCIClass_2023:PCIClass_2020, 
                      names_to = "Year", 
                      values_to = "PCIClass") %>%
  dplyr::mutate(Year = str_replace(Year, "PCIClass_", "")) %>%
  dplyr::mutate(Year = as.numeric(Year)) %>%
  dplyr::filter(Year > 2021) %>%
  dplyr::mutate(Year = as.factor(Year)) %>%
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
  #mutate(percentage = ifelse(RoadType == "2+1 väg" & PCIClass == "Mycket dålig" & Year == 2023, 
  #                           0.0655, percentage)) %>%
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
cond_v_eng <- sw23 %>%
  dplyr::select(RoadType, indxkls, PCIClass_23, längd) %>%
  dplyr::rename(PCIClass_2023 = PCIClass_23) %>%
  dplyr::rename(PCIClass_2020 = indxkls) %>%
  tidyr::pivot_longer(cols = PCIClass_2020:PCIClass_2023, 
                      names_to = "Year", 
                      values_to = "PCIClass") %>%
  dplyr::mutate(Year = factor(Year)) %>%
  dplyr::mutate(Year = recode(Year, 
                              "PCIClass_2020" = "2020", 
                              "PCIClass_2023" = "2023")) %>% 
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
  mutate(percentage = ifelse(RoadType == "2+1 road" & PCIClass == "Very poor" & Year == 2023, 
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
sw23_lankom <- dplyr::left_join(sw23,lankom, by = c("län_nr" = "Län", "kmmn_nr" = "Kommunnr"))

pci_lan_2023_vs_2020 <- ConditionComparisonBetweenYears(sw23_lankom,sw23_lankom, quo(Länsnamn),"Länsnamn")
print(pci_lan_2023_vs_2020, n=Inf)

###############################################################
# Above maintenance standard
maintstandlengthdou <- sw24 %>%
  group_by(dou2017) %>%
  summarise(grouplen = sum(längd)/1000,
            percabove= sum(längd[(sparm17_24 > SP_maint & vägbrdd > 6) | (sparm15_24 > SP_maint & vägbrdd <= 6)| irih_24 > IRI_maint], na.rm = TRUE)/1000/grouplen,
            lenabove= percabove*grouplen)
print(maintstandlengthdou)
sum(maintstandlengthdou$lenabove)/sum(maintstandlengthdou$grouplen)
sum(maintstandlengthdou$lenabove)

print(xtable(maintstandlengthdou), include.rownames = FALSE)

# Older than expected lifetime
age_above <- sw24 %>%
  group_by(trfkkls) %>%
  summarise(mean_PredSL = round(mean(PredictedServiceLife,na.rm=TRUE),0),
            grouplen = sum(längd)/1000,
            percabove= sum(längd[ålder > PredictedServiceLife & RoadType == "Ordinary road"], na.rm = TRUE)/1000/grouplen,
            lenabove= percabove*grouplen)
print(age_above)

###############################################################
# Jämför teckningsgrad

sw24 <- sw24 %>% dplyr::mutate(bel_year = format(blggnngsd,"%Y")) %>%
  dplyr::mutate(tackning = if_else_na(tackning == "NULL",NA,tackning))

tackning_per_year <- QualitativeStatsDoubleGroup(sw24, quo(bel_year),quo(tackning), quo(längd))
tackning_per_year <- tackning_per_year %>% dplyr::filter(bel_year > 2016) %>% 
  dplyr::mutate(prop = round(prop*100,1)) %>%
  dplyr::mutate(grouplen = round(grouplen,0)) %>%
  dplyr::mutate(tackning = if_else_na(is.na(tackning),"Okänd",tackning))
print(tackning_per_year, n=Inf)
print(xtable(tackning_per_year), include.rownames = FALSE)

# Plot täckningsgrad 2019-2023
tackning_per_year_hel_vs_flack <- tackning_per_year  %>%
  dplyr::filter(bel_year > 2017) %>% 
  dplyr::filter(tackning != "Okänd") %>% 
  dplyr::mutate(perc = round(prop/100,1)) %>%
  dplyr::mutate(tackning_hel_flack = if_else_na(tackning != "Heltäckande","Fläckvis",tackning))
tackning_per_year_hel_vs_flack <- QualitativeStatsDoubleGroup(tackning_per_year_hel_vs_flack, quo(bel_year),quo(tackning_hel_flack), quo(grouplen*1000))

tackning_per_year_hel_vs_flack_plot <- tackning_per_year_hel_vs_flack  %>%  
  ggplot(aes(x = bel_year, y = prop, fill = tackning_hel_flack, label = paste0(round(100*prop,0)," %"))) +
  geom_bar(position = 'fill', stat = 'identity') +
  labs(y="", x = "") +
  scale_y_continuous(labels = scales::percent) +
 scale_fill_manual(values=c("seashell3","seashell4")) +
  theme(legend.position="bottom", legend.direction="vertical",
        legend.title = element_blank(), 
        legend.text=element_text(size=16), 
        axis.text=element_text(size=14),
        strip.text.x = element_text(size=16)) +
  guides(fill=guide_legend(label.position = "bottom",
                           nrow = 1)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
tackning_per_year_hel_vs_flack_plot

tackning_per_year_hel_vs_flack_plot_km <- tackning_per_year_hel_vs_flack  %>%  
  ggplot(aes(x = bel_year, y = grouplen, fill = tackning_hel_flack, label = paste0(grouplen," km"))) +
  geom_bar(stat="identity") +
  labs(y="", x = "") +
  scale_fill_manual(values=c("seashell3","seashell4")) +
  theme(legend.position="bottom", legend.direction="vertical",
        legend.title = element_blank(), 
        legend.text=element_text(size=16), 
        axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        strip.text.x = element_text(size=16)) +
  guides(fill=guide_legend(label.position = "bottom",
                           nrow = 1)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
tackning_per_year_hel_vs_flack_plot_km

sw23_2020 <- sw23 %>% dplyr::filter(bel_year >= 2020)
tackning_per_trafikklass <- QualitativeStatsDoubleGroup(sw23_2020, quo(trfkkls), quo(tackning), quo(längd))
tackning_per_trafikklass <- tackning_per_trafikklass %>% dplyr::mutate(prop = round(prop*100,1)) %>%
  dplyr::mutate(grouplen = round(grouplen,0)) %>%
  dplyr::mutate(tackning = if_else_na(is.na(tackning),"Okänd",tackning)) %>%
  dplyr::filter(tackning != "Okänd")
tackning_per_trafikklass <- na.omit(tackning_per_trafikklass)
print(tackning_per_trafikklass, n=Inf)

print(xtable(tackning_per_trafikklass), include.rownames = FALSE)

trafikklass_tackning <- QualitativeStatsDoubleGroup(sw23_2020, quo(tackning), quo(trfkkls), quo(längd))
trafikklass_tackning <- dplyr::mutate(prop = round(prop*100,1)) %>%
  dplyr::mutate(grouplen = round(grouplen,0)) %>%
  dplyr::mutate(tackning = if_else_na(is.na(tackning),"Okänd",tackning))
trafikklass_tackning <- na.omit(trafikklass_tackning)
print(trafikklass_tackning, n=Inf)

print(xtable(trafikklass_tackning), include.rownames = FALSE)

###############################################################
# Jämför rekonstruktionsbehov

sw23_pci_below_5_2020 <- sw23 %>% dplyr::filter(tllstni <= 5)
sw23_pci_below_5_2023 <- sw23 %>% dplyr::filter(PCI_23 <= 5)

rek_2020 <- QualitativeStatsSingleGroup(sw23_pci_below_5_2020, quo(trfkkls), quo(längd))
rek_2020 <- na.omit(rek_2020)
rek_2023 <- QualitativeStatsSingleGroup(sw23_pci_below_5_2023, quo(trfkkls), quo(längd))
rek_2023 <- na.omit(rek_2023)

tot_langd <- QualitativeStatsSingleGroup(sw23, quo(trfkkls), quo(längd))
tot_langd <- na.omit(tot_langd)
print(tot_langd)

rek_2020_2023 <- left_join(rek_2020,rek_2023, by=c("trfkkls"))
names(rek_2020_2023) <- c("trfkkls","grouplen_2020","prop_2020","grouplen_2023","prop_2023")
rek_2020_2023 <- left_join(rek_2020_2023,tot_langd, by=c("trfkkls"))
rek_2020_2023 <- rek_2020_2023 %>% 
  dplyr::mutate(prop_2020 = grouplen_2020/grouplen) %>%
  dplyr::mutate(prop_2023 = grouplen_2023/grouplen)
print(rek_2020_2023)
sum(rek_2020_2023$grouplen_2023)

print(xtable(rek_2020_2023), include.rownames = FALSE)


################################
# Bitumenindex 2021-2023

i2021 <- mean(c(2244,2450,2700,3070,3035,3090,3080,3410,3380,3350,3845,3805))
i2023 <- mean(c(3675,3655,3960,4080,4155,4315,4455,4830,5390,5935,5785,5075))
i2023/i2021

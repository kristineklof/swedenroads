fill <- c("#20AC65", "#71C94B","#FABF20","#F2303E","#C40A3B")

# PCI 
cond <- sw26 %>%
  drop_na(PCIClass_21) %>%
  drop_na(PCIClass_22) %>%
  drop_na(PCIClass_23) %>%
  drop_na(PCIClass_24) %>%
  tidyr::pivot_longer(cols = c(PCIClass_21, PCIClass_22,
                               PCIClass_23, PCIClass_24,
                               PCIClass_25, PCIClass_26), 
                      names_to = "År", 
                      values_to = "PCIClass") %>%
  dplyr::mutate(År = factor(År)) %>%
  dplyr::mutate(År = recode(År, 
                            "PCIClass_21" = "2020", 
                            "PCIClass_22" = "2021", 
                            "PCIClass_23" = "2022", 
                            "PCIClass_24" = "2023", 
                            "PCIClass_25" = "2024",
                            "PCIClass_26" = "2025")) %>% 
  dplyr::mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                  "5" ="Mycket bra", 
                                  "4" = "Bra", 
                                  "3" = "Tillfredsställande", 
                                  "2" = "Dålig", 
                                  "1" = "Mycket dålig")) %>%
  group_by(År, PCIClass) %>%
  summarise(grouplen = sum(längd)/1000) %>%
  mutate(percentage = grouplen/sum(grouplen)) %>%
  mutate(rounded_pct = round(100 * percentage)) %>%
  mutate(adjustment = 100 - sum(rounded_pct)) %>%
  arrange(desc(rounded_pct)) %>%
  mutate(rounded_pct = ifelse(row_number() == 1, rounded_pct + adjustment, rounded_pct)) %>%
  #mutate(rounded_pct = if_else(År == 2025 & PCIClass == "Bra", 26, rounded_pct))  %>%
  #mutate(rounded_pct = if_else(År == 2025 & PCIClass == "Mycket dålig", 10, rounded_pct))  %>%
  ggplot(aes(x = År, y = percentage, fill = PCIClass, label = paste0(rounded_pct, " %"))) +
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
cond

####################################################################
# PCI barchart region
cond_p <- sw26 %>%
  dplyr::select(Länsnamn, PCIClass_21, PCIClass_22, PCIClass_23, PCIClass_24, PCIClass_25, PCIClass_26, längd) %>%
  dplyr::mutate(PCIClass_21 = if_else(is.na(PCIClass_21),PCIClass_25,PCIClass_21),
                PCIClass_22 = if_else(is.na(PCIClass_22),PCIClass_25,PCIClass_22),
                PCIClass_23 = if_else(is.na(PCIClass_23),PCIClass_25,PCIClass_23),
                PCIClass_24 = if_else(is.na(PCIClass_24),PCIClass_25,PCIClass_24)) %>%
  # drop_na(PCIClass_21) %>%
  # drop_na(PCIClass_22) %>%
  # drop_na(PCIClass_23) %>%
  # drop_na(PCIClass_24) %>%
  tidyr::pivot_longer(
    cols = c(PCIClass_21, PCIClass_22, PCIClass_23, PCIClass_24, PCIClass_25, PCIClass_26),
    names_to = "Year",
    values_to = "PCIClass"
  ) %>%
  dplyr::mutate(Year = str_replace(Year, "PCIClass_", "")) %>%
  dplyr::mutate(Year = (as.numeric(Year))-1+2000) %>%
  #dplyr::filter(Year > 2022) %>%
  dplyr::mutate(Year = as.factor(Year)) %>%
  dplyr::mutate(Region = TRVRegionFrom2024(Länsnamn)) %>%
  dplyr::mutate(Region = factor(Region)) %>%
  dplyr::mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                  "5" ="Mycket bra", 
                                  "4" = "Bra", 
                                  "3" = "Tillfredsställande", 
                                  "2" = "Dålig", 
                                  "1" = "Mycket dålig")) %>%
  group_by(Region, Year, PCIClass) %>%
  summarise(grouplen = sum(längd)/1000) %>%
  mutate(percentage = grouplen/sum(grouplen)) %>%
  # Normalize percentages so that rounded values sum to 100%
  group_by(Region, Year) %>%
  mutate(rounded_pct = round(100 * percentage)) %>%
  mutate(adjustment = 100 - sum(rounded_pct)) %>%
  arrange(desc(rounded_pct)) %>%
  mutate(rounded_pct = ifelse(row_number() == 1, rounded_pct + adjustment, rounded_pct)) %>%
  ggplot(aes(x = Year, y = percentage, fill = PCIClass, label = paste0(rounded_pct, " %"))) +
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

##########################################################################
# PCI barchart vägtyp
cond_v <- sw26 %>%
  dplyr::select(roadtyp, PCIClass_21, PCIClass_22, PCIClass_23, PCIClass_24, PCIClass_25, PCIClass_26, längd) %>%
  dplyr::mutate(PCIClass_21 = if_else(is.na(PCIClass_21),PCIClass_25,PCIClass_21),
                PCIClass_22 = if_else(is.na(PCIClass_22),PCIClass_25,PCIClass_22),
                PCIClass_23 = if_else(is.na(PCIClass_23),PCIClass_25,PCIClass_23),
                PCIClass_24 = if_else(is.na(PCIClass_24),PCIClass_25,PCIClass_24)) %>%
  
  # drop_na(PCIClass_21) %>%
  # drop_na(PCIClass_22) %>%
  # drop_na(PCIClass_23) %>%
  # drop_na(PCIClass_24) %>%
  tidyr::pivot_longer(cols = c(PCIClass_21, PCIClass_22, PCIClass_23, PCIClass_24, PCIClass_25, PCIClass_26), 
                      names_to = "Year", 
                      values_to = "PCIClass") %>%
  dplyr::mutate(Year = str_replace(Year, "PCIClass_", "")) %>%
  dplyr::mutate(Year = (as.numeric(Year))-1+2000) %>%
  dplyr::filter(Year > 2022) %>%
  dplyr::mutate(Year = as.factor(Year)) %>%
  drop_na(roadtyp) %>%
  dplyr::mutate(RoadType = as.factor(roadtyp)) %>%
  dplyr::mutate(RoadType = recode(RoadType, "Ordinary road" = "Vanlig väg", 
                                  "2+1 road" = "2+1 väg", 
                                  "Undivided motorway" = "Motortrafikled", 
                                  "Motorway" = "Motorväg", 
                                  "4-lane road" = "4-fälts väg")) %>%
  dplyr::filter(RoadType != "Motortrafikled") %>%
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
  # Normalize percentages so that rounded values sum to 100%
  group_by(RoadType, Year) %>%
  mutate(rounded_pct = round(100 * percentage)) %>%
  mutate(adjustment = 100 - sum(rounded_pct)) %>%
  arrange(desc(rounded_pct)) %>%
  mutate(rounded_pct = ifelse(row_number() == 1, rounded_pct + adjustment, rounded_pct)) %>%
  ggplot(aes(x = Year, y = percentage, fill = PCIClass, label = paste0(rounded_pct, " %"))) +
  #mutate(percentage = ifelse(RoadType == "2+1 väg" & PCIClass == "Mycket dålig" & Year == 2023, 
  #                           0.0655, percentage)) %>%
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

# Above mainteance standard
maintstandlengthdou <- sw26 %>%
  group_by(dou2017) %>%
  summarise(grouplen = sum(längd)/1000,
            groupperc = grouplen/85750.23,
            percabove= sum(längd[spårdjp > sp_mant| iri > iri_mnt], na.rm = TRUE)/1000/grouplen,
            lenabove = percabove*grouplen)
print(maintstandlengthdou)
sum(maintstandlengthdou$lenabove)/sum(maintstandlengthdou$grouplen)

#  Vägytemätningsdatum
as.Date(quantile(unclass(sw26$matdatum), probs = c(0.02, 0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE), origin = "1970-01-01")d

# Older than expected lifetime
age_above <- sw26 %>%
  group_by(trfkkls) %>%
  summarise(mean_PredSL = round(mean(PredictedServiceLife,na.rm=TRUE),0),
            grouplen = sum(längd)/1000,
            percabove= sum(längd[ålder > PredictedServiceLife & roadtyp == "Ordinary road"], na.rm = TRUE)/1000/grouplen,
            lenabove= percabove*grouplen)
print(age_above)

# How many roads have been maintained since 2024?
sw26 <- sw26 %>% 
  dplyr::mutate(ny_bel = if_else_na(beldatum > as.Date("2024-12-31"), "Ja", "Nej"))
nybel_2025 <- QualitativeStatsSingleGroup(sw26, quo(ny_bel), quo(längd))
nybel_2025

sw26 <- sw26 %>% 
  dplyr::mutate(ny_bel_24 = if_else_na((beldatum > as.Date("2023-12-31") & beldat_25 < as.Date("2025-01-01")), "Ja", "Nej"))
nybel_2024 <- QualitativeStatsSingleGroup(sw26, quo(ny_bel_24), quo(längd))
nybel_2024

# Beläggning 2023-2025 per tillståndsklass och trafikmängd
# procent
nybel_age <- sw26 %>% drop_na(trfkkls) %>%
  drop_na(PCIClass_25) %>%
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
  dplyr::mutate(PCIClass = factor(PCIClass_25, levels = c("5","4","3","2","1"))) %>%
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
  guides(fill=guide_legend(title="Ny beläggning 2024",
                           title.position = "top",
                           label.position = "bottom",
                           nrow = 1),
         color = "none") +
  scale_y_continuous(labels = scales::percent) 
grid::grid.draw(ShiftLegend(nybel_age))

# Täckning per tillståndsklass och trafikmängd
nybel_tackning <- sw26 %>% dplyr::filter(ny_bel == "Ja") %>%
  drop_na(trfkkls) %>%
  drop_na(tacknng) %>%
  drop_na(PCIClass_25) %>%
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
  dplyr::mutate(PCIClass = factor(PCIClass_25, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                  "5" ="Mycket bra", 
                                  "4" = "Bra", 
                                  "3" = "Tillfredsställande", 
                                  "2" = "Dålig", 
                                  "1" = "Mycket dålig")) %>%
  group_by(trfkkls, PCIClass, tacknng) %>%
  summarise(grouplen = sum(längd)/1000) %>%
  dplyr::mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = PCIClass, y = percentage, 
             fill = tacknng, col = PCIClass)) +
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
  guides(fill=guide_legend(title="Täckningsgrad beläggningsåtgärder 2025",
                           title.position = "top",
                           label.position = "bottom",
                           nrow = 2),
         color = "none") +
  scale_y_continuous(labels = scales::percent)
grid::grid.draw(ShiftLegend(nybel_tackning))

###############################################################
# Jämför teckningsgrad

sw26_tackning <- sw26 %>% dplyr::mutate(bel_year = format(beldatum,"%Y")) %>%
  dplyr::mutate(tackning = if_else_na(tacknng == "NULL",NA,tacknng))

tackning_per_year <- QualitativeStatsDoubleGroup(sw26_tackning, quo(bel_year),quo(tackning), quo(längd))
tackning_per_year <- tackning_per_year %>% dplyr::filter(bel_year > 2016) %>% 
  dplyr::mutate(prop = round(prop*100,1)) %>%
  dplyr::mutate(grouplen = round(grouplen,0)) %>%
  dplyr::mutate(tackning = if_else_na(is.na(tackning),"Okänd",tackning))
print(tackning_per_year, n=Inf)

# Plot täckningsgrad 2019-2025
tackning_per_year_hel_vs_flack <- tackning_per_year  %>%
  dplyr::filter(bel_year > 2018) %>% 
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
  filter( bel_year %in% c("2021","2022","2023","2024","2025")) %>%
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

# Per trafikklass
sw26_tackning_trfk <- sw26 %>%
  mutate(
    bel_year = as.integer(format(beldatum, "%Y")),
    tackning = if_else_na(tacknng == "NULL", NA, tacknng)
  ) %>%
  filter(bel_year > 2020) %>%            # same period as your plot, adjust if you want
  filter(!is.na(trfkkls))

tackning_km_year_trfk_hel_flack <- sw26_tackning_trfk %>%
  filter(!is.na(tackning)) %>%
  mutate(
    tackning_hel_flack = if_else_na(tackning != "Heltäckande", "Fläckvis", tackning)
  ) %>%
  group_by(trfkkls, bel_year, tackning_hel_flack) %>%
  summarise(grouplen = sum(längd, na.rm = TRUE) / 1000, .groups = "drop")

tackning_km_year_trfk_hel_flack_plot <- tackning_km_year_trfk_hel_flack %>%
  ggplot(aes(x = factor(bel_year), y = grouplen, fill = tackning_hel_flack,
             label = paste0(round(grouplen, 0), " km"))) +
  geom_bar(stat="identity") +
  facet_wrap(~ trfkkls) +
  labs(y="", x="") +
  guides(fill=guide_legend(label.position="bottom", nrow=1)) +
  geom_text(size=3, position=position_stack(vjust=0.5))

tackning_km_year_trfk_hel_flack_plot

# Per DoU2017
sw26_tackning_dou <- sw26 %>%
  mutate(
    bel_year = as.integer(format(beldatum, "%Y")),
    tackning = if_else_na(tacknng == "NULL", NA, tacknng)
  ) %>%
  filter(bel_year > 2020) %>%            # same period as your plot, adjust if you want
  filter(!is.na(dou2017))

tackning_km_year_dou_hel_flack <- sw26_tackning_trfk %>%
  filter(!is.na(tackning)) %>%
  mutate(
    tackning_hel_flack = if_else_na(tackning != "Heltäckande", "Fläckvis", tackning)
  ) %>%
  group_by(dou2017, bel_year, tackning_hel_flack) %>%
  summarise(grouplen = sum(längd, na.rm = TRUE) / 1000, .groups = "drop")

tackning_km_year_dou_hel_flack_plot <- tackning_km_year_dou_hel_flack %>%
  dplyr::mutate(
    dou2017 = dplyr::recode(
      as.character(dou2017),
      "1" = "Storstadsvägar",
      "2" = "Vägar som bildar större sammanhängande stråk",
      "3" = "Vägar för dagliga resor och arbetspendling",
      "4" = "Övriga för näringslivet viktiga vägar",
      "5" = "Vägar som är viktiga för landsbygden",
      "6" = "Lågtrafikerade vägar",
      .default = as.character(dou2017)
    ),
    dou2017 = factor(dou2017, levels = c(
      "Storstadsvägar",
      "Vägar som bildar större sammanhängande stråk",
      "Vägar för dagliga resor och arbetspendling",
      "Övriga för näringslivet viktiga vägar",
      "Vägar som är viktiga för landsbygden",
      "Lågtrafikerade vägar"
    )),
    tackning_hel_flack = factor(tackning_hel_flack, levels = c("Fläckvis", "Heltäckande"))
  ) %>%
  # REMOVE Fläckvis only in the Storstadsvägar panel
  dplyr::filter(!(dou2017 == "Storstadsvägar" & tackning_hel_flack == "Fläckvis")) %>%
  ggplot(aes(x = factor(bel_year), y = grouplen, fill = tackning_hel_flack,
             label = paste0(round(grouplen, 0), " km"))) +
  geom_bar(stat = "identity") +
  facet_wrap(~ dou2017) +
  labs(y = "", x = "", fill = "Täckningsgrad") +
  scale_fill_manual(values = c("Fläckvis" = "seashell3", "Heltäckande" = "seashell4")) +
  guides(fill = guide_legend(title = "Täckningsgrad", label.position = "bottom", nrow = 1)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

tackning_km_year_dou_hel_flack_plot

# Per region
tackning_km_year_region_hel_flack <- sw26 %>%
  dplyr::mutate(
    bel_year = as.integer(format(beldatum, "%Y")),
    tackning = if_else_na(tacknng == "NULL", NA, tacknng),
    tackning = if_else_na(is.na(tackning), "Okänd", tackning),
    tackning_hel_flack = if_else_na(tackning != "Heltäckande", "Fläckvis", "Heltäckande"),
    tackning_hel_flack = factor(tackning_hel_flack, levels = c("Fläckvis", "Heltäckande")),
    Region = factor(TRVRegionFrom2024(Länsnamn))
  ) %>%
  dplyr::filter(bel_year > 2018, tackning != "Okänd") %>%
  dplyr::group_by(Region, bel_year, tackning_hel_flack) %>%
  dplyr::summarise(grouplen = sum(längd, na.rm = TRUE) / 1000, .groups = "drop")

tackning_km_year_region_hel_flack_plot <- tackning_km_year_region_hel_flack %>%
  ggplot(ggplot2::aes(x = factor(bel_year), y = grouplen, fill = tackning_hel_flack,
                      label = paste0(round(grouplen, 0), " km"))) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap(~ Region) +
  ggplot2::labs(y = "", x = "", fill = "Täckningsgrad") +
  ggplot2::scale_fill_manual(values = c("Fläckvis" = "lightblue", "Heltäckande" = "steelblue3")) +
  ggplot2::guides(fill = ggplot2::guide_legend(title = "Täckningsgrad", label.position = "bottom", nrow = 1)) +
  ggplot2::geom_text(size = 3, position = ggplot2::position_stack(vjust = 0.5))
tackning_km_year_region_hel_flack_plot

# Condition per treatment year
cond_km_per_year <- sw26 %>%
  mutate(
    bel_year = as.integer(format(beldatum, "%Y"))
  ) %>%
  filter(!is.na(bel_year)) %>%                       # only roads that got a treatment (have beldatum)
  select(bel_year, längd, starts_with("PCIClass_")) %>%
  pivot_longer(
    cols = starts_with("PCIClass_"),
    names_to = "pci_col",
    values_to = "PCIClass"
  ) %>%
  mutate(
    # PCIClass_21 -> 2020, PCIClass_25 -> 2024 etc (same logic you used earlier)
    cond_year = as.integer(str_replace(pci_col, "PCIClass_", "")) - 1 + 2000
  ) %>%
  filter(cond_year == bel_year) %>%                  # condition class from the treatment year
  mutate(
    PCIClass = factor(PCIClass, levels = c("5","4","3","2","1")),
    PCIClass = recode(PCIClass,
                      "5" = "Mycket bra",
                      "4" = "Bra",
                      "3" = "Tillfredsställande",
                      "2" = "Dålig",
                      "1" = "Mycket dålig"
    ),
    PCIClass = factor(PCIClass, levels = c("Mycket bra","Bra","Tillfredsställande","Dålig","Mycket dålig"))
  ) %>%
  group_by(bel_year, PCIClass) %>%
  summarise(km = sum(längd, na.rm = TRUE) / 1000, .groups = "drop")

cond_km_per_year_plot <- cond_km_per_year %>%
  ggplot(aes(x = factor(bel_year), y = km, fill = PCIClass,
             label = paste0(round(km, 0), " km"))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = setNames(fill, c("Mycket bra","Bra","Tillfredsställande","Dålig","Mycket dålig"))
  ) +
  labs(x = "", y = "", fill = "Tillståndsklass") +
  guides(fill = guide_legend(label.position = "bottom", nrow = 1)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
cond_km_per_year_plot

segments_2025_dalig <- sw26 %>%
  mutate(bel_year = as.integer(format(beldatum, "%Y"))) %>%
  filter(
    bel_year == 2025,
    PCIClass_26 == "2"   # 2025 condition = Dålig
  )
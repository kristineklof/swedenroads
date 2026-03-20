#=================================================================#
#              Condition 2025 per län and per kommun
#=================================================================#

head(sw26)
swedt_PCI_2025 <- sw26 %>% dplyr::select(Länsnamn, Kommunnamn, längd, PCIClass_26)

##########################################
# Nationellt
##########################################

QualitativeStatsSingleGroup(swedt_PCI_2025, quo(PCIClass_26), quo(längd))

##########################################
# Län
##########################################

pci_lan_2025 <- QualitativeStatsDoubleGroup(swedt_PCI_2025, quo(Länsnamn), quo(PCIClass_26), quo(längd))
pci_lan_2025 <- pci_lan_2025 %>% 
  mutate(PCIClass = factor(PCIClass_26, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(grouplen = round(grouplen,0)) %>%
  mutate(prop = prop*100) %>%
  mutate(prop = round(prop,1))

sum(pci_lan_2025$grouplen)


# Export to Excel
wb <- createWorkbook()
addWorksheet(wb, "Tillstånd län 2025")
writeData(wb, sheet = 1, pci_lan_2025)
#Save Workbook
saveWorkbook(wb, paste0(datapath,"2026/Tillstånd per län 2025.xlsx"), overwrite = TRUE)

##########################################
# Kommun
##########################################

cols <- c("id","Kommunnamn","längd")

pci_kom_2024 <- QualitativeStatsDoubleGroup(swedt_PCI_2024, quo(Kommunnamn), quo(PCIClass_25), quo(längd))
pci_kom_2024 <- pci_kom_2024 %>% 
  mutate(PCIClass = factor(PCIClass_25, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass_25, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(grouplen = round(grouplen,0)) %>%
  mutate(prop = prop*100) %>%
  mutate(prop = round(prop,1))

sum(pci_kom_2024$grouplen)
sw_lan_lookup <- swedt_PCI_2024[, .(Länsnamn = first(Länsnamn)), by = Kommunnamn]
pci_kom_2024 <- pci_kom_2024 %>%
  left_join(
    as.data.frame(sw_lan_lookup),
    by = "Kommunnamn"
  )

# Export to Excel
wb <- createWorkbook()
addWorksheet(wb, "Tillstånd 2024")
writeData(wb, sheet = 1, pci_kom_2024)
#Save Workbook
saveWorkbook(wb, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Uppdatering 2025/Tillstånd per kommun 2024.xlsx", overwrite = TRUE)

##########################################
# Vägtrummor
##########################################

vagtrummor_with_status <- st_read(paste0(datapath,"2026/vagtrummor_with_status.shp"))
only_vagtrummor_with_status <- vagtrummor_with_status %>% dplyr::filter(!is.na(globald))

trumstatus_by_lan <- function(data, include_na_status = FALSE, digits = 1) {
  # Drop geometry if it's an sf object
  if (inherits(data, "sf")) {
    data <- sf::st_drop_geometry(data)
  }
  
  # Rename status value
  data <- data %>%
    mutate(status = recode(status, "Åtgärdsbehov" = "Brister finns"))
  
  # Keep or remove NA in status
  if (!include_na_status) {
    data <- data %>% filter(!is.na(status))
  }
  
  data %>%
    count(länsnmn, status, name = "n") %>%
    group_by(länsnmn) %>%
    mutate(
      pct = round(100 * n / sum(n), digits)
    ) %>%
    ungroup() %>%
    arrange(länsnmn, desc(n))
}

status_summary <- trumstatus_by_lan(only_vagtrummor_with_status)
print(status_summary)

status_summary %>%
  filter(status == "Brister finns") %>%
  arrange(desc(n)) %>%
  print()

# Export to Excel
wb <- createWorkbook()
addWorksheet(wb, "Vägtrummor län 2025")
writeData(wb, sheet = 1, status_summary)
#Save Workbook
saveWorkbook(wb, paste0(datapath,"2026/Vägtrummor tillstånd per län 2025.xlsx"), overwrite = TRUE)


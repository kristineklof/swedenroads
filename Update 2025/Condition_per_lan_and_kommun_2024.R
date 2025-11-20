#=================================================================#
#              Condition 2025 per län and per kommun
#=================================================================#

head(sw25)
swedt_PCI_2024 <- sw25 %>% dplyr::select(Länsnamn, Kommunnamn, längd, PCIClass_25)

##########################################
# Nationellt
##########################################

QualitativeStatsSingleGroup(swedt_PCI_2024, quo(PCIClass_25), quo(längd))

##########################################
# Län
##########################################

pci_lan_2024 <- QualitativeStatsDoubleGroup(swedt_PCI_2024, quo(Länsnamn), quo(PCIClass_25), quo(längd))
pci_lan_2024 <- pci_lan_2024 %>% 
  mutate(PCIClass = factor(PCIClass_25, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(grouplen = round(grouplen,0)) %>%
  mutate(prop = prop*100) %>%
  mutate(prop = round(prop,1))

sum(pci_lan_2024$grouplen)


# Export to Excel
wb <- createWorkbook()
addWorksheet(wb, "Tillstånd län 2024")
writeData(wb, sheet = 1, pci_lan_2024)
#Save Workbook
saveWorkbook(wb, paste0(datapath,"2025/Tillstånd per län 2024.xlsx"), overwrite = TRUE)

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

# Export to Excel
wb <- createWorkbook()
addWorksheet(wb, "Tillstånd 2024")
writeData(wb, sheet = 1, pci_kom_2024)
#Save Workbook
saveWorkbook(wb, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Uppdatering 2025/Tillstånd per kommun 2024.xlsx", overwrite = TRUE)



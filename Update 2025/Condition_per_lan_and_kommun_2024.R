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

cols <- c("id","Kmmnnmn","längd")
pci2033 <- pci2033[swedt_PCI_2023[, ..cols], on = 'Objectd']
nrow(pci2033[Year == 2033])


pci_kom_2023 <- QualitativeStatsDoubleGroup(swedt_PCI_2023, quo(Kmmnnmn), quo(PCIClass), quo(längd))
pci_kom_2023 <- pci_kom_2023 %>% 
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(grouplen = round(grouplen,0)) %>%
  mutate(prop = prop*100) %>%
  mutate(prop = round(prop,1))

pci_kom_2033 <- QualitativeStatsDoubleGroup(pci2033[Year == 2033], quo(Kmmnnmn), quo(PCIClass), quo(Length))
pci_kom_2033 <- pci_kom_2033 %>% 
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(grouplen = round(grouplen,0)) %>%
  mutate(prop = prop*100) %>%
  mutate(prop = round(prop,1))

sum(pci_kom_2023$grouplen)
sum(pci_kom_2033$grouplen)

# Export to Excel
wb <- createWorkbook()
addWorksheet(wb, "Tillstånd 2023")
#addWorksheet(wb, "Tillstånd 2033")
writeData(wb, sheet = 1, pci_kom_2023)
#writeData(wb, sheet = 2, pci_kom_2033)
#Save Workbook
saveWorkbook(wb, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Uppdatering 2024/Tillstånd per kommun 2024.xlsx", overwrite = TRUE)



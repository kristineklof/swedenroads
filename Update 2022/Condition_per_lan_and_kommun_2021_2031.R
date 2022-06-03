#=================================================================#
#                 PCI values 2022 vs 2031 per län
#                       and per kommun
#=================================================================#

swedt_PCI <- st_read(paste0(datapath,"2022/sweden_2022_validation_file.shp"))
setDT(swedt_PCI)
setnames(swedt_PCI, "PCIC_22", "PCIClass")
head(swedt_PCI)
#pci2030 <- read.xlsx("C:/Users/winte/Swedenroads_outputs/2030_PCI_20201218.xlsx")
pci2030 <- fread("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2022/Scenarios/2022 Scenario 6 (2) - 2021 PCI's_ CI_All_Latest.csv")
names(pci2030) <- c("Objectd","Year","PCI")
setDT(pci2030)

pci2030 <- PCIClass(pci2030)
cols <- c("id","Länsnmn","längd")
pci2030 <- pci2030[swedt_PCI[, ..cols], on = c(Objectd='id')]
nrow(pci2030[Year == 2030])
head(pci2030)

##########################################
# Nationellt
##########################################

QualitativeStatsSingleGroup(swedt_PCI, quo(PCIClass), quo(längd))
QualitativeStatsSingleGroup(pci2030[Year == 2030], quo(PCIClass), quo(längd))

##########################################
# Län
##########################################

# Per Trafikklass
QualitativeStatsDoubleGroup(swedt_PCI, quo(Länsnmn), quo(trfkkls), quo(längd))

pci_lan_2022 <- QualitativeStatsDoubleGroup(swedt_PCI, quo(Länsnmn), quo(PCIClass), quo(längd))
pci_lan_2022 <- pci_lan_2022 %>% 
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(grouplen = round(grouplen,0)) %>%
  mutate(prop = prop*100) %>%
  mutate(prop = round(prop,1))

pci_lan_2030 <- QualitativeStatsDoubleGroup(pci2030[Year == 2030], quo(Länsnmn), quo(PCIClass), quo(längd))
pci_lan_2030 <- pci_lan_2030 %>% 
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(grouplen = round(grouplen,0)) %>%
  mutate(prop = prop*100) %>%
  mutate(prop = round(prop,1))

print(xtable(pci_lan_2022,digits=c(0,0,0,0,1)), include.rownames=FALSE)
print(xtable(pci_lan_2030,digits=c(0,0,0,0,1)), include.rownames=FALSE)

# Export to Excel
wb <- createWorkbook()
addWorksheet(wb, "Tillstånd 2022")
addWorksheet(wb, "Tillstånd 2030")
writeData(wb, sheet = 1, pci_lan_2022)
writeData(wb, sheet = 2, pci_lan_2030)
#Save Workbook
saveWorkbook(wb, paste0(datapath,"2022/Tillstånd per län 2022 och 2030.xlsx"), overwrite = TRUE)

##########################################
# Kommun
##########################################

cols <- c("Objectd","Kmmnnmn","Length")
pci2030 <- pci2030[swedt_PCI[, ..cols], on = 'Objectd']
nrow(pci2030[Year == 2030])


pci_kom_2020 <- QualitativeStatsDoubleGroup(swedt_PCI, quo(Kmmnnmn), quo(PCIClass), quo(Length))
pci_kom_2020 <- pci_kom_2020 %>% 
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(grouplen = round(grouplen,0)) %>%
  mutate(prop = prop*100) %>%
  mutate(prop = round(prop,1))

pci_kom_2030 <- QualitativeStatsDoubleGroup(pci2030[Year == 2030], quo(Kmmnnmn), quo(PCIClass), quo(Length))
pci_kom_2030 <- pci_kom_2030 %>% 
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(grouplen = round(grouplen,0)) %>%
  mutate(prop = prop*100) %>%
  mutate(prop = round(prop,1))

sum(pci_kom_2020$grouplen)
sum(pci_kom_2030$grouplen)

# Export to Excel
wb <- createWorkbook()
addWorksheet(wb, "Tillstånd 2020")
addWorksheet(wb, "Tillstånd 2030")
writeData(wb, sheet = 1, pci_kom_2020)
writeData(wb, sheet = 2, pci_kom_2030)
#Save Workbook
saveWorkbook(wb, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Slutrapport/Tillstånd per kommun 2020 och 2030.xlsx", overwrite = TRUE)

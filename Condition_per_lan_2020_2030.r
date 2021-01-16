#=================================================================#
#                 PCI values 2020 vs 2030 per län
#=================================================================#

swedt_PCI <- st_read( "C:/Users/winte/Swedenroads_outputs/sweden_v3_pci201206.shp") 
setDT(swedt_PCI)
#pci2030 <- read.xlsx("C:/Users/winte/Swedenroads_outputs/2030_PCI_20201218.xlsx")
pci2030 <- fread("C:/Users/winte/Swedenroads_outputs/7018-Scenario6.csv")

head(pci2030)
names(pci2030) <- c("Objectd","Year","PCI")
setDT(pci2030)

swedt_PCI <- PCIClass(swedt_PCI)
swedt_PCI[, PCIClass := as.factor(PCIClass)]
str(swedt_PCI)

pci2030 <- PCIClass(pci2030)
#pci2030[, PCIClass_2030 := as.factor(PCIClass)]
#pci2030[, PCIClass := NULL]
str(pci2030)
head(pci2030)

cols <- c("Objectd","Länsnmn","Length")
pci2030 <- pci2030[swedt_PCI[, ..cols], on = 'Objectd']
nrow(pci2030[Year == 2030])

# Stockholm
QualitativeStatsDoubleGroup(swedt_PCI, quo(Länsnmn), quo(tkl8), quo(Length))

pci_lan_2020 <- QualitativeStatsDoubleGroup(swedt_PCI, quo(Länsnmn), quo(PCIClass), quo(Length))
pci_lan_2020 <- pci_lan_2020 %>% 
    mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
    mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
    mutate(grouplen = round(grouplen,0)) %>%
    mutate(prop = prop*100) %>%
    mutate(prop = round(prop,1))

pci_lan_2030 <- QualitativeStatsDoubleGroup(pci2030[Year == 2030], quo(Länsnmn), quo(PCIClass), quo(Length))
pci_lan_2030 <- pci_lan_2030 %>% 
    mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
    mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
    mutate(grouplen = round(grouplen,0)) %>%
    mutate(prop = prop*100) %>%
    mutate(prop = round(prop,1))

print(xtable(pci_lan_2020,digits=c(0,0,0,0,1)), include.rownames=FALSE)
print(xtable(pci_lan_2030,digits=c(0,0,0,0,1)), include.rownames=FALSE)

# Export to Excel
wb <- createWorkbook()
addWorksheet(wb, "Tillstånd 2020")
addWorksheet(wb, "Tillstånd 2030")
writeData(wb, sheet = 1, pci_lan_2020)
writeData(wb, sheet = 2, pci_lan_2030)
#Save Workbook
saveWorkbook(wb, "C:/Users/winte/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Slutrapport/Tillstånd per län 2020 och 2030.xlsx", overwrite = TRUE)

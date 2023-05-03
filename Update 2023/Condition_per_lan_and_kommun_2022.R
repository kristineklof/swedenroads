#=================================================================#
#                 PCI values 2022 vs 2032 per län
#                       and per kommun
#=================================================================#

swedt_PCI_2022 <- st_read("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2023/sweden_2023_validation_file.shp")
setDT(swedt_PCI_2022)
setnames(swedt_PCI_2022, "PCIC_23", "PCIClass")
head(swedt_PCI_2022)
#pci2032 <- read.xlsx("C:/Users/winte/Swedenroads_outputs/2032_PCI_20201218.xlsx")
pci2032 <- fread("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2023/Scenarion/20230308_2023 Scenario 10_CI_OUT.csv")
names(pci2032) <- c("Year","Objectd","PCI")
setDT(pci2032)

pci2032 <- PCIClass(pci2032)
cols <- c("id","Länsnmn","längd")
pci2032 <- pci2032[swedt_PCI_2022[, ..cols], on = c(Objectd='id')]
nrow(pci2032[Year == 2032])
head(pci2032)

##########################################
# Nationellt
##########################################

QualitativeStatsSingleGroup(swedt_PCI_2022, quo(PCIClass), quo(längd))
QualitativeStatsSingleGroup(pci2032[Year == 2032], quo(PCIClass), quo(längd))

##########################################
# Län
##########################################

# Per Trafikklass
QualitativeStatsDoubleGroup(swedt_PCI_2022, quo(Länsnmn), quo(trfkkls), quo(längd))

pci_lan_2022 <- QualitativeStatsDoubleGroup(swedt_PCI_2022, quo(Länsnmn), quo(PCIClass), quo(längd))
pci_lan_2022 <- pci_lan_2022 %>% 
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(grouplen = round(grouplen,0)) %>%
  mutate(prop = prop*100) %>%
  mutate(prop = round(prop,1))

sum(pci_lan_2022$grouplen)

pci_lan_2032 <- QualitativeStatsDoubleGroup(pci2032[Year == 2032], quo(Länsnmn), quo(PCIClass), quo(längd))
pci_lan_2032 <- pci_lan_2032 %>% 
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(grouplen = round(grouplen,0)) %>%
  mutate(prop = prop*100) %>%
  mutate(prop = round(prop,1))

print(xtable(pci_lan_2022,digits=c(0,0,0,0,1)), include.rownames=FALSE)
print(xtable(pci_lan_2032,digits=c(0,0,0,0,1)), include.rownames=FALSE)

# Export to Excel
wb <- createWorkbook()
addWorksheet(wb, "Tillstånd 2022")
addWorksheet(wb, "Tillstånd 2032")
writeData(wb, sheet = 1, pci_lan_2022)
writeData(wb, sheet = 2, pci_lan_2032)
#Save Workbook
saveWorkbook(wb, paste0(datapath,"2023/Tillstånd per län 2022 och 2032.xlsx"), overwrite = TRUE)

##########################################
# Kommun
##########################################

cols <- c("Objectd","Kmmnnmn","Length")
pci2032 <- pci2032[swedt_PCI_2022[, ..cols], on = 'Objectd']
nrow(pci2032[Year == 2032])


pci_kom_2022 <- QualitativeStatsDoubleGroup(swedt_PCI_2022, quo(Kmmnnmn), quo(PCIClass), quo(Length))
pci_kom_2022 <- pci_kom_2022 %>% 
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(grouplen = round(grouplen,0)) %>%
  mutate(prop = prop*100) %>%
  mutate(prop = round(prop,1))

pci_kom_2032 <- QualitativeStatsDoubleGroup(pci2032[Year == 2032], quo(Kmmnnmn), quo(PCIClass), quo(Length))
pci_kom_2032 <- pci_kom_2032 %>% 
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(grouplen = round(grouplen,0)) %>%
  mutate(prop = prop*100) %>%
  mutate(prop = round(prop,1))

sum(pci_kom_2022$grouplen)
sum(pci_kom_2032$grouplen)

# Export to Excel
wb <- createWorkbook()
addWorksheet(wb, "Tillstånd 2022")
addWorksheet(wb, "Tillstånd 2032")
writeData(wb, sheet = 1, pci_kom_2022)
writeData(wb, sheet = 2, pci_kom_2032)
#Save Workbook
saveWorkbook(wb, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Slutrapport/Tillstånd per kommun 2022 och 2032.xlsx", overwrite = TRUE)

# Eksjö kommun
str(swedt_PCI_2022)
table(swedt_PCI_2022$vägtyp)
eksjo <- swedt_PCI_2022 %>% dplyr::filter(Kmmnnmn == "Eksjö") %>% 
  dplyr::filter(PCIClass == 1 | PCIClass == 2) %>%
  dplyr::select(PCIClass, vägnmmr, vägktgr, vägtyp, hastght, längd, Länsnmn, Kmmnnmn) %>%
  dplyr::mutate(vägtyp = VagtypNumTillText(vägtyp),
                vägktgr = VagkatNumTillText(vägktgr),
                PCIClass = if_else_na(PCIClass == 1, "Mycket dålig", "Dålig")) %>%
  dplyr::arrange(PCIClass,desc=TRUE) %>%
  dplyr::arrange(vägnmmr)
names(eksjo) <- c("Tillstånd","Vägnummer","Vägkategori","Vägtyp","Hastighet","Sträcklängd","Län","Kommun")
head(eksjo)
nrow(eksjo)

# Export to Excel
wb_eksjo <- createWorkbook()
addWorksheet(wb_eksjo, "Eksjö tillstånd 2022")
writeData(wb_eksjo, sheet = 1, eksjo)
#Save Workbook
saveWorkbook(wb_eksjo, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Slutrapport/Eksjö_2022.xlsx", overwrite = TRUE)

# Härnösand, Kramfors och Sollefteå
v_norr <- c("Härnösand", "Kramfors", "Sollefteå")
hks <- swedt_PCI_2022 %>% dplyr::filter(Kmmnnmn %in% v_norr) %>% 
  #dplyr::filter(PCIClass == 1 | PCIClass == 2) %>%
  dplyr::select(PCIClass, vägnmmr, vägktgr, vägtyp, hastght, längd, Länsnmn, Kmmnnmn) %>%
  dplyr::mutate(vägtyp = VagtypNumTillText(vägtyp),
                vägktgr = VagkatNumTillText(vägktgr),
                PCIClass = factor(PCIClass, levels = c("5","4","3","2","1")),
                PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  dplyr::arrange(PCIClass,desc=TRUE) %>%
  dplyr::arrange(Kmmnnmn, vägnmmr)
names(hks) <- c("Tillstånd","Vägnummer","Vägkategori","Vägtyp","Hastighet","Sträcklängd","Län","Kommun")
head(hks)
nrow(hks)

# Export to Excel
wb_hks <- createWorkbook()
addWorksheet(wb_hks, "H K S tillstånd 2022")
writeData(wb_hks, sheet = 1, hks)
#Save Workbook
saveWorkbook(wb_hks, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Uppdatering 2023/Härnösand Kramfors Sollefteå tillstånd 2022.xlsx", overwrite = TRUE)


# Gotland
gotland <- swedt_PCI_2022 %>% dplyr::filter(Kmmnnmn == "Gotland") %>% 
  dplyr::select(PCI_23, vägnmmr, längd) %>%
  group_by(vägnmmr) %>%
  summarise(PCI = weighted.mean(PCI_23, längd))
gotland <- PCIClass(gotland)

gotland <- gotland %>% 
  dplyr::mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                  "5" ="Mycket bra", 
                                  "4" = "Bra", 
                                  "3" = "Tillfredsställande", 
                                  "2" = "Dålig",
                                  "1" = "Mycket dålig")) %>%
  dplyr::mutate(PCI = round(PCI,0))
names(gotland) <- c("Vägnummer","Tillståndsindex","Tillstånd")
head(gotland)
nrow(gotland)

# Export to Excel
wb_gotland <- createWorkbook()
addWorksheet(wb_gotland, "Gotland tillstånd 2022")
writeData(wb_gotland, sheet = 1, gotland)
#Save Workbook
saveWorkbook(wb_gotland, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Slutrapport/Gotland_2022.xlsx", overwrite = TRUE)

########################################################
# Export validation shapefiles for Nacka and Norrköping

nacka <- swedt_PCI_2022 %>% dplyr::filter(Kmmnnmn == "Nacka") %>% 
  dplyr::select(id, brghtsk, hastght, dou2017, ådt_frd, ådt_tng, ådt_mtr, vägbrdd, vägnmmr, vägktgr, vägtyp, längd,
    Kmmnnmn, PCI_23, PCIClass, IRI_I_2, R_17_I_, R_15_I_, RMS_I_2, Rt_I_23, sp17_23, sp15_23, irih_23, iriv_23, mätdatm, iri_mnt, sp_mant, geometry)
nacka <- st_as_sf(nacka)
nacka <- st_transform(nacka, crs = 3011)
class(nacka)
head(nacka)
nrow(nacka)

norrkoping <- swedt_PCI_2022 %>% dplyr::filter(Kmmnnmn == "Norrköping") %>% 
  dplyr::select(id, brghtsk, hastght, dou2017, ådt_frd, ådt_tng, ådt_mtr, vägbrdd, vägnmmr, vägktgr, vägtyp, längd,
                Kmmnnmn, PCI_23, PCIClass, IRI_I_2, R_17_I_, R_15_I_, RMS_I_2, Rt_I_23, sp17_23, sp15_23, irih_23, iriv_23, mätdatm, iri_mnt, sp_mant, geometry)
norrkoping <- st_as_sf(norrkoping)
norrkoping <- st_transform(norrkoping, crs = 3011)
head(norrkoping)
nrow(norrkoping)

st_write(nacka, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/NIRA/Energimyndigheten/Swedenroads/nacka_swedenroads.shp", append=FALSE)
st_write(norrkoping, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/NIRA/Energimyndigheten/Swedenroads/norrkoping_swedenroads.shp", append=FALSE)



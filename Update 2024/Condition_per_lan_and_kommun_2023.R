#=================================================================#
#                 PCI values 2023 vs 2033 per län
#                       and per kommun
#=================================================================#

swedt_PCI_2023 <- st_read("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2024/sweden_2024_validation_file.shp")
setDT(swedt_PCI_2023)
setnames(swedt_PCI_2023, "PCIC_24", "PCIClass")
head(swedt_PCI_2023)
#pci2033 <- read.xlsx("C:/Users/winte/Swedenroads_outputs/2033_PCI_20201218.xlsx")
pci2033 <- fread("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2024/Scenarion/2024-03-04_2024 Nuvarande budget (5)_CI_OUT.csv")
names(pci2033) <- c("Year","Objectd","PCI")
setDT(pci2033)

pci2033 <- PCIClass(pci2033)
cols <- c("id","Länsnmn","längd")
pci2033 <- pci2033[swedt_PCI_2023[, ..cols], on = c(Objectd='id')]
nrow(pci2033[Year == 2033])
head(pci2033)

##########################################
# Nationellt
##########################################

QualitativeStatsSingleGroup(swedt_PCI_2023, quo(PCIClass), quo(längd))
QualitativeStatsSingleGroup(pci2033[Year == 2033], quo(PCIClass), quo(längd))

##########################################
# Län
##########################################

# Per Trafikklass
QualitativeStatsDoubleGroup(swedt_PCI_2023, quo(Länsnmn), quo(trfkkls), quo(längd))

pci_lan_2023 <- QualitativeStatsDoubleGroup(swedt_PCI_2023, quo(Länsnmn), quo(PCIClass), quo(längd))
pci_lan_2023 <- pci_lan_2023 %>% 
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(grouplen = round(grouplen,0)) %>%
  mutate(prop = prop*100) %>%
  mutate(prop = round(prop,1))

sum(pci_lan_2023$grouplen)

pci_lan_2033 <- QualitativeStatsDoubleGroup(pci2033[Year == 2033], quo(Länsnmn), quo(PCIClass), quo(längd))
pci_lan_2033 <- pci_lan_2033 %>% 
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(grouplen = round(grouplen,0)) %>%
  mutate(prop = prop*100) %>%
  mutate(prop = round(prop,1))

print(xtable(pci_lan_2023,digits=c(0,0,0,0,1)), include.rownames=FALSE)
print(xtable(pci_lan_2033,digits=c(0,0,0,0,1)), include.rownames=FALSE)

# Export to Excel
wb <- createWorkbook()
addWorksheet(wb, "Tillstånd 2023")
addWorksheet(wb, "Tillstånd 2033")
writeData(wb, sheet = 1, pci_lan_2023)
writeData(wb, sheet = 2, pci_lan_2033)
#Save Workbook
saveWorkbook(wb, paste0(datapath,"2024/Tillstånd per län 2023 och 2033.xlsx"), overwrite = TRUE)

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

# Eksjö kommun
str(swedt_PCI_2023)
table(swedt_PCI_2023$vägtyp)
eksjo <- swedt_PCI_2023 %>% dplyr::filter(Kmmnnmn == "Eksjö") %>% 
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
addWorksheet(wb_eksjo, "Eksjö tillstånd 2023")
writeData(wb_eksjo, sheet = 1, eksjo)
#Save Workbook
saveWorkbook(wb_eksjo, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Slutrapport/Eksjö_2023.xlsx", overwrite = TRUE)

# Härnösand, Kramfors och Sollefteå
v_norr <- c("Härnösand", "Kramfors", "Sollefteå")
hks <- swedt_PCI_2023 %>% dplyr::filter(Kmmnnmn %in% v_norr) %>% 
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
addWorksheet(wb_hks, "H K S tillstånd 2023")
writeData(wb_hks, sheet = 1, hks)
#Save Workbook
saveWorkbook(wb_hks, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Uppdatering 2023/Härnösand Kramfors Sollefteå tillstånd 2023.xlsx", overwrite = TRUE)


# Gotland
gotland <- swedt_PCI_2023 %>% dplyr::filter(Kmmnnmn == "Gotland") %>% 
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
addWorksheet(wb_gotland, "Gotland tillstånd 2023")
writeData(wb_gotland, sheet = 1, gotland)
#Save Workbook
saveWorkbook(wb_gotland, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Slutrapport/Gotland_2023.xlsx", overwrite = TRUE)

########################################################
# Export validation shapefiles for Nacka and Norrköping

nacka <- swedt_PCI_2023 %>% dplyr::filter(Kmmnnmn == "Nacka") %>% 
  dplyr::select(id, brghtsk, hastght, dou2017, ådt_frd, ådt_tng, ådt_mtr, vägbrdd, vägnmmr, vägktgr, vägtyp, längd,
                Kmmnnmn, PCI_23, PCIClass, IRI_I_2, R_17_I_, R_15_I_, RMS_I_2, Rt_I_23, sp17_23, sp15_23, irih_23, iriv_23, mätdatm, iri_mnt, sp_mant, geometry)
nacka <- st_as_sf(nacka)
nacka <- st_transform(nacka, crs = 3011)
class(nacka)
head(nacka)
nrow(nacka)

norrkoping <- swedt_PCI_2023 %>% dplyr::filter(Kmmnnmn == "Norrköping") %>% 
  dplyr::select(id, brghtsk, hastght, dou2017, ådt_frd, ådt_tng, ådt_mtr, vägbrdd, vägnmmr, vägktgr, vägtyp, längd,
                Kmmnnmn, PCI_23, PCIClass, IRI_I_2, R_17_I_, R_15_I_, RMS_I_2, Rt_I_23, sp17_23, sp15_23, irih_23, iriv_23, mätdatm, iri_mnt, sp_mant, geometry)
norrkoping <- st_as_sf(norrkoping)
norrkoping <- st_transform(norrkoping, crs = 3011)
head(norrkoping)
nrow(norrkoping)

st_write(nacka, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/NIRA/Energimyndigheten/Swedenroads/nacka_swedenroads.shp", append=FALSE)
st_write(norrkoping, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/NIRA/Energimyndigheten/Swedenroads/norrkoping_swedenroads.shp", append=FALSE)

########################################################
# Stockholm, Norrbotten, Skåne

sthlm <- swedt_PCI_2023 %>% dplyr::filter(Länsnmn == "Stockholms län")
skane <- swedt_PCI_2023 %>% dplyr::filter(Länsnmn == "Skåne län")
norrbotten <- swedt_PCI_2023%>% dplyr::filter(Länsnmn == "Norrbottens län") 

QualitativeStatsSingleGroup(sthlm, quo(PCIClass), quo(längd))
QualitativeStatsSingleGroup(skane, quo(PCIClass), quo(längd))
QualitativeStatsSingleGroup(norrbotten, quo(PCIClass), quo(längd))

nrow(sthlm) + nrow(skane) + nrow(norrbotten)
(sum(sthlm$längd)/1000 + sum(skane$längd)/1000 + sum(norrbotten$längd)/1000)/83000

230000+200000+80000



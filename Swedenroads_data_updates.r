#=================================================================#
#            Swedenroads: updates to existing datasets
#=================================================================#

swedt <- st_read("C:/Users/winte/Swedenroads_outputs/sweden_v3_201119.shp")
setDT(swedt)

swedt_PCI <- CreatePCI(swedt)
swedt_PCI <- PCIClass(swedt_PCI)
head(swedt_PCI)
itShouldTestPCI(swedt_PCI)

# Export PCI as Excelfile
exp <- c("Objectd", "PCI", "MsrmntD", "IRI_Index", "Rut_Index","RMS_Index","PCIClass")
id_pci <- swedt_PCI[, ..exp]
#write.xlsx(id_pci, "C:/Users/winte/Swedenroads_outputs/objectid_pci.xlsx", append = TRUE)

# Export only ID's that have changed
id_pci_old <- read.xlsx("C:/Users/winte/Swedenroads_outputs/objectid_pci.xlsx")
id_pci_old$MsrmntD <- as.Date(id_pci_old$MsrmntD, origin = "1899-12-30")
id_pci_new <- anti_join(id_pci[,c("Objectd", "PCI")], id_pci_old[,c("Objectd", "PCI")])
stopifnot(round(nrow(id_pci_new)/nrow(id_pci_old),3) == 0.115)

# Export excelfile with only new values
write.xlsx(id_pci_new, "C:/Users/winte/Swedenroads_outputs/objectid_pci_updatedvalues.xlsx", append = TRUE)

#################################################################################################
# Add PCI values for 2030 to Swedish data set
outdat_swe_small <- st_read("C:/Users/winte/Swedenroads_outputs/swedenroads_2020_v2.shp") 
setDT(outdat_swe_small)
pci2030 <- fread("C:/Users/winte/Swedenroads_outputs/7018-Scenario6.csv")
names(pci2030) <- c("Objectd","Year","PCI")
setDT(pci2030)
pci2030 <- PCIClass(pci2030)
head(pci2030)
head(outdat_swe_small)
pci2030_year <- pci2030[Year == 2030]
names(pci2030_year) <- c("Objectd","Year","Tl_2030","IndK2030")

outdat_swe_small <- left_join(outdat_swe_small, pci2030_year[,c("Objectd", "IndK2030")], by = c("ID" = "Objectd"))

# Check PCI statistics
QualitativeStatsSingleGroup(outdat_swe_small, quo(IndK2030), quo(Längd))

# Save as shapefile
st_write(outdat_swe_small, "C:/Users/winte/Swedenroads_outputs/swedenroads_2020_v3.shp", driver="ESRI Shapefile", append=TRUE) 

###################################################################################################################
## Update Swedish dataset with correct expected lifetime and more scenarios
swedt_PCI <- st_read( "C:/Users/winte/Swedenroads_outputs/sweden_v3_pci201206.shp") 
outdat_swe_small <- st_read( "C:/Users/winte/Swedenroads_outputs/swedenroads_2020_v3.shp") 
setDT(outdat_swe_small)
setDT(swedt_PCI)
swedt_PCI <- swedt_PCI[order(Objectd)]
outdat_swe_small <- outdat_swe_small[order(ID)]

head(outdat_swe_small)
head(swedt_PCI)
nrow(outdat_swe_small)
nrow(swedt_PCI)

# Scenario maintain
pci2030_mcur <- fread("C:/Users/winte/Swedenroads_outputs/7045-Scenario4e.csv")
names(pci2030_mcur) <- c("Objectd","Year","PCI")
setDT(pci2030_mcur)
pci2030_mcur <- PCIClass(pci2030_mcur)
pci2030_mcur <- pci2030_mcur[Year == 2030]
head(pci2030_mcur)
names(pci2030_mcur) <- c("Objectd","Year","PCI", "IKls_2")

# Scenario minimize
pci2030_min <- fread("C:/Users/winte/Swedenroads_outputs/7046-Scenario9b.csv")
names(pci2030_min) <- c("Objectd","Year","PCI")
setDT(pci2030_min)
pci2030_min <- PCIClass(pci2030_min)
pci2030_min <- pci2030_min[Year == 2030]
head(pci2030_min)
names(pci2030_min) <- c("Objectd","Year","PCI", "IKls_3")

# Replace variables
outdat_swe_small[, FrvntdL := swedt_PCI$PrdctSL]
outdat_swe_small[, ÅtrstnL := swedt_PCI$RmnngSL]

# Add classes
outdat_swe_small <- outdat_swe_small[pci2030_min[, c("Objectd","IKls_3")], on = c("ID" = "Objectd")]
outdat_swe_small <- outdat_swe_small[pci2030_mcur[, c("Objectd","IKls_2")], on = c("ID" = "Objectd")]

setcolorder(outdat_swe_small, c("ID", "Brghtsk", "Hastght", "DoU2017", "ÅDT_frd", "ÅDT_tng", "ÅDT_mtr", "Vägbrdd", "Vägnmmr", "Vägktgr",
                "Vägtyp", "Längd",  "Blggnngsd", "Blggnngst", "Spårdjp",  "IRI",  "Mätdatm", "Län_nr", "Kmmn_nr",
                "Trfkkls", "IRI_ndr", "Sprdjp_", "Region", "Ålder", "FrvntdL", "ÅtrstnL", "TllstnI", "IndxKls",
                 "IndK2030", "IKls_2", "IKls_3", "geometry"))

names(outdat_swe_small)

st_write(outdat_swe_small, "C:/Users/winte/Swedenroads_outputs/swedenroads_2020_v4.shp", driver="ESRI Shapefile", append=TRUE)




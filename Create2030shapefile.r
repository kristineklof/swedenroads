#=================================================================#
#                 Import and create output for 2030
#=================================================================#

pci2030 <- read.xlsx("C:/Users/winte/Swedenroads_outputs/Sweden Scenario 1_2030PCI.xlsx")
head(pci2030)
names(pci2030) <- c("Year","Objectd","PCI")

setDT(pci2030)
pci2030 <- PCIClass(pci2030)

swedt_PCI <- st_read( "C:/Users/winte/Swedenroads_outputs/sweden_v3_pci201119.shp") 
setDT(swedt_PCI)
head(swedt_PCI)
cols <- c("Objectd","Length","geometry")

pci2030 <- pci2030[swedt_PCI[, ..cols], on = 'Objectd']

pclasslength <- pci2030 %>%
              group_by(PCIClass) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen))
print(pclasslength)

quantile(pci2030$PCI, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE) # quartile
mean(pci2030$PCI)

st_write(pci2030, "C:/Users/winte/Swedenroads_outputs/swedenroads_2030.shp", driver="ESRI Shapefile", append=FALSE) 
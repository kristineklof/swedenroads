#=================================================================#
#                 Import and create output for 2030
#=================================================================#

pci2030_out <- fread("C:/Users/winte/Swedenroads_outputs/7018-Scenario6.csv")
pci2030_out <- fread("C:/Users/winte/Swedenroads_outputs/7046-Scenario9b.csv")
pci2030_out <- fread("C:/Users/winte/Swedenroads_outputs/7045-Scenario4e.csv")

head(pci2030_out)
names(pci2030_out) <- c("Objectd","Year","PCI")

setDT(pci2030_out)
pci2030_out <- PCIClass(pci2030_out)
pci2030_out <- pci2030_out[Year == 2030]

swedt_PCI <- st_read( "C:/Users/winte/Swedenroads_outputs/sweden_v3_pci201206.shp") 
setDT(swedt_PCI)
head(swedt_PCI)
cols <- c("Objectd","Length","geometry")

pci2030_out <- pci2030_out[swedt_PCI[, ..cols], on = 'Objectd']

pclasslength <- pci2030_out %>%
              group_by(PCIClass) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen))
print(pclasslength)

quantile(pci2030_out$PCI, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE) # quartile
mean(pci2030_out$PCI)
nrow(pci2030_out)

st_write(pci2030_out, "C:/Users/winte/Swedenroads_outputs/swedenroads_2030_v6.shp", driver="ESRI Shapefile", append=FALSE) 

st_write(pci2030_out, "C:/Users/winte/Swedenroads_outputs/swedenroads_maintain_cur_2030_v1.shp", driver="ESRI Shapefile", append=FALSE) 

st_write(pci2030_out, "C:/Users/winte/Swedenroads_outputs/swedenroads_minimize_def_2030_v2.shp", driver="ESRI Shapefile", append=FALSE) 

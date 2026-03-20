#=================================================================#
#           Swedenroads: Outputs
#=================================================================#

##########################################################
# Validation file

# Join with geometry
sw26_full_geo <- dplyr::left_join(sw26,sw26_geo, by="id")
st_write(sw26_full_geo, paste0(datapath,"2026/sweden_2026_validation_file.shp"), append=FALSE)
head(sw26_full_geo)

##########################################################
# English output for analysis in DOT
sweden_2026_dot <- sw26 %>% dplyr::select(id, brghtsk, hastght, dou2017, 
                                          ådt_frd, ådt_tng, ådt_mtr, vägbrdd, 
                                          vägktgr,roadtyp, vägnmmr,längd, 
                                          län_nr, kmmn_nr, trfkkls, region,  
                                          pvmntty, beldat_25, tacknng, 
                                          ålder, prdctsl, rmnngsl,
                                          iri_mnt, sp_mant, spårdjp, iri, matdatum, 
                                          PCI_26, PCIClass_26)

sweden_2026_dot <- sweden_2026_dot %>% 
  dplyr::mutate(pvmntty = factor(pvmntty)) %>%
  dplyr::mutate(pvmntty = recode(pvmntty, 
                                      "Varm" = "Hot Mix Ashphalt", 
                                      "Hot mix asphalt (asphalt concrete)" = "Hot Mix Asphalt",
                                      "Tunnskikt" = "Thin Asphalt Layer", 
                                      "Thin asphalt layer" = "Thin Asphalt Layer",
                                      "Halvvarm" = "Half Warm Asphalt", 
                                      "Half warm asphalt" = "Half Warm Asphalt",
                                      "Indränkt makadam" = "Grouted Macadam", 
                                      "Grouted macadam" = "Grouted Macadam",
                                      "Försegling" = "Seal Coat", 
                                      "Seal coat" = "Seal Coat", 
                                      "Ytbehandling på grus" = "Surface dressing on gravel", 
                                      "Surface dressing on gravel" = "Surface dressing on gravel",
                                      "Surface dressing on bituminous surface" = "Surface dressing on bituminous surface",
                                      "Ytbehandling på bituminöst underlag" = "Surface dressing on bituminous surface", 
                                      "Varm stenrik" = "Stone Mastic Asphalt",
                                      "Hot mix asphalt (stone mastic)" = "Stone Mastic Asphalt",
                                      "Övrigt" = "Other")) %>%
  dplyr::mutate(SurfaceType = if_else(pvmntty == "Grouted Macadam" | 
                                        pvmntty == "Seal Coat" | 
                                        pvmntty == "Surface dressing on gravel" |
                                        pvmntty == "Surface dressing on bituminous surface",
                                      "Surface Treated",
                                      if_else(pvmntty == "Half Warm Asphalt",
                                              "Half Warm Asphalt",
                                              if_else(pvmntty == "Stone Mastic Asphalt",
                                                      "Stone Mastic Asphalt", "Hot Mix Asphalt")))) %>% 
  dplyr::mutate( brghtsk = as.integer(brghtsk),
                 hastght = as.integer(hastght),
                 dou2017 = as.integer(dou2017),
                 ådt_mtr = as.character(ådt_mtr),
                 dou2017 = as.integer(dou2017),
                 ådt_tng = as.integer(ådt_tng),
                 ådt_frd = as.integer(ådt_frd),
                 vägbrdd = as.integer(vägbrdd),
                 vägktgr = as.integer(vägktgr),
                 vägnmmr = as.integer(vägnmmr),
                 trfkkls = as.integer(trfkkls)) %>%
  dplyr::mutate(pcidate = as.Date("2025-01-01")) %>%
  dplyr::mutate(area = längd*vägbrdd)

sweden_2026_dot <- dplyr::left_join(sweden_2026_dot, lankom, by = c("län_nr" = "Län", "kmmn_nr" = "Kommunnr"))

head(sweden_2026_dot)

colnames(sweden_2026_dot) <- c("Objectd","BrngCpC","SpedLmt","DoU2017",
                               "AADT", "AADT_hv","AADT_m", "RodWdth",
                               "RdCtgry", "RoadTyp", "Rd_Nmbr","Length",
                               "County","Mncplty","tkl8","Region",
                               "PvmntTy","TrtmntD","Coverag","Age",
                               "PrdctSL","RmnngSL","IRI_mnt","SP_mant",
                               "Rut","IRI","MsmntD","PCI","PCICls","SrfcTyp",
                               "PCIDate","Area","CountN","MncpN")

itShouldCheckIfAnyVariableHasNAs <- function(){
  stopifnot(sum(is.na(sweden_2026_dot$PvmntTy)) == 0)
  stopifnot(sum(is.na(sweden_2026_dot$PCI)) == 0)
  stopifnot(sum(is.na(sweden_2026_dot$AADT)) == 0)
  stopifnot(sum(is.na(sweden_2026_dot$AADT_hv)) == 0)
  stopifnot(sum(is.na(sweden_2026_dot$Region)) == 0)
  stopifnot(sum(is.na(sweden_2026_dot$DoU2017)) == 0)
  stopifnot(sum(is.na(sweden_2026_dot$RoadTyp)) == 0)
  stopifnot(sum(is.na(sweden_2026_dot$RodWdth)) == 0)
  stopifnot(sum(is.na(sweden_2026_dot$Length)) == 0)
  stopifnot(sum(is.na(sweden_2026_dot$tkl8)) == 0)
  stopifnot(max(sweden_2026_dot$PCI) == 100)
  stopifnot(min(sweden_2026_dot$PCI) == 0)
  stopifnot(sum(is.na(sweden_2026_dot$PCIDate)) == 0)
  stopifnot(with(sweden_2026_dot,
                 is.na(Rut) & is.na(IRI) | (!is.na(Rut) | !is.na(IRI)) & inherits(MsmntD, c("Date", "POSIXt"))))
}

itShouldCheckIfAnyVariableHasNAs()

# Join with geometry
sweden_2026_dot <- dplyr::left_join(sweden_2026_dot, sw26_geo, by=c("Objectd" = "id"))
sweden_2026_dot <- sweden_2026_dot %>% dplyr::mutate(Objectd = as.integer(Objectd))

QualitativeStatsSingleGroup(sweden_2026_dot, quo(PCICls), quo(Length))
nrow(sweden_2026_dot)
head(sweden_2026_dot)

# Export shapefile
st_write(sweden_2026_dot, paste0(datapath,"2026/DOT/sweden_2026_dot_20260129.shp"), append=FALSE)

# Export Excel
mapping <- c(
  Objectd = "Asset ID",
  BrngCpC = "Bearing Capacity",
  SpedLmt = "Speed Limit",
  DoU2017 = "DoU2017",
  AADT = "AADT",
  AADT_hv = "AADT Trucks",
  AADT_m = "AADT Measurement Year",
  RodWdth = "Width",
  Length = "Length",
  Area = "Area",
  RdCtgry = "Road Category",
  Region = "Region",
  RoadTyp = "Road Type",
  County = "County",
  Mncplty = "Municipality",
  SrfcTyp = "Surface Type",
  tkl8 = "Traffic Class (tkl8)",
  PvmntTy = "Pavement Type",
  Coverag = "Coverage",
  RmnngSL = "Remaining Service Life",
  SP_mant = "SP_maint",
  IRI_mnt = "IRI_maint",
  Age = "Age",
  CountN = "County Name",
  MncpN = "Municipality Name",
  PCI = "PCI",
  MsmntD = "Measurement Date",
  MsmntD = "Rut Assessment Date",
  MsmntD = "IRI Assessment Date",
  TrtmntD = "Treatment Date",
  Rd_Nmbr = "Road Number",
  Rut = "Rut",
  IRI = "IRI",  
  PCICls = "PCI Class",
  PCIDate = "PCI Assessment Date"
)

export_swedenroads_split_10(df = sweden_2026_dot,
                            template_path= file.path(datapath, "2026", "Roads_Inventory_Nov_26_2025.xlsx"),
                            output_dir   = file.path(datapath, "2026", "DOT"),
                            mapping      = mapping,
                            base_name    = "sweden_20260129",
                            n_parts      = 10)

##########################################################
# Output till Anders/swedenroads/våravägar
swroads_cols <- c("id", "brghtsk", "hastght", "dou2017",
                  "ådt_frd", "ådt_tng", "ådt_mtr", "vägbrdd",
                  "vägnmmr","vägktgr", "roadtyp", "längd",  
                  "beldatum","pvmntty", "tacknng",
                  "spårdjp",  "iri","matdatum","iri_mnt", "sp_mant",
                  "län_nr", "kmmn_nr",
                  "ålder","prdctsl", "rmnngsl",
                  "PCI_26", "PCIClass_26")
swedenroads_2026 <-  sw26[,..swroads_cols]

swedenroads_2026 <- swedenroads_2026 %>%
  dplyr::mutate(pvmntty = ChangeBeltypNumerisk(pvmntty)) %>%
  dplyr::mutate(tacknng = ChangeTackningNumerisk(tacknng)) %>%
  rename(blggnngsd = beldatum, mätdatm = matdatum)

# Use values from PMSv3
swedenroads_2026 <- swedenroads_2026  %>%
  mutate(
    vägktgr = sweden26$vägktgr,
    vägtyp = sweden26$vägtyp,
    dou2017 = sweden26$dou2017,
    vägbrdd = sweden26$vägbrdd,
    hastght = sweden26$hastght,
    brghtsk = sweden26$brghtsk
  )

swedenroads_2026 %>% summarise(na_count = sum(is.na(pvmntty))) 
swedenroads_2026 %>% summarise(na_count = sum(is.na(vägktgr))) 
swedenroads_2026 %>% summarise(na_count = sum(is.na(roadtyp))) 
swedenroads_2026 %>% summarise(na_count = sum(is.na(dou2017))) 
swedenroads_2026 %>% summarise(na_count = sum(is.na(vägbrdd))) 
swedenroads_2026 %>% summarise(na_count = sum(is.na(hastght))) 
swedenroads_2026 %>% summarise(na_count = sum(is.na(brghtsk))) 


# Add classes
pci2035_cur <- pci2035 %>% dplyr::filter(Year == 2034) %>%
  dplyr::select(Objectd,PCIClass)
names(pci2035_cur) <- c("id","IKls_1")
nrow(pci2035_cur)

vagtummor_stracka_2025 <- st_read(paste0(datapath,"2026/vagtrummeindex_per_stracka.shp"))
vagtummor_stracka_2025 <- st_drop_geometry(vagtummor_stracka_2025)
vagtrummor_stracka <- vagtummor_stracka_2025 %>%
  dplyr::select(id,sctn_st)
names(vagtrummor_stracka) <- c("id","trumindex")
nrow(vagtrummor_stracka)

swedenroads_2026 <- dplyr::left_join(swedenroads_2026, pci2035_cur , by = c("id"))
swedenroads_2026 <- dplyr::left_join(swedenroads_2026, vagtrummor_stracka, by = c("id"))
swedenroads_2026 <- swedenroads_2026 %>% mutate(IKls_1 = if_else(is.na(IKls_1),PCIClass_26,IKls_1))

swedenroads_2026 %>% summarise(na_count = sum(is.na(IKls_1))) 
swedenroads_2026 %>% summarise(na_count = sum(is.na(trumindex)))

swroads_cols2 <- c("id", "brghtsk", "hastght", "dou2017",
                   "ådt_frd", "ådt_tng", "ådt_mtr", "vägbrdd",
                   "vägnmmr","vägktgr", "roadtyp", "längd",  
                   "blggnngsd","pvmntty", "tacknng",
                   "spårdjp",  "iri","mätdatm","iri_mnt", "sp_mant",
                   "län_nr", "kmmn_nr",
                   "ålder","prdctsl", "rmnngsl",
                   "PCI_26", "PCIClass_26", "IKls_1", "trumindex")
swedenroads_2026 <-  swedenroads_2026[,..swroads_cols2]
names(swedenroads_2026) <- c("id", "brghtsk", "hastght", "dou2017",
                             "ådt_frd", "ådt_tng", "ådt_mtr", "vägbrdd",
                             "vägnmmr","vägktgr", "vägtyp", "längd",  
                             "blggnngsd","blggnngst", "tackning",
                             "spårdjp",  "iri","mätdatm","IRI_maint", "SP_maint",
                             "län_nr", "kmmn_nr",
                             "ålder","FrvntdL", "ÅtrstnL",
                             "Tllstnl", "IndxKls","IKls_1","trumindex")
# Join with geometry
swedenroads_2026 <- dplyr::left_join(swedenroads_2026,sw26_geo, by=c("id" = "id"))
head(swedenroads_2026)

# Quality check
QualitativeStatsSingleGroup(swedenroads_2026, quo(IndxKls), quo(längd))
QualitativeStatsSingleGroup(swedenroads_2026, quo(IKls_1), quo(längd))
QualitativeStatsSingleGroup(swedenroads_2026, quo(trumindex), quo(längd))

# Export
st_write(swedenroads_2026, paste0(datapath,"2026/VåraVägar/swedenroads_2026_all_scenarios.shp"), append=FALSE)

#################################################################
# Vägtrummor
vagtummor_2025 <- st_read("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Uppdatering 2026/Vägtrummor/vagtrummor_2025.shp")
head(vagtummor_2025)
vagtummor_2025 <- st_drop_geometry(vagtummor_2025)
vagtummor_2025 <- vagtummor_2025 %>% 
  dplyr::mutate(date_def = as.Date(date_def)) %>% 
  dplyr::select(globalid, inloppstyp, inloppsmat, inloppsbre, utloppstyp,utloppsmat, utloppsbre, v_gtruml_n, date_def)

probs <- c(0.001, 0.05, 0.25, 0.50, 0.75, 0.95)
q_num <- quantile(as.numeric(vagtummor_2025$date_def),
                  probs = probs, na.rm = TRUE, type = 7)
as.Date(q_num, origin = "1970-01-01")

swedenroads_trummor <- st_read(paste0(datapath,"2026/vagtrummor_with_status.shp"))
head(swedenroads_trummor)
swedenroads_trummor <- swedenroads_trummor %>% 
  dplyr::filter(!is.na(globald)) %>% 
  dplyr::select(id, globald, län_nr, vägnmmr) %>%
  dplyr::distinct(globald, .keep_all = TRUE)
nrow(swedenroads_trummor)

swedenroads_trummor <- left_join(swedenroads_trummor,vagtummor_2025,
  by = c("globald" = "globalid"))

nrow(swedenroads_trummor)
length(unique(swedenroads_trummor$globald))

# Export
st_write(swedenroads_trummor, paste0(datapath,"2026/VåraVägar/Vägtrummor/swedenroads_2026_trummor.shp"), append=FALSE)




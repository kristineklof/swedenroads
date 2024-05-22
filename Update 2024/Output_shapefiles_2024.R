#=================================================================#
#           Swedenroads: Outputs
#=================================================================#

##########################################################
# Validation file

# Join with län and kommunnamn
sw24 <- dplyr::left_join(sw24,lankom,by=c("län_nr" = "Län",
                                          "kmmn_nr" = "Kommunnr"))
swedenroads_2020 <- st_read(paste0(datapath,"swedenroads_2020_v4.shp"))
sw24_geo <- swedenroads_2020[,c("ID")]

# Join with geometry
sw24_full_geo <- dplyr::left_join(sw24,sw24_geo, by=c("id" = "ID"))
st_write(sw24_full_geo, paste0(datapath,"2024/sweden_2024_validation_file.shp"), append=FALSE)
head(sw24_full_geo)

# Import analysis version from 2020
swedt_2020_geo <- st_read(paste0(datapath,"sweden_v3_pci201206.shp")) 
swedt_2020 <- st_drop_geometry(swedt_2020_geo)

##########################################################
# English output for analysis in DOT
sweden_2024_dot <- sw24 %>% dplyr::select(id, brghtsk, hastght, dou2017, 
                                          ådt_frd, ådt_tng, ådt_mtr, vägbrdd, 
                                          vägktgr,RoadType, vägnmmr,längd, 
                                          län_nr, kmmn_nr, trfkkls, region,  
                                          PavementType, blggnngsd, tackning, 
                                          ålder, PredictedServiceLife, RemainingServiceLife,
                                          IRI_maint, SP_maint, spårdjp, iri, mätdatm, 
                                          PCI_24, PCIClass_24)

sweden_2024_dot <- dplyr::left_join(sweden_2024_dot, swedt_2020[,c("Objectd","PvmntTy","TrtmntD",
                                                                   "AADT","AADT_hv","RoadTyp","RodWdth")], 
                                    by = c("id" = "Objectd"))

sweden_2024_dot <- sweden_2024_dot %>% 
  dplyr::mutate(PavementType = as.character(PavementType)) %>%
  dplyr::mutate(PavementType = if_else(is.na(PavementType),PvmntTy,PavementType)) %>%
  dplyr::mutate(blggnngsd = if_else(is.na(blggnngsd),TrtmntD,blggnngsd)) %>%
  dplyr::mutate(vägbrdd = if_else(is.na(vägbrdd),RodWdth,vägbrdd)) %>%
  dplyr::mutate(RoadType = if_else(is.na(RoadType),RoadTyp,RoadType)) %>%
  dplyr::mutate(ådt_frd = if_else(is.na(ådt_frd),AADT,ådt_frd)) %>%
  dplyr::mutate(ådt_tng = if_else(is.na(ådt_tng),AADT_hv,ådt_tng))

sweden_2024_dot <- sweden_2024_dot %>% 
  dplyr::mutate(PavementType = factor(PavementType)) %>%
  dplyr::mutate(PavementType = recode(PavementType, 
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
  dplyr::mutate(SurfaceType = if_else(PavementType == "Grouted Macadam" | 
                                        PavementType == "Seal Coat" | 
                                        PavementType == "Surface dressing on gravel" |
                                        PavementType == "Surface dressing on bituminous surface",
                                      "Surface Treated",
                                      if_else(PavementType == "Half Warm Asphalt",
                                              "Half Warm Asphalt",
                                              if_else(PavementType == "Stone Mastic Asphalt",
                                                      "Stone Mastic Asphalt", "Hot Mix Asphalt"))))

sweden_2024_dot <- dplyr::left_join(sweden_2024_dot, lankom, by = c("län_nr" = "Län", "kmmn_nr" = "Kommunnr"))
sweden_2024_dot <- sweden_2024_dot %>% dplyr::select(-c("PvmntTy","TrtmntD",
                                                        "AADT","AADT_hv","RoadTyp","RodWdth"))

colnames(sweden_2024_dot) <- c("Objectd","BrngCpC","SpedLmt","DoU2017",
                               "AADT", "AADT_hv","AADT_m", "RodWdth",
                               "RdCtgry", "RoadTyp", "Rd_Nmbr","Length",
                               "County","Mncplty","tkl8","Region",
                               "PvmntTy","TrtmntD","Coverag","Age",
                               "PrdctSL","RmnngSL","IRI_mnt","SP_mant",
                               "Rut","IRI","MsmntD","PCI","PCICls","SrfcTyp",
                               "CountN","MncpN")

itShouldCheckIfAnyVariableHasNAs <- function(){
  stopifnot(sum(is.na(sweden_2024_dot$PvmntTy)) == 0)
  stopifnot(sum(is.na(sweden_2024_dot$PCI)) == 0)
  stopifnot(sum(is.na(sweden_2024_dot$AADT)) == 0)
  stopifnot(sum(is.na(sweden_2024_dot$AADT_hv)) == 0)
  stopifnot(sum(is.na(sweden_2024_dot$Region)) == 0)
  stopifnot(sum(is.na(sweden_2024_dot$DoU2017)) == 0)
  stopifnot(sum(is.na(sweden_2024_dot$RoadTyp)) == 0)
  stopifnot(sum(is.na(sweden_2024_dot$RodWdth)) == 0)
  stopifnot(sum(is.na(sweden_2024_dot$Length)) == 0)
  stopifnot(max(sweden_2024_dot$PCI) == 100)
  stopifnot(min(sweden_2024_dot$PCI) == 0)
}

itShouldCheckIfAnyVariableHasNAs()

QualitativeStatsSingleGroup(sweden_2024_dot, quo(PCICls), quo(Length))
nrow(sweden_2024_dot)

# Join with geometry
sweden_2024_dot <- dplyr::left_join(sweden_2024_dot,sw24_geo, by=c("Objectd" = "ID"))

# Export
st_write(sweden_2024_dot, paste0(datapath,"2024/DOT/sweden_2024_dot_20240202.shp"))

##########################################################
# Output till Anders/swedenroads/våravägar
swroads_cols <- c("id", "brghtsk", "hastght", "dou2017",
                  "ådt_frd", "ådt_tng", "ådt_mtr", "vägbrdd",
                  "vägnmmr","vägktgr", "vägtyp", "längd",  
                  "blggnngsd","PavementType", "blggnngst", "tackning",
                  "spårdjp",  "iri","mätdatm","IRI_maint", "SP_maint",
                  "län_nr", "kmmn_nr",
                  "ålder","PredictedServiceLife", "RemainingServiceLife",
                  "PCI_24", "PCIClass_24")
swedenroads_2024 <-  sw24[,..swroads_cols]
swedenroads_2024 <- swedenroads_2024 %>%
  dplyr::mutate(PavementType = ChangeBeltypNumerisk(PavementType)) %>%
  dplyr::mutate(blggnngst = if_else(!is.na(PavementType), PavementType, blggnngst)) %>%
  dplyr::mutate(tackning = ChangeTackningNumerisk(tackning))

# Add classes
pci2033_cur <- pci2033 %>% dplyr::filter(Year == 2031) %>%
  dplyr::select(Objectd,PCIClass)
names(pci2033_cur) <- c("id","IKls_1")

pci2033_mcur_sw <- pci2033_mcur %>% dplyr::filter(Year == 2029) %>%
  dplyr::select(Objectd,PCIClass)
names(pci2033_mcur_sw) <- c("id","IKls_2")

pci2033_min_sw <- pci2033_min %>% dplyr::filter(Year == 2029) %>%
  dplyr::select(Objectd,PCIClass)
names(pci2033_min_sw) <- c("id","IKls_3")

swedenroads_2024 <- dplyr::left_join(swedenroads_2024, pci2033_cur, by = c("id"))
swedenroads_2024 <- dplyr::left_join(swedenroads_2024, pci2033_mcur_sw, by = c("id"))
swedenroads_2024 <- dplyr::left_join(swedenroads_2024, pci2033_min_sw, by = c("id"))

swroads_cols2 <- c("id", "brghtsk", "hastght", "dou2017",
                   "ådt_frd", "ådt_tng", "ådt_mtr", "vägbrdd",
                   "vägnmmr","vägktgr", "vägtyp", "längd",  
                   "blggnngsd", "blggnngst", "tackning",
                   "spårdjp",  "iri","mätdatm","IRI_maint", "SP_maint",
                   "län_nr", "kmmn_nr",
                   "ålder","PredictedServiceLife", "RemainingServiceLife",
                   "PCI_24", "PCIClass_24","IKls_1","IKls_2","IKls_3")
swedenroads_2024 <-  swedenroads_2024[,..swroads_cols2]
names(swedenroads_2024) <- c("id", "brghtsk", "hastght", "dou2017",
                             "ådt_frd", "ådt_tng", "ådt_mtr", "vägbrdd",
                             "vägnmmr","vägktgr", "vägtyp", "längd",  
                             "blggnngsd","blggnngst", "tackning",
                             "spårdjp",  "iri","mätdatm","IRI_maint", "SP_maint",
                             "län_nr", "kmmn_nr",
                             "ålder","FrvntdL", "ÅtrstnL",
                             "Tllstnl", "IndxKls","IKls_1","IKls_2","IKls_3")
# Join with geometry
swedenroads_2024 <- dplyr::left_join(swedenroads_2024,sw24_geo, by=c("id" = "ID"))
head(swedenroads_2024)

# Quality check
QualitativeStatsSingleGroup(swedenroads_2024, quo(IndxKls), quo(längd))
QualitativeStatsSingleGroup(swedenroads_2024, quo(IKls_1), quo(längd))
QualitativeStatsSingleGroup(swedenroads_2024, quo(IKls_2), quo(längd))
QualitativeStatsSingleGroup(swedenroads_2024, quo(IKls_3), quo(längd))

# Export
st_write(swedenroads_2024, paste0(datapath,"2024/VåraVägar/swedenroads_2024_all_scenarios.shp"), append=FALSE)


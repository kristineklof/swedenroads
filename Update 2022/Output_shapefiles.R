#=================================================================#
#           Swedenroads: Outputs
#=================================================================#

##########################################################
# Validation file

# Join with geometry
sw22_full_geo <- dplyr::left_join(sw22,sw22_geo, by=c("id" = "ID"))
st_write(sw22_full_geo, paste0(datapath,"2022/sweden_2022_validation_file.shp"))

# Import analysis version from 2020
swedt_2020_geo <- st_read(paste0(datapath,"sweden_v3_pci201206.shp")) 
swedt_2020 <- st_drop_geometry(swedt_2020_geo)

##########################################################
# English output for analysis in DOT
sweden_2022_dot <- sw22 %>% dplyr::select(id, brghtsk, hastght, dou2017, 
                                          Ådt_frd, Ådt_tng, Ådt_mtr, vägbrdd, 
                                          vägktgr,RoadType, vägnmmr,längd, 
                                          län_nr, kmmn_nr, trfkkls, region,  
                                          PavementType, blggnngsd, tackning, 
                                          Ålder, PredictedServiceLife, RemainingServiceLife,
                                          IRI_maint, SP_maint, spårdjp, iri, mätdatm, 
                                          PCI_22, PCIClass_22)

sweden_2022_dot <- dplyr::left_join(sweden_2022_dot, swedt_2020[,c("Objectd","PvmntTy","TrtmntD",
                                                                   "AADT","AADT_hv","RoadTyp","RodWdth")], 
                                    by = c("id" = "Objectd"))

sweden_2022_dot <- sweden_2022_dot %>% 
  dplyr::mutate(PavementType = as.character(PavementType)) %>%
  dplyr::mutate(PavementType = if_else(is.na(PavementType),PvmntTy,PavementType)) %>%
  dplyr::mutate(blggnngsd = if_else(is.na(blggnngsd),TrtmntD,blggnngsd)) %>%
  dplyr::mutate(vägbrdd = if_else(is.na(vägbrdd),RodWdth,vägbrdd)) %>%
  dplyr::mutate(RoadType = if_else(is.na(RoadType),RoadTyp,RoadType)) %>%
  dplyr::mutate(Ådt_frd = if_else(is.na(Ådt_frd),AADT,Ådt_frd)) %>%
  dplyr::mutate(Ådt_tng = if_else(is.na(Ådt_tng),AADT_hv,Ådt_tng))

sweden_2022_dot <- sweden_2022_dot %>% 
  dplyr::mutate(PavementType = factor(PavementType)) %>%
  dplyr::mutate(PavementType = recode(PavementType, 
                                      "Varm" = "Hot Mix Ashphalt", 
                                      "Hot mix asphalt (asphalt concrete)" = "Hot Mix Ashphalt",
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
                                                      "Stone Mastic Asphalt", "Hot Mix Ashphalt"))))

sweden_2022_dot <- dplyr::left_join(sweden_2022_dot, lankom, by = c("län_nr" = "Län", "kmmn_nr" = "Kommunnr"))
sweden_2022_dot <- sweden_2022_dot %>% dplyr::select(-c("PvmntTy","TrtmntD",
                                                        "AADT","AADT_hv","RoadTyp","RodWdth"))

colnames(sweden_2022_dot)

colnames(sweden_2022_dot) <- c("Objectd","BrngCpC","SpedLmt","DoU2017",
                               "AADT", "AADT_hv","AADT_m", "RodWdth",
                               "RdCtgry", "RoadTyp", "Rd_Nmbr","Length",
                               "County","Mncplty","tkl8","Region",
                               "PvmntTy","TrtmntD","Coverag","Age",
                               "PrdctSL","RmnngSL","IRI_mnt","SP_mant",
                               "Rut","IRI","MsmntD","PCI","PCICls","SrfcTyp",
                               "CountN","MncpN")

itShouldCheckIfAnyVariableHasNAs <- function(){
  stopifnot(sum(is.na(sweden_2022_dot$PvmntTy)) == 0)
  stopifnot(sum(is.na(sweden_2022_dot$PCI)) == 0)
  stopifnot(sum(is.na(sweden_2022_dot$AADT)) == 0)
  stopifnot(sum(is.na(sweden_2022_dot$AADT_hv)) == 0)
  stopifnot(sum(is.na(sweden_2022_dot$Region)) == 0)
  stopifnot(sum(is.na(sweden_2022_dot$DoU2017)) == 0)
  stopifnot(sum(is.na(sweden_2022_dot$RoadTyp)) == 0)
  stopifnot(sum(is.na(sweden_2022_dot$RodWdth)) == 0)
  stopifnot(sum(is.na(sweden_2022_dot$Length)) == 0)
  stopifnot(max(sweden_2022_dot$PCI) == 100)
  stopifnot(min(sweden_2022_dot$PCI) == 0)
}

itShouldCheckIfAnyVariableHasNAs()

QualitativeStatsSingleGroup(sweden_2022_dot, quo(PCICls), quo(Length))

# Join with geometry
sweden_2022_dot <- dplyr::left_join(sweden_2022_dot,sw22_geo, by=c("Objectd" = "ID"))
st_write(sweden_2022_dot, paste0(datapath,"2022/DOT/sweden_2022_dot_20220311.shp"))

##########################################################
# Output till Anders/swedenroads/våravägar
swroads_cols <- c("id", "brghtsk", "hastght", "dou2017",
                  "Ådt_frd", "Ådt_tng", "Ådt_mtr", "vägbrdd",
                  "vägnmmr","vägktgr", "vägtyp", "längd",  
                  "blggnngsd","PavementType", "blggnngst", "tackning",
                  "spårdjp",  "iri","mätdatm","IRI_maint", "SP_maint",
                   "län_nr", "kmmn_nr",
                  "Ålder","PredictedServiceLife", "RemainingServiceLife",
                  "PCI_22", "PCIClass_22")
swedenroads_2022 <-  sw22[,..swroads_cols]
swedenroads_2022 <- swedenroads_2022 %>%
  dplyr::mutate(PavementType = ChangeBeltypNumerisk(PavementType)) %>%
  dplyr::mutate(blggnngst = if_else(!is.na(PavementType), PavementType, blggnngst)) %>%
  dplyr::mutate(tackning = ChangeTackningNumerisk(tackning))

# Add classes
pci2031_cur <- pci2030 %>% dplyr::filter(Year == 2031) %>%
  dplyr::select(Objectd,PCIClass)
names(pci2031_cur) <- c("id","IKls_1")

pci2031_mcur <- pci2030_mcur %>% dplyr::filter(Year == 2031) %>%
  dplyr::select(Objectd,PCIClass)
names(pci2031_mcur) <- c("id","IKls_2")

pci2031_min <- pci2030_min %>% dplyr::filter(Year == 2031) %>%
  dplyr::select(Objectd,PCIClass)
names(pci2031_min) <- c("id","IKls_3")

swedenroads_2022 <- dplyr::left_join(swedenroads_2022, pci2031_cur, by = c("id"))
swedenroads_2022 <- dplyr::left_join(swedenroads_2022, pci2031_mcur, by = c("id"))
swedenroads_2022 <- dplyr::left_join(swedenroads_2022, pci2031_min, by = c("id"))

swroads_cols2 <- c("id", "brghtsk", "hastght", "dou2017",
                  "Ådt_frd", "Ådt_tng", "Ådt_mtr", "vägbrdd",
                  "vägnmmr","vägktgr", "vägtyp", "längd",  
                  "blggnngsd", "blggnngst", "tackning",
                  "spårdjp",  "iri","mätdatm","IRI_maint", "SP_maint",
                  "län_nr", "kmmn_nr",
                  "Ålder","PredictedServiceLife", "RemainingServiceLife",
                  "PCI_22", "PCIClass_22","IKls_1","IKls_2","IKls_3")
swedenroads_2022 <-  swedenroads_2022[,..swroads_cols2]
names(swedenroads_2022) <- c("id", "brghtsk", "hastght", "dou2017",
                        "Ådt_frd", "Ådt_tng", "Ådt_mtr", "vägbrdd",
                        "vägnmmr","vägktgr", "vägtyp", "längd",  
                        "blggnngsd","blggnngst", "tackning",
                        "spårdjp",  "iri","mätdatm","IRI_maint", "SP_maint",
                        "län_nr", "kmmn_nr",
                        "Ålder","FrvntdL", "ÅtrstnL",
                        "Tllstnl", "IndxKls","IKls_1","IKls_2","IKls_3")
# Join with geometry
swedenroads_2022 <- dplyr::left_join(swedenroads_2022,sw22_geo, by=c("id" = "ID"))

# Quality check
head(swedenroads_2022)
QualitativeStatsSingleGroup(swedenroads_2022, quo(IndxKls), quo(längd))
QualitativeStatsSingleGroup(swedenroads_2022, quo(IKls_1), quo(längd))
QualitativeStatsSingleGroup(swedenroads_2022, quo(IKls_2), quo(längd))
QualitativeStatsSingleGroup(swedenroads_2022, quo(IKls_3), quo(längd))

# Export
st_write(swedenroads_2022, paste0(datapath,"2022/swedenroads_v1_2022.shp"))


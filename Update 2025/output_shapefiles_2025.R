#=================================================================#
#           Swedenroads: Outputs
#=================================================================#

##########################################################
# Validation file

# Join with geometry
sw25_full_geo <- dplyr::left_join(sw25,sw25_geo, by="id")
st_write(sw25_full_geo, paste0(datapath,"2025/sweden_2025_validation_file.shp"), append=FALSE)
head(sw25_full_geo)

##########################################################
# English output for analysis in DOT
sweden_2025_dot <- sw25 %>% dplyr::select(id, brghtsk, hastght, dou2017, 
                                          ådt_frd, ådt_tng, ådt_mtr, vägbrdd, 
                                          vägktgr,RoadType, vägnmmr,längd, 
                                          län_nr, kmmn_nr, trfkkls, region,  
                                          PavementType, beldat_25, tackning, 
                                          ålder, PredictedServiceLife, RemainingServiceLife,
                                          IRI_maint, SP_maint, spårdjp, iri, matdatum_2, 
                                          index_25, PCIClass_25)

sweden_2025_dot <- sweden_2025_dot %>% 
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
                trfkkls = as.integer(trfkkls)) 
                  

sweden_2025_dot <- dplyr::left_join(sweden_2025_dot, lankom, by = c("län_nr" = "Län", "kmmn_nr" = "Kommunnr"))

head(sweden_2025_dot)

colnames(sweden_2025_dot) <- c("Objectd","BrngCpC","SpedLmt","DoU2017",
                               "AADT", "AADT_hv","AADT_m", "RodWdth",
                               "RdCtgry", "RoadTyp", "Rd_Nmbr","Length",
                               "County","Mncplty","tkl8","Region",
                               "PvmntTy","TrtmntD","Coverag","Age",
                               "PrdctSL","RmnngSL","IRI_mnt","SP_mant",
                               "Rut","IRI","MsmntD","PCI","PCICls","SrfcTyp",
                               "CountN","MncpN")

itShouldCheckIfAnyVariableHasNAs <- function(){
  stopifnot(sum(is.na(sweden_2025_dot$PvmntTy)) == 0)
  stopifnot(sum(is.na(sweden_2025_dot$PCI)) == 0)
  stopifnot(sum(is.na(sweden_2025_dot$AADT)) == 0)
  stopifnot(sum(is.na(sweden_2025_dot$AADT_hv)) == 0)
  stopifnot(sum(is.na(sweden_2025_dot$Region)) == 0)
  stopifnot(sum(is.na(sweden_2025_dot$DoU2017)) == 0)
  stopifnot(sum(is.na(sweden_2025_dot$RoadTyp)) == 0)
  stopifnot(sum(is.na(sweden_2025_dot$RodWdth)) == 0)
  stopifnot(sum(is.na(sweden_2025_dot$Length)) == 0)
  stopifnot(sum(is.na(sweden_2025_dot$tkl8)) == 0)
  stopifnot(max(sweden_2025_dot$PCI) == 100)
  stopifnot(min(sweden_2025_dot$PCI) == 0)
}

itShouldCheckIfAnyVariableHasNAs()

QualitativeStatsSingleGroup(sweden_2025_dot, quo(PCICls), quo(Length))
nrow(sweden_2025_dot)

# Join with geometry
sweden_2025_dot <- dplyr::left_join(sweden_2025_dot, sw25_geo, by=c("Objectd" = "id"))
sweden_2025_dot <- sweden_2025_dot %>% dplyr::mutate(Objectd = as.integer(Objectd))

unique(sweden_2025_dot$RoadTyp)

# Export
st_write(sweden_2025_dot, paste0(datapath,"2025/DOT/sweden_2025_dot_20250130.shp"), append=FALSE)

##########################################################
# Output till Anders/swedenroads/våravägar
swroads_cols <- c("id", "brghtsk", "hastght", "dou2017",
                  "ådt_frd", "ådt_tng", "ådt_mtr", "vägbrdd",
                  "vägnmmr","vägktgr", "vägtyp", "längd",  
                  "beldat_25","beltyp_25", "tackning",
                  "spårdjp",  "iri","matdatum_2","IRI_maint", "SP_maint",
                  "län_nr", "kmmn_nr",
                  "ålder","PredictedServiceLife", "RemainingServiceLife",
                  "index_25", "PCIClass_25")
swedenroads_2025 <-  sw25[,..swroads_cols]

swedenroads_2025 <- UpdateBeltyp25(swedenroads_2025, lans_dt)

swedenroads_2025 <- swedenroads_2025 %>%
  dplyr::mutate(PavementType = ChangeBeltypNumerisk(PavementType)) %>%
  dplyr::mutate(tackning = ChangeTackningNumerisk(tackning)) %>%
  rename(blggnngsd = beldat_25, mätdatm = matdatum_2)

# Use values from PMSv3
swedenroads_2025 <- swedenroads_2025  %>%
  mutate(
    vägktgr = sweden25$vägktgr,
    vägtyp = sweden25$vägtyp,
    dou2017 = sweden25$dou2017,
    vägbrdd = sweden25$vägbrdd,
    hastght = sweden25$hastght,
    brghtsk = sweden25$brghtsk
  )

swedenroads_2025 %>% summarise(na_count = sum(is.na(PavementType))) # NA count before missing: 19385
swedenroads_2025 %>% summarise(na_count = sum(is.na(vägktgr))) # NA count before missing: 9
swedenroads_2025 %>% summarise(na_count = sum(is.na(vägtyp))) # NA count before missing: 4
swedenroads_2025 %>% summarise(na_count = sum(is.na(dou2017))) # NA count before missing: 8
swedenroads_2025 %>% summarise(na_count = sum(is.na(vägbrdd))) # NA count before missing: 98
swedenroads_2025 %>% summarise(na_count = sum(is.na(hastght))) # NA count before missing: 710
swedenroads_2025 %>% summarise(na_count = sum(is.na(brghtsk))) # NA count before missing: 0


# Add classes
pci2034_cur <- pci2034 %>% dplyr::filter(Year == 2033) %>%
  dplyr::select(Objectd,PCIClass)
names(pci2034_cur) <- c("id","IKls_1")

pci2034_mcur_sw <- pci2034_mcur %>% dplyr::filter(Year == 2031) %>%
  dplyr::select(Objectd,PCIClass)
names(pci2034_mcur_sw) <- c("id","IKls_2")

pci2034_min <- st_read( "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2025/Scenarion/dot_2025_with_min_uhskuld_2024.shp") 
pci2034_min <- st_drop_geometry(pci2034_min)

pci2034_min_sw <- pci2034_min %>% 
  dplyr::select(objectd, ikl_minuh_)
names(pci2034_min_sw) <- c("id","IKls_3")

swedenroads_2025 <- dplyr::left_join(swedenroads_2025, pci2034_cur, by = c("id"))
swedenroads_2025 <- dplyr::left_join(swedenroads_2025, pci2034_mcur_sw, by = c("id"))
swedenroads_2025 <- dplyr::left_join(swedenroads_2025, pci2034_min_sw, by = c("id"))

swedenroads_2025 %>% summarise(na_count = sum(is.na(IKls_3))) # NA count before missing: 19781
swedenroads_2025 %>% summarise(na_count = sum(is.na(IKls_2))) # NA count before missing: 406
swedenroads_2025 %>% summarise(na_count = sum(is.na(IKls_1))) # NA count before missing: 406

swedenroads_2025 <- swedenroads_2025 %>%
  mutate(
    IKls_3 = if_else(is.na(IKls_3), "5", IKls_3),
    IKls_2 = if_else(is.na(IKls_2), "3", IKls_2),
    IKls_1 = if_else(is.na(IKls_1), 3, IKls_1),
    IKls_1 = as.character(IKls_1)
  )

swroads_cols2 <- c("id", "brghtsk", "hastght", "dou2017",
                   "ådt_frd", "ådt_tng", "ådt_mtr", "vägbrdd",
                   "vägnmmr","vägktgr", "vägtyp", "längd",  
                   "blggnngsd", "beltyp_25", "tackning",
                   "spårdjp",  "iri","mätdatm","IRI_maint", "SP_maint",
                   "län_nr", "kmmn_nr",
                   "ålder","PredictedServiceLife", "RemainingServiceLife",
                   "index_25", "PCIClass_25","IKls_1","IKls_2","IKls_3")
swedenroads_2025 <-  swedenroads_2025[,..swroads_cols2]
names(swedenroads_2025) <- c("id", "brghtsk", "hastght", "dou2017",
                             "ådt_frd", "ådt_tng", "ådt_mtr", "vägbrdd",
                             "vägnmmr","vägktgr", "vägtyp", "längd",  
                             "blggnngsd","blggnngst", "tackning",
                             "spårdjp",  "iri","mätdatm","IRI_maint", "SP_maint",
                             "län_nr", "kmmn_nr",
                             "ålder","FrvntdL", "ÅtrstnL",
                             "Tllstnl", "IndxKls","IKls_1","IKls_2","IKls_3")
# Join with geometry
swedenroads_2025 <- dplyr::left_join(swedenroads_2025,sw25_geo, by=c("id" = "id"))
head(swedenroads_2025)

# Quality check
QualitativeStatsSingleGroup(swedenroads_2025, quo(IndxKls), quo(längd))
QualitativeStatsSingleGroup(swedenroads_2025, quo(IKls_1), quo(längd))
QualitativeStatsSingleGroup(swedenroads_2025, quo(IKls_2), quo(längd))
QualitativeStatsSingleGroup(swedenroads_2025, quo(IKls_3), quo(längd))

# Export
st_write(swedenroads_2025, paste0(datapath,"2025/VåraVägar/swedenroads_2025_all_scenarios.shp"), append=FALSE)


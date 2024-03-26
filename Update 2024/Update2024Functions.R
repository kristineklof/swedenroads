#=================================================================#
#                  Update 2024 data functions
#=================================================================#

UpdateVariables24 <- function(sweden_yy, lans_dt){
  # Add beläggning 2024
  sw_yy <- sweden_yy %>% dplyr::mutate(Treatment = beltyp_24)
  sw_yy <- AddPavementType(sw_yy, lans_dt)
  sw_yy[, Treatment := NULL]
  sw_yy <- sw_yy %>% dplyr::mutate(PavementType = as.character(PavementType)) %>% 
    dplyr::mutate(PavementType = if_else_na(beltyp_24 == "MJOG - Mjukbitumenbundet grus med oljegrusgradering",
                                            "Halvvarm",PavementType)) %>%
    dplyr::mutate(PavementType = if_else_na(beltyp_24 == "G 0/16 - Grus slitlager 0/16",
                                            "Ytbehandling på grus",PavementType)) %>%
    dplyr::mutate(PavementType = if_else_na(beltyp_24 == "G ÅK - Grus återvinning kantmaterial",
                                            "Ytbehandling på grus",PavementType)) %>%
    dplyr::mutate(PavementType = if_else_na(beltyp_24 == "G 8/16 - Grus slitlager fraktion 8/16",
                                            "Ytbehandling på grus",PavementType)) %>%
    dplyr::mutate(PavementType = as.factor(PavementType)) 
  
  # Add ÅDT mätår 202x
  sw_yy <- sw_yy %>% dplyr::mutate(adt_mat_24 = substr(as.character(adt_mat_24), 1, 4)) %>%
    dplyr::mutate(adt_mat_24 = as.numeric(adt_mat_24))
  
  # Uppdatera NVDB variabler
  sw_yy <- sw_yy %>% 
    dplyr::mutate(brghtsk = if_else_na(brghtsk != barig_24, barig_24, brghtsk)) %>%
    #dplyr::mutate(hastght = if_else_na(hastght != hast_24, hast_24, hastght)) %>%
    dplyr::mutate(dou2017 = if_else_na(dou2017 != dou_24, dou_24, dou2017)) %>%
    dplyr::mutate(ådt_frd = if_else_na(ådt_frd !=  adt_24,  adt_24, ådt_frd)) %>%
    dplyr::mutate(ådt_tng = if_else_na(ådt_tng != adt_tung_2, adt_tung_2, ådt_tng)) %>%
    dplyr::mutate(ådt_mtr = if_else_na(ådt_mtr != adt_mat_24, adt_mat_24, ådt_mtr)) %>%
    dplyr::mutate(vägbrdd = if_else_na(vägbrdd != vagbredd_2, vagbredd_2, vägbrdd)) %>%
    dplyr::mutate(vägtyp = if_else_na(vägtyp != vagtyp_24, vagtyp_24, vägtyp))
  
  # Uppdatera trafikklass
  sw_yy <- TrafficClass(sw_yy)
  
  return(sw_yy)
}

NewLifetime24 <- function(sw_yy, nvdb_2020, lan_surv_dt, datum){
  # Uppdatera ålder
  sw_yy <- sw_yy %>% dplyr::mutate(blggnngsd = UpdateBelaggningsdatum(blggnngsd, beldat_24)) %>%
    dplyr::mutate(blggnngsd = as.Date(as.numeric(blggnngsd), origin = "1970-01-01")) %>%
    dplyr::mutate(ålder = CalculateAge(blggnngsd,datum))
  
  # Uppdatera mätningar
  sw_yy <- sw_yy %>% 
    dplyr::mutate(matdatum_2 = as.Date(matdatum_2)) %>%
    dplyr::mutate(mätdatm = UpdateMatdatum(mätdatm, matdatum_2)) %>%
    dplyr::mutate(mätdatm = as.Date(as.numeric(mätdatm), origin = "1970-01-01")) %>%
    dplyr::mutate(iri = if_else_na(!is.na(irih_24), irih_24, iri)) %>%
    dplyr::mutate(spårdjp = UpdateSpårdjup(spårdjp, sparm17_24, sparm15_24, vägbrdd))
  
  # Infoga täckning 2019 och tidigare
  nv20 <- st_drop_geometry(nvdb_2020[c("omfttnn","objectid")])
  sw_yy <- dplyr::left_join(sw_yy, nv20[c("omfttnn","objectid")],
                            by = c("id"="objectid"))
  sw_yy <- sw_yy %>% dplyr::mutate(tackning = if_else(!is.na(tackning_2), tackning_2, omfttnn))
  
  svars <- c("id","trfkkls", "ådt_frd", "ådt_tng", "PavementType", "region", 
             "brghtsk", "vägtyp", "vägbrdd", "hastght","tackning","ålder", "län_nr")
  sw_yy_servlife <- sw_yy[,..svars]
  
  sw_yy_servlife <- sw_yy_servlife %>% 
    dplyr::rename(BearingCapacityClass = brghtsk) %>%
    dplyr::rename(AADT = ådt_frd) %>%
    dplyr::rename(AADT_heavy = ådt_tng) %>%
    dplyr::rename(tkl8 = trfkkls) %>%
    dplyr::rename(SpeedLimit = hastght) %>%
    dplyr::rename(Coverage = tackning) %>%
    dplyr::rename(Age = ålder) %>%
    dplyr::mutate(RoadWidth = 10*vägbrdd) %>%
    dplyr::mutate(Region = region) %>%
    dplyr::mutate(Region = if_else(län_nr == 9, "Gotland", region)) %>%
    dplyr::mutate(RoadType = if_else(vägtyp == 1, "2+1 road", 
                                     if_else(vägtyp == 2, "4-lane road", 
                                             if_else(vägtyp == 3, "Motorway", 
                                                     if_else(vägtyp == 4, "Ordinary road", "Undivided motorway"))))) %>%
    select(-c("vägtyp", "vägbrdd"))
  
  lan_surv_dt_gotland <- lan_surv_dt %>% 
    dplyr::mutate(Region = if_else(LAN_nr == 9, "Gotland", Region))
  
  sw_yy_servlife <- CreateServiceLifeData(sw_yy_servlife, survdat=lan_surv_dt_gotland, metod = "AFT", 
                                          distribution = "lognormal", percentil_high = 0.5, percentil_low = 0.75,
                                          div = "no")
  
  sw_yy_servlife <- MaintenanceStandard(sw_yy_servlife)
  
  preds <- sw_yy_servlife %>%
    dplyr::select(id, Pred_50perc, Pred_75perc, PredictedServiceLife, RemainingServiceLife,
                  IRI_maint, SP_maint, RoadType)
  sw_yy <- dplyr::left_join(sw_yy, preds, by="id")
  
  # If remaining service life is missing, replace it with "Åtrstnl-1"
  sw_yy <- sw_yy %>%
    dplyr::mutate(RemainingServiceLife = if_else(is.na(RemainingServiceLife),
                                                 åtrstnl-1,RemainingServiceLife)) %>%
    dplyr::mutate(PredictedServiceLife = if_else(is.na(PredictedServiceLife),
                                                 frvntdl,PredictedServiceLife))
  
  return(sw_yy)
}

CalculatePCI24 <- function(sw_yy, datum){
  pci_data <- sw_yy %>% dplyr::select(id, PredictedServiceLife, RemainingServiceLife,
                                      IRI_maint, SP_maint, sparm17_24, sparm15_24, irih_24, 
                                      blggnngsd, mätdatm, RoadType, vägbrdd, trfkkls) %>%
    dplyr::rename(IRI_r_p = irih_24) %>%
    dplyr::rename(IRI_mnt = IRI_maint) %>%
    dplyr::rename(rt_m17_ = sparm17_24) %>%
    dplyr::rename(rt_m15_ = sparm15_24) %>%
    dplyr::rename(SP_mant = SP_maint) %>%
    dplyr::rename(RmnngSL = RemainingServiceLife) %>%
    dplyr::rename(PrdctSL = PredictedServiceLife) %>%
    dplyr::rename(tkl8 = trfkkls) %>%
    dplyr::rename(RoadTyp = RoadType) %>%
    dplyr::rename(RodWdth = vägbrdd) %>%
    dplyr::rename(TrtmntD = blggnngsd) %>%
    dplyr::rename(MsrmntD = mätdatm)
  
  pci_data <- CreatePCI(pci_data, datum = datum)
  pci_data <- PCIClass(pci_data)
  
  pci_data <- pci_data %>%
    dplyr::rename(IRI_Index_24 = IRI_Index) %>%
    dplyr::rename(Rut_17_Index_24 = Rut_17_Index) %>%
    dplyr::rename(Rut_15_Index_24 = Rut_15_Index) %>%
    dplyr::rename(RMS_Index_24 = RMS_Index) %>%
    dplyr::rename(Rut_Index_24 = Rut_Index) %>%
    dplyr::rename(PCI_24 = PCI) %>%
    dplyr::rename(PCIClass_24 = PCIClass) 
  
  pci_data <- pci_data %>% dplyr::select(id, IRI_Index_24,
                                         Rut_17_Index_24, Rut_15_Index_24,
                                         RMS_Index_24, Rut_Index_24,
                                         PCI_24, PCIClass_24)
  sw_yy <- dplyr::left_join(sw_yy, pci_data, by="id")
  
  return(sw_yy)
}
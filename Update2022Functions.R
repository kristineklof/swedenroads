#=================================================================#
#                  Update 2022 data functions
#=================================================================#

UpdateVariables22 <- function(sweden_yy){
  # Add beläggning 2022
  sw_yy <- sweden_yy %>% dplyr::mutate(Treatment = beltyp_22)
  sw_yy <- AddPavementType(sw_yy, lans_dt)
  sw_yy[, Treatment := NULL]
  
  # Add ÅDT mätår 202x
  sw_yy <- sw_yy %>% dplyr::mutate(adt_mat_21 = substr(as.character(adt_mat_21), 1, 4)) %>%
    dplyr::mutate(adt_mat_21 = as.numeric(adt_mat_21))
  
  # Uppdatera NVDB variabler
  sw_yy <- sw_yy %>% 
    dplyr::mutate(brghtsk = if_else_na(brghtsk != barig_21, barig_21, brghtsk)) %>%
    #dplyr::mutate(hastght = if_else_na(hastght != hast_21, hast_21, hastght)) %>%
    dplyr::mutate(dou2017 = if_else_na(dou2017 != dou_21, dou_21, dou2017)) %>%
    dplyr::mutate(Ådt_frd = if_else_na(Ådt_frd !=  adt_21,  adt_21, Ådt_frd)) %>%
    dplyr::mutate(Ådt_tng = if_else_na(Ådt_tng != adt_tung_2, adt_tung_2, Ådt_tng)) %>%
    dplyr::mutate(Ådt_mtr = if_else_na(Ådt_mtr != adt_mat_21, adt_mat_21, Ådt_mtr)) %>%
    dplyr::mutate(vägbrdd = if_else_na(vägbrdd != bredd_21, bredd_21, vägbrdd)) %>%
    dplyr::mutate(vägktgr = if_else_na(vägktgr != vagkategor, vagkategor, vägktgr)) %>%
    dplyr::mutate(vägtyp = if_else_na(vägtyp != vagtyp_21, vagtyp_21, vägtyp))
  
  # Uppdatera trafikklass
  sw_yy <- TrafficClass2022(sw_yy)
  
  return(sw_yy)
}

NewLifetime22 <- function(sw_yy, nvdb_2020, lan_surv_dt){
  # Uppdatera ålder
  sw_yy <- sw_yy %>% dplyr::mutate(blggnngsd = UpdateBelaggningsdatum(blggnngsd, beldat_22)) %>%
    dplyr::mutate(blggnngsd = as.Date(blggnngsd, origin="1970-01-01")) %>%
    dplyr::mutate(Ålder = CalculateAge(blggnngsd,"2022-01-01"))
  
  # Uppdatera mätningar
  sw_yy <- sw_yy %>% dplyr::mutate(mätdatm = UpdateMatdatum(mätdatm, matdatum_2)) %>%
    dplyr::mutate(mätdatm = as.Date(mätdatm, origin="1970-01-01")) %>%
    dplyr::mutate(iri = if_else_na(!is.na(irih_22), irih_22, iri)) %>%
    dplyr::mutate(spårdjp = UpdateSpårdjup(spårdjp, sparm17_22, sparm15_22, vägbrdd))
  
  # Infoga täckning 2019 och tidigare
  nv20 <- st_drop_geometry(nvdb_2020[c("omfttnn","objectid")])
  sw_yy <- dplyr::left_join(sw_yy, nv20[c("omfttnn","objectid")],
                            by = c("id"="objectid"))
  sw_yy <- sw_yy %>% dplyr::mutate(tackning = if_else(!is.na(tackning_2), tackning_2, omfttnn))
  
  svars <- c("id","trfkkls", "Ådt_frd", "Ådt_tng", "PavementType", "region", 
             "brghtsk", "vägtyp", "vägbrdd", "hastght","tackning","Ålder", "län_nr")
  sw_yy_servlife <- sw_yy[,..svars]
  
  sw_yy_servlife <- sw_yy_servlife %>% 
    dplyr::rename(BearingCapacityClass = brghtsk) %>%
    dplyr::rename(AADT = Ådt_frd) %>%
    dplyr::rename(AADT_heavy = Ådt_tng) %>%
    dplyr::rename(tkl8 = trfkkls) %>%
    dplyr::rename(SpeedLimit = hastght) %>%
    dplyr::rename(Coverage = tackning) %>%
    dplyr::rename(Age = Ålder) %>%
    dplyr::mutate(RoadWidth = 10*vägbrdd) %>%
    dplyr::mutate(Region = if_else(region == 1, "Mitt",
                                   if_else(region == 2, "Nord",
                                           if_else(region == 3, "Ost",
                                                   if_else(region == 4, "Sthlm",
                                                           if_else(region == 5, "Syd", "Vast")))))) %>%
    dplyr::mutate(Region = if_else(län_nr == 9, "Gotland", Region)) %>%
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
  
  # If remaining service life is missing, replace it with "Åtrstnl-2"
  sw_yy <- sw_yy %>%
    dplyr::mutate(RemainingServiceLife = if_else(is.na(RemainingServiceLife),
                                                 Åtrstnl-2,RemainingServiceLife)) %>%
    dplyr::mutate(PredictedServiceLife = if_else(is.na(PredictedServiceLife),
                                                 frvntdl,PredictedServiceLife))
  
  return(sw_yy)
}

CalculatePCI22 <- function(sw_yy){
  pci_data <- sw_yy %>% dplyr::select(id, PredictedServiceLife, RemainingServiceLife,
                                      IRI_maint, SP_maint, sparm17_22, sparm15_22, irih_22, 
                                      blggnngsd, mätdatm, RoadType, vägbrdd, trfkkls) %>%
    dplyr::rename(IRI_r_p = irih_22) %>%
    dplyr::rename(IRI_mnt = IRI_maint) %>%
    dplyr::rename(rt_m17_ = sparm17_22) %>%
    dplyr::rename(rt_m15_ = sparm15_22) %>%
    dplyr::rename(SP_mant = SP_maint) %>%
    dplyr::rename(RmnngSL = RemainingServiceLife) %>%
    dplyr::rename(PrdctSL = PredictedServiceLife) %>%
    dplyr::rename(tkl8 = trfkkls) %>%
    dplyr::rename(RoadTyp = RoadType) %>%
    dplyr::rename(RodWdth = vägbrdd) %>%
    dplyr::rename(TrtmntD = blggnngsd) %>%
    dplyr::rename(MsrmntD = mätdatm)
  
  pci_data <- CreatePCI(pci_data, datum = "2021-01-01")
  pci_data <- PCIClass(pci_data)
  
  pci_data <- pci_data %>%
    dplyr::rename(IRI_Index_22 = IRI_Index) %>%
    dplyr::rename(Rut_17_Index_22 = Rut_17_Index) %>%
    dplyr::rename(Rut_15_Index_22 = Rut_15_Index) %>%
    dplyr::rename(RMS_Index_22 = RMS_Index) %>%
    dplyr::rename(Rut_Index_22 = Rut_Index) %>%
    dplyr::rename(PCI_22 = PCI) %>%
    dplyr::rename(PCIClass_22 = PCIClass) 
  
  pci_data <- pci_data %>% dplyr::select(id, IRI_Index_22,
                                         Rut_17_Index_22, Rut_15_Index_22,
                                         RMS_Index_22, Rut_Index_22,
                                         PCI_22, PCIClass_22)
  sw_yy <- dplyr::left_join(sw_yy, pci_data, by="id")
  
  return(sw_yy)
}
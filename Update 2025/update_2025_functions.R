#=================================================================#
#              Swedenroads: update 2025 data
#=================================================================#

# Add län
AddLan2025 <- function(dat, lankom){
  # Get lan and kommun
  names(lankom) <- c('län_nr','Länsnamn','kmmn_nr','Kommunnamn')
  
  dat$kmmn_nr <- as.integer(dat$kmmn_nr)
  dat <- left_join(dat, lankom, by = "kmmn_nr")
  dat$län_nr <- as.integer(dat$län_nr)
  
  return(dat)
}

AddAADTIfMissing2025 <- function(dat){
  # Impute missing AADT by municipality, roadtype, roadcategory
  # If AADT and AADT_heavy is missing, impute mean for the municipality
  dat <- dat %>% group_by(kmmn_nr,vägtyp,vägktgr) %>% 
    mutate(ådt_frd_imp = round(mean(ådt_frd, na.rm = TRUE), digits=0)) %>%
    mutate(ådt_tng_imp = round(mean(ådt_tng, na.rm = TRUE), digits=0)) %>%
    mutate(ådt_frd = coalesce(ådt_frd,ådt_frd_imp)) %>%
    mutate(ådt_tng = coalesce(ådt_tng,ådt_tng_imp)) %>%
    select(-c(ådt_frd_imp,ådt_tng_imp)) %>%
    ungroup() %>%
    group_by(kmmn_nr) %>% 
    mutate(ådt_frd_imp = round(mean(ådt_frd, na.rm = TRUE), digits=0)) %>%
    mutate(ådt_tng_imp = round(mean(ådt_tng, na.rm = TRUE), digits=0)) %>%
    mutate(ådt_frd = coalesce(ådt_frd,ådt_frd_imp)) %>%
    mutate(ådt_tng = coalesce(ådt_tng,ådt_tng_imp)) %>%
    select(-c(ådt_frd_imp,ådt_tng_imp)) %>%
    ungroup()
  
  return(dat)
}


AddRoadWidthIfMissing2025 <- function(dat){
  # If AADT and AADT_heavy is missing, impute mean for the municipality & roadtype & roadcategory
  mcols = c("kmmn_nr", "vägtyp", "vägktgr")
  dat[is.na(vägbrdd), vägbrdd := dat[.BY, mean(vägbrdd, na.rm=TRUE), on=mcols], by=mcols]
  
  # If still missing, impute mean per municipality
  dat[is.na(vägbrdd), vägbrdd := dat[.BY, mean(vägbrdd, na.rm=TRUE), on="kmmn_nr"], by="kmmn_nr"]
  
  # Round
  dat[, vägbrdd := round(vägbrdd, digits = 1)]
  
  return(dat)
}

AddSpeedLimitIfMissing2025 <- function(dat){
  # If AADT and AADT_heavy is missing, impute mean for the municipality & roadtype & roadcategory
  mcols = c("kmmn_nr", "vägtyp", "vägktgr")
  dat[is.na(hastght), hastght := dat[.BY, mean(hastght, na.rm=TRUE), on=mcols], by=mcols]
  
  # If still missing, impute mean per municipality
  dat[is.na(hastght), hastght := dat[.BY, mean(hastght, na.rm=TRUE), on="kmmn_nr"], by="kmmn_nr"]
  
  # Round
  dat[, hastght := RoundUpTo10(hastght)]
  
  return(dat)
}

AddPavementTypeIfMissing2025 <- function(dat){
  # If PavementType is missing, add most common type based on tkl8, RoadCategory, RoadType
  pave <- sw25 %>% group_by(trfkkls,vägktgr,vägtyp) %>% 
    count(PavementType) %>% 
    drop_na() %>% 
    top_n(1, n) %>%
    slice(1) %>%  # In case of remaining ties, take the first row
    select(-n)
  names(pave) <- c("trfkkls","vägktgr","vägtyp","PavementType_imp")
  
  dat <- dat %>%
    left_join(pave, by = c("trfkkls", "vägktgr", "vägtyp"))
  dat <- dat %>% mutate(PavementType = coalesce(PavementType,PavementType_imp)) %>%
    select(-PavementType_imp)
  
  # If still missing, add "Varm"
  setDT(dat)
  dat[, PavementType := as.character(PavementType)]
  dat[, PavementType := ifelse(is.na(PavementType), "Varm", PavementType)]
  dat[, PavementType := as.factor(PavementType)]
  dat[, PavementType := relevel(PavementType, "Varm")]
  
  return(dat)
}

AddDOU2017IfMissing2025 <- function(dat){
  # If PavementType is missing, add most common type based on tkl8, RoadCategory, RoadType
  dou <- dat %>% group_by(län_nr,trfkkls,vägktgr,vägtyp) %>% 
    count(dou2017) %>% 
    drop_na() %>% 
    top_n(1, n) %>%
    slice(1) %>%  # In case of remaining ties, take the first row
    select(-n)
  names(dou) <- c("län_nr","trfkkls", "vägktgr", "vägtyp", "DoU2017_imp")
  
  dat <- left_join(dat, dou, by = c("län_nr","trfkkls", "vägktgr", "vägtyp"))
  dat <- dat %>% mutate(dou2017 = coalesce(dou2017,DoU2017_imp)) %>%
    select(-DoU2017_imp)
  
  # If still missing, add based on tkl8 only
  setDT(dat)
  dou2 <- dat %>% group_by(trfkkls) %>% 
    count(dou2017) %>% 
    drop_na() %>% 
    top_n(1, n) %>%
    slice(1) %>%  # In case of remaining ties, take the first row
    select(-n)
  names(dou2) <- c("trfkkls", "DoU2017_imp")
  
  dat <- left_join(dat, dou2, by = c("trfkkls"))
  dat <- dat %>% mutate(dou2017 = dplyr::coalesce(dou2017,DoU2017_imp)) %>%
    select(-DoU2017_imp)
  
  return(dat)
}

AddAlderIfMissing2025 <- function(dat){
  # If Alder is missing, impute mean for the municipality & roadtype & roadcategory
  mcols = c("län_nr","trfkkls", "vägtyp", "vägktgr","PavementType")
  dat[is.na(ålder), ålder := dat[.BY, mean(ålder, na.rm=TRUE), on=mcols], by=mcols]
  
  # If still missing, impute mean per län and trafikklass
  m2cols = c("län_nr","trfkkls")
  dat[is.na(ålder), ålder := dat[.BY, mean(ålder, na.rm=TRUE), on=m2cols], by=m2cols]
  
  # Round
  dat[, ålder := round(ålder,0)]
  
  return(dat)
}

#=================================================================#
#                  Update 2025 data functions
#=================================================================#

UpdateBeltyp25 <- function(sweden_yy, lans_dt){
  # Add beläggning 2024
  sw_yy <- sweden_yy %>% dplyr::mutate(Treatment = beltyp_25)
  sw_yy <- AddPavementType(sw_yy, lans_dt)
  sw_yy[, Treatment := NULL]
  sw_yy <- sw_yy %>% dplyr::mutate(PavementType = as.character(PavementType)) %>% 
    dplyr::mutate(PavementType = if_else_na(beltyp_25 == "MJOG - Mjukbitumenbundet grus med oljegrusgradering",
                                            "Halvvarm",PavementType)) %>%
    dplyr::mutate(PavementType = if_else_na(beltyp_25 == "G 0/16 - Grus slitlager 0/16",
                                            "Ytbehandling på grus",PavementType)) %>%
    dplyr::mutate(PavementType = if_else_na(beltyp_25 == "G ÅK - Grus återvinning kantmaterial",
                                            "Ytbehandling på grus",PavementType)) %>%
    dplyr::mutate(PavementType = if_else_na(beltyp_25 == "G 8/16 - Grus slitlager fraktion 8/16",
                                            "Ytbehandling på grus",PavementType)) %>%
    dplyr::mutate(PavementType = as.factor(PavementType)) 
  
  return(sw_yy)
}

UpdateAlder25 <- function(dat, datum){
  # Uppdatera ålder
  dat <- dat %>% 
    dplyr::mutate(beldat_25 = as.Date(beldat_25)) %>%
    dplyr::mutate(ålder = CalculateAge(beldat_25,datum))
  
  return(dat)
}

UpdateMatning25 <- function(dat){
  # Uppdatera mätningar
  dat <- dat %>% 
    dplyr::mutate(matdatum_2 = as.Date(matdatum_2)) %>%
    dplyr::mutate(iri = irih_25) %>%
    dplyr::mutate(spårdjp = NA) %>%
    dplyr::mutate(spårdjp = UpdateSpårdjup(spårdjp, sparm17_25, sparm15_25, vägbrdd))
  
  return(dat)
}


NewLifetime25 <- function(sw_yy, lan_surv_dt, datum){
  svars <- c("id","trfkkls", "ådt_frd", "ådt_tng", "PavementType", "region", 
             "brghtsk", "vägtyp", "vägbrdd", "hastght","tackning","ålder", "län_nr")
  sw_yy_servlife <- sw_yy[,..svars]
  
  sw_yy_servlife <- sw_yy_servlife %>% 
    dplyr::rename(BearingCapacityClass = brghtsk) %>%
    dplyr::mutate(BearingCapacityClass = as.factor(BearingCapacityClass)) %>%
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
  
  return(sw_yy)
}

CalculatePCI25 <- function(sw_yy, datum){
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


CalculatePCI25 <- function(sw_yy, datum){
  pci_data <- sw_yy %>% dplyr::select(id, PredictedServiceLife, RemainingServiceLife,
                                      IRI_maint, SP_maint, sparm17_25, sparm15_25, irih_25, 
                                      beldat_25, matdatum_2, RoadType, vägbrdd, trfkkls) %>%
    dplyr::rename(IRI_r_p = irih_25) %>%
    dplyr::rename(IRI_mnt = IRI_maint) %>%
    dplyr::rename(rt_m17_ = sparm17_25) %>%
    dplyr::rename(rt_m15_ = sparm15_25) %>%
    dplyr::rename(SP_mant = SP_maint) %>%
    dplyr::rename(RmnngSL = RemainingServiceLife) %>%
    dplyr::rename(PrdctSL = PredictedServiceLife) %>%
    dplyr::rename(tkl8 = trfkkls) %>%
    dplyr::rename(RoadTyp = RoadType) %>%
    dplyr::rename(RodWdth = vägbrdd) %>%
    dplyr::rename(TrtmntD = beldat_25) %>%
    dplyr::rename(MsrmntD = matdatum_2)
  
  pci_data <- CreatePCI(pci_data, datum = datum)
  
  pci_data <- pci_data %>%
    dplyr::rename(IRI_Index_25 = IRI_Index) %>%
    dplyr::rename(Rut_17_Index_25 = Rut_17_Index) %>%
    dplyr::rename(Rut_15_Index_25 = Rut_15_Index) %>%
    dplyr::rename(RMS_Index_25 = RMS_Index) %>%
    dplyr::rename(Rut_Index_25 = Rut_Index) %>%
    dplyr::rename(index_25 = PCI) 
  
  pci_data <- pci_data %>% dplyr::select(id, IRI_Index_25,
                                         Rut_17_Index_25, Rut_15_Index_25,
                                         RMS_Index_25, Rut_Index_25,
                                         index_25)
  sw_yy <- dplyr::left_join(sw_yy, pci_data, by="id")
  
  return(sw_yy)
}


PCIClass2025 <- function(dat, pci_var, pci_class){
  setDT(dat)
  
  dat[, (pci_class) := if_else_na(get(pci_var) > 80, 5, NA)]
  dat[, (pci_class) := if_else_na(get(pci_var) <= 80, 4, get(pci_class))]
  dat[, (pci_class) := if_else_na(get(pci_var) <= 60, 3, get(pci_class))]
  dat[, (pci_class) := if_else_na(get(pci_var) <= 40, 2, get(pci_class))]
  dat[, (pci_class) := if_else_na(get(pci_var) <= 20, 1, get(pci_class))]
  
  return(dat)
}

#############################
# New region fr.o.m. 2024
TRVRegionFrom2024 <- function(lansnamn){
  region <- if_else(lansnamn == "Västernorrlands län" | lansnamn == "Jämtlands län" | 
                     lansnamn == "Västerbottens län" | lansnamn == "Norrbottens län", 
                    "Norra", ifelse(
                       lansnamn == "Uppsala län" | lansnamn == "Södermanlands län" | 
                         lansnamn == "Västmanlands län" | lansnamn == "Värmlands län" | 
                         lansnamn == "Örebro län" | lansnamn == "Dalarnas län" | lansnamn == "Gävleborgs län",
                       "Mellersta", ifelse(
                         lansnamn == "Stockholms län" | lansnamn == "Gotlands län",
                         "Östra", ifelse(
                           lansnamn == "Hallands län" | lansnamn == "Västra Götalands län",
                           "Västra", ifelse(
                             lansnamn == "Jönköpings län" | lansnamn == "Kalmar län" | lansnamn == "Östergötlands län",
                             "Sydöstra", ifelse(
                               lansnamn == "Kronobergs län" | lansnamn == "Blekinge län" | lansnamn == "Skåne län",
                               "Södra",NA))))))
  return(region)
}

itShouldReturn2024ArsRegionIndelning <- function(){
  test <- data.frame(Länsnamn = c("Västra Götalands län", "Jämtlands län",        "Gävleborgs län",       "Hallands län",        
                                  "Skåne län",            "Norrbottens län",      "Värmlands län",        "Stockholms län",      
                                  "Blekinge län",         "Västerbottens län",    "Södermanlands län",    "Uppsala län",        
                                  "Västmanlands län",     "Västernorrlands län",  "Örebro län",           "Dalarnas län",        
                                  "Jönköpings län",       "Kalmar län",           "Kronobergs län",       "Östergötlands län",   
                                  "Gotlands län"))
  
  res <- test %>% dplyr::mutate(Region = TRVRegionFrom2024(Länsnamn))
  gold <- c("Västra","Norra","Mellersta","Västra","Södra","Norra","Mellersta","Östra","Södra","Norra",
            "Mellersta","Mellersta","Mellersta","Norra","Mellersta","Mellersta","Sydöstra","Sydöstra","Södra",
            "Sydöstra","Östra")
  stopifnot(res$Region == gold)
}

itShouldReturn2024ArsRegionIndelning()

# Summarize missing

missing_length_summary <- function(data, var_missing, var_length) {
  # Capture the variable name as a string
  var_missing_label <- as_label(enquo(var_missing))
  
  out <- data %>%
    summarise(
      missing_length_km = sum({{ var_length }}[is.na({{ var_missing }})], na.rm = TRUE) / 1000,
      proportion_missing = sum({{ var_length }}[is.na({{ var_missing }})], na.rm = TRUE) /
        sum({{ var_length }}, na.rm = TRUE)
    ) %>%
    # Convert the one-row tibble to a base R data frame
    as.data.frame()
  
  # Assign the row name to be the variable's name
  rownames(out) <- var_missing_label
  
  out
}

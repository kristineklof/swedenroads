MaintenanceStandard2026 <- function(dat){
  sl <- c(120,110,100,90,80,70,60,50)

  maint_stand <- data.frame(
    trfkkls = c(rep(1,7),rep(2,7),rep(3,7),rep(4,7),rep(5,8),rep(6,8),rep(7,8),rep(8,8)),
    hastght = c(rep(sl[-1],4),rep(sl,4)),
    iri_mnt = c(4.3,4.7,5.2,5.9,6.7,6.7,6.7,
                4.0,4.4,4.9,5.5,6.3,6.3,6.3,
                3.7,4.1,4.5,5.1,5.8,5.8,5.8,
                3.0,3.3,3.7,4.2,4.8,5.2,5.2,
                2.4,2.6,2.9,3.2,3.2,4.1,4.9,4.9,
                2.4,2.6,2.9,3.2,3.2,4.1,4.9,4.9,
                2.4,2.6,2.9,3.2,3.2,4.1,4.9,4.9,
                2.4,2.6,2.9,3.2,3.2,4.1,4.9,4.9),
    sp_mant = c(18,18,24,24,30,30,30,
                18,18,22,22,27,27,27,
                18,18,20,20,24,24,24,
                15,16,17,18,20,21,21,
                13,13,14,14,14,16,18,18,
                13,13,14,14,14,16,18,18,
                13,13,14,14,14,16,18,18,
                13,13,14,14,14,16,18,18)
  )

  out <- dplyr::left_join(
    dat,
    maint_stand,
    by = c("trfkkls", "hastght"),
    suffix = c("", ".std")
  )

  # If dat doesn't already have these, create them
  if (!"iri_mnt"  %in% names(out)) out$iri_mnt  <- NA_real_
  if (!"sp_mant" %in% names(out)) out$sp_mant <- NA_real_

  # Fill ONLY missing values in dat, using matched standards (exact match only)
  out$iri_mnt  <- dplyr::if_else(is.na(out$iri_mnt),  out$iri_mnt.std,  out$iri_mnt)
  out$sp_mant <- dplyr::if_else(is.na(out$sp_mant), out$sp_mant.std, out$sp_mant)

  out$iri_mnt.std  <- NULL
  out$sp_mant.std <- NULL

  out
}

# MaintenanceStandard2026 <- function(dat){
#   sl <- c(120,110,100,90,80,70,60,50)
# 
#   maint_stand <- data.frame(
#     trfkkls = c(rep(1,7),rep(2,7),rep(3,7),rep(4,7),rep(5,8),rep(6,8),rep(7,8),rep(8,8)),
#     hastght = c(rep(sl[-1],4),rep(sl,4)),
#     iri_mnt = c(4.3,4.7,5.2,5.9,6.7,6.7,6.7,
#                 4.0,4.4,4.9,5.5,6.3,6.3,6.3,
#                 3.7,4.1,4.5,5.1,5.8,5.8,5.8,
#                 3.0,3.3,3.7,4.2,4.8,5.2,5.2,
#                 2.4,2.6,2.9,3.2,3.2,4.1,4.9,4.9,
#                 2.4,2.6,2.9,3.2,3.2,4.1,4.9,4.9,
#                 2.4,2.6,2.9,3.2,3.2,4.1,4.9,4.9,
#                 2.4,2.6,2.9,3.2,3.2,4.1,4.9,4.9),
#     sp_mant = c(18,18,24,24,30,30,30,
#                 18,18,22,22,27,27,27,
#                 18,18,20,20,24,24,24,
#                 15,16,17,18,20,21,21,
#                 13,13,14,14,14,16,18,18,
#                 13,13,14,14,14,16,18,18,
#                 13,13,14,14,14,16,18,18,
#                 13,13,14,14,14,16,18,18)
#   )
# 
#   # snap hastght to an available standard step, per trfkkls group
#   out <- dat %>%
#     mutate(
#       hastght_std = case_when(
#         trfkkls %in% 1:4 ~ pmax(50, pmin(110, 10 * floor(hastght / 10))),
#         trfkkls %in% 5:8 ~ pmax(50, pmin(120, 10 * floor(hastght / 10))),
#         TRUE ~ NA_real_
#       )
#     ) %>%
#     dplyr::left_join(maint_stand, by = c("trfkkls" = "trfkkls", "hastght_std" = "hastght"),
#                      suffix = c("", ".std"))
# 
#   # fill only missing
#   if (!"iri_mnt" %in% names(out))  out$iri_mnt  <- NA_real_
#   if (!"sp_mant" %in% names(out))  out$sp_mant <- NA_real_
# 
#   out$iri_mnt  <- dplyr::if_else(is.na(out$iri_mnt),  out$iri_mnt.std,  out$iri_mnt)
#   out$sp_mant <- dplyr::if_else(is.na(out$sp_mant), out$sp_mant.std, out$sp_mant)
# 
#   out$iri_mnt.std  <- NULL
#   out$sp_mant.std <- NULL
#   out$hastght_std  <- NULL
# 
#   out
# }

itShouldReplaceMaintenanceStandardWhenMissing <- function(){
  library(dplyr)
  library(tibble)
  
  dat <- tibble(
    trfkkls = c(8, 8, 3, 2),
    hastght = c(110, 100, 90, 70),
    iri_mnt = c(NA, 2.9, NA, 6.3),
    sp_mant = c(NA, 14, NA, 27)
  )
  
  res <- MaintenanceStandard2026(dat)
  
  #print(dat)
  #print(res)
  
  # 1) NA rows were filled with the correct matched standard values
  stopifnot(res$iri_mnt[1] == 2.6, res$sp_mant[1] == 13.0)  # (1,110)
  stopifnot(res$iri_mnt[3] == 4.5, res$sp_mant[3] == 20.0)  # (1,110)

  # 2) Pre-populated rows were NOT overwritten
  stopifnot(res$iri_mnt[2] == 2.9,      res$sp_mant[2] == 14)
  stopifnot(res$iri_mnt[4] == 6.3, res$sp_mant[4] == 27)
}

itShouldReplaceMaintenanceStandardWhenMissing()

# Add län
AddLan2026 <- function(dat, lankom){
  # Get lan and kommun
  names(lankom) <- c('län_nr','Länsnamn','kmmn_nr','Kommunnamn')
  
  dat$kmmn_nr <- as.integer(dat$kmmn_nr)
  dat <- left_join(dat, lankom, by = c("kmmn_nr",'län_nr'))
  
  return(dat)
}

UpdateBelaggningAlderBeltyp26 <- function(dat, lans_dt, datum){
  
  dat %>%
    # --- Build PavementType from beltyp_25 ---
    dplyr::mutate(Treatment = beltyp_25) %>%
    AddPavementType(lans_dt) %>%                           # assumes AddPavementType(dat, lans_dt)
    dplyr::select(-Treatment) %>%
    dplyr::mutate(
      PavementType = as.character(PavementType),
      
      PavementType = if_else_na(
        beltyp_25 == "MJOG - Mjukbitumenbundet grus med oljegrusgradering",
        "Halvvarm", PavementType
      ),
      PavementType = if_else_na(
        beltyp_25 == "G 0/16 - Grus slitlager 0/16",
        "Ytbehandling på grus", PavementType
      ),
      PavementType = if_else_na(
        beltyp_25 == "G ÅK - Grus återvinning kantmaterial",
        "Ytbehandling på grus", PavementType
      ),
      PavementType = if_else_na(
        beltyp_25 == "G 8/16 - Grus slitlager fraktion 8/16",
        "Ytbehandling på grus", PavementType
      ),
      
      PavementType = dplyr::coalesce(PavementType, as.character(pvmntty))
    ) %>%
    dplyr::mutate(PavementType = as.factor(PavementType)) %>%
    
    # --- Update beldatum (if beldat_25 later), then update pvmntty/tackning + compute ålder ---
    dplyr::mutate(
      beldatum  = as.Date(beldatum),
      beldat_25 = as.Date(beldat_25),
      
      .update_bel = !is.na(beldat_25) & (is.na(beldatum) | beldat_25 > beldatum),
      
      beldatum = dplyr::if_else(.update_bel, beldat_25, beldatum),
      
      pvmntty  = dplyr::if_else(.update_bel, as.character(PavementType), pvmntty),
      tacknng = dplyr::if_else(.update_bel, tackning_2, tacknng),
      
      ålder = dplyr::if_else(
        !is.na(beldatum),
        CalculateAge(beldatum, datum),
        ålder + 1
      )
    ) %>%
    dplyr::select(-.update_bel)
}

UpdateMatning26 <- function(dat){
  dat %>%
    dplyr::mutate(
      matdatum   = as.Date(matdatum),
      matdatum_2 = as.Date(matdatum_2),
      
      # Which rows should be updated?
      .update_mat = !is.na(matdatum_2) & (is.na(matdatum) | matdatum_2 > matdatum),
      
      # Update matdatum only where the new date is later
      matdatum = dplyr::if_else(.update_mat, matdatum_2, matdatum),
      
      # Update iri only where matdatum was updated
      iri = dplyr::if_else(.update_mat, irih_25, iri),
      
      # Update spårdjp only where matdatum was updated
      spårdjp = dplyr::if_else(
        .update_mat,
        UpdateSpårdjup(spårdjp, sparm17_25, sparm15_25, vägbrdd),
        spårdjp
      )
    ) %>%
    dplyr::select(-.update_mat)
}

NewLifetime26 <- function(sw_yy, lan_surv_dt, datum){
  svars <- c("id","trfkkls", "ådt_frd", "ådt_tng", "PavementType", "region", 
             "brghtsk", "roadtyp", "vägbrdd", "hastght","tacknng","ålder", "län_nr",
             "iri_mnt", "sp_mant")
  sw_yy_servlife <- sw_yy[,..svars]
  
  sw_yy_servlife <- sw_yy_servlife %>% 
    dplyr::rename(BearingCapacityClass = brghtsk) %>%
    dplyr::mutate(BearingCapacityClass = as.factor(BearingCapacityClass)) %>%
    dplyr::rename(AADT = ådt_frd) %>%
    dplyr::rename(AADT_heavy = ådt_tng) %>%
    dplyr::rename(tkl8 = trfkkls) %>%
    dplyr::rename(SpeedLimit = hastght) %>%
    dplyr::rename(Coverage = tacknng) %>%
    dplyr::rename(Age = ålder) %>%
    dplyr::mutate(RoadWidth = 10*vägbrdd) %>%
    dplyr::mutate(Region = region) %>%
    dplyr::mutate(Region = if_else(län_nr == 9, "Gotland", region)) %>%
    dplyr::mutate(RoadType = roadtyp) %>%
    dplyr::mutate(IRI_maint = iri_mnt) %>%
    dplyr::mutate(SP_maint = sp_mant) %>%
    select(-c("roadtyp", "vägbrdd"))
  
  lan_surv_dt_gotland <- lan_surv_dt %>% 
    dplyr::mutate(Region = if_else(LAN_nr == 9, "Gotland", Region))
  
  sw_yy_servlife <- CreateServiceLifeData(sw_yy_servlife, survdat=lan_surv_dt_gotland, metod = "AFT", 
                                          distribution = "lognormal", percentil_high = 0.5, percentil_low = 0.75,
                                          div = "no", cov_factor=0.3)
  
  sw_yy_servlife <- MaintenanceStandard(sw_yy_servlife)
  
  preds <- sw_yy_servlife %>%
    dplyr::select(id, PredictedServiceLife, RemainingServiceLife)
  sw_yy <- dplyr::left_join(sw_yy, preds, by="id")
  
  return(sw_yy)
}

UpdateServiceLife26 <- function(dat){
  data.table::setDT(dat)
  
  # Update only where new values exist
  dat[!is.na(PredictedServiceLife), prdctsl := PredictedServiceLife]
  dat[!is.na(RemainingServiceLife), rmnngsl := RemainingServiceLife]
  dat[!is.na(PavementType), pvmntty := PavementType]
  
  # Drop temporary columns if they exist
  drop_cols <- intersect(c("PredictedServiceLife", "RemainingServiceLife", "PavementType"), names(dat))
  if (length(drop_cols) > 0) dat[, (drop_cols) := NULL]
  
  dat
}

CalculatePCI26 <- function(sw_yy, datum){
  pci_data <- sw_yy %>% dplyr::select(id, PredictedServiceLife, RemainingServiceLife,
                                      iri_mnt, sp_mant, sparm17_25, sparm15_25, irih_25, 
                                      iri, spårdjp, tacknng, 
                                      beldatum, matdatum, roadtyp, vägbrdd, trfkkls) %>%
    dplyr::rename(IRI_r_p = iri) %>%
    dplyr::rename(IRI_mnt = iri_mnt) %>%
    dplyr::rename(rt_m17_ = spårdjp) %>%
    dplyr::mutate(rt_m15_ = rt_m17_) %>%
    dplyr::rename(SP_mant = sp_mant) %>%
    dplyr::rename(RmnngSL = RemainingServiceLife) %>%
    dplyr::rename(PrdctSL = PredictedServiceLife) %>%
    dplyr::rename(tkl8 = trfkkls) %>%
    dplyr::rename(RoadTyp = roadtyp) %>%
    dplyr::rename(RodWdth = vägbrdd) %>%
    dplyr::rename(TrtmntD = beldatum) %>%
    dplyr::rename(MsrmntD = matdatum) %>%
    dplyr::rename(Coverage = tacknng)
  
  pci_data <- CreatePCI(pci_data, datum = datum)
  pci_data <- PCIClass(pci_data)
  
  pci_data <- pci_data %>%
    dplyr::rename(IRI_Index_26 = IRI_Index) %>%
    dplyr::rename(Rut_17_Index_26 = Rut_17_Index) %>%
    dplyr::rename(Rut_15_Index_26 = Rut_15_Index) %>%
    dplyr::rename(RMS_Index_26 = RMS_Index) %>%
    dplyr::rename(Rut_Index_26 = Rut_Index) %>%
    dplyr::rename(PCI_26 = PCI) %>%
    dplyr::rename(PCIClass_26 = PCIClass) 
  
  pci_data <- pci_data %>% dplyr::select(id, IRI_Index_26,
                                         Rut_17_Index_26, Rut_15_Index_26,
                                         RMS_Index_26, Rut_Index_26,
                                         PCI_26, PCIClass_26)
  sw_yy <- dplyr::left_join(sw_yy, pci_data, by="id")
  
  return(sw_yy)
}

PCIClass2026 <- function(dat, pci_var, pci_class){
  setDT(dat)
  
  dat[, (pci_class) := if_else_na(get(pci_var) > 80, 5, NA)]
  dat[, (pci_class) := if_else_na(get(pci_var) <= 80, 4, get(pci_class))]
  dat[, (pci_class) := if_else_na(get(pci_var) <= 60, 3, get(pci_class))]
  dat[, (pci_class) := if_else_na(get(pci_var) <= 40, 2, get(pci_class))]
  dat[, (pci_class) := if_else_na(get(pci_var) <= 20, 1, get(pci_class))]
  
  return(dat)
}


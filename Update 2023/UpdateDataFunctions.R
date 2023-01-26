#=================================================================#
#              Swedenroads: update data functions
#=================================================================#

ChangeVagtyp <- function(vagtyp){
  vagtyp <- if_else_na(vagtyp == 1, 3, 
                       if_else_na(vagtyp == 2, 5, 
                                  if_else_na(vagtyp == 3, 1, 
                                             if_else_na(vagtyp == 4, 2,
                                                        if_else_na(vagtyp == 5, 4,
                                                                   if_else_na(vagtyp == 6, 1, NA))))))
  return(vagtyp)
}

itShouldChangeValuesOfVariableVagtyp <- function(){
  vagtyp <- c(1, 2, 3, 4, 5, 6)
  res <- ChangeVagtyp(vagtyp)
  gold <- c(3, 5, 1, 2, 4, 1)
  stopifnot(gold == res)
}

itShouldChangeValuesOfVariableVagtyp()

AddRegion2023 <- function(län_nr){
  region <- ifelse(län_nr == 24 | län_nr == 25, "Nord", NA)
  region <- ifelse(län_nr == 20 | län_nr == 21 | län_nr == 22 | län_nr == 23, "Mitt", region)
  region <- ifelse(län_nr == 1, "Sthlm", region)
  region <- ifelse(län_nr == 9, "Gotland", region)
  region <- ifelse(län_nr == 3 | län_nr == 4 | län_nr == 5 | län_nr == 18 | län_nr == 19, "Ost", region)
  region <- ifelse(län_nr == 13 | län_nr == 14 | län_nr == 17, "Vast", region)
  region <- ifelse(län_nr == 6 | län_nr == 7 | län_nr == 8 | län_nr == 10 | län_nr == 12, "Syd", region)
  
  return(region)
}

itShouldAddRegion <- function(){
  län_nr <- c(3,4,1,25,13,7)
  res <- AddRegion2023(län_nr)
  gold <- c("Ost",   "Ost",   "Sthlm", "Nord",  "Vast",  "Syd")
  stopifnot(res == gold)
}

itShouldAddRegion()

ChangeRegion <- function(region){
  Region <- if_else(region == 1, "Mitt",
                    if_else(region == 2, "Nord",
                            if_else(region == 3, "Ost",
                                    if_else(region == 4, "Sthlm",
                                            if_else(region == 5, "Syd", "Vast")))))
  return(Region)
}

itShouldChangeValuesOfVariableRegion <- function(){
  region <- c(1, 1, 3, 2, 4, 6, 5)
  res <- ChangeRegion(region)
  gold <- c("Mitt", "Mitt", "Ost", "Nord", "Sthlm", "Vast", "Syd")
  stopifnot(gold == res)
}

itShouldChangeValuesOfVariableRegion()

UpdateMatdatum <- function(mätdatm, new_matdatum){
  matdat <- if_else_na(mätdatm != new_matdatum | (is.na(mätdatm) & !is.na(new_matdatum)), 
                       new_matdatum, mätdatm)
  
  return(matdat)
}

UpdateBelaggningsdatum <- function(blggnngsd, new_beldat){
  new_beldat <- as.Date(new_beldat)
  beldat <- if_else_na(blggnngsd != new_beldat | (is.na(blggnngsd) & !is.na(new_beldat)), 
                       new_beldat, blggnngsd)

  return(beldat)
}

itShouldUpdateBelaggningsdatum <- function(){
  dat <- data.frame(blggnngsd = as.Date(c(NA,"2018-05-30","2007-06-25","2017-06-25","2017-07-25")),
                    beldat_22 = c("2018-11-30","2020-08-25","2007-06-25","2007-06-25",NA))
  
  res <- dat %>% 
    dplyr::mutate(blggnngsd = UpdateBelaggningsdatum(blggnngsd, beldat_22))
  gold <- as.Date(c("2018-11-30","2020-08-25","2007-06-25","2007-06-25","2017-07-25"))
  
  stopifnot(res$blggnngsd == gold)
}

itShouldUpdateBelaggningsdatum()

UpdateSpårdjup <- function(oldspar, nyspar_17, nyspar_15, vagbr){
  spårdjp <- if_else_na(vagbr > 6 & !is.na(nyspar_17), nyspar_17, oldspar)
  spårdjp <- if_else_na(vagbr <= 6 & !is.na(nyspar_15), nyspar_15, spårdjp)
  return(spårdjp)
}

itShouldUpdateSpårdjup <- function(){
  dat <- data.frame(spårdjp = c(1,2,3),
                    vägbrdd = c(5.9,6.2,6),
                    sparm15_21 = c(2.2,2.5,NA), 
                    sparm17_21 = c(1.5,2.8,4))
  
  res <- dat %>% 
    dplyr::mutate(spårdjp = UpdateSpårdjup(spårdjp, sparm17_21, sparm15_21, vägbrdd))
  gold <- c(2.2,2.8,3)
  
  stopifnot(res$spårdjp == gold)
}

itShouldUpdateSpårdjup()

TrafficClass2023 <- function(dat){
  setDT(dat)
  # Add Traffic class variable
  dat[, trfkkls := ifelse(ådt_frd <250, 1, NA)]
  dat[, trfkkls := ifelse(ådt_frd >=250, 2, trfkkls)]
  dat[, trfkkls := ifelse(ådt_frd >499, 3, trfkkls)]
  dat[, trfkkls := ifelse(ådt_frd >999, 4, trfkkls)]
  dat[, trfkkls := ifelse(ådt_frd >1999, 5, trfkkls)]
  dat[, trfkkls := ifelse(ådt_frd >3999, 6, trfkkls)]
  dat[, trfkkls := ifelse(ådt_frd >7999, 7, trfkkls)]
  dat[, trfkkls := ifelse(ådt_frd >11999, 8, trfkkls)]
  
  return(dat)
}

ChangeBeltyp <- function(blggnngst){
  beltyp <- if_else_na(blggnngst == 1, "Varm", 
                       if_else_na(blggnngst == 2, "Försegling", 
                                  if_else_na(blggnngst == 3, "Halvvarm", 
                                             if_else_na(blggnngst == 4, "Indränkt makadam",
                                                        if_else_na(blggnngst == 5, "Tunnskikt",
                                                                   if_else_na(blggnngst == 6, "Varm stenrik",
                                                                              if_else_na(blggnngst == 7, "Ytbehandling på bituminöst underlag",
                                                                                         if_else_na(blggnngst == 8, "Ytbehandling på grus",
                                                                                                    if_else_na(blggnngst == 9, "Övrigt",NA)))))))))
  return(beltyp)
}

ChangeBeltypNumerisk <- function(blggnngst){
  beltyp <- if_else_na(blggnngst == "Varm", 1, 
                       if_else_na(blggnngst == "Försegling", 2, 
                                  if_else_na(blggnngst == "Halvvarm", 3, 
                                             if_else_na(blggnngst == "Indränkt makadam", 4, 
                                                        if_else_na(blggnngst == "Tunnskikt",5, 
                                                                   if_else_na(blggnngst == "Varm stenrik",6, 
                                                                              if_else_na(blggnngst == "Ytbehandling på bituminöst underlag",7, 
                                                                                         if_else_na(blggnngst == "Ytbehandling på grus",8, 
                                                                                                    if_else_na(blggnngst == "Övrigt",9, NA)))))))))
  return(beltyp)
}

ChangeTackningNumerisk <- function(tackning){
  tackning <- if_else_na(tackning == "Heltäckande", 1, 
                       if_else_na(tackning == "Fläckvis <20%", 2, 
                                  if_else_na(tackning == "Fläckvis >20%", 3, 
                                             if_else_na(tackning == "Spårlagning", 2, 
                                                        if_else_na(tackning == "Kanthäng",2, 
                                                                   if_else_na(tackning == "Fläckvis spårlagning",2, 
                                                                              if_else_na(tackning == "Fläckvis",2, NA)))))))
  return(tackning)
}

ConditionComparisonBetweenYears <- function(dat1,dat2,grp,grp_name,single=FALSE){
  if(single){
    pci_old <- QualitativeStatsSingleGroup(dat1, quo(indxkls), quo(längd))
    pci_old <- pci_old %>% 
      dplyr::mutate(PCIClass = factor(indxkls, levels = c("5","4","3","2","1"))) %>%
      dplyr::mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
      dplyr::mutate(grouplen_old = round(grouplen,0)) %>%
      dplyr::mutate(prop_old = prop*100) %>%
      dplyr::mutate(prop_old = round(prop_old,1)) %>%
      dplyr::select(-c(prop, grouplen,indxkls))
    
    pci_new <- QualitativeStatsSingleGroup(dat2, quo(PCIClass_23), quo(längd))
    pci_new <- pci_new %>% 
      dplyr::mutate(PCIClass = factor(PCIClass_23, levels = c("5","4","3","2","1"))) %>%
      dplyr::mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
      dplyr::mutate(grouplen_new = round(grouplen,0)) %>%
      dplyr::mutate(prop_new = prop*100) %>%
      dplyr::mutate(prop_new = round(prop_new,1)) %>%
      dplyr::select(-c(prop, grouplen,PCIClass_23))
    
    pci_old_vs_new <- dplyr::left_join(pci_old, pci_new, by=c("PCIClass"))
    pci_old_vs_new <- na.omit(pci_old_vs_new)
  } else {
    pci_old <- QualitativeStatsDoubleGroup(dat1, grp, quo(indxkls), quo(längd))
    pci_old <- pci_old %>% 
      dplyr::mutate(PCIClass = factor(indxkls, levels = c("5","4","3","2","1"))) %>%
      dplyr::mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
      dplyr::mutate(grouplen_old = round(grouplen,0)) %>%
      dplyr::mutate(prop_old = prop*100) %>%
      dplyr::mutate(prop_old = round(prop_old,1)) %>%
      dplyr::select(-c(prop, grouplen,indxkls))
    
    pci_new <- QualitativeStatsDoubleGroup(dat2, grp, quo(PCIClass_23), quo(längd))
    pci_new <- pci_new %>% 
      dplyr::mutate(PCIClass = factor(PCIClass_23, levels = c("5","4","3","2","1"))) %>%
      dplyr::mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
      dplyr::mutate(grouplen_new = round(grouplen,0)) %>%
      dplyr::mutate(prop_new = prop*100) %>%
      dplyr::mutate(prop_new = round(prop_new,1)) %>%
      dplyr::select(-c(prop, grouplen,PCIClass_23))
    
    pci_old_vs_new <- dplyr::left_join(pci_old, pci_new, by=c(grp_name, "PCIClass"))
    pci_old_vs_new <- na.omit(pci_old_vs_new)
  }

  return(pci_old_vs_new)
}

ComparisonBetweenYears <- function(dat1,dat2,grp,grp_name){
  var_2020 <- QualitativeStatsSingleGroup(dat1, grp, quo(längd))
  var_2020 <- var_2020 %>% 
    dplyr::mutate(grouplen20 = round(grouplen,0)) %>%
    dplyr::mutate(prop20 = prop*100) %>%
    dplyr::mutate(prop20 = round(prop20,1)) %>%
    dplyr::select(-c(prop, grouplen))
  
  var_2022 <- QualitativeStatsSingleGroup(dat2, grp, quo(längd))
  var_2022 <- var_2022 %>% 
    dplyr::mutate(grouplen22 = round(grouplen,0)) %>%
    dplyr::mutate(prop22 = prop*100) %>%
    dplyr::mutate(prop22 = round(prop22,1)) %>%
    dplyr::select(-c(prop, grouplen))
  
  var_2022_vs_2020 <- dplyr::left_join(var_2020, var_2022, by=c(grp_name))

  return(var_2022_vs_2020)
}
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

TrafficClass2022 <- function(dat){
  setDT(dat)
  # Add Traffic class variable
  dat[, trfkkls := ifelse(Ådt_frd <250, 1, NA)]
  dat[, trfkkls := ifelse(Ådt_frd >=250, 2, trfkkls)]
  dat[, trfkkls := ifelse(Ådt_frd >499, 3, trfkkls)]
  dat[, trfkkls := ifelse(Ådt_frd >999, 4, trfkkls)]
  dat[, trfkkls := ifelse(Ådt_frd >1999, 5, trfkkls)]
  dat[, trfkkls := ifelse(Ådt_frd >3999, 6, trfkkls)]
  dat[, trfkkls := ifelse(Ådt_frd >7999, 7, trfkkls)]
  dat[, trfkkls := ifelse(Ådt_frd >11999, 8, trfkkls)]
  
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

ConditionComparisonBetweenYears <- function(dat1,dat2,grp,grp_name,single=FALSE){
  if(single){
    pci_2020 <- QualitativeStatsSingleGroup(dat1, quo(indxkls), quo(längd))
    pci_2020 <- pci_2020 %>% 
      dplyr::mutate(PCIClass = factor(indxkls, levels = c("5","4","3","2","1"))) %>%
      dplyr::mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
      dplyr::mutate(grouplen20 = round(grouplen,0)) %>%
      dplyr::mutate(prop20 = prop*100) %>%
      dplyr::mutate(prop20 = round(prop20,1)) %>%
      dplyr::select(-c(prop, grouplen,indxkls))
    
    pci_2022 <- QualitativeStatsSingleGroup(dat2, quo(PCIClass_22), quo(längd))
    pci_2022 <- pci_2022 %>% 
      dplyr::mutate(PCIClass = factor(PCIClass_22, levels = c("5","4","3","2","1"))) %>%
      dplyr::mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
      dplyr::mutate(grouplen22 = round(grouplen,0)) %>%
      dplyr::mutate(prop22 = prop*100) %>%
      dplyr::mutate(prop22 = round(prop22,1)) %>%
      dplyr::select(-c(prop, grouplen,PCIClass_22))
    
    pci_2022_vs_2020 <- dplyr::left_join(pci_2020, pci_2022, by=c("PCIClass"))
    pci_2022_vs_2020 <- na.omit(pci_2022_vs_2020)
  } else {
    pci_2020 <- QualitativeStatsDoubleGroup(dat1, grp, quo(indxkls), quo(längd))
    pci_2020 <- pci_2020 %>% 
      dplyr::mutate(PCIClass = factor(indxkls, levels = c("5","4","3","2","1"))) %>%
      dplyr::mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
      dplyr::mutate(grouplen20 = round(grouplen,0)) %>%
      dplyr::mutate(prop20 = prop*100) %>%
      dplyr::mutate(prop20 = round(prop20,1)) %>%
      dplyr::select(-c(prop, grouplen,indxkls))
    
    pci_2022 <- QualitativeStatsDoubleGroup(dat2, grp, quo(PCIClass_22), quo(längd))
    pci_2022 <- pci_2022 %>% 
      dplyr::mutate(PCIClass = factor(PCIClass_22, levels = c("5","4","3","2","1"))) %>%
      dplyr::mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
      dplyr::mutate(grouplen22 = round(grouplen,0)) %>%
      dplyr::mutate(prop22 = prop*100) %>%
      dplyr::mutate(prop22 = round(prop22,1)) %>%
      dplyr::select(-c(prop, grouplen,PCIClass_22))
    
    pci_2022_vs_2020 <- dplyr::left_join(pci_2020, pci_2022, by=c(grp_name, "PCIClass"))
    pci_2022_vs_2020 <- na.omit(pci_2022_vs_2020)
  }

  return(pci_2022_vs_2020)
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
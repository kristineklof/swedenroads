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

ChangeBeltyp <- function(Blggnngst){
  beltyp_imp <- if_else_na(vagtyp == 1, 3, 
                       if_else_na(vagtyp == 2, 5, 
                                  if_else_na(vagtyp == 3, 1, 
                                             if_else_na(vagtyp == 4, 2,
                                                        if_else_na(vagtyp == 5, 4,
                                                                   if_else_na(vagtyp == 6, 1, NA))))))
  return(Blggnngst)
}


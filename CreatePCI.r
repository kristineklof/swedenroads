#=================================================================#
#                      This file calculates PCI
#=================================================================#

if_else_na <- function(a,b,c){
  a[is.na(a)] <- FALSE
  return(ifelse(a,b,c))
}

PCIClass <- function(dat){
    setDT(dat)

  dat[, PCIClass := if_else_na(PCI > 80, 5, NA)]
  dat[, PCIClass := if_else_na(PCI <=80, 4, PCIClass)]
  dat[, PCIClass := if_else_na(PCI <=60, 3, PCIClass)]
  dat[, PCIClass := if_else_na(PCI <=40, 2, PCIClass)]
  dat[, PCIClass := if_else_na(PCI <=20, 1, PCIClass)]

  return(dat)
}

CreatePCI <- function(dat, datum = "2019-01-01"){
    setDT(dat)
    dat <- CreateIndex(dat)
    dat <- PCI(dat, datum)

    return(setDT(dat))
}

Index <- function(actual, cutoff){
    # Curve should be 0.2 when x = 1
    x <- (cutoff - actual)/cutoff
    k <- -log(0.2)
    index <- round(100*(exp(-k*x)), digits=0)
    
    return(index)
}

itShouldCreateIndex <- function(){
    testdat <- data.frame(IRI_mnt = c(5,5,5,5),
                          IRI_l_p = c(NA,1,6,10),
                          IRI_r_p = c(NA,1,5,15),
                          PrdctSL = c(10,10,10,10),
                          RmnngSL = c(-10,-1,9,10),
                          rt_m17_ = c(NA,1,6,10),
                          rt_m15_ = c(3,1,5,15),
                          SP_mant = c(5,5,5,5),
                          RodWdth = c(6,6.5,7,11))

    res_iri <- Index(actual = (testdat$IRI_mnt-testdat$IRI_r_p), cutoff = (testdat$IRI_mnt-1))
    #print(res_iri)

    res_sp <- Index(actual = (testdat$SP_mant-testdat$rt_m17_), cutoff = testdat$SP_mant)
    #print(res_sp)

    res_rms <- Index(actual = testdat$RmnngSL, cutoff = testdat$PrdctSL)
    #print(res_rms)

    gold_iri <- c(NA, 100, 20, 0)
    gold_sp <- c(NA, 72, 14, 4)
    gold_rms <- c(4, 17, 85, 100)

    stopifnot(identical(res_iri, gold_iri))
    stopifnot(identical(res_sp, gold_sp))
    stopifnot(identical(res_rms, gold_rms))
}

itShouldCreateIndex()

CreateIndex <- function(dat){

     dat[, IRI_r_p_ceil := if_else_na(IRI_r_p < 1, ceiling(IRI_r_p), IRI_r_p)]
     dat[, IRI_Index := Index(actual = (IRI_mnt-IRI_r_p_ceil), cutoff = (IRI_mnt-1))]
     dat[, Rut_17_Index  := Index(actual = (SP_mant-rt_m17_), cutoff = SP_mant)]
     dat[, Rut_15_Index := Index(actual = (SP_mant-rt_m15_), cutoff = SP_mant)]
     dat[, RMS_Index := Index(actual = RmnngSL, cutoff = PrdctSL)]
     dat[, Rut_Index := ifelse(RodWdth <= 6, Rut_15_Index, Rut_17_Index)]

    return(dat)
}

PCI <- function(dat, datum){
    setDT(dat)

   dat[, Matning_min := pmin(IRI_Index, Rut_Index, na.rm=TRUE)]
   dat[, Matning_RMS_min := pmin(IRI_Index, Rut_Index, RMS_Index, na.rm=TRUE)]
   dat[, Matning_mean := rowMeans(.SD, na.rm = TRUE), .SDcols = c("IRI_Index", "Rut_Index")]

   dat[, PCI := ifelse(!is.na(Matning_mean), 
                            ifelse(tkl8 <= 4,
                                    0.25*Matning_mean + 0.75*RMS_Index, 
                                        0.75*Matning_mean + 0.25*RMS_Index),
                            RMS_Index)]          
   dat[, PCI := if_else_na(IRI_Index <= 20 | Rut_Index <= 20 | RMS_Index <= 20,
                            Matning_RMS_min, PCI)]   
   dat[, PCI := ifelse((RoadTyp %in% c("Motorway", "Undivided motorway", "4-lane road", "2+1 road")) & !is.na(Matning_min),
                        Matning_min, PCI)]     
   dat[, PCI :=  if_else_na(TrtmntD > MsrmntD | TrtmntD >= as.Date(datum), RMS_Index, PCI)] 
   dat[, PCI :=  if_else_na(is.na(PCI) & !is.na(Matning_mean), Matning_mean, PCI)]   

  dat[, PCI := round(PCI, digits=0)]
  dat[, c("Matning_min", "Matning_RMS_min", "Matning_mean") := NULL]

    return(setDT(dat))
}

itShouldCreatePCI <- function(){
    testdat <- data.frame(Rut_Index = c(89,100,30,15,NA,20,25,30),
                          IRI_Index= c(86,NA,44,5,NA,NA,40,30),
                          RMS_Index = c(20,90,15,35,20,40,30,NA),
                          tkl8 = c(2,4,5,8,2,6,8,4),
                          RoadTyp = c("Motorway", "2+1 road", "Ordinary road", "4-lane road", "Ordinary road", "Ordinary road", "Motorway","Ordinary road"),
                          TrtmntD = as.Date(c("2018-05-05","2018-05-05","2018-05-05","2018-05-05","2018-05-05","2019-05-05","2018-05-05","2018-05-05")),
                          MsrmntD = as.Date(c("2019-05-05","2017-05-05","2019-05-05","2019-05-05","2019-05-05","2019-05-05","2019-05-05","2017-05-05")))
    
    res <- PCI(testdat, datum="2019-01-01")
    gold <- c(86, 90, 15, 5, 20, 40, 25, 30)
    #print(res)
    stopifnot(gold == res$PCI)
}

itShouldCreatePCI()


# IRI_Index <- function(dat){
#     setDT(dat)
#     dat[, IRI_Index := ifelse(IRI_r_p >= IRI_mnt, 
#                                 rescale((1-IRI_r_p/IRI_mnt)*100, to=c(0,20)), 
#                                 rescale((1-IRI_r_p/IRI_mnt)*100, to=c(21,100)))]

#     dat[, IRI_Index := round(IRI_Index, digits=0)]

#     return(setDT(dat))
# }

# itShouldCreateIRIIndex <- function(){
#     testdat <- data.frame(IRI_mnt = c(5,5,5,5),
#                           IRI_l_p = c(NA,1,6,10),
#                           IRI_r_p = c(3,1,5,15),
#                           Objectd = c(1,2,3,4))

#     res <- IRI_Index(testdat)
#     print(res)
#     gold <- c(86, 100, 14, 0)
#     stopifnot(gold == res$IRI_Index)
# }

# itShouldCreateIRIIndex()

# Rut_Index <- function(dat){
#     setDT(dat)
#     dat[, Rut_Index := ifelse(RodWdth <= 6, ifelse(rt_m15_ >= SP_mant , 
#                                                     rescale((1-rt_m15_/SP_mant)*100, to=c(0,20)),
#                                                     rescale((1-rt_m15_/SP_mant)*100, to=c(21,100))),
#                                                         ifelse(rt_m17_ >= SP_mant , 
#                                                                 rescale((1-rt_m17_/SP_mant)*100, to=c(0,20)),
#                                                                 rescale((1-rt_m17_/SP_mant)*100, to=c(21,100))))]
#     dat[, Rut_Index := round(Rut_Index, digits=0)]

#     return(setDT(dat))
# }

# itShouldCreateRutIndex <- function(){
#     testdat <- data.frame(SP_mant = c(5,5,5,5),
#                           rt_m17_ = c(NA,1,6,10),
#                           rt_m15_ = c(3,1,5,15),
#                           RodWdth = c(6,6.5,7,11))

#     res <- Rut_Index(testdat)
#     print(res)
#     gold <- c(89, 100, 9, 0)
#     stopifnot(gold == res$Rut_Index)
# }

# itShouldCreateRutIndex()













#=================================================================#
#            Handeling of missing NVDB Data
#=================================================================#

AddCountyIfMissing <- function(dat, lankom){
  # Get lan and kommun
  names(lankom) <- c('Län','Länsnamn','Municipality','Kommunnamn')

  dat <- left_join(dat, lankom, by = "Municipality")
  dat$County <- as.integer(dat$County)
  dat <- dat %>% mutate(County = coalesce(County,Län)) %>% 
                            select(c(-Län))
  
  return(setDT(dat))
}

AddAADTIfMissing <- function(dat){
    # Impute missing AADT by municipality, roadtype, roadcategory
    # If AADT and AADT_heavy is missing, impute mean for the municipality
    dat <- dat %>% group_by(Municipality,RoadType,RoadCategory) %>% 
              mutate(AADT_imp = round(mean(AADT, na.rm = TRUE), digits=0)) %>%
              mutate(AADT_heavy_imp = round(mean(AADT_heavy, na.rm = TRUE), digits=0)) %>%
              mutate(AADT_axle_imp = round(mean(AADT_axle, na.rm = TRUE), digits=0)) %>%
              mutate(AADT = coalesce(AADT,AADT_imp)) %>%
              mutate(AADT_heavy = coalesce(AADT_heavy,AADT_heavy_imp)) %>%
              mutate(AADT_axle = coalesce(AADT_axle,AADT_axle_imp)) %>%
              select(-c(AADT_imp,AADT_heavy_imp,AADT_axle_imp)) %>%
              ungroup() %>%
              group_by(Municipality) %>% 
              mutate(AADT_imp = round(mean(AADT, na.rm = TRUE), digits=0)) %>%
              mutate(AADT_heavy_imp = round(mean(AADT_heavy, na.rm = TRUE), digits=0)) %>%
              mutate(AADT_axle_imp = round(mean(AADT_axle, na.rm = TRUE), digits=0)) %>%
              mutate(AADT = coalesce(AADT,AADT_imp)) %>%
              mutate(AADT_heavy = coalesce(AADT_heavy,AADT_heavy_imp)) %>%
              mutate(AADT_axle = coalesce(AADT_axle,AADT_axle_imp)) %>%
              select(-c(AADT_imp,AADT_heavy_imp,AADT_axle_imp)) %>%
              ungroup()

  #mcols = c("Municipality", "RoadType", "RoadCategory")
  #dat[is.na(AADT), AADT := dat[.BY, mean(AADT, na.rm=TRUE), on=mcols], by=mcols]
  #dat[is.na(AADT_heavy), AADT_heavy := dat[.BY, mean(AADT_heavy, na.rm=TRUE), on=mcols], by=mcols]
  #dat[is.na(AADT_heavy), AADT_axle := dat[.BY, mean(AADT_axle, na.rm=TRUE), on=mcols], by=mcols]

  # If still missing, impute mean per municipality
  #dat[is.na(AADT), AADT := dat[.BY, mean(AADT, na.rm=TRUE), on="Municipality"], by="Municipality"]
  #dat[is.na(AADT_heavy), AADT_heavy := dat[.BY, mean(AADT_heavy, na.rm=TRUE), on="Municipality"], by="Municipality"]
 # dat[is.na(AADT_axle), AADT_axle := dat[.BY, mean(AADT_axle, na.rm=TRUE), on="Municipality"], by="Municipality"]

  # Round
  #dat[, AADT := round(AADT, digits = 0)]
  #dat[, AADT_heavy := round(AADT_heavy, digits = 0)]
  #dat[, AADT_axle := round(AADT_axle, digits = 0)]

    return(setDT(dat))
}

itShouldAddMeanAADTIfMissing <- function(){
    testdat <- data.frame(AADT = c(10,20,NA,30,40,NA),
                          AADT_heavy = c(1,2,NA,3,4,NA),
                          AADT_axle = c(5,5,NA,8,5,NA),
                          Municipality = c(1,1,1,2,2,2),
                          RoadType = c("Ordinary road","Ordinary road","Ordinary road","Motorway","2+1 road", "2+1 road"),
                          RoadCategory = c(1,1,1,1,1,1))
    setDT(testdat)
    
    res <- AddAADTIfMissing(testdat)

    gold <- c(10,20,15,30,40,40)
    gold2 <- c(1,2,2,3,4,4)
    gold3 <- c(5,5,5,8,5,5)

    stopifnot(res$AADT == gold)
    stopifnot(res$AADT_heavy == gold2)
    stopifnot(res$AADT_axle == gold3)
}
itShouldAddMeanAADTIfMissing()

AddRoadWidthIfMissing <- function(dat){
  # If AADT and AADT_heavy is missing, impute mean for the municipality & roadtype & roadcategory
  mcols = c("Municipality", "RoadType", "RoadCategory")
  dat[is.na(RoadWidth), RoadWidth := dat[.BY, mean(RoadWidth, na.rm=TRUE), on=mcols], by=mcols]

  # If still missing, impute mean per municipality
  dat[is.na(RoadWidth), RoadWidth := dat[.BY, mean(RoadWidth, na.rm=TRUE), on="Municipality"], by="Municipality"]
  
  # Round
  dat[, RoadWidth := round(RoadWidth, digits = 1)]

    return(setDT(dat))
}

AddSpeedLimitIfMissing <- function(dat){
  # If AADT and AADT_heavy is missing, impute mean for the municipality & roadtype & roadcategory
  mcols = c("Municipality", "RoadType", "RoadCategory")
  dat[is.na(SpeedLimit), SpeedLimit := dat[.BY, mean(SpeedLimit, na.rm=TRUE), on=mcols], by=mcols]

  # If still missing, impute mean per municipality
  dat[is.na(SpeedLimit), SpeedLimit := dat[.BY, mean(SpeedLimit, na.rm=TRUE), on="Municipality"], by="Municipality"]

  # Round
  dat[, SpeedLimit := RoundUpTo10(SpeedLimit)]
  
  return(setDT(dat))
}

AddPavementTypeIfMissing <- function(dat){
  # If PavementType is missing, add most common type based on tkl8, RoadCategory, RoadType
  pave <- dat %>% group_by(tkl8,RoadCategory,RoadType) %>% 
                count(PavementType) %>% 
                drop_na() %>% 
                top_n(1, n) %>%
                select(-n)
  names(pave) <- c("tkl8", "RoadCategory", "RoadType", "PavementType_imp")

  dat <- left_join(dat, pave, by = c("tkl8", "RoadCategory", "RoadType"))
  dat <- dat %>% mutate(PavementType = coalesce(PavementType,PavementType_imp)) %>%
                select(-PavementType_imp)

 # If still missing, add "Varm"
 setDT(dat)
 dat[, PavementType := as.character(PavementType)]
 dat[, PavementType := ifelse(is.na(PavementType), "Varm", PavementType)]
 dat[, PavementType := as.factor(PavementType)]
 dat[, PavementType := relevel(PavementType, "Varm")]

  return(setDT(dat))
}

AddDOU2017IfMissing <- function(dat){
  # If PavementType is missing, add most common type based on tkl8, RoadCategory, RoadType
  dou <- dat %>% group_by(County,tkl8,RoadCategory,RoadType) %>% 
                count(DoU2017) %>% 
                drop_na() %>% 
                top_n(1, n) %>%
                select(-n)
  names(dou) <- c("County","tkl8", "RoadCategory", "RoadType", "DoU2017_imp")

  dat <- left_join(dat, dou, by = c("County","tkl8", "RoadCategory", "RoadType"))
  dat <- dat %>% mutate(DoU2017 = coalesce(DoU2017,DoU2017_imp)) %>%
                select(-DoU2017_imp)

 # If still missing, add based on tkl8 only
 setDT(dat)
 dou2 <- dat %>% group_by(tkl8) %>% 
                count(DoU2017) %>% 
                drop_na() %>% 
                top_n(1, n) %>%
                select(-n)
 names(dou2) <- c("tkl8", "DoU2017_imp")

  dat <- left_join(dat, dou2, by = c("tkl8"))
  dat <- dat %>% mutate(DoU2017 = dplyr::coalesce(DoU2017,DoU2017_imp)) %>%
                select(-DoU2017_imp)

  return(setDT(dat))
}




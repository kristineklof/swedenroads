#=================================================================#
#            Create Age variable for NVDB data
#=================================================================#

CalculateAge <- function(TreatmentDate){
    TreatmentDate <- as.character(TreatmentDate)
    Age <- 2020-as.numeric(substring(TreatmentDate, 1, 4))
    return(Age)
}

itShouldCalculateAge <- function(){
    TreatmentDate <- c(19850912, 19881110, 20190506)
    res <- CalculateAge(TreatmentDate)
    gold <- c(35,32,1)
    stopifnot(res == gold)
}
itShouldCalculateAge()

AddIfAgeMissing <- function(dat){
    # Impute missing age
    dat <- dat %>% group_by(Municipality,tkl8,RoadCategory,RoadType) %>% 
              mutate(Age_imp = round(mean(Age, na.rm = TRUE), digits=0)) %>%
              mutate(Age = coalesce(Age,Age_imp)) %>%
              select(-Age_imp) %>%
              ungroup()
    
    # If still missing, set to median age
    setDT(dat)
    dat[, Age := ifelse(is.na(Age), 9, Age)]

    return(setDT(dat))
}
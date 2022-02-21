#=================================================================#
#            Create Age variable for NVDB data
#=================================================================#

IsDate <- function(x) inherits(x, "Date")

CalculateAge <- function(TreatmentDate, DateToday="2020-01-01"){
    #print(IsDate(TreatmentDate))
    if(!IsDate(TreatmentDate)){
        TreatmentDate <- as.Date(as.character(TreatmentDate), format="%Y%m%d")
    }

    #TreatmentDate <- as.Date(TreatmentDate, origin = "1970-01-01")
    Age <- year(as.Date(DateToday))-year(TreatmentDate)
    return(as.integer(Age))
}

itShouldCalculateAge <- function(){
    TreatmentDate <- c(19850912, 19881110, 20190506, 20151201)
    TreatmentDateD <- as.Date(as.character(TreatmentDate), format="%Y%m%d")
    res <- CalculateAge(TreatmentDateD)
    #print(res)
    gold <- c(35,32,1, 5)
    stopifnot(res == gold)
    stopifnot(is.integer(res) == TRUE)
}
itShouldCalculateAge()

AddIfAgeMissing <- function(dat){
    # Impute missing age
    dat <- dat %>% group_by(Municipality,tkl8,RoadCategory,RoadType) %>% 
              mutate(Age_imp = as.integer(round(mean(Age, na.rm = TRUE), digits=0))) %>%
              mutate(Age = coalesce(Age,Age_imp)) %>%
              select(-Age_imp) %>%
              ungroup()
    
    # If still missing, set to median age
    setDT(dat)
    dat[, Age := ifelse(is.na(Age), 9, Age)]

    return(setDT(dat))
}
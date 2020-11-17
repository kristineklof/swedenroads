#=================================================================#
#        Combine NVDB beltyp and PMS/survival data beltyp
#=================================================================#

CombineBeltyp <- function(dat){
    setDT(dat)

    dat[, beldatum := as.Date(as.character(beldatum), format = "%Y%m%d")]
    dat[, beldatum := as.Date(beldatum, origin = "1970-01-01")]
    dat[, beldatum := ifelse(!is.na(atgd2_f) & !is.na(survbeltyp) & is.na(beltyp) & is.na(beldatum), atgd2_f, beldatum)]
    dat[, beltyp := ifelse(!is.na(survbeltyp) & is.na(beltyp), survbeltyp, beltyp)]
    dat[, beldatum := as.Date(beldatum, origin = "1970-01-01")]

    return(dat)
}

itShouldChangeBeltypAndBeldatumIfSurvbeltypExists <- function(){
    testdat <- data.frame(beltyp = c(NA, "ABS", "ABT",NA),
                         beldatum = c(NA, 20100101,20150101,20180101),
                         survbeltyp = c("Y1G","ABS","ABT",NA),
                         atgd2_f = as.Date(c("2011-01-01",NA,"2016-01-01","2015-01-01")),
                         atgdt_f = as.Date(c(NA,NA,NA,NA)))

    res <- CombineBeltyp(testdat)  
    #print(res)
    #gold <- as.Date(c("2011-01-01","2015-01-01","2015-01-01"))
    gold <- as.Date(c("2011-01-01","2010-01-01","2015-01-01","2018-01-01"))
    stopifnot(res$beldatum == gold)  
}

itShouldChangeBeltypAndBeldatumIfSurvbeltypExists()

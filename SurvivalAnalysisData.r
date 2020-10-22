#=================================================================#
#                      This file performs the
#                Survival Analysis dataset from PMS-data
#=================================================================#

# Old dataset, up until 2011:
CreateSurvivalData2011 <- function(dat){
  # Keep variables needed
  keepvars <- c("LSTRKAID", "atgdatA", "atgdatB", "atgdatN", "LAN", "trafik", "trafikny",
                "tungtr","tungtrny","tkl8","LGD","yta","year","SP_MEAN","IR_MEAN",
                "region","tkl4grp","VAGKAT","VAGTYP","DRIFTOMR","VAGBR","BARIGHET",
                "HAST","VAGNR","MAXSTEN","pave8_a2","year_n","year_a","year_b",
                "alder","alder_US","event","d","OID","RELSTART","RELSLUT")
  dat <- dat[, keepvars, with=FALSE]

  # Add variable Roadtype
  vagtyper <- data.frame(VAGTYP = c(1,2,3,4,5,6),
            Roadtype = c("Motorway", "Undivided motorway", "2+1 road", "4-lane road", "Ordinary road", "2+1 road"))
  dat <- left_join(dat,vagtyper,by=c("VAGTYP"))   
  setDT(dat)
  dat[, Roadtype := as.factor(Roadtype)]
  dat[, Roadtype := relevel(Roadtype, "Ordinary road")]
       

  # Correct traffic and traffic class variable (if vagtyp21 = NA, trafikny=trafik)
  dat[, trafikny := ifelse(is.na(Roadtype),trafik, trafikny)]
  dat[, tungtrny := ifelse(is.na(Roadtype), tungtr, tungtrny)]
  
  # Traffic class variable
  dat[, tkl8 := ifelse(trafikny <250, 1, tkl8)]
  dat[, tkl8 := ifelse(trafikny >=250, 2, tkl8)]
  dat[, tkl8 := ifelse(trafikny >499, 3, tkl8)]
  dat[, tkl8 := ifelse(trafikny >999, 4, tkl8)]
  dat[, tkl8 := ifelse(trafikny >1999, 5, tkl8)]
  dat[, tkl8 := ifelse(trafikny >3999, 6, tkl8)]
  dat[, tkl8 := ifelse(trafikny >7999, 7, tkl8)]
  dat[, tkl8 := ifelse(trafikny >11999, 8, tkl8)]
  
  # Round traffic variables
  dat[, AADT := round(trafikny, digits=0)]
  dat[, AADT_heavy := round(tungtrny, digits=0)]
  
  # Road category
  dat[, RoadCategory := ifelse(VAGKAT == 1, "European", NA)]
  dat[, RoadCategory := ifelse(VAGKAT == 2, "National", RoadCategory)]
  dat[, RoadCategory := ifelse(VAGKAT == 3, "Primary", RoadCategory)]
  dat[, RoadCategory := ifelse(VAGKAT == 4, "Secondary", RoadCategory)]
  dat[, RoadCategory := ifelse(VAGKAT == 5, "Tertiary", RoadCategory)]
  dat[, RoadCategory := ifelse(VAGKAT == 6, "SoT", RoadCategory)]
  
  # Create climate zone variable
  dat[, CZON := ifelse(LAN == 17 | LAN >=20, "Central", "South")]
  dat[, CZON := ifelse(LAN >=23, "North", CZON)]
  
  # Create region variable
  dat[, Region := ifelse(LAN == 24 | LAN == 25, "Nord", NA)]
  dat[, Region := ifelse(LAN == 20 | LAN == 21 | LAN == 22 | LAN == 23, "Mitt", Region)]
  dat[, Region := ifelse(LAN == 2 | LAN == 9, "Sthlm", Region)]
  dat[, Region := ifelse(LAN == 3 | LAN == 4 | LAN == 5 | LAN == 18 | LAN == 19, "Ost", Region)]
  dat[, Region := ifelse(LAN == 13 | LAN == 14 | LAN == 15 | LAN == 16 | LAN == 17, "Vast", Region)]
  dat[, Region := ifelse(LAN == 6 | LAN == 7 | LAN == 8 | LAN == 10 | LAN == 11 | LAN == 12, "Syd", Region)]
  
  # Create ID variable
  dat[, ID := LAN*(10^ceiling(log10(max(LSTRKAID))+1)) + LSTRKAID]
  
  # Create variable categorical maxsten
  dat[, StoneSize := ifelse(MAXSTEN >=10, "medium", "small")]
  dat[, StoneSize := ifelse(MAXSTEN >=15, "large", StoneSize)]
  dat[, StoneSize := ifelse(MAXSTEN >=20, "xlarge", StoneSize)]
  
  # Replace HAST = 5 with 30
  dat[, SpeedLimit := ifelse(HAST == 5, 30, HAST)]
  
  # Create PavementType
  dat[, PavementType := ifelse(pave8_a2 == "ABS", "Stone mastic",NA)]
  dat[, PavementType := ifelse(pave8_a2 == "ABT", "Asphalt concrete",PavementType)]
  dat[, PavementType := ifelse(pave8_a2 == "Ytbeha", "Surface dressing",PavementType)]
  dat[, PavementType := ifelse(pave8_a2 == "SDG", "Surface dressing on gravel",PavementType)]
  dat[, PavementType := ifelse(pave8_a2 == "Varm m", "Hot mix",PavementType)]
  dat[, PavementType := ifelse(pave8_a2 == "Halvva", "Half-warm mix",PavementType)]
  dat[, PavementType := ifelse(pave8_a2 == "GM", "Grouted macadam",PavementType)]
  dat[, PavementType := ifelse(pave8_a2 == "Förseg", "Seal coat",PavementType)]
  dat[, PavementType := ifelse(pave8_a2 == "Kall m", "Cold mix",PavementType)]
  
  # Replace values over 4000 with year 2012
  dat[, year_n := ifelse(year_n > 2200, 2012, year_n)]
  
  # Create age variable for lognormal survival analysis
  dat[, age_non0 := ifelse(alder_US == 0, 1, alder_US)]

  # Create LAN_nr
  dat[, LAN_nr := LAN]

  # Create BARIGHET as factor
  dat[, BARIGHET:= as.factor(BARIGHET)]

  # Remove sections shorter than 50 m
  dat <- dat[LGD >= 50]

  # Select variables
  survvars <- c("ID", "LAN_nr", "AADT", "AADT_heavy", "LGD",
                "tkl8","PavementType","SP_MEAN","IR_MEAN",
                "Region","StoneSize","RoadCategory","VAGBR","BARIGHET",
                "SpeedLimit","Roadtype","age_non0","d","CZON")
  dat <- dat[, survvars, with=FALSE]

  return(dat)
}

MaintenanceStandard <- function(dat){
  # Add maintenance standard values
  sl <- c(120,110,100,90,80,70,60,50)
  maint_stand <- data.frame(tkl8 = c(rep(1,7),rep(2,7),rep(3,7),rep(4,7),rep(5,8),rep(6,8),rep(7,8),rep(8,8)),
                            SpeedLimit = c(rep(sl[-1],4),rep(sl,4)),
                            IRI_maint = c(4.3, 4.7, 5.2, 5.9, 6.7, 6.7, 6.7,
                                          4.0, 4.4, 4.9, 5.5, 6.3, 6.3, 6.3,
                                          3.7, 4.1, 4.5, 5.1, 5.8, 5.8, 5.8,
                                          3.0, 3.3, 3.7, 4.2, 4.8, 5.2, 5.2,
                                          2.4, 2.6, 2.9, 3.2, 3.6, 4.1, 4.9, 4.9,
                                          2.4, 2.6, 2.9, 3.2, 3.6, 4.1, 4.9, 4.9,
                                          2.4, 2.6, 2.9, 3.2, 3.6, 4.1, 4.9, 4.9,
                                          2.4, 2.6, 2.9, 3.2, 3.6, 4.1, 4.9, 4.9),
                            SP_maint = c(18.0, 18.0, 24.0, 24.0, 30.0, 30.0, 30.0,
                                         18.0, 18.0, 22.0, 22.0, 27.0, 27.0, 27.0,
                                         18.0, 18.0, 20.0, 20.0, 24.0, 24.0, 24.0,
                                         15.0, 16.0, 17.0, 18.0, 20.0, 21.0, 21.0,
                                         13.0, 13.0, 14.0, 14.0, 16.0, 16.0, 18.0, 18.0,
                                         13.0, 13.0, 14.0, 14.0, 16.0, 16.0, 18.0, 18.0,
                                         13.0, 13.0, 14.0, 14.0, 16.0, 16.0, 18.0, 18.0,
                                         13.0, 13.0, 14.0, 14.0, 16.0, 16.0, 18.0, 18.0))

    dat <- left_join(dat, maint_stand, by=c("tkl8","SpeedLimit"))

    return(dat)
}

CreateSurvivalData2019 <- function(dat){
  # Create variable LSTRKAID
  dat[, LSTRKAID := 1:nrow(dat)]

  lannummer <- data.frame(LAN = c("AB","C","D","E","F","G","H","I","K","L","M","N","O","P","R","S","T","U","W","X","Y","Z","AC","BD"),
                          LAN_nr = 2:25)

  # Create LAN_nr
  dat <- left_join(dat, lannummer, by=c("LAN"))
  setDT(dat)

  # Create ID variable
  dat[, ID := LAN_nr*(10^ceiling(log10(max(LSTRKAID))+1)) + LSTRKAID]

  # Create VAGBR
  dat[, VAGBR := VagBredd*10]

  # Create CZON
  dat[, CZON := ifelse(LAN_nr == 17 | LAN_nr >=20, "Central", "South")]
  dat[, CZON := ifelse(LAN_nr >=23, "North", CZON)]

  # Create variable categorical maxsten
  dat[, StoneSize := ifelse(MaxStenStorlek >=10, "medium", "small")]
  dat[, StoneSize := ifelse(MaxStenStorlek >=15, "large", StoneSize)]
  dat[, StoneSize := ifelse(MaxStenStorlek >=20, "xlarge", StoneSize)]

  # Create Roadcategory
  dat[, RoadCategory := ifelse(Vagkategori1 == "Europaväg", "European", NA)]
  dat[, RoadCategory := ifelse(Vagkategori1 == "Riksväg", "National", RoadCategory)]
  dat[, RoadCategory := ifelse(Vagkategori1 == "Primär länsväg", "Primary", RoadCategory)]
  dat[, RoadCategory := ifelse(Vagkategori1 == "Övrig länsvägar", "Secondary", RoadCategory)]
  dat[, RoadCategory := ifelse(Vagkategori1 == "Okänt", "Unknown", RoadCategory)]

  # Create variable Roadtype
  vagtyper <- data.frame(vagtyp2 = c("1. Motorväg", "2. Motortrafikled",  "3. Motortrafikled mötesfri", "4. 4-fältsväg", "5. Vanlig väg", "6. Vanlig väg mötesfri"),
                          Roadtype = c("Motorway", "Undivided motorway", "2+1 road", "4-lane road", "Ordinary road", "2+1 road"))
  dat <- left_join(dat,vagtyper,by=c("vagtyp2"))   
  setDT(dat)  
  dat[, Roadtype := as.factor(Roadtype)]
  dat[, Roadtype := relevel(Roadtype, "Ordinary road")]

  # Create AADT and AADT_heavy
  dat[, AADT := ifelse(Roadtype == "Motorway" | Roadtype == "4-lane road", round(ADT*2, digits=0), round(ADT, digits=0))]
  dat[, AADT := ifelse(Roadtype == "2+1 road", round(ADT*1.5, digits=0), round(ADT, digits=0))]

  dat[, AADT_heavy := ifelse(Roadtype == "Motorway" | Roadtype == "4-lane road", round(ADT_tung*2, digits=0), round(ADT_tung, digits=0))]
  dat[, AADT_heavy := ifelse(Roadtype == "2+1 road", round(ADT_tung*1.5, digits=0), round(ADT_tung, digits=0))]

  # Traffic class variable
  dat[, tkl8 := ifelse(AADT <250, 1, NA)]
  dat[, tkl8 := ifelse(AADT >=250, 2, tkl8)]
  dat[, tkl8 := ifelse(AADT >499, 3, tkl8)]
  dat[, tkl8 := ifelse(AADT >999, 4, tkl8)]
  dat[, tkl8 := ifelse(AADT >1999, 5, tkl8)]
  dat[, tkl8 := ifelse(AADT >3999, 6, tkl8)]
  dat[, tkl8 := ifelse(AADT >7999, 7, tkl8)]
  dat[, tkl8 := ifelse(AADT >11999, 8, tkl8)]

  # Create BARIGHET
  dat[, BARIGHET := as.factor(BK)]
  dat[, BARIGHET := relevel(BARIGHET, "1")]

  # Create SpeedLimit
  dat[, SpeedLimit := HAST]

  # Create Region 
  labs <- c("Ost")
  dat[, Region := factor(Region, labels = labs)]

  # Create IRI mean and Spar mean OBS BEHÖVER NY DATA
  dat[, IR_MEAN := f_irih_100]
  dat[, SP_MEAN := f_spa_100]

    # Create PavementType
    dat[, PavementType := substring(Atgard02,5)]
    dat[, PavementType := ifelse(Atgard02 == "iktiv", NA, PavementType)]
    dat[, PavementType := as.factor(PavementType)]
    levels(dat$PavementType)[levels(dat$PavementType) == 'iktiv'] <- NA
    print(levels(dat$PavementType))
    dat[, PavementType := relevel(PavementType, "Varm")]
  #dat[, PavementType := ifelse(Atgard2 == "02. Varm stenrik", "Stone mastic", NA)]
  #dat[, PavementType := ifelse(Atgard2 == "02. Varm stenrik + P", "Stone mastic", PavementType)]
  #dat[, PavementType := ifelse(Atgard2 == "01. Varm", "Asphalt concrete", PavementType)]
  #dat[, PavementType := ifelse(Atgard2 == "01. Varm + P", "Asphalt concrete", PavementType)]
  #dat[, PavementType := ifelse(Atgard2 == "05. Ytbehandling på bituminöst underlag", "Surface dressing", PavementType)]
  #dat[, PavementType := ifelse(Atgard2 == "06. Ytbehandling på grus", "Surface dressing on gravel", PavementType)]
  #dat[, PavementType := ifelse(Atgard2 == "06. Ytbehandling på grus + P", "Surface dressing on gravel", PavementType)]
  #dat[, PavementType := ifelse(Atgard2 == "04. Halvvarm", "Half-warm mix", PavementType)]
  #dat[, PavementType := ifelse(Atgard2 == "04. Halvvarm + P", "Half-warm mix", PavementType)]
  #dat[, PavementType := ifelse(Atgard2 == "08. Övrigt", "Grouted macadam", PavementType)]
  #dat[, PavementType := ifelse(Atgard2 == "08. Övrigt + P", "Grouted macadam", PavementType)]
  #dat[, PavementType := ifelse(Atgard2 == "07. Försegling", "Seal coat", PavementType)]
  #dat[, PavementType := ifelse(Atgard2 == "07. Försegling + P", "Seal coat", PavementType)]

  # Select variables
  survvars <- c("ID", "LAN_nr", "AADT", "AADT_heavy", "hom_id2", "langd",
                "tkl8","PavementType","SP_MEAN","IR_MEAN",
                "Region","StoneSize","RoadCategory","VAGBR","BARIGHET",
                "SpeedLimit","Roadtype","CZON","atgdat2e_Fikeff", "atgdatne_Fikeff")
  dat <- dat[, survvars, with=FALSE]

  return(dat)
}

CreateHomoData <- function(dat){
  # Group by hom_id2
  dat[, hom_id2 := as.character(hom_id2)]

  charcols <- c("hom_id2","LAN_nr","tkl8","PavementType","Region","StoneSize",
                "RoadCategory","BARIGHET","Roadtype","CZON","atgdat2e_Fikeff", "atgdatne_Fikeff")
  lencols <- c(charcols,"langd")

  # Sum section lengths
  lang_dat <- dat[, ..lencols]
  lang_dat <- lang_dat[, sum(langd)*1000, by=charcols]
  setnames(lang_dat, "V1", "langd")

  print(head(lang_dat))

  # Calculate mean values
  cols <- c("AADT","AADT_heavy","VAGBR","SpeedLimit","SP_MEAN","IR_MEAN")
  dat_means <- dat[, lapply(.SD, mean), by=charcols, .SDcols = cols]
  dat_means <- left_join(dat_means,lang_dat, by=charcols)
  setDT(dat_means)

  dat_means[, VAGBR := round(VAGBR, digits = 0)]
  print(head(dat_means))
  print(nrow(dat_means))

  # Extract first values of every homo_id2
 #charcols <- c("ID", "hom_id2","LAN_nr","tkl8","PavementType","Region","StoneSize",
  #              "RoadCategory","BARIGHET","Roadtype","CZON","atgdat2e_Fikeff", "atgdatne_Fikeff")
  #dat_char <- dat[, ..charcols]
  #setkey(dat_char, "hom_id2", "atgdat2e_Fikeff", "atgdatne_Fikeff")
  #dat_char <- unique(dat_char,by=c("hom_id2", "atgdat2e_Fikeff", "atgdatne_Fikeff"))
  #print(head(dat_char))
  #print(nrow(dat_char))

  #dat <- left_join(dat_means,dat_char, by="hom_id2")

  return(dat_means)
}

CreateAgeEvent <- function(survdat){
  date_cols <- c("atgdat2e_Fikeff", "atgdatne_Fikeff")
  setDT(survdat)[, (date_cols) := lapply(.SD, fasttime::fastPOSIXct), .SDcols = date_cols]

  print(survdat[order(atgdat2e_Fikeff), head(.SD, 1L)])
 
  # Create age variable
  survdat[, age := round(as.numeric(difftime(atgdatne_Fikeff, atgdat2e_Fikeff, unit="weeks")/52.25), digits = 0)]
  survdat[, age_non0 := ifelse(is.na(age),
                              round(as.numeric(difftime(as.Date("2019-12-31"), atgdat2e_Fikeff, unit="weeks")/52.25), digits = 0),
                              age)]
  survdat[, age_non0 := ifelse(age_non0 == 0, 1, age_non0)]

  # Create event variable "d" if life is ended or maintenance standard surpassed
  survdat[, d := ifelse(is.na(age), 0, 1)]
  survdat[, d := ifelse(SP_MEAN > SP_maint | IR_MEAN > IRI_maint,
                        1, d)]
  
  return(survdat)
}

itShouldCreateAgeAndEvent <- function(){
  testdat <- data.frame(atgdat2e_Fikeff = c("2019-11-18","2010-08-17","2000-08-17"),
                        atgdatne_Fikeff = c("2233-10-15","2019-11-18","2233-10-15"),
                        SP_MEAN = c(17,15,30),
                        IR_MEAN = c(2,3,6),
                        IRI_maint = c(3,4,6),
                        SP_maint = c(20,24,24))

  res <- CreateAgeEvent(survdat = testdat)

  gold_d <- c(0,1,1)
  gold_age <- c(1,9,19)

  stopifnot(gold_d == res$d)
  stopifnot(gold_age == res$age_non0)
  print("OK")
}

itShouldCreateAgeAndEvent()
#=================================================================#
#                      This file creates a union
#                         PostgreSQL query 
#=================================================================#

lan <- c("dalarna","gavleborg","gotland","halland",
        "jamtland","jonkoping","kalmar","kronoberg","norrbotten",
        "orebro","ostergotland","skane","sodermanland","stockholm",
        "uppsala","varmland","vasterbotten","vasternorrland","vastmanland",
        "vastra-gotaland")
length(lan)

query_bel <- paste0("CREATE TABLE sweden_belaggning_2020 AS SELECT * FROM \"blekinge-belaggning-2020\" ", paste0("UNION SELECT * FROM \"", lan, "-belaggning-2020\"", collapse = " "))
cat(query_bel)
query_mat <- paste0("CREATE TABLE sweden_matdata_2020 AS SELECT * FROM \"blekinge-matdata-2020\" ", paste0("UNION SELECT * FROM \"", lan, "-matdata-2020\"", collapse = " "))
cat(query_mat)

#=================================================================#
#            Test of opening lastkajen PMSv3 data
#=================================================================#

norrbotten <- st_read("C:/Users/winte/Swedenroads_Lastkajen_PMS_data/Norrbotten-2020/Norrbotten-Beläggning-2020.shp", options = "ENCODING=WINDOWS-1252")
head(norrbotten)
nrow(norrbotten)
str(norrbotten)

norrbotten_mat <- st_read("C:/Users/winte/Swedenroads_Lastkajen_PMS_data/Norrbotten-2020/Norrbotten-Mätdata-2020.shp", options = "ENCODING=WINDOWS-1252")
head(norrbotten_mat)
nrow(norrbotten_mat)
str(norrbotten_mat)


##########################################################
# Output till Regionanalys
datapath <- "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/"
sweden_2024_dot <- st_read(paste0(datapath,"2024/DOT/sweden_2024_dot_20240202.shp"))
dot_geo <- sweden_2024_dot[,c("Objectd")]
dot_geo <- dot_geo %>% mutate(Objectd = as.character(Objectd))

# Skåne
skane_2024_dot <- sweden_2024_dot %>% filter(CountN == "Skåne län")
skane_2024 <- st_drop_geometry(skane_2024_dot)
skane_2024$geometry <- NULL

# Norrbotten
norrbotten_2024_dot <- sweden_2024_dot %>% filter(CountN == "Norrbottens län")
norrbotten_2024 <- norrbotten_2024_dot %>% st_drop_geometry()
norrbotten_2024$geometry <- NULL

# Sthlm
sthlm_2024_dot <- sweden_2024_dot %>% filter(CountN == "Stockholms län")
sthlm_2024 <- sthlm_2024_dot %>% st_drop_geometry()
sthlm_2024$geometry <- NULL

# Import Excel template
template <- read.xlsx("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Svenskt Näringsliv/DOT/Roads_Inventory_May_14_2024_14_04_24.xlsx", sheet = 1)
names(template)

names(sthlm_2024_dot)
new.names <- c("Asset.ID","Bearing.Capacity","Speed.Limit","DoU2017",
               "AADT","AADT.Trucks", "Traffic.Count.Year","Width",
               "Road.Category","Road.Type","Count","Length","CountN",
               "MncpN","Traffic.Class.(tkl8)","Region","Pavement.Type",
               "Treatment.Year","Coverage","Age","PrdctSL","Remaining.Service.Life",
               "IRI_maint","SP_maint","Rut.(mm)","IRI","PCI.Assessment.Year",
               "PCI", "PCI.Class", "Surface.Type", "County", "Municipality")

# Drop Age, CountN, MncpN, PrdctSL  

TemplateData <- function(new.names, template, df){
  names(df) <- new.names
  df <- as.data.frame(df)
  df <- df %>% select(-PrdctSL, -MncpN, -CountN)
  
  old.names <- names(template)
  
  rows_to_add <- nrow(df) - nrow(template)
  na_rows <- as.data.frame(matrix(NA, nrow = rows_to_add, ncol = ncol(template)))
  names(na_rows) <- names(template)
  template <- rbind(template, na_rows)
  
  template <- template[, !names(template) %in% new.names]
  template$Asset.ID <- df$Asset.ID
  
  # Join with df based on Asset.ID
  template <- dplyr::left_join(template,df,by="Asset.ID")
  
  template <- template %>% dplyr::select(all_of(old.names))
  
  return(template)
}

itShouldPopulateTemplate <- function(){
  # Example usage
  new.names1 <- c("Asset.ID", "Age", "PrdctSL", "MncpN", "CountN", "value1", "value2")
  
  # Example template dataframe
  template1 <- data.frame(
    Asset.ID = NA,
    Blaj = NA,
    Age = NA,
    Mupp = NA,
    value1 = NA,
    value2 = NA
  )
  
  # Example df dataframe
  df1 <- data.frame(
    Asset.ID = c("Alice", "Bob", "Charlie", "David", "Eve"),
    Age = c(25, 30, 35, 40, 45),
    PrdctSL = c(1, 2, 3, 4, 5),
    MncpN = c("A", "B", "C", "D", "E"),
    CountN = c(10, 20, 30, 40, 50),
    value1 = as.Date(c("2024-01-01","2022-01-01","2023-01-01","2019-01-01","2015-01-01")),
    value2 = c(150, 250, 350, 450, 550)
  )
  
  # Apply the function
  result <- TemplateData(new.names1, template1, df1)
  print(result)
}

itShouldPopulateTemplate()

skane_2024 <- TemplateData(new.names, template, skane_2024)
sthlm_2024 <- TemplateData(new.names, template, sthlm_2024)
norrbotten_2024 <- TemplateData(new.names, template, norrbotten_2024)

skane_2024 <- skane_2024 %>% mutate(Asset.ID = as.character(Asset.ID),
                                    PCI.Assessment.Year = 2024,
                                    Treatment.Year = substr(as.character(Treatment.Year),1,4),
                                    Road.Category = factor(Road.Category),
                                    Road.Category = recode(Road.Category, 
                                                           "1" = "1 - European", 
                                                           "2" = "2 - National",
                                                           "3" = "3 - Primary County", 
                                                           "4" = "4 - Secondary County"),
                                    DoU2017 = factor(DoU2017),
                                    DoU2017 = recode(DoU2017, 
                                                     "1" = "1 = Roads in metropolitan areas", 
                                                     "2" = "2 = Important connected roads",
                                                     "3" = "3 = Roads for daily travels and commuting", 
                                                     "4" = "4 = Other roads of commercial importance",
                                                     "5" = "5 = Roads important for rural areas",
                                                     "6" = "6 = Low traffic roads"),
                                    `Traffic.Class.(tkl8)` = factor(`Traffic.Class.(tkl8)`),
                                    `Traffic.Class.(tkl8)` = recode(`Traffic.Class.(tkl8)`, 
                                                     "1" = "1 - AADT <250", 
                                                     "2" = "2 - AADT 250-499",
                                                     "3" = "3 - AADT 500-999", 
                                                     "4" = "4 - AADT 1000-1999",
                                                     "5" = "5 - AADT 2000-3999",
                                                     "6" = "6 - AADT 4000-7999",
                                                     "7" = "7 - AADT 8000-11999",
                                                     "8" = "8 - AADT >11999"),
                                    Region = "5 - Syd",
                                    Bearing.Capacity = NA,
                                    PCI.Class = NA,
                                    Coverage = NA,
                                    Pavement.Type = if_else(Pavement.Type == "Hot Mix Ashphalt",
                                                            "Hot Mix Asphalt",
                                                            Pavement.Type),
                                    Surface.Type = if_else(Surface.Type == "Hot Mix Ashphalt",
                                                            "Hot Mix Asphalt",
                                                           Surface.Type),
                                    Name = Asset.ID,
                                    GIS.ID = Asset.ID)

sthlm_2024 <- sthlm_2024 %>% mutate(Asset.ID = as.character(Asset.ID),
                                    PCI.Assessment.Year = 2024,
                                    Treatment.Year = substr(as.character(Treatment.Year),1,4),
                                    Road.Category = factor(Road.Category),
                                    Road.Category = recode(Road.Category, 
                                                           "1" = "1 - European", 
                                                           "2" = "2 - National",
                                                           "3" = "3 - Primary County", 
                                                           "4" = "4 - Secondary County"),
                                    DoU2017 = factor(DoU2017),
                                    DoU2017 = recode(DoU2017, 
                                                     "1" = "1 = Roads in metropolitan areas", 
                                                     "2" = "2 = Important connected roads",
                                                     "3" = "3 = Roads for daily travels and commuting", 
                                                     "4" = "4 = Other roads of commercial importance",
                                                     "5" = "5 = Roads important for rural areas",
                                                     "6" = "6 = Low traffic roads"),
                                    `Traffic.Class.(tkl8)` = factor(`Traffic.Class.(tkl8)`),
                                    `Traffic.Class.(tkl8)` = recode(`Traffic.Class.(tkl8)`, 
                                                                    "1" = "1 - AADT <250", 
                                                                    "2" = "2 - AADT 250-499",
                                                                    "3" = "3 - AADT 500-999", 
                                                                    "4" = "4 - AADT 1000-1999",
                                                                    "5" = "5 - AADT 2000-3999",
                                                                    "6" = "6 - AADT 4000-7999",
                                                                    "7" = "7 - AADT 8000-11999",
                                                                    "8" = "8 - AADT >11999"),
                                    Region = "4 - Stockholm",
                                    Bearing.Capacity = NA,
                                    PCI.Class = NA,
                                    Coverage = NA,
                                    Pavement.Type = if_else(Pavement.Type == "Hot Mix Ashphalt",
                                                            "Hot Mix Asphalt",
                                                            Pavement.Type),
                                    Surface.Type = if_else(Surface.Type == "Hot Mix Ashphalt",
                                                           "Hot Mix Asphalt",
                                                           Surface.Type),
                                    Name = Asset.ID,
                                    GIS.ID = Asset.ID)

norrbotten_2024 <- norrbotten_2024 %>% mutate(Asset.ID = as.character(Asset.ID),
                              PCI.Assessment.Year = 2024,
                              Treatment.Year = substr(as.character(Treatment.Year),1,4),
                              Road.Category = factor(Road.Category),
                              Road.Category = recode(Road.Category, 
                                                     "1" = "1 - European", 
                                                     "2" = "2 - National",
                                                     "3" = "3 - Primary County", 
                                                     "4" = "4 - Secondary County"),
                              DoU2017 = factor(DoU2017),
                              DoU2017 = recode(DoU2017, 
                                               "1" = "1 = Roads in metropolitan areas", 
                                               "2" = "2 = Important connected roads",
                                               "3" = "3 = Roads for daily travels and commuting", 
                                               "4" = "4 = Other roads of commercial importance",
                                               "5" = "5 = Roads important for rural areas",
                                               "6" = "6 = Low traffic roads"),
                              `Traffic.Class.(tkl8)` = factor(`Traffic.Class.(tkl8)`),
                              `Traffic.Class.(tkl8)` = recode(`Traffic.Class.(tkl8)`, 
                                                              "1" = "1 - AADT <250", 
                                                              "2" = "2 - AADT 250-499",
                                                              "3" = "3 - AADT 500-999", 
                                                              "4" = "4 - AADT 1000-1999",
                                                              "5" = "5 - AADT 2000-3999",
                                                              "6" = "6 - AADT 4000-7999",
                                                              "7" = "7 - AADT 8000-11999",
                                                              "8" = "8 - AADT >11999"),
                              Region = "2 - Nord",
                              Bearing.Capacity = NA,
                              PCI.Class = NA,
                              Coverage = NA,
                              Pavement.Type = if_else(Pavement.Type == "Hot Mix Ashphalt",
                                                      "Hot Mix Asphalt",
                                                      Pavement.Type),
                              Surface.Type = if_else(Surface.Type == "Hot Mix Ashphalt",
                                                     "Hot Mix Asphalt",
                                                     Surface.Type),
                              Name = Asset.ID,
                              GIS.ID = Asset.ID)

# Export to Excel
wb <- createWorkbook()
addWorksheet(wb, "Norrbotten")
addWorksheet(wb, "Stockholm")
addWorksheet(wb, "Skåne")
writeData(wb, sheet = 1, norrbotten_2024)
writeData(wb, sheet = 2, sthlm_2024)
writeData(wb, sheet = 3, skane_2024)
#Save Workbook
saveWorkbook(wb,"C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Svenskt Näringsliv/DOT/Norrbotten_Skane_Stockholm.xlsx", overwrite = TRUE)

norrbotten_2024_geo <- dplyr::left_join(norrbotten_2024,dot_geo, by=c("Asset.ID" = "Objectd"))
skane_2024_geo <- dplyr::left_join(skane_2024,dot_geo, by=c("Asset.ID" = "Objectd"))
sthlm_2024_geo <- dplyr::left_join(sthlm_2024,dot_geo, by=c("Asset.ID" = "Objectd"))

st_write(skane_2024_geo, paste0("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Svenskt Näringsliv/DOT/Skåne/skane_2024_dot.shp"), append=FALSE)
st_write(norrbotten_2024_geo, paste0("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Svenskt Näringsliv/DOT/Norrbotten/norrbotten_2024_dot.shp"), append=FALSE)
st_write(sthlm_2024_geo, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Svenskt Näringsliv/DOT/Stockholm/sthlm_2024_dot.shp", append=FALSE)

sum(sthlm_2024$Length)/1000/84000
sum(skane_2024$Length)/1000/84000
sum(norrbotten_2024$Length)/1000/84000
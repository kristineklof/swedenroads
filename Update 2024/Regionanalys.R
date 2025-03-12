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
                                    Area = Length*Width,
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
                                    Area = Length*Width,
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
                              Area = Length*Width,
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

# Percentages of road network
sum(sthlm_2024$Length)/1000/84000
sum(skane_2024$Length)/1000/84000
sum(norrbotten_2024$Length)/1000/84000

#====================================#
# Summary statistics for TRB Paper

# Road network length in km
sum(sthlm_2024$Length)/1000
sum(skane_2024$Length)/1000
sum(norrbotten_2024$Length)/1000

# Road network AADT
min(sthlm_2024$AADT)
min(skane_2024$AADT)
min(norrbotten_2024$AADT)

mean(sthlm_2024$AADT)
mean(skane_2024$AADT)
mean(norrbotten_2024$AADT)

max(sthlm_2024$AADT)
max(skane_2024$AADT)
max(norrbotten_2024$AADT)

# Road network AADT heavy
min(sthlm_2024$AADT.Trucks)
min(skane_2024$AADT.Trucks)
min(norrbotten_2024$AADT.Trucks)

mean(sthlm_2024$AADT.Trucks)
mean(skane_2024$AADT.Trucks)
mean(norrbotten_2024$AADT.Trucks)

max(sthlm_2024$AADT.Trucks)
max(skane_2024$AADT.Trucks)
max(norrbotten_2024$AADT.Trucks)

# Speed Limit
min(sthlm_2024$Speed.Limit, na.rm=TRUE)
min(skane_2024$Speed.Limit, na.rm=TRUE)
min(norrbotten_2024$Speed.Limit, na.rm=TRUE)

mean(sthlm_2024$Speed.Limit, na.rm=TRUE)
mean(skane_2024$Speed.Limit, na.rm=TRUE)
mean(norrbotten_2024$Speed.Limit, na.rm=TRUE)

max(sthlm_2024$Speed.Limit, na.rm=TRUE)
max(skane_2024$Speed.Limit, na.rm=TRUE)
max(norrbotten_2024$Speed.Limit, na.rm=TRUE)

# Age
min((2023-as.numeric(sthlm_2024$Treatment.Year)), na.rm=TRUE)
min((2023-as.numeric(skane_2024$Treatment.Year)), na.rm=TRUE)
min((2023-as.numeric(norrbotten_2024$Treatment.Year)), na.rm=TRUE)

mean((2023-as.numeric(sthlm_2024$Treatment.Year)), na.rm=TRUE)
mean((2023-as.numeric(skane_2024$Treatment.Year)), na.rm=TRUE)
mean((2023-as.numeric(norrbotten_2024$Treatment.Year)), na.rm=TRUE)

quantile((2023 - as.numeric(sthlm_2024$Treatment.Year)), 0.98, na.rm = TRUE)
quantile((2023 - as.numeric(skane_2024$Treatment.Year)), 0.98, na.rm = TRUE)
quantile((2023 - as.numeric(norrbotten_2024$Treatment.Year)), 0.98, na.rm = TRUE)

# Section length
min(sthlm_2024$Length, na.rm=TRUE)
min(skane_2024$Length, na.rm=TRUE)
min(norrbotten_2024$Length, na.rm=TRUE)

mean(sthlm_2024$Length, na.rm=TRUE)
mean(skane_2024$Length, na.rm=TRUE)
mean(norrbotten_2024$Length, na.rm=TRUE)

max(sthlm_2024$Length, na.rm=TRUE)
max(skane_2024$Length, na.rm=TRUE)
max(norrbotten_2024$Length, na.rm=TRUE)

# Road type
QualitativeStatsSingleGroup(sthlm_2024, quo(Road.Type), quo(Length))
QualitativeStatsSingleGroup(skane_2024, quo(Road.Type), quo(Length))
QualitativeStatsSingleGroup(norrbotten_2024, quo(Road.Type), quo(Length))

# Surface type
QualitativeStatsSingleGroup(sthlm_2024, quo(Surface.Type), quo(Length))
QualitativeStatsSingleGroup(skane_2024, quo(Surface.Type), quo(Length))
QualitativeStatsSingleGroup(norrbotten_2024, quo(Surface.Type), quo(Length))

# IRI
min(sthlm_2024$IRI, na.rm=TRUE)
min(skane_2024$IRI, na.rm=TRUE)
min(norrbotten_2024$IRI, na.rm=TRUE)

mean(sthlm_2024$IRI, na.rm=TRUE)
mean(skane_2024$IRI, na.rm=TRUE)
mean(norrbotten_2024$IRI, na.rm=TRUE)

quantile(sthlm_2024$IRI, 0.995, na.rm=TRUE)
quantile(skane_2024$IRI, 0.995, na.rm=TRUE)
quantile(norrbotten_2024$IRI, 0.995, na.rm=TRUE)

# Rut depth
min(sthlm_2024$'Rut.(mm)', na.rm=TRUE)
min(skane_2024$'Rut.(mm)', na.rm=TRUE)
min(norrbotten_2024$'Rut.(mm)', na.rm=TRUE)

mean(sthlm_2024$'Rut.(mm)', na.rm=TRUE)
mean(skane_2024$'Rut.(mm)', na.rm=TRUE)
mean(norrbotten_2024$'Rut.(mm)', na.rm=TRUE)

quantile(sthlm_2024$'Rut.(mm)', 0.998, na.rm=TRUE)
quantile(skane_2024$'Rut.(mm)', 0.998, na.rm=TRUE)
quantile(norrbotten_2024$'Rut.(mm)', 0.998, na.rm=TRUE)

##########################
# Road assessment
sthlm_2024 <- PCIClass(sthlm_2024)
skane_2024 <- PCIClass(skane_2024)
norrbotten_2024 <- PCIClass(norrbotten_2024)
sthlm_2024  <- as.data.frame(sthlm_2024)
skane_2024  <- as.data.frame(skane_2024)
norrbotten_2024  <- as.data.frame(norrbotten_2024)

QualitativeStatsSingleGroup(skane_2024, quo(PCIClass), quo(Length))
QualitativeStatsSingleGroup(sthlm_2024, quo(PCIClass), quo(Length))
QualitativeStatsSingleGroup(norrbotten_2024, quo(PCIClass), quo(Length))

ssn <- rbind(sthlm_2024[c("County","Length","PCIClass")],
             skane_2024[c("County","Length","PCIClass")],
             norrbotten_2024[c("County","Length","PCIClass")])

fill <- c("#20AC65", "#71C94B","#FABF20","#F2303E","#C40A3B")

cond_regs <- ssn %>%
  dplyr::select(PCIClass, Length, County) %>%
  dplyr::mutate(County = factor(County, levels = c("Stockholms län","Norrbottens län","Skåne län"))) %>%
  dplyr::mutate(County = recode(County, 
                                  "Stockholms län" ="Stockholm", 
                                  "Norrbottens län" = "Norrbotten", 
                                  "Skåne län" = "Skåne")) %>%
  dplyr::mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                  "5" ="81-100: Excellent", 
                                  "4" = "61-80: Good", 
                                  "3" = "41-60: Fair", 
                                  "2" = "21-40: Poor", 
                                  "1" = "0-20: Very poor")) %>%
  group_by(County, PCIClass) %>%
  summarise(grouplen = sum( Length)/1000) %>%
  dplyr::mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = County, y = percentage, fill = PCIClass, label = paste0(round(100*percentage,digits=0)," %"))) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_fill_manual(values=fill) +
  labs(y="", x = "") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="right", legend.direction="horizontal",
        legend.title = element_blank(), 
        legend.text=element_text(size=16), 
        axis.text=element_text(size=14),
        strip.text.x = element_text(size=16)) +
  guides(fill=guide_legend(label.position = "right",
                           nrow = 5)) +
  geom_text(size = 5, position = position_stack(vjust = 0.5))
print(cond_regs)

cond_regs_swe <- ssn %>%
  dplyr::select(PCIClass, Length, County) %>%
  dplyr::mutate(County = factor(County, levels = c("Stockholms län","Norrbottens län","Skåne län"))) %>%
  dplyr::mutate(County = recode(County, 
                                "Stockholms län" ="Stockholm", 
                                "Norrbottens län" = "Norrbotten", 
                                "Skåne län" = "Skåne")) %>%
  dplyr::mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  dplyr::mutate(PCIClass = recode(PCIClass, 
                                  "5" ="Mycket bra", 
                                  "4" = "Bra", 
                                  "3" = "Tillfredsställande", 
                                  "2" = "Dålig", 
                                  "1" = "Mycket dålig")) %>%
  group_by(County, PCIClass) %>%
  summarise(grouplen = sum( Length)/1000) %>%
  dplyr::mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = County, y = percentage, fill = PCIClass, label = paste0(round(100*percentage,digits=0)," %"))) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_fill_manual(values=fill) +
  labs(y="", x = "") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="right", legend.direction="horizontal",
        legend.title = element_blank(), 
        legend.text=element_text(size=16), 
        axis.text=element_text(size=14),
        strip.text.x = element_text(size=16)) +
  guides(fill=guide_legend(label.position = "right",
                           nrow = 5)) +
  geom_text(size = 5, position = position_stack(vjust = 0.5))
print(cond_regs_swe)


# Plot index curve
PlotIndexCurve <- function(cutoff, actual, x_lab){
  df <- data.frame(x = (cutoff - actual)/cutoff)
  index <- function(x) 100*exp(--log(0.2)*x)
  
  p <- ggplot(data = df, mapping = aes(x = x)) + 
    stat_function(fun = index, linewidth = 1) +
    geom_hline(yintercept=20, linetype="dashed", color = "red", size = 2) +
    theme(axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16),
          axis.title = element_text(size=14)) +
    scale_x_continuous(name=x_lab, limits=c(0, 3.5)) +
    scale_y_continuous(name="IRI-index", breaks=seq(0,100,20))
  
  
  return(p)              
}

iridat <- rbind(sthlm_2024[c("IRI","IRI_maint")],
                skane_2024[c("IRI","IRI_maint")],
                norrbotten_2024[c("IRI","IRI_maint")])

rutdat <- rbind(sthlm_2024[c("Rut.(mm)","SP_maint")],
                skane_2024[c("Rut.(mm)","SP_maint")],
                norrbotten_2024[c("Rut.(mm)","SP_maint")])
names(rutdat) <- c("Rut","SP_maint")

# IRI
cutoff <- (iridat$IRI_maint)
IRI_ceil <- if_else_na(iridat$IRI < 1, ceiling(iridat$IRI), iridat$IRI)
actual <- iridat$IRI_maint - IRI_ceil

p <- PlotIndexCurve(cutoff, actual, x_lab = "Relative IRI-value")
print(p)

# Rut deoth
cutoff <- (rutdat$SP_maint)
actual <- rutdat$SP_maint - rutdat$Rut

p <- PlotIndexCurve(cutoff, actual, x_lab = "Relative rut depth value")
print(p)

####################################################
# Lineplot över sammanlagda utsläpp per scenario

skane_ems <- data.frame(Län = rep("Skåne",30),
                  År = rep(c(2024,2025,2026,2027,2028,2029,2030,2031,2032,2033),3),
                  Scenario = c(rep("1: Inget förebyggande underhåll",10),
                               rep("2: Förebyggande underhåll",10),
                               rep("3: Eliminera underhållsskulden",10)),
                  Kostnad = c(529,615,682,780,863,851,749,696,714,732,
                              698,592,514,494,501,484,462,464,456,761,
                              480,478,374,322,2074,996,808,740,424,463),
                  CO2e = c(12886,15542,16999,19638,21699,21120,18327,16414,16938,18762,
                           17870,14252,11905,10811,10816,10544,9637,9749,9341,13617.6,
                           13031,12792,10103,8378,45981,13440,10754,9516,5569,9263))

sthlm_ems <- data.frame(Län = rep("Stockholm",30),
                    År = rep(c(2024,2025,2026,2027,2028,2029,2030,2031,2032,2033),3),
                    Scenario = c(rep("1: Inget förebyggande underhåll",10),
                                 rep("2: Förebyggande underhåll",10),
                                 rep("3: Eliminera underhållsskulden",10)),
                    Kostnad = c(554,472,437,544,938,581,140,182,841,267,
                                340,331,275,250,189,188,197,197,569,479,
                                460,347,229,1086,363,340,371,298,221,699),
                    CO2e = c(12968,11489,10445,13250,22202,13047,3293,4151,19690,7029,
                             9502,8872,7030,6100,4624,4725,4748,4753,13806,12084,
                             12886,9350,5807,23981,3907,3625,4176,3870,2649,17133))

norr_ems <- data.frame(Län = rep("Norrbotten",30),
                    År = rep(c(2024,2025,2026,2027,2028,2029,2030,2031,2032,2033),3),
                    Scenario = c(rep("1: Inget förebyggande underhåll",10),
                                 rep("2: Förebyggande underhåll",10),
                                 rep("3: Eliminera underhållsskulden",10)),
                    Kostnad = c(747,303,459,313,149,219,244,234,188,201,
                                348,271,307,275,288,281,254,251,258,288,
                                400,312,275,225,228,196,1184,541,375,402),
                    CO2e = c(20592,7284,9602,7303,3535,5116,5282,5289,4739,5851,
                             7901,5911,6612,5611,5949,5562,5213,4888,4863,6083,
                             9331,7184,6294,5044,5211,4354,23908,8456,5359,9586))

ems <- rbind(skane_ems, sthlm_ems, norr_ems)

custom_colors <- c("1: Inget förebyggande underhåll" = "#E41A1C",
                   "2: Förebyggande underhåll" = "#377EB8",
                   "3: Eliminera underhållsskulden" = "#4DAF4A")

# Alla län
kum_ems <- ems %>%
  group_by(Scenario, År) %>%
  arrange(År) %>%
  summarise(Kostnad = sum(Kostnad),
           CO2e = sum(CO2e)) %>%
  mutate(Cumulative_Kostnad = cumsum(Kostnad),
         Cumulative_CO2e = cumsum(CO2e))

# Plot cumulative Kostnad
kostnad_plot <- ggplot(kum_ems, aes(x = År, y = Cumulative_Kostnad, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = custom_colors) +
  scale_x_continuous(breaks = 2024:2033) +
  labs(title = "Kumulativ underhållskostnad",
       x = "År",
       y = "Miljoner SEK",
       color = "Scenario") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 13),  
    axis.text = element_text(size = 12),  
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank() 
  )

# Plot cumulative CO2e
co2e_plot <- ggplot(kum_ems, aes(x = År, y = Cumulative_CO2e, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = custom_colors) +
  scale_x_continuous(breaks = 2024:2033) +
  labs(title = "Kumulativa växthusgasutsläpp",
       x = "År",
       y = "Ton CO2e",
       color = "Scenario") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 13),  
    axis.text = element_text(size = 12),  
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank() 
  )

# Combine the plots with a shared legend
combined_plot <- kostnad_plot + co2e_plot + 
  plot_layout(ncol = 1, guides = 'collect') & # Combine plots in one column and collect guides (legend)
  theme(legend.position = "bottom")           # Position legend at the bottom

# Print the combined plot
print(combined_plot)

# Stockholm
# Calculate cumulative sums for Kostnad and CO2e
sthlm_ems_cumulative <- sthlm_ems %>%
  group_by(Scenario) %>%
  arrange(År) %>%
  mutate(Cumulative_Kostnad = cumsum(Kostnad),
         Cumulative_CO2e = cumsum(CO2e))

# Plot cumulative Kostnad
kostnad_plot_sthlm <- ggplot(sthlm_ems_cumulative, aes(x = År, y = Cumulative_Kostnad, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = custom_colors) +
  scale_x_continuous(breaks = 2024:2033) +
  labs(title = "Kumulativ underhållskostnad",
       x = "År",
       y = "Miljoner SEK",
       color = "Scenario") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 13),  
    axis.text = element_text(size = 12),  
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank() 
  )

# Plot cumulative CO2e
co2e_plot_sthlm <- ggplot(sthlm_ems_cumulative, aes(x = År, y = Cumulative_CO2e, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = custom_colors) +
  scale_x_continuous(breaks = 2024:2033) +
  labs(title = "Kumulativa växthusgasutsläpp",
       x = "År",
       y = "Ton CO2e",
       color = "Scenario") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 13),  
    axis.text = element_text(size = 12),  
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank() 
  )

# Combine the plots with a shared legend
combined_plot_sthlm <- kostnad_plot_sthlm + co2e_plot_sthlm + 
  plot_layout(ncol = 1, guides = 'collect') & # Combine plots in one column and collect guides (legend)
  theme(legend.position = "bottom")           # Position legend at the bottom

# Print the combined plot
print(combined_plot_sthlm)

# Skåne
# Calculate cumulative sums for Kostnad and CO2e
skane_ems_cumulative <- skane_ems %>%
  group_by(Scenario) %>%
  arrange(År) %>%
  mutate(Cumulative_Kostnad = cumsum(Kostnad),
         Cumulative_CO2e = cumsum(CO2e))

# Plot cumulative Kostnad
kostnad_plot_skane <- ggplot(skane_ems_cumulative, aes(x = År, y = Cumulative_Kostnad, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = custom_colors) +
  scale_x_continuous(breaks = 2024:2033) +
  labs(title = "Kumulativ underhållskostnad",
       x = "År",
       y = "Miljoner SEK",
       color = "Scenario") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 13),  
    axis.text = element_text(size = 12),  
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank() 
  )

# Plot cumulative CO2e
co2e_plot_skane <- ggplot(skane_ems_cumulative, aes(x = År, y = Cumulative_CO2e, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = custom_colors) +
  scale_x_continuous(breaks = 2024:2033) +
  labs(title = "Kumulativa växthusgasutsläpp",
       x = "År",
       y = "Ton CO2e",
       color = "Scenario") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 13),  
    axis.text = element_text(size = 12),  
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank() 
  )

# Combine the plots with a shared legend
combined_plot_skane <- kostnad_plot_skane + co2e_plot_skane + 
  plot_layout(ncol = 1, guides = 'collect') & # Combine plots in one column and collect guides (legend)
  theme(legend.position = "bottom")           # Position legend at the bottom

# Print the combined plot
print(combined_plot_skane)

# Norrbotten
# Calculate cumulative sums for Kostnad and CO2e
norr_ems_cumulative <- norr_ems %>%
  group_by(Scenario) %>%
  arrange(År) %>%
  mutate(Cumulative_Kostnad = cumsum(Kostnad),
         Cumulative_CO2e = cumsum(CO2e))

# Plot cumulative Kostnad
kostnad_plot_norr <- ggplot(norr_ems_cumulative, aes(x = År, y = Cumulative_Kostnad, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = custom_colors) +
  scale_x_continuous(breaks = 2024:2033) +
  labs(title = "Kumulativ underhållskostnad",
       x = "År",
       y = "Miljoner SEK",
       color = "Scenario") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 13),  
    axis.text = element_text(size = 12),  
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank() 
  )

# Plot cumulative CO2e
co2e_plot_norr <- ggplot(norr_ems_cumulative, aes(x = År, y = Cumulative_CO2e, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = custom_colors) +
  scale_x_continuous(breaks = 2024:2033) +
  labs(title = "Kumulativa växthusgasutsläpp",
       x = "År",
       y = "Ton CO2e",
       color = "Scenario") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 13),  
    axis.text = element_text(size = 12),  
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank() 
  )

# Combine the plots with a shared legend
combined_plot_norr <- kostnad_plot_norr + co2e_plot_norr + 
  plot_layout(ncol = 1, guides = 'collect') & # Combine plots in one column and collect guides (legend)
  theme(legend.position = "bottom")           # Position legend at the bottom

# Print the combined plot
print(combined_plot_norr)

#######################################################
# Nacka kommun
nacka_ems <- data.frame(Year = rep(c(2024,2025,2026,2027,2028,2029,2030,2031,2032,2033),2),
                       Scenario = c(rep("Optimerat underhåll",10),
                                    rep('"Värst först"',10)),
                       Cost = c(39.7,39.4,35.5,39.8,39.9,38.4,38.3,39.4,39.2,23.6,
                                54.9, 54.9, 54.9, 54.9, 54.9, 54.9, 54.9 ,54.9, 54.9, 54.9),
                       CO2e = c(361.1,	340.5,	327.0,	345.1,	365.1,	325.0,	346.8,	362.3,	334.4,	195.8,
                                626.3,	655.2,	694.5,	710.1,	707.3,	570.4,	556.5,	511.8,	510.5,	509.5))

# Calculate cumulative sums for Kostnad and CO2e
nacka_ems_cumulative <- nacka_ems %>%
  group_by(Scenario) %>%
  arrange(Year) %>%
  mutate(Cumulative_Kostnad = cumsum(Cost),
         Cumulative_CO2e = cumsum(CO2e))

nacka_custom_colors <- c('"Värst först"' = "#E41A1C",
                        "Optimerat underhåll" = "#4DAF4A")

# Plot cumulative Kostnad
kostnad_plot_nacka <- ggplot(nacka_ems_cumulative, aes(x = Year, y = Cumulative_Kostnad, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = nacka_custom_colors) +
  scale_x_continuous(breaks = 2024:2033) +
  labs(title = "Ackumulerad underhållskostnad",
       x = "År",
       y = "Miljoner SEK",
       color = "Underhållsstrategi:") +
  theme_minimal() +
  theme(
    text = element_text(family = "Verdana"),  # Set font to Verdana
    axis.title = element_text(size = 10),  
    axis.text = element_text(size = 9),  
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),  # Major grid lines color
    plot.title = element_text(size = 11),
    panel.border = element_rect(color = "#55657C", fill = NA, size = 1)  # White border around the panel
  )

# Plot cumulative CO2e
co2e_plot_nacka <- ggplot(nacka_ems_cumulative, aes(x = Year, y = Cumulative_CO2e, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = nacka_custom_colors) +
  scale_x_continuous(breaks = 2024:2033) +
  labs(title = "Ackumulerade växthusgasutsläpp",
       x = "År",
       y = "Ton CO2e",
       color = "Underhållsstrategi:") +
  theme_minimal() +
  theme(
    text = element_text(family = "Verdana"),  # Set font to Verdana
    axis.title = element_text(size = 10),  
    axis.text = element_text(size = 9),  
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),  # Major grid lines color
    plot.title = element_text(size = 11),
    panel.border = element_rect(color = "#55657C", fill = NA, size = 1)  # White border around the panel
  )

# Combine the plots with a shared legend
combined_plot_nacka <- kostnad_plot_nacka + co2e_plot_nacka + 
  plot_layout(ncol = 1, guides = 'collect') & # Combine plots in one column and collect guides (legend)
  theme(legend.position = "bottom")           # Position legend at the bottom

# Print the combined plot
print(combined_plot_nacka)

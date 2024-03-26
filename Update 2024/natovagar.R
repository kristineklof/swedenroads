############################
# Nato-vägar
ggm <- c("ggmap")
LoadInstall(ggm)
register_stadiamaps(key = "3c27d615-58e8-45ea-8435-e7bb672c5444", write = TRUE)

swroads <- st_read(paste0(datapath,"2024/VåraVägar/swedenroads_2024_all_scenarios.shp"))
head(swroads)

#E14 från norska Trondheim via Östersund till Sundsvall bör byggas ut när USA:s 
#förhandslagring av krigsmaterial utökas. USA ska ha vapenlager 
#inte bara i norska Tröndelag, utan även i nedre Norrland.
#E10 Gällivare-Kiruna, som löper genom en terräng som kan bli Natos främsta 
#träningsområde för vinterförmåga, bör byggas ut.
#E65 Ystad-Malmö, som är viktig för militära transporter till Bornholm.
#E22 Trelleborg-Norrköping

natovagar <- c(14,10,65,22)
nato <- swroads %>% dplyr::filter(vägnmmr %in% natovagar) %>%
  mutate(Tillstånd = as.character(IndxKls)) %>%
  mutate(Tillstånd = factor(Tillstånd, levels = c("5","4","3","2","1"))) %>%
  mutate(Tillstånd = recode(Tillstånd, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(Tillstånd = factor(Tillstånd, levels = c("Mycket bra","Bra","Tillfredsställande","Dålig","Mycket dålig"))) %>%
  mutate(vägnmmr = as.character(vägnmmr))
  
head(nato)
nrow(nato)
sum(nato$längd)/1000

nato_no_geo <- st_set_geometry(nato, NULL)
natostats <- QualitativeStatsDoubleGroup(nato_no_geo, quo(vägnmmr), quo(Tillstånd), quo(längd))
natostats <- natostats %>%
  dplyr::mutate(prop = prop*100)

# Export to Excel
wb_nato <- createWorkbook()
addWorksheet(wb_nato, "Natovägar tillstånd 2023")
writeData(wb_nato, sheet = 1, natostats)
#Save Workbook
saveWorkbook(wb_nato, "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Uppdatering 2024/Natovägar_tillstånd_2023.xlsx", overwrite = TRUE)


st_write(nato, paste0(datapath,"2024/natovagar.shp"), append=FALSE)

# Define the approximate bounding box for Sweden
sweden_bbox <- make_bbox(lon = c(11.118, 24.167), lat = c(55.34, 69.06))

# Fetch the map
sweden_map <- get_stadiamap(bbox = sweden_bbox, zoom=5)
nato_wgs84 <- st_transform(nato, crs = 4326)
plot(st_geometry(nato_wgs84))
nato_wgs84 <- nato_wgs84 %>% dplyr::select(geometry,IndxKls)

fillc <- c("#20AC65", "#71C94B","#FABF20","#F2303E","#C40A3B")

ggmap(sweden_map) +
  geom_sf(data = nato_wgs84, aes(color = IndxKls), inherit.aes = FALSE) +  # Ensure yourGroupVariable exists in nato_wgs84
  scale_color_manual(values = fillc) +  # fillc should be a vector of colors
  theme_minimal()

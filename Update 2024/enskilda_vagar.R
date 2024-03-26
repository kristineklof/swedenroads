####################################
# Filtrera enskilda vägar 

nvdb_2024 <- st_read("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/LRF/nvdb_homo_2024.shp")
head(nvdb_2024)

nvdb_2024_enskild <- nvdb_2024 %>%
  dplyr::filter(vagha_6 == 3) %>%
  dplyr::filter(farjeled == 0) %>%
  dplyr::select(objectid,route_id,from_measu, to_measure,
                klass_181, funktionel, f_hogst_22, slitl_152,
                vagha_6, vagtr_474, typ_369, kommu_141, shape_leng, geometry)
head(nvdb_2024_enskild)
sum(nvdb_2024_enskild$shape_leng)/1000/10
nrow(nvdb_2024_enskild)

nira_enskild <- nvdb_2024_enskild %>% dplyr::select(objectid,shape_leng, geometry)

st_write(nvdb_2024_enskild,"C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/LRF/nvdb_2024_enskild.shp", append=FALSE)
st_write(nira_enskild,"C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/LRF/enskilda_vagar.shp", append=FALSE)

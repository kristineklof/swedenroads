#=================================================================#
#              Swedenroads: Update the baseline map
#=================================================================#

baseline_2025 <- st_read("C:/Users/krist/Swedenroads update 2025/nvdb_homo_2024_all_roads.shp")
head(baseline_2025)
nrow(baseline_2025)

# Keep only väghållartyp = statlig
baseline_2025 <- baseline_2025 %>% filter(Vagha_6 == 1) %>%   # Keep only väghållartyp = statlig
  filter(Farjeled == 0) %>%  # Keep only farjeled = 0
  filter(Vagtr_474 == 1) %>% # Keep only nättyp = bilnät
  filter(Slitl_152 == 1) # Keep only slitlager = belagd
nrow(baseline_2025)

# Keep only sträckor längre än 30 m
baseline_2025 <- baseline_2025 %>% filter(SHAPE_Leng > 25)
nrow(baseline_2025)
sum(baseline_2025$SHAPE_Leng)/1000


st_write(baseline_2025, "C:/Users/krist/Swedenroads update 2025/nvdb_homo_2024.shp", append=FALSE)



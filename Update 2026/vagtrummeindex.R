#====================================================================#
#   This script imports vägtrummor and and creates "vägtrummeindex"
#====================================================================#

source("LoadInstall.R")
deps <- c("sf", "DBI","tidyverse","RPostgres", "readxl", "rcompanion")
LoadInstall(deps)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "swedenroads",
  host   = "localhost",
  port   = 5432,
  user   = "postgres",   # change if needed
  password = "strodie"
)

# If vagtrummor has a geometry column, this returns an sf object
vagtrummor <- st_read(con, query = "SELECT * FROM public.swedenroads_2025_vagtrummor_joined")
dbDisconnect(con)

crs_obj <- st_crs(vagtrummor)  # grab CRS once

# geom
vagtrummor$geom <- st_zm(
  st_sfc(vagtrummor$geom, crs = crs_obj),
  drop = TRUE
)

# p_geom
vagtrummor$p_geom <- st_zm(
  st_sfc(vagtrummor$p_geom, crs = crs_obj),
  drop = TRUE
)

# 3) Keep first occurrence per key (very fast)
i_id  <- match(unique(vagtrummor$id),  vagtrummor$id)
i_pid <- match(unique(vagtrummor$p_id), vagtrummor$p_id)

geom_by_id   <- vagtrummor[i_id,  c("id", "geom")]
pgeom_by_pid <- vagtrummor[i_pid, c("p_id", "p_geom")]

# Optional: remove rows where key or geometry is NA
geom_by_id   <- geom_by_id[!is.na(geom_by_id$id)     & !is.na(geom_by_id$geom), ]
pgeom_by_pid <- pgeom_by_pid[!is.na(pgeom_by_pid$p_id) & !is.na(pgeom_by_pid$p_geom), ]

# 4) Drop geometry columns from main df
vagtrummor_no_geom <- vagtrummor
vagtrummor_no_geom$geom <- NULL
vagtrummor_no_geom$p_geom <- NULL

# Descriptive statistics vagtrummor
stats <- vagtrummor_no_geom %>%
  group_by(id) %>%
  summarise(n_globalid = sum(!is.na(globalid)), .groups = "drop") %>%
  summarise(
    min    = min(n_globalid),
    max    = max(n_globalid),
    mean   = mean(n_globalid),
    median = median(n_globalid)
  )
stats

summary_ids <- vagtrummor_no_geom %>%
  group_by(id) %>%
  summarise(n_globalid = sum(!is.na(globalid)), .groups = "drop") %>%
  summarise(
    n_ids_total = n(),
    n_ids_with_globalid = sum(n_globalid > 0),
    share_with_globalid = mean(n_globalid > 0)
  )
summary_ids

vars <- c(
  "funkstatu","materialst","vattenbort","igenslamni",
  "erosion_in","erosion_ut","hinder_inl","hinder_utl",
  "corrosion","deformatio","is_rdragna"
)

summary_by_value <- vagtrummor_no_geom %>%
  filter(!is.na(globalid)) %>%
  select(globalid, all_of(vars)) %>%
  pivot_longer(all_of(vars), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  distinct(globalid, variable, value) %>%          # if duplicates exist per globalid/value
  count(variable, value, name = "n_globalids") %>% # globalids per unique value
  arrange(variable, desc(n_globalids))

print(summary_by_value,n=45)


########################################################################
# Vägtrummeindex

vars <- c("corrosion","deformatio","erosion_in","erosion_ut","hinder_inl","hinder_utl",
          "igenslamni","is_rdragna","funkstatu","materialst","vattenbort")

# weights (edit as you like)
w <- c(
  corrosion=1.0, deformatio=1.0, erosion_in=1.0, erosion_ut=1.0,
  hinder_inl=1.0, hinder_utl=1.0, igenslamni=1.0, is_rdragna=1.0,
  funkstatu=2.0, materialst=2.0, vattenbort=1.0
)

score_long <- vagtrummor_no_geom %>%
  select(globalid, all_of(vars)) %>%
  pivot_longer(all_of(vars), names_to="variable", values_to="value") %>%
  mutate(
    score = case_when(
      variable %in% c("funkstatu","materialst","vattenbort") & value == "God" ~ 3,
      variable %in% c("funkstatu","materialst","vattenbort") & value == "Åtgärd på sikt" ~ 2,
      variable %in% c("funkstatu","materialst","vattenbort") & value == "Åtgärdsbehov" ~ 1,
      
      variable %in% c("corrosion","deformatio","erosion_in","erosion_ut","hinder_inl","hinder_utl",
                      "igenslamni","is_rdragna") & value == "Nej" ~ 3,
      variable %in% c("corrosion","deformatio","erosion_in","erosion_ut","hinder_inl","hinder_utl",
                      "igenslamni","is_rdragna") & value == "Ja" ~ 1,
      
      TRUE ~ NA_real_   # Okänt / Ej besiktningsbart / anything else
    ),
    weight = w[variable]
  )

# If multiple rows per globalid exist: collapse to worst score per variable (min) ignoring NA
score_per_gid_var <- score_long %>%
  group_by(globalid, variable) %>%
  summarise(
    score = if (all(is.na(score))) NA_real_ else min(score, na.rm = TRUE),
    weight = first(weight),
    .groups = "drop"
  )

ranked <- score_per_gid_var %>%
  group_by(globalid) %>%
  summarise(
    n_total = n(),
    n_known = sum(!is.na(score)),
    share_known = n_known / n_total,
    
    # weighted mean over known
    mean_score = ifelse(n_known == 0, NA_real_,
                        weighted.mean(score, weight, na.rm = TRUE)),
    
    status = case_when(
      is.na(mean_score) | share_known < 0.5 ~ "Okänd",
      mean_score < 1.67 ~ "Åtgärdsbehov",
      mean_score < 2.33 ~ "Åtgärdsbehov på sikt",
      TRUE ~ "God status"
    ),
    rank = case_when(
      status == "Åtgärdsbehov (1)" ~ 1L,
      status == "Åtgärdsbehov på sikt (2)" ~ 2L,
      status == "God status (3)" ~ 3L,
      TRUE ~ NA_integer_
    ),
    .groups = "drop"
  )
ranked

rank_pct <- ranked %>%
  distinct(globalid, status) %>%
  count(status, name = "n_globalids") %>%
  mutate(pct_globalids = 100 * n_globalids / sum(n_globalids)) %>%
  arrange(desc(pct_globalids))
rank_pct

# Calculate value per road segment
vagtrummor_no_geom <- left_join(vagtrummor_no_geom, ranked[c("globalid", "mean_score")], by="globalid")  

culvert_scores <- vagtrummor_no_geom %>%
  filter(!is.na(id), !is.na(globalid)) %>%
  group_by(id, globalid) %>%
  summarise(
    mean_score = if (all(is.na(mean_score))) NA_real_ else min(mean_score, na.rm = TRUE),
    .groups = "drop"
  )

section_scores <- culvert_scores %>%
  group_by(id) %>%
  summarise(
    n_culverts = n(),
    n_known = sum(!is.na(mean_score)),
    share_known = n_known / n_culverts,
    
    any_one = any(mean_score < 1.67, na.rm = TRUE),
    
    section_mean = ifelse(n_known == 0, NA_real_,
                          weighted.mean(mean_score, w = rep(1, n()), na.rm = TRUE)),
    
    section_score = case_when(
      any_one ~ 1,
      TRUE ~ section_mean
    ),
    .groups = "drop"
  )
section_scores <- section_scores %>%
  mutate(
    section_status = case_when(
      share_known < 0.5 ~ "Okänd",
      section_score < 1.67 ~ "Åtgärdsbehov",
      section_score < 2.33 ~ "Åtgärd på sikt",
      TRUE ~ "God status"
    )
  )

section_pct <- section_scores %>%
  distinct(id, section_status) %>%
  count(section_status, name = "n_ids") %>%
  mutate(pct_ids = 100 * n_ids / sum(n_ids)) %>%
  arrange(desc(pct_ids))
section_pct

section_len_pct <- left_join(section_scores, vagtrummor_no_geom[c("id","längd")], by="id") %>%
  distinct(id, section_status, längd) %>%              # ensure 1 row per id
  filter(!is.na(längd)) %>%
  group_by(section_status) %>%
  summarise(
    total_length = sum(längd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_length = 100 * total_length / sum(total_length)
  ) %>%
  arrange(desc(pct_length))
section_len_pct

vagtrummor_no_geom <- left_join(vagtrummor_no_geom, section_scores[c("id", "section_score", "section_status")], by="id")  

section_len_pci_pct <- vagtrummor_no_geom %>%
  distinct(id, pcic_26, section_status, längd) %>%
  filter(
    !is.na(längd),
    !is.na(pcic_26),
    !is.na(section_status)
  ) %>%
  group_by(pcic_26, section_status) %>%
  summarise(
    total_length = sum(längd, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    pct_length = 100 * total_length / sum(total_length)
  ) %>%
  ungroup() %>%
  arrange(pcic_26, desc(pct_length))
print(section_len_pci_pct,n=30)

tab_len <- xtabs(total_length ~ pcic_26 + section_status, data = section_len_pci_pct)
chisq.test(tab_len)
res <- chisq.test(tab_len)
round(res$stdres, 2)
rcompanion::cramerV(tab_len)
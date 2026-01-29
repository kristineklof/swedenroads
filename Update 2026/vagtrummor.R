#=================================================================#
#   This script imports vägtrummor and attaches inspection data
#=================================================================#

source("LoadInstall.R")
deps <- c("sf", "DBI","tidyverse","RPostgres", "readxl")
LoadInstall(deps)
source("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/repos/swedenroads/Update 2026/vagtrummor_functions.R", encoding = 'UTF-8')

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "swedenroads",
  host   = "localhost",
  port   = 5432,
  user   = "postgres",   # change if needed
  password = "strodie"
)

# If vagtrummor has a geometry column, this returns an sf object
vagtrummor <- st_read(con, query = "SELECT * FROM public.vagtrummor")
dbDisconnect(con)

# Inspection data
trumrapport <- read_excel("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Uppdatering 2026/Vägtrummor/Trumrapport_20251118_nationellt.xlsx", sheet = 1)
trumrapport$GlobalID <- gsub("[\\{\\}]", "", as.character(trumrapport$GlobalID))

# Join vagtrummor and trumrapport
vagtrummor <- vagtrummor %>%
  left_join(trumrapport, by = c("globalid" = "GlobalID"))

# Delete columns that aren't needed and/or duplicates
vagtrummor <- vagtrummor %>%
  mutate(
    # --- compare as character (safe for mixed types), then convert back where needed ---
    across(
      c(
        utloppstyp, Utloppstyp,
        utloppsmaterial, `Utlopp mat.`,
        inloppstyp, Inloppstyp,
        inloppsmaterial, `Inlopp mat.`,
        
        `utloppsbredd/diameter (mm)`, `Utlopp bredd`,
        `utloppshöjd (mm)`, `Utlopp höjd`,
        `inloppsbredd/diameter (mm)`, `Inlopp bredd`,
        `inloppshöjd (mm)`, `Inlopp höjd`,
        
        `skyddsanordning inlopp`, `Skyddsanordning Inlopp`,
        `skyddsanordning utlopp`, `Skyddsanordning utlopp`,
        
        `vägtrumlängd (m)`, `Trumlängd (m)`,
        `vägnummer`, `Vägnummer`,
        
        `ihopkoppling skarvar`, `Ihopkoppling skarvar`,
        erosionsskydd, Erosionsskydd,
        
        `antal trumdelar`, `Antal trummor på plats`,
        infodrad, Infodrad
      ),
      as.character
    ),
    
    # --- updates (prefer the capitalized column when it is not NA and differs) ---
    utloppstyp = if_else(
      !is.na(Utloppstyp) & (is.na(utloppstyp) | utloppstyp != Utloppstyp),
      Utloppstyp, utloppstyp
    ),
    utloppsmaterial = if_else(
      !is.na(`Utlopp mat.`) & (is.na(utloppsmaterial) | utloppsmaterial != `Utlopp mat.`),
      `Utlopp mat.`, utloppsmaterial
    ),
    inloppstyp = if_else(
      !is.na(Inloppstyp) & (is.na(inloppstyp) | inloppstyp != Inloppstyp),
      Inloppstyp, inloppstyp
    ),
    inloppsmaterial = if_else(
      !is.na(`Inlopp mat.`) & (is.na(inloppsmaterial) | inloppsmaterial != `Inlopp mat.`),
      `Inlopp mat.`, inloppsmaterial
    ),
    
    `utloppsbredd/diameter (mm)` = if_else(
      !is.na(`Utlopp bredd`) &
        (is.na(`utloppsbredd/diameter (mm)`) | `utloppsbredd/diameter (mm)` != `Utlopp bredd`),
      `Utlopp bredd`, `utloppsbredd/diameter (mm)`
    ),
    `utloppshöjd (mm)` = if_else(
      !is.na(`Utlopp höjd`) &
        (is.na(`utloppshöjd (mm)`) | `utloppshöjd (mm)` != `Utlopp höjd`),
      `Utlopp höjd`, `utloppshöjd (mm)`
    ),
    `inloppsbredd/diameter (mm)` = if_else(
      !is.na(`Inlopp bredd`) &
        (is.na(`inloppsbredd/diameter (mm)`) | `inloppsbredd/diameter (mm)` != `Inlopp bredd`),
      `Inlopp bredd`, `inloppsbredd/diameter (mm)`
    ),
    `inloppshöjd (mm)` = if_else(
      !is.na(`Inlopp höjd`) &
        (is.na(`inloppshöjd (mm)`) | `inloppshöjd (mm)` != `Inlopp höjd`),
      `Inlopp höjd`, `inloppshöjd (mm)`
    ),
    
    `skyddsanordning inlopp` = if_else(
      !is.na(`Skyddsanordning Inlopp`) &
        (is.na(`skyddsanordning inlopp`) | `skyddsanordning inlopp` != `Skyddsanordning Inlopp`),
      `Skyddsanordning Inlopp`, `skyddsanordning inlopp`
    ),
    `skyddsanordning utlopp` = if_else(
      !is.na(`Skyddsanordning utlopp`) &
        (is.na(`skyddsanordning utlopp`) | `skyddsanordning utlopp` != `Skyddsanordning utlopp`),
      `Skyddsanordning utlopp`, `skyddsanordning utlopp`
    ),
    
    `vägtrumlängd (m)` = if_else(
      !is.na(`Trumlängd (m)`) &
        (is.na(`vägtrumlängd (m)`) | `vägtrumlängd (m)` != `Trumlängd (m)`),
      `Trumlängd (m)`, `vägtrumlängd (m)`
    ),
    `vägnummer` = if_else(
      !is.na(Vägnummer) & (is.na(`vägnummer`) | `vägnummer` != Vägnummer),
      Vägnummer, `vägnummer`
    ),
    
    `ihopkoppling skarvar` = if_else(
      !is.na(`Ihopkoppling skarvar`) &
        (is.na(`ihopkoppling skarvar`) | `ihopkoppling skarvar` != `Ihopkoppling skarvar`),
      `Ihopkoppling skarvar`, `ihopkoppling skarvar`
    ),
    erosionsskydd = if_else(
      !is.na(Erosionsskydd) & (is.na(erosionsskydd) | erosionsskydd != Erosionsskydd),
      Erosionsskydd, erosionsskydd
    ),
    
    `antal trumdelar` = if_else(
      !is.na(`Antal trummor på plats`) &
        (is.na(`antal trumdelar`) | `antal trumdelar` != `Antal trummor på plats`),
      `Antal trummor på plats`, `antal trumdelar`
    ),
    infodrad = if_else(
      !is.na(Infodrad) & (is.na(infodrad) | infodrad != Infodrad),
      Infodrad, infodrad
    )
  ) %>%
  # --- drop the duplicate source columns ---
  select(
    -Utloppstyp, -`Utlopp mat.`, -Inloppstyp, -`Inlopp mat.`,
    -`Utlopp bredd`, -`Utlopp höjd`, -`Inlopp bredd`, -`Inlopp höjd`,
    -`Skyddsanordning Inlopp`, -`Skyddsanordning utlopp`,
    -`Trumlängd (m)`, -Vägnummer,
    -`Ihopkoppling skarvar`, -Erosionsskydd,
    -`Antal trummor på plats`, -Infodrad,
    -`Foto 1`, -`Foto 2`, -`Foto 3`, -`Foto 4`,-`Foto 5`,-`Foto 6`,-`Foto 7`,-`Foto 8`,
    -`googlemaps`,-`googleStreet`
  ) %>%
  # --- convert selected columns back to numeric ---
  mutate(
    across(
      c(
        `utloppsbredd/diameter (mm)`,
        `utloppshöjd (mm)`,
        `inloppsbredd/diameter (mm)`,
        `inloppshöjd (mm)`,
        `vägtrumlängd (m)`,
        `vägnummer`,
        `antal trumdelar`
      ),
      ~ suppressWarnings(as.numeric(.x))
    )
  )

# Export as shapefile for Anders
vagtrummor_sf <- vagtrummor
names(vagtrummor_sf) <- unique_short_names(names(vagtrummor_sf), max_len = 10)

launder10 <- function(nm) toupper(substr(gsub("[^A-Za-z0-9_]+","_", nm), 1, 10))
nm <- names(vagtrummor_sf)
nm[launder10(nm) %in% launder10(nm)[duplicated(launder10(nm))]]

sf::st_write(
  vagtrummor_sf,
  "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Uppdatering 2026/Vägtrummor/vagtrummor_2025.shp",
  driver = "ESRI Shapefile",
  delete_dsn = TRUE
)



library(sf)
library(dplyr)
library(readxl)
library(tigris)

options(tigris_use_cache = TRUE)

data_dir <- "data-raw"

# 1) Load full flowlines
flow_full <- readRDS(file.path(data_dir, "flowlines_precomputed_full.rds"))

# 1a) Trim flowlines to only the columns needed by the app
keep_cols <- c(
  "Hydroseq", "QE_MA",           # basic
  # %WW columns
  grep("^pct_mean_", names(flow_full), value = TRUE),
  grep("^pct_min_",  names(flow_full), value = TRUE),
  grep("^pct_max_",  names(flow_full), value = TRUE),
  grep("^pct_mean_.._nhd$", names(flow_full), value = TRUE),
  # WW cumulative flow columns
  grep("^WW_cum_mean_", names(flow_full), value = TRUE),
  grep("^WW_cum_low_",  names(flow_full), value = TRUE),
  grep("^WW_cum_high_", names(flow_full), value = TRUE),
  grep("^WW_cum_sd_",   names(flow_full), value = TRUE),
  # gage meta for popups
  c("gage_used", "flow_source", "start_year", "end_year", "n_obs")
)

keep_cols <- intersect(keep_cols, names(flow_full))
flow_small <- flow_full[, keep_cols]

# Make sure geometry is WGS84 (EPSG 4326) for leaflet
flow_small <- st_transform(flow_small, 4326)

# Save trimmed flowlines for the app
saveRDS(flow_small, file.path(data_dir, "flowlines_small.rds"))


# 2) Build CEJST + rurality once and save ----------------------------------

cejst <- read_excel(file.path(data_dir, "ColoradoCEJSTData.xlsx")) %>%
  rename(
    GEOID          = `Census tract 2010 ID`,
    wastewater_pct = `Wastewater discharge (percentile)`,
    low_income     = `Is low income?`
  ) %>%
  mutate(GEOID = sprintf("%011.0f", GEOID))

# 2010 tracts for CO
tracts10 <- tracts(state = "08", year = 2010, class = "sf") %>%
  st_make_valid() %>%
  st_transform(5070) %>%
  mutate(GEOID10 = sprintf("%011s", as.character(GEOID10)))

# tract areas (km2)
tract_area <- tracts10 %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  st_drop_geometry() %>%
  group_by(GEOID10) %>%
  summarise(area_km2 = sum(area_km2, na.rm = TRUE), .groups = "drop")

# CEJST pop by GEOID10
cejst_pop <- cejst %>%
  mutate(GEOID10 = sprintf("%011s", as.character(GEOID))) %>%
  select(GEOID10, total_pop = `Total population`)

# Rurality classification
ru_base <- tract_area %>%
  left_join(cejst_pop, by = "GEOID10") %>%
  mutate(
    pop_km2 = ifelse(is.finite(total_pop) & area_km2 > 0, total_pop / area_km2, NA_real_),
    rurality = case_when(
      is.finite(pop_km2) & pop_km2 < 50    ~ "Rural",
      is.finite(pop_km2) & pop_km2 < 1000  ~ "Suburban",
      is.finite(pop_km2) & pop_km2 >= 1000 ~ "Urban",
      TRUE                                 ~ NA_character_
    )
  ) %>%
  select(GEOID10, rurality)

# Attach rurality back to CEJST table
cejst_with_ru <- cejst %>%
  mutate(GEOID10 = sprintf("%011s", as.character(GEOID))) %>%
  left_join(ru_base, by = "GEOID10") %>%
  select(-GEOID10)

# Save CEJST+ rurality without geometry
saveRDS(cejst_with_ru, file.path(data_dir, "cejst_with_rurality.rds"))


# 3) Precompute flowlines–CEJST join (for Equity & Seasonality tabs) -----

# use the trimmed flowlines in 4326
flow_small_4326 <- flow_small   # already 4326

# 2010 tracts in WGS84 + CEJST attributes
tracts_sf <- tracts10 %>%
  st_transform(4326) %>%
  left_join(
    cejst_with_ru %>%
      mutate(GEOID10 = sprintf("%011s", as.character(GEOID))) %>%
      select(GEOID10, GEOID, everything()),
    by = "GEOID10"
  )

# spatial join: each flowline gets tract + CEJST fields
fl_cejst_sf <- st_join(flow_small_4326, tracts_sf, left = TRUE)

# drop geometry to make a lean table for Equity/Seasonality
fl_cejst <- st_drop_geometry(fl_cejst_sf)

saveRDS(fl_cejst, file.path(data_dir, "flowlines_cejst_table.rds"))

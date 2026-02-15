library(tidyverse)
library(readxl)
library(sf)
library(readr)

data_dir <- "data-raw"

paths <- list(
  dwtp          = file.path(data_dir, "filtereddwtpCOFinal.csv"),
  wwtp          = file.path(data_dir, "PermittedPOTW.xlsx"),
  monthly_flows = file.path(data_dir, "CO_Seasonal_Flow_Summary_Feb2026.csv")
)

# --- Constants (same as app) -------------------------------------------------
mon_codes    <- sprintf("%02d", 1:12)
month_labels <- setNames(month.name, mon_codes)

safe_round <- function(x) ifelse(is.finite(x), round(x, 1), "NA")

# -------------------------------------------------------------------
# 1) Drinking water treatment plants → geocoded_dwtp_app.rds
# -------------------------------------------------------------------

geocoded_dwtp <- read.csv(paths$dwtp) %>%
  rename(
    `PWS Name`          = PWS.Name,
    `Population Served` = Population.Served,
    `Primary Source`    = Primary.Source
  )
# (Assumes Longitude / Latitude columns already exist)

saveRDS(
  geocoded_dwtp,
  file.path(data_dir, "geocoded_dwtp_app.rds")
)

# -------------------------------------------------------------------
# 2) WWTP design data (points) → wwtp_pts_app.rds
# -------------------------------------------------------------------

wwtp_df <- read_excel(paths$wwtp) %>%
  filter(!is.na(LATITUDE83), !is.na(LONGITUDE83)) %>%
  rename(
    Facility   = FACILITY_NAME,
    Latitude   = LATITUDE83,
    Longitude  = LONGITUDE83,
    DesignFlow = TOTAL_DESIGN_FLOW_NMBR
  )

wwtp_pts <- st_as_sf(
  wwtp_df,
  coords = c("Longitude", "Latitude"),
  crs    = 4326
) %>%
  # drop any huge columns you don't need in the app
  dplyr::select(Facility, DesignFlow, geometry)

saveRDS(
  wwtp_pts,
  file.path(data_dir, "wwtp_pts_app.rds")
)

# -------------------------------------------------------------------
# 3) USGS gages + popup strings → usgs_gages_sf_app.rds
# -------------------------------------------------------------------

usgs_gages <- read_csv(
  paths$monthly_flows,
  show_col_types = FALSE
)

# Build popup string per gage with monthly mean±sd and low/high
usgs_gages$popup_str <- sapply(seq_len(nrow(usgs_gages)), function(i) {
  lines <- sapply(mon_codes, function(mm) {
    paste0(
      month_labels[mm], " mean±sd: ",
      safe_round(usgs_gages[[paste0("mean_cfs_", mm)]][i]), " cfs ",
      "(sd ",  safe_round(usgs_gages[[paste0("sd_cfs_",   mm)]][i]), "; ",
      "low ", safe_round(usgs_gages[[paste0("low_cfs_",  mm)]][i]), ", ",
      "high ",safe_round(usgs_gages[[paste0("high_cfs_", mm)]][i]), ")"
    )
  })
  paste0(
    "<b>", usgs_gages$station_nm[i], "</b> (", usgs_gages$site_no[i], ")<br>",
    paste(lines, collapse = "<br>"), "<br><br>",
    "<i>Years: ", usgs_gages$start_year[i], "–", usgs_gages$end_year[i],
    ", N = ", usgs_gages$n_obs[i], "</i>"
  )
})

usgs_gages_sf <- st_as_sf(
  usgs_gages,
  coords = c("dec_long_va", "dec_lat_va"),
  crs    = 4326
)

# You can drop unused columns to shrink it a bit:
usgs_gages_sf_app <- usgs_gages_sf %>%
  dplyr::select(
    site_no,
    station_nm,
    start_year,
    end_year,
    n_obs,
    popup_str,
    geometry
  )

saveRDS(
  usgs_gages_sf_app,
  file.path(data_dir, "usgs_gages_sf_app.rds")
)

cat("Preprocessing complete. Wrote:\n",
    "  - geocoded_dwtp_app.rds\n",
    "  - wwtp_pts_app.rds\n",
    "  - usgs_gages_sf_app.rds\n")

# ============================================================
# ReSource Tracker - Full Preprocessing Script
#   - Builds full flowlines with %WW and gage denominators
#   - Creates app-ready RDS files:
#       * flowlines_precomputed_full.rds
#       * flowlines_small.rds
#       * geocoded_dwtp_app.rds
#       * wwtp_pts_app.rds
#       * usgs_gages_sf_app.rds
#       * cejst_with_rurality.rds (UR10-based)
#       * tracts_sf_app.rds 
#       * flowlines_cejst_table.rds
# ============================================================

# --- Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(readr)
library(sf)
library(tigris)
library(viridisLite)
library(here)

setwd("/Users/kylieboenischoakes/Documents/ResourceTracker/ReSourceTracker")

options(tigris_use_cache = TRUE)

# --- Base paths --------------------------------------------------------------
data_dir <- "data-raw"

paths <- list(
  dwtp          = file.path(data_dir, "filtereddwtpCOFinal.csv"),
  cejst         = file.path(data_dir, "ColoradoCEJSTData.xlsx"),
  matched_dmr   = file.path(data_dir, "matchedDMRwwtpupdated6.csv"),
  wwtp          = file.path(data_dir, "PermittedPOTW.xlsx"),
  flowlines     = file.path(data_dir, "Flowlines_Colorado_Filtered.gpkg"),
  monthly_flows = file.path(data_dir, "CO_Seasonal_Flow_Summary_Feb2026.csv"),
  gages_sf      = file.path(data_dir, "processed_gages_sf_combinedV4.gpkg"),
  ur10          = file.path(data_dir, "CO_2010_URBANRURAL_TRACTS.csv")
)

# --- Helper functions --------------------------------------------------------

check_required_file <- function(path, label, url = NULL) {
  if (!file.exists(path)) {
    msg <- paste0(
      "Missing required data file: ", label, "\n",
      "Expected at:\n  ", normalizePath(path, winslash = "/", mustWork = FALSE), "\n\n"
    )
    if (!is.null(url)) {
      msg <- paste0(
        msg,
        "Download it from:\n  ", url, "\n\n",
        "Save it at the path above and then rerun the script.\n"
      )
    }
    stop(msg, call. = FALSE)
  }
}

safe_round <- function(x) ifelse(is.finite(x), round(x, 1), "NA")

# Simple % helper
compute_pct <- function(ww, denom) {
  ifelse(
    is.na(denom) | is.na(ww) | denom <= 0,
    NA_real_,
    pmin(100, pmax(0, 100 * ww / denom))
  )
}

# --- Required file checks ----------------------------------------------------
check_required_file(
  paths$flowlines,
  "Flowlines_Colorado_Filtered.gpkg",
  "https://github.com/sheldonmasters/Masters-Research-Group/releases/tag/v0.1.0"
)

# (others could be checked similarly if you want strictness)
check_required_file(paths$dwtp,        "filtereddwtpCOFinal.csv")
check_required_file(paths$cejst,       "ColoradoCEJSTData.xlsx")
check_required_file(paths$matched_dmr, "matchedDMRwwtpupdated6.csv")
check_required_file(paths$wwtp,        "PermittedPOTW.xlsx")
check_required_file(paths$monthly_flows, "CO_Seasonal_Flow_Summary_Feb2026.csv")
check_required_file(paths$gages_sf,    "processed_gages_sf_combinedV4.gpkg")
check_required_file(paths$ur10,        "CO_2010_URBANRURAL_TRACTS.csv")

# --- Constants ---------------------------------------------------------------
mon_codes    <- sprintf("%02d", 1:12)
month_labels <- setNames(month.name, mon_codes)

# ======================================================================
# 1) Load CEJST + UR10 rurality + 2010 tracts 
# ======================================================================

# CEJST table (attributes only)
cejst <- read_excel(paths$cejst) %>%
  rename(
    GEOID          = `Census tract 2010 ID`,
    wastewater_pct = `Wastewater discharge (percentile)`,
    low_income     = `Is low income?`
  ) %>%
  mutate(
    GEOID = sprintf("%011.0f", GEOID),
    status = dplyr::case_when(
      wastewater_pct > 90 & low_income ~ "Both",
      wastewater_pct > 90              ~ "High Wastewater",
      low_income                       ~ "Low Income",
      TRUE                             ~ NA_character_
    )
  )

# 2010 tracts for Colorado
tracts10 <- tracts(state = "08", year = 2010, class = "sf") %>%
  st_make_valid() %>%
  st_transform(5070) %>%
  mutate(GEOID10 = sprintf("%011s", as.character(GEOID10)))

# UR10 rurality classification
ur10 <- read_csv(paths$ur10, show_col_types = FALSE) %>%
  mutate(
    GEOID10  = sprintf("%011s", as.character(GEOID)),
    rurality = as.character(rurality)  # 
  ) %>%
  select(GEOID10, rurality)

# Attach UR10 rurality to CEJST table
cejst_with_ru <- cejst %>%
  mutate(GEOID10 = sprintf("%011s", as.character(GEOID))) %>%
  left_join(ur10, by = "GEOID10") %>%
  select(-GEOID10)

# ======================================================================
# 2) Load / prepare DWTPS, WWTPs, DMR flows, flowlines, gages
# ======================================================================

# Drinking water treatment plants (for app & popups)
geocoded_dwtp <- read.csv(paths$dwtp) %>%
  rename(
    `PWS Name`          = PWS.Name,
    `Population Served` = Population.Served,
    `Primary Source`    = Primary.Source
  )

# WWTP design data
wwtp_df <- read_excel(paths$wwtp) %>%
  filter(!is.na(LATITUDE83), !is.na(LONGITUDE83)) %>%
  rename(
    Facility   = FACILITY_NAME,
    Latitude   = LATITUDE83,
    Longitude  = LONGITUDE83,
    DesignFlow = TOTAL_DESIGN_FLOW_NMBR
  )

# Flowlines (NHDPlus-based network)
flowlines <- st_read(paths$flowlines)

# DMR monthly flows matched to NHDPlus COMIDs
matched_dmr <- read_csv(
  paths$matched_dmr,
  col_types = cols(.default = col_guess(), geometry = col_skip(), `...1` = col_skip()),
  show_col_types = FALSE
)

dmr_monthly <- matched_dmr %>%
  mutate(
    matched_COMID = as.character(matched_COMID),
    mon_code      = sprintf("%02d", as.integer(mon_code))
  ) %>%
  filter(mon_code %in% mon_codes)

# ======================================================================
# 3) Collapse DMR to COMID–month, then accumulate WW downstream
# ======================================================================

# 3.1) Average within Facility–COMID–month to collapse duplicate outfalls
dmr_monthly_fac <- dmr_monthly %>%
  group_by(Facility, matched_COMID, mon_code) %>%
  summarise(
    mean_dmr_cfs_fac = mean(mean_dmr_cfs, na.rm = TRUE),
    sd_dmr_cfs_fac   = mean(sd_dmr_cfs,   na.rm = TRUE),
    .groups = "drop"
  )

# 3.2) Sum across facilities per COMID-month (track variance)
dmr_monthly_comid <- dmr_monthly_fac %>%
  mutate(var_dmr_cfs_fac = sd_dmr_cfs_fac^2) %>%
  group_by(matched_COMID, mon_code) %>%
  summarise(
    WW_mean = sum(mean_dmr_cfs_fac, na.rm = TRUE),
    WW_var  = sum(var_dmr_cfs_fac,  na.rm = TRUE),
    .groups = "drop"
  )

# 3.3) Wide-format DMR table (WW_mean_ mm, WW_var_ mm)
dmr_monthly_wide <- dmr_monthly_comid %>%
  pivot_wider(
    names_from  = mon_code,
    values_from = c(WW_mean, WW_var),
    names_sep   = "_"
  )

# 3.4) Map WWTPs to flowlines via nearest COMID (for completeness, if needed)
wwtp_pts <- st_as_sf(wwtp_df, coords = c("Longitude", "Latitude"), crs = 4326)
wwtp_pts <- st_transform(wwtp_pts, st_crs(flowlines))
wwtp_pts$matched_COMID <- flowlines$COMID[st_nearest_feature(wwtp_pts, flowlines)]

flowlines <- flowlines %>%
  mutate(COMID = as.character(COMID)) %>%
  left_join(dmr_monthly_wide, by = c("COMID" = "matched_COMID")) %>%
  left_join(
    wwtp_pts %>%
      st_drop_geometry() %>%
      distinct(matched_COMID, .keep_all = TRUE) %>%
      select(Facility, matched_COMID) %>%
      mutate(matched_COMID = as.character(matched_COMID)),
    by = c("COMID" = "matched_COMID")
  )

# --- Downstream WW Flow & Contributor Accumulation (monthly) -----------------
flowlines$Hydroseq   <- as.character(flowlines$Hydroseq)
flowlines$DnHydroseq <- as.character(flowlines$DnHydroseq)

flow_map <- flowlines %>%
  st_drop_geometry() %>%
  select(
    Hydroseq, DnHydroseq,
    starts_with("WW_mean_"),
    starts_with("WW_var_")
  ) %>%
  mutate(across(starts_with("WW_"), as.numeric)) %>%
  arrange(desc(as.numeric(Hydroseq)))

flow_map <- flow_map %>%
  group_by(Hydroseq) %>%
  summarise(
    DnHydroseq = first(DnHydroseq),
    across(starts_with("WW_mean_"), ~sum(.x, na.rm = TRUE)),
    across(starts_with("WW_var_"),  ~sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(desc(as.numeric(Hydroseq)))

idx    <- setNames(seq_len(nrow(flow_map)), flow_map$Hydroseq)
dn_idx <- idx[flow_map$DnHydroseq]

for (mm in mon_codes) {
  flow_map[[paste0("WW_cum_mean_", mm)]] <- dplyr::coalesce(flow_map[[paste0("WW_mean_", mm)]], 0)
  flow_map[[paste0("WW_cum_var_",  mm)]] <- dplyr::coalesce(flow_map[[paste0("WW_var_",  mm)]], 0)
}

for (i in seq_len(nrow(flow_map))) {
  j <- dn_idx[i]
  if (!is.na(j)) {
    for (mm in mon_codes) {
      flow_map[[paste0("WW_cum_mean_", mm)]][j] <- flow_map[[paste0("WW_cum_mean_", mm)]][j] +
        flow_map[[paste0("WW_cum_mean_", mm)]][i]
      flow_map[[paste0("WW_cum_var_",  mm)]][j] <- flow_map[[paste0("WW_cum_var_",  mm)]][j] +
        flow_map[[paste0("WW_cum_var_",  mm)]][i]
    }
  }
}

for (mm in mon_codes) {
  sd_col <- paste0("WW_cum_sd_", mm)
  flow_map[[sd_col]] <- sqrt(pmax(flow_map[[paste0("WW_cum_var_", mm)]], 0))
  flow_map[[paste0("WW_cum_low_",  mm)]] <- pmax(flow_map[[paste0("WW_cum_mean_", mm)]] - flow_map[[sd_col]], 0)
  flow_map[[paste0("WW_cum_high_", mm)]] <-      flow_map[[paste0("WW_cum_mean_", mm)]] + flow_map[[sd_col]]
}

flowlines <- flowlines %>%
  left_join(
    flow_map %>%
      select(
        Hydroseq,
        starts_with("WW_cum_mean_"),
        starts_with("WW_cum_sd_"),
        starts_with("WW_cum_low_"),
        starts_with("WW_cum_high_")
      ) %>%
      distinct(),
    by = "Hydroseq"
  )

# ======================================================================
# 5) Load monthly gage flows, map to COMID, propagate down network
# ======================================================================

monthly_flows <- read_csv(paths$monthly_flows, show_col_types = FALSE)

# Gage monthly stats: rename to Qgage_* / Qgage_sd_*
gage_monthlies <- monthly_flows %>%
  select(
    site_no,
    starts_with("mean_cfs_"),
    starts_with("sd_cfs_"),
    start_year, end_year, n_obs
  ) %>%
  rename_with(~ gsub("mean_cfs_", "Qgage_",    .x), starts_with("mean_cfs_")) %>%
  rename_with(~ gsub("sd_cfs_",   "Qgage_sd_", .x), starts_with("sd_cfs_"))

# Build Qgage_var_*
for (mm in mon_codes) {
  sd_name  <- paste0("Qgage_sd_",  mm)
  var_name <- paste0("Qgage_var_", mm)
  if (sd_name %in% names(gage_monthlies)) {
    gage_monthlies[[var_name]] <- gage_monthlies[[sd_name]]^2
  }
}

# Gage-to-COMID spatial mapping (best_COMID)
gages_sf <- st_read(paths$gages_sf, layer = "gages_sf")
gages_sf$best_COMID <- as.character(gages_sf$best_COMID)

flowlines$Hydroseq   <- as.character(flowlines$Hydroseq)
flowlines$DnHydroseq <- as.character(flowlines$DnHydroseq)
flowlines$COMID      <- as.character(flowlines$COMID)

gages_joined <- gages_sf %>%
  st_drop_geometry() %>%
  select(site_no, best_COMID) %>%
  left_join(gage_monthlies, by = "site_no")

# One gage per COMID: prioritize longer & more recent records
gages_per_comid <- gages_joined %>%
  group_by(best_COMID) %>%
  arrange(desc(n_obs), desc(end_year), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

# Attach gage stats to flowlines
flowlines <- flowlines %>%
  left_join(
    gages_per_comid %>% rename(COMID = best_COMID),
    by = "COMID"
  )

# --- Build flow_order for propagation ---------------------------------------
flow_order <- flowlines %>%
  st_drop_geometry() %>%
  select(Hydroseq, DnHydroseq, site_no, QE_MA, starts_with("Qgage_")) %>%
  arrange(as.numeric(Hydroseq))

# Low-flow screen
flow_order <- flow_order %>%
  mutate(
    low_flow = if_all(all_of(paste0("Qgage_", mon_codes)), ~ . < 0.5),
    site_no  = if_else(low_flow, NA_character_, site_no)
  )

for (mm in mon_codes) {
  mcol  <- paste0("Qgage_",     mm)
  sdcol <- paste0("Qgage_sd_",  mm)
  vcol  <- paste0("Qgage_var_", mm)
  if (mcol  %in% names(flow_order)) flow_order[[mcol]]  <- ifelse(flow_order$low_flow, NA_real_, flow_order[[mcol]])
  if (sdcol %in% names(flow_order)) flow_order[[sdcol]] <- ifelse(flow_order$low_flow, NA_real_, flow_order[[sdcol]])
  if (vcol  %in% names(flow_order)) flow_order[[vcol]]  <- ifelse(flow_order$low_flow, NA_real_, flow_order[[vcol]])
}
flow_order <- flow_order %>% select(-low_flow)

# One row per Hydroseq
flow_order <- flow_order %>%
  group_by(Hydroseq) %>%
  slice(1) %>%
  ungroup()

# Manual overrides
forced_hseq <- c(
  "550025960",
  "550020503",
  "390005386",
  "390025451",
  "390005505"
)

# Same-seg seeds
same_seg_ww_adj_seed <- c(
  "550022475", # Loveland
  "550015536", # Robert H White
  "550062530", # Louisville
  "390045364", # Tri-lakes
  "550025946", # Boulder
  "550019184", # Mulberry
  "760015713", # Aspen
  "390005515", # Las Animas
  "550018746"  # BoxElder
)

flow_order <- flow_order %>%
  mutate(
    force_gage = Hydroseq %in% forced_hseq,
    gage_hseq  = if_else(!is.na(site_no), Hydroseq, NA_character_)
  )

# Downstream index
dn_index <- match(flow_order$DnHydroseq, flow_order$Hydroseq)

# Initialize propagation columns
flow_order$gage_used <- flow_order$site_no
for (mm in mon_codes) {
  mcol <- paste0("Qgage_",     mm)
  vcol <- paste0("Qgage_var_", mm)
  flow_order[[paste0(mcol, "_prop")]] <- as.numeric(flow_order[[mcol]])
  flow_order[[paste0(vcol, "_prop")]] <- as.numeric(flow_order[[vcol]])
}

# Mark confluences
flow_order$is_confluence <- flow_order$Hydroseq %in% flow_order$DnHydroseq[duplicated(flow_order$DnHydroseq)]

# Join local WW onto flow_order
ww_local_wide <- flow_map %>%
  select(Hydroseq, starts_with("WW_mean_"))
flow_order <- flow_order %>%
  left_join(ww_local_wide, by = "Hydroseq") %>%
  mutate(across(starts_with("WW_mean_"), ~coalesce(as.numeric(.x), 0)))

# Bake local WW into Qgage_prop at same-seg seeds
for (mm in mon_codes) {
  mcol     <- paste0("Qgage_", mm, "_prop")
  ww_col   <- paste0("WW_mean_", mm)
  seed_idx <- which(flow_order$Hydroseq %in% same_seg_ww_adj_seed)
  for (i in seed_idx) {
    ww_val <- coalesce(as.numeric(flow_order[[ww_col]][i]), 0)
    flow_order[[mcol]][i] <- flow_order[[mcol]][i] + ww_val
  }
}

# Propagate downstream
known   <- which(rowSums(is.finite(as.matrix(flow_order[, paste0("Qgage_", mon_codes)]))) > 0)
queue   <- known
visited <- rep(FALSE, nrow(flow_order))

while (length(queue) > 0) {
  i <- queue[1]
  queue <- queue[-1]
  
  j <- dn_index[i]
  if (is.na(j)) next
  
  if (!is.na(flow_order$site_no[j])) { visited[j] <- TRUE; next }
  if (visited[j]) next
  
  flow_i <- flow_order$QE_MA[i]
  flow_j <- flow_order$QE_MA[j]
  flows_similar <- is.finite(flow_i) && is.finite(flow_j) &&
    abs(flow_i - flow_j) / pmax(flow_j, 1e-6) < 0.5
  
  allow_propagate <- flows_similar || isTRUE(flow_order$force_gage[j])
  if (!allow_propagate) next
  
  should_add_ww <- is.na(flow_order$site_no[j]) |
    flow_order$Hydroseq[j] %in% same_seg_ww_adj_seed
  for (mm in mon_codes) {
    mcol <- paste0("Qgage_", mm)
    vcol <- paste0("Qgage_var_", mm)
    ww_local <- if (should_add_ww) coalesce(flow_order[[paste0("WW_mean_", mm)]][j], 0) else 0
    flow_order[[paste0(mcol, "_prop")]][j] <- flow_order[[paste0(mcol, "_prop")]][i] + ww_local
    flow_order[[paste0(vcol, "_prop")]][j] <- flow_order[[paste0(vcol, "_prop")]][i]
  }
  
  flow_order$gage_used[j] <- flow_order$gage_used[i]
  if (is.na(flow_order$gage_hseq[j])) flow_order$gage_hseq[j] <- flow_order$gage_hseq[i]
 
  
  visited[j] <- TRUE
  queue <- c(queue, j)
}

# Confluence handling
confluence_hseqs <- flow_order %>% filter(is_confluence) %>% pull(Hydroseq)

for (hseq in confluence_hseqs) {
  upstream_idxs <- which(flow_order$DnHydroseq == hseq & !is.na(flow_order$gage_used))
  if (length(upstream_idxs) >= 2) {
    summed_means <- sapply(mon_codes, function(mm)
      sum(flow_order[[paste0("Qgage_", mm, "_prop")]][upstream_idxs], na.rm = TRUE))
    summed_vars <- sapply(mon_codes, function(mm)
      sum(flow_order[[paste0("Qgage_var_", mm, "_prop")]][upstream_idxs], na.rm = TRUE))
    combined_gages <- paste(flow_order$gage_used[upstream_idxs], collapse = "+")
    
    current_hseq <- hseq
    repeat {
      idx <- which(flow_order$Hydroseq == current_hseq)
      if (length(idx) == 0 || !is.na(flow_order$site_no[idx])) break
      for (k in seq_along(mon_codes)) {
        mm <- mon_codes[k]
        ww_local <- coalesce(flow_order[[paste0("WW_mean_", mm)]][idx], 0)
        summed_means[k] <- summed_means[k] + ww_local
        flow_order[[paste0("Qgage_",     mm, "_prop")]][idx] <- summed_means[k]
        flow_order[[paste0("Qgage_var_", mm, "_prop")]][idx] <- summed_vars[k]
      }
      flow_order$gage_used[idx] <- combined_gages
      flow_order$gage_hseq[idx] <- NA_character_
      current_hseq <- flow_order$DnHydroseq[idx]
      if (is.na(current_hseq)) break
    }
  }
}

# Derive low/high bands
for (mm in mon_codes) {
  mcol  <- paste0("Qgage_",     mm, "_prop")
  vcol  <- paste0("Qgage_var_", mm, "_prop")
  sdvec <- sqrt(pmax(flow_order[[vcol]], 0))
  flow_order[[paste0("Qgage_low_",  mm, "_prop")]] <- pmax(flow_order[[mcol]] - sdvec, 0)
  flow_order[[paste0("Qgage_high_", mm, "_prop")]] <- flow_order[[mcol]] + sdvec
}

# Finalize QE columns
for (mm in mon_codes) {
  flow_order[[paste0("QE_", mm, "_final")]]      <- flow_order[[paste0("Qgage_", mm, "_prop")]]
  flow_order[[paste0("QE_", mm, "_low_final")]]  <- flow_order[[paste0("Qgage_low_",  mm, "_prop")]]
  flow_order[[paste0("QE_", mm, "_high_final")]] <- flow_order[[paste0("Qgage_high_", mm, "_prop")]]
}

# Attach gage flows to flowlines
flowlines <- flowlines %>%
  select(-site_no, -starts_with("Qgage_")) %>%
  left_join(
    flow_order %>%
      select(
        Hydroseq, site_no, gage_used, gage_hseq,
        matches("^QE_\\d{2}_final$"),
        matches("^QE_\\d{2}_low_final$"),
        matches("^QE_\\d{2}_high_final$")
      ),
    by = "Hydroseq"
  ) %>%
  mutate(
    flow_source = case_when(
      !is.na(site_no)                     ~ "gage_direct",
      is.na(site_no) & !is.na(gage_used)  ~ "gage_propagated",
      TRUE                                ~ "nhd_only"
    )
  )

# ======================================================================
# 6) Compute %WW (gage-based + NHDPlus-based) by month
# ======================================================================

for (mm in mon_codes) {
  flowlines[[paste0("QE_", mm, "_nhd")]] <- as.numeric(flowlines[[paste0("QE_", mm)]])
}

for (mm in mon_codes) {
  ww_mean <- paste0("WW_cum_mean_", mm)
  ww_low  <- paste0("WW_cum_low_",  mm)
  ww_high <- paste0("WW_cum_high_", mm)
  q_mean  <- paste0("QE_", mm, "_final")
  q_low   <- paste0("QE_", mm, "_low_final")
  q_high  <- paste0("QE_", mm, "_high_final")
  
  flowlines[[paste0("pct_mean_", mm, "_base")]] <- compute_pct(flowlines[[ww_mean]], flowlines[[q_mean]])
  flowlines[[paste0("pct_mean_", mm)]]          <- compute_pct(flowlines[[ww_mean]], flowlines[[q_mean]])
  flowlines[[paste0("pct_min_",  mm)]]          <- compute_pct(flowlines[[ww_low]],  flowlines[[q_high]])
  flowlines[[paste0("pct_max_",  mm)]]          <- compute_pct(flowlines[[ww_high]], flowlines[[q_low]])
  
  q_nhd <- paste0("QE_", mm, "_nhd")
  flowlines[[paste0("pct_mean_", mm, "_nhd")]] <- compute_pct(flowlines[[ww_mean]], flowlines[[q_nhd]])
  flowlines[[paste0("pct_min_",  mm, "_nhd")]] <- compute_pct(flowlines[[ww_low]],  flowlines[[q_nhd]])
  flowlines[[paste0("pct_max_",  mm, "_nhd")]] <- compute_pct(flowlines[[ww_high]], flowlines[[q_nhd]])
}

# Drop Z/M coords if present
flowlines <- st_zm(flowlines, drop = TRUE, what = "ZM")

# Save full precomputed flowlines
saveRDS(
  flowlines,
  file = file.path(data_dir, "flowlines_precomputed_full.rds")
)
message("Saved: data-raw/flowlines_precomputed_full.rds")

# ======================================================================
# 7) App-ready RDS: DWTPS, WWTP points, USGS gages
# ======================================================================

# 7.1) DWTPS
saveRDS(
  geocoded_dwtp,
  file.path(data_dir, "geocoded_dwtp_app.rds")
)

# 7.2) WWTP points in WGS84 for leaflet
wwtp_pts_app <- st_as_sf(
  wwtp_df,
  coords = c("Longitude", "Latitude"),
  crs    = 4326
) %>%
  select(Facility, DesignFlow, geometry)

saveRDS(
  wwtp_pts_app,
  file.path(data_dir, "wwtp_pts_app.rds")
)

# 7.3) USGS gages + popup strings (from monthly_flows)
usgs_gages <- monthly_flows

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

usgs_gages_sf_app <- usgs_gages_sf %>%
  select(
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

# ======================================================================
# 8) Trim flowlines for app + CEJST/UR10 joins
# ======================================================================

flow_full <- flowlines  # use the in-memory full object

keep_cols <- c(
  "Hydroseq", "QE_MA",
  grep("^pct_mean_", names(flow_full), value = TRUE),
  grep("^pct_min_",  names(flow_full), value = TRUE),
  grep("^pct_max_",  names(flow_full), value = TRUE),
  grep("^pct_mean_.._nhd$", names(flow_full), value = TRUE),
  grep("^WW_cum_mean_", names(flow_full), value = TRUE),
  grep("^WW_cum_low_",  names(flow_full), value = TRUE),
  grep("^WW_cum_high_", names(flow_full), value = TRUE),
  grep("^WW_cum_sd_",   names(flow_full), value = TRUE),
  c("gage_used", "flow_source", "start_year", "end_year", "n_obs")
)

keep_cols  <- intersect(keep_cols, names(flow_full))
flow_small <- flow_full[, keep_cols]

# Leaflet uses WGS84
flow_small <- st_transform(flow_small, 4326)

saveRDS(flow_small, file.path(data_dir, "flowlines_small.rds"))
message("Saved: data-raw/flowlines_small.rds")

# CEJST + UR10 rurality (no geometry)
saveRDS(cejst_with_ru, file.path(data_dir, "cejst_with_rurality.rds"))
message("Saved: data-raw/cejst_with_rurality.rds")

# 2010 tracts in WGS84 + CEJST (including UR10 rurality)
tracts_sf <- tracts10 %>%
  st_transform(4326) %>%
  left_join(
    cejst_with_ru %>%
      mutate(GEOID10 = sprintf("%011s", as.character(GEOID))) %>%
      select(GEOID10, GEOID, everything()),
    by = "GEOID10"
  )

# Save tracts for the app (census polygons + CEJST + rurality + status)
saveRDS(
  tracts_sf,
  file.path(data_dir, "tracts_sf_app.rds")
)
message("Saved: data-raw/tracts_sf_app.rds")

# Spatial join flowlines ↔ CEJST tracts
fl_cejst_sf <- st_join(flow_small, tracts_sf, left = TRUE)

fl_cejst <- st_drop_geometry(fl_cejst_sf)

saveRDS(fl_cejst, file.path(data_dir, "flowlines_cejst_table.rds"))
message("Saved: data-raw/flowlines_cejst_table.rds")

cat("Preprocessing complete. Wrote:\n",
    "  - flowlines_precomputed_full.rds\n",
    "  - flowlines_small.rds\n",
    "  - geocoded_dwtp_app.rds\n",
    "  - wwtp_pts_app.rds\n",
    "  - usgs_gages_sf_app.rds\n",
    "  - cejst_with_rurality.rds (UR10-based rurality)\n",
    "  - tracts_sf_app.rds\n",
    "  - flowlines_cejst_table.rds\n")


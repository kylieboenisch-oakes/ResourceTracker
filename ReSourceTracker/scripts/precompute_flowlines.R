
# --- Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(sf)
library(tigris)
library(viridisLite)
# --- Relative Paths ----------------------------------------------------------
data_dir <- "data-raw"

paths <- list(
  dwtp          = file.path(data_dir, "filtereddwtpCOFinal.csv"),
  cejst         = file.path(data_dir, "ColoradoCEJSTData.xlsx"),
  matched_dmr   = file.path(data_dir, "matchedDMRwwtpupdated6.csv"),
  wwtp          = file.path(data_dir, "PermittedPOTW.xlsx"),
  flowlines     = file.path(data_dir, "Flowlines_Colorado_Filtered.gpkg"),
  monthly_flows = file.path(data_dir, "CO_Seasonal_Flow_Summary_Feb2026.csv"),
  gages_sf      = file.path(data_dir, "processed_gages_sf_combinedV4.gpkg")
)

# ---- Required file checks ---------------------------------------------------
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
        "Save it at the path above and then rerun.\n"
      )
    }
    stop(msg, call. = FALSE)
  }
}

check_required_file(
  paths$flowlines,
  "Flowlines_Colorado_Filtered.gpkg",
  "https://github.com/sheldonmasters/Masters-Research-Group/releases/tag/v0.1.0"
)

# --- Constants ---------------------------------------------------------------
mon_codes    <- sprintf("%02d", 1:12)          
month_labels <- setNames(month.name, mon_codes)

# --- Load Datasets -----------------------------------------------------------

# Drinking water treatment plants
geocoded_dwtp <- read.csv(paths$dwtp) %>%
  rename(
    `PWS Name`          = PWS.Name,
    `Population Served` = Population.Served,
    `Primary Source`    = Primary.Source
  )

# CEJST data
cejst <- read_excel(paths$cejst) %>%
  rename(
    GEOID          = `Census tract 2010 ID`,
    wastewater_pct = `Wastewater discharge (percentile)`,
    low_income     = `Is low income?`
  ) %>%
  mutate(
    GEOID  = sprintf("%011.0f", GEOID),
    status = case_when(
      wastewater_pct > 90 & low_income ~ "Both",
      wastewater_pct > 90              ~ "High Wastewater",
      low_income                       ~ "Low Income",
      TRUE                             ~ NA_character_
    )
  )

# ------------------ 2010 TRACTS + DENSITY-BASED RURALITY --------------------
options(tigris_use_cache = TRUE)

# 2010 tracts for Colorado, equal-area CRS for area
tracts10 <- tracts(state = "08", year = 2010, class = "sf") %>%
  st_make_valid() %>%
  st_transform(5070) %>%
  mutate(
    GEOID10 = sprintf("%011s", as.character(GEOID10))
  )

# Area per tract (km^2)
tract_area <- tracts10 %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  st_drop_geometry() %>%
  group_by(GEOID10) %>%
  summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    .groups  = "drop"
  )

# CEJST population aligned to GEOID10
cejst_pop <- cejst %>%
  mutate(GEOID10 = sprintf("%011s", as.character(GEOID))) %>%
  select(GEOID10, total_pop = `Total population`)

# Density-based rurality classification
#   Rural    : pop_km2 <  50
#   Suburban : 50 ≤ pop_km2 < 1000
#   Urban    : pop_km2 ≥ 1000
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

# Attach rurality back to CEJST tract table
cejst <- cejst %>%
  mutate(GEOID10 = sprintf("%011s", as.character(GEOID))) %>%
  left_join(ru_base, by = "GEOID10") %>%
  select(-GEOID10)

# 2010 tracts + CEJST (including rurality), in WGS84 for app
tracts_sf <- tracts10 %>%
  st_transform(4326) %>%
  left_join(
    cejst %>%
      mutate(GEOID10 = sprintf("%011s", as.character(GEOID))) %>%
      select(GEOID10, GEOID, everything()),
    by = "GEOID10"
  )

# ------------------------- DMR (WW) mean ± SD (monthly) ----------------------
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

# 1) Average within Facility–COMID–month to collapse duplicate outfalls
#    (mean across outfalls; combine SDs conservatively by averaging SD field)
dmr_monthly_fac <- dmr_monthly %>%
  group_by(Facility, matched_COMID, mon_code) %>%
  summarise(
    mean_dmr_cfs_fac = mean(mean_dmr_cfs, na.rm = TRUE),
    sd_dmr_cfs_fac   = mean(sd_dmr_cfs,   na.rm = TRUE),
    .groups = "drop"
  )

# 2) Sum across facilities per COMID-month
# carry variance (sd^2) to accumulate uncertainty downstream
dmr_monthly_comid <- dmr_monthly_fac %>%
  mutate(var_dmr_cfs_fac = sd_dmr_cfs_fac^2) %>%
  group_by(matched_COMID, mon_code) %>%
  summarise(
    WW_mean = sum(mean_dmr_cfs_fac, na.rm = TRUE),
    WW_var  = sum(var_dmr_cfs_fac,  na.rm = TRUE),
    .groups = "drop"
  )

# Wide format: keep mean + variance 
dmr_monthly_wide <- dmr_monthly_comid %>%
  pivot_wider(
    names_from  = mon_code,
    values_from = c(WW_mean, WW_var),
    names_sep   = "_"
  )

# --- WWTP design data (for popups, markers) ----------------------------------
wwtp_df <- read_excel(paths$wwtp) %>%
  filter(!is.na(LATITUDE83), !is.na(LONGITUDE83)) %>%
  rename(
    Facility   = FACILITY_NAME,
    Latitude   = LATITUDE83,
    Longitude  = LONGITUDE83,
    DesignFlow = TOTAL_DESIGN_FLOW_NMBR
  )

# Flowlines
flowlines <- st_read(paths$flowlines)

# --- WWTP to Flowline Mapping ------------------------------------------------
wwtp_pts <- st_as_sf(wwtp_df, coords = c("Longitude", "Latitude"), crs = 4326)
wwtp_pts <- st_transform(wwtp_pts, st_crs(flowlines))
wwtp_pts$matched_COMID <- flowlines$COMID[st_nearest_feature(wwtp_pts, flowlines)]

# Join DMR monthly WW mean + variance (COMID-level) only
flowlines <- flowlines %>%
  mutate(COMID = as.character(COMID)) %>%
  left_join(dmr_monthly_wide, by = c("COMID" = "matched_COMID"))

# --- Downstream WW Flow & Contributor Accumulation (monthly) -----------------
flowlines$Hydroseq   <- as.character(flowlines$Hydroseq)
flowlines$DnHydroseq <- as.character(flowlines$DnHydroseq)

# Reduced routing table: Hydroseq, DnHydroseq, and WW mean/var by month
flow_map <- flowlines %>%
  st_drop_geometry() %>%
  select(
    Hydroseq, DnHydroseq,
    starts_with("WW_mean_"),
    starts_with("WW_var_")
  ) %>%
  mutate(across(starts_with("WW_"), as.numeric)) %>%
  arrange(desc(as.numeric(Hydroseq)))

# Aggregate any duplicated Hydroseq rows
# (sum mean and sum variance across duplicates)
flow_map <- flow_map %>%
  group_by(Hydroseq) %>%
  summarise(
    DnHydroseq = first(DnHydroseq),
    across(starts_with("WW_mean_"), ~sum(.x, na.rm = TRUE)),
    across(starts_with("WW_var_"),  ~sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(desc(as.numeric(Hydroseq)))

# Check: Hydroseq should decrease downstream
check_hseq <- flow_map %>%
  mutate(
    Hydroseq_num   = as.numeric(Hydroseq),
    DnHydroseq_num = as.numeric(DnHydroseq)
  ) %>%
  filter(!is.na(DnHydroseq_num))

if (any(check_hseq$DnHydroseq_num >= check_hseq$Hydroseq_num)) {
  warning("Some DnHydroseq have Hydroseq >= upstream segment; verify network ordering.")
}

# ------------------- CUMULATIVE WASTEWATER (numeric only) -------------------

# Hydroseq → row index, and downstream neighbor index vector
idx    <- setNames(seq_len(nrow(flow_map)), flow_map$Hydroseq)
dn_idx <- idx[flow_map$DnHydroseq]

# Initialize cumulative mean + variance
for (mm in mon_codes) {
  flow_map[[paste0("WW_cum_mean_", mm)]] <- dplyr::coalesce(flow_map[[paste0("WW_mean_", mm)]], 0)
  flow_map[[paste0("WW_cum_var_",  mm)]] <- dplyr::coalesce(flow_map[[paste0("WW_var_",  mm)]],  0)
}

# Accumulate wastewater downstream:
for (i in seq_len(nrow(flow_map))) {
  j <- dn_idx[i]
  if (!is.na(j)) {
    for (mm in mon_codes) {
      flow_map[[paste0("WW_cum_mean_", mm)]][j] <- flow_map[[paste0("WW_cum_mean_", mm)]][j] +
        flow_map[[paste0("WW_cum_mean_", mm)]][i]
      
      flow_map[[paste0("WW_cum_var_", mm)]][j] <- flow_map[[paste0("WW_cum_var_", mm)]][j] +
        flow_map[[paste0("WW_cum_var_", mm)]][i]
    }
  }
}

# Now compute cumulative SD, low, high downstream (derived from mean+var)
for (mm in mon_codes) {
  sd_col <- paste0("WW_cum_sd_", mm)
  flow_map[[sd_col]] <- sqrt(pmax(flow_map[[paste0("WW_cum_var_", mm)]], 0))
  
  flow_map[[paste0("WW_cum_low_",  mm)]] <- pmax(flow_map[[paste0("WW_cum_mean_", mm)]] - flow_map[[sd_col]], 0)
  flow_map[[paste0("WW_cum_high_", mm)]] <-       flow_map[[paste0("WW_cum_mean_", mm)]] + flow_map[[sd_col]]
}

# Join cumulative WW bands + contributors back to flowlines
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

# --- Load and Process Gage Flow Data (monthly) -------------------------------
# Monthly gage stats for each USGS site
monthly_flows <- read_csv(
  paths$monthly_flows,
  show_col_types = FALSE
)

# Rename columns to Qgage_* pattern for easier handling
gage_monthlies <- monthly_flows %>%
  select(
    site_no,
    starts_with("mean_cfs_"),
    starts_with("sd_cfs_"),
    start_year, end_year, n_obs
  ) %>%
  # rename means and sds
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


# Load gage-to-COMID spatial mapping
gages_sf <- st_read(
  paths$gages_sf,
  layer = "gages_sf"
)

gages_sf$best_COMID <- as.character(gages_sf$best_COMID)

# Ensure flowlines IDs are character
flowlines$Hydroseq   <- as.character(flowlines$Hydroseq)
flowlines$DnHydroseq <- as.character(flowlines$DnHydroseq)
flowlines$COMID      <- as.character(flowlines$COMID)

# Join gage monthly stats to each COMID (via best_COMID)
gages_joined <- gages_sf %>%
  st_drop_geometry() %>%
  select(site_no, best_COMID) %>%
  left_join(gage_monthlies, by = "site_no")

# De-duplicate: pick 1 best gage per COMID (prioritize longer and more recent records)
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

# --- Build flow order data for propagation (monthly) -------------------------
# Set up a flow_order table used for gage propagation down the network
flow_order <- flowlines %>%
  st_drop_geometry() %>%
  select(
    Hydroseq, DnHydroseq, site_no, QE_MA,
    starts_with("Qgage_")
  ) %>%
  arrange(as.numeric(Hydroseq))

# Low-flow screen: ignore gages where all monthly flows are < 0.5 cfs
flow_order <- flow_order %>%
  mutate(
    low_flow = if_all(all_of(paste0("Qgage_", mon_codes)), ~ . < 0.5), #changed to if_all on 1/28/26
    site_no  = if_else(low_flow, NA_character_, site_no)
  )

# Blank out Qgage_* where low_flow is TRUE
for (mm in mon_codes) {
  mcol  <- paste0("Qgage_",      mm)
  sdcol <- paste0("Qgage_sd_",   mm)
  vcol  <- paste0("Qgage_var_",  mm)
  
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

# --- Manual overrides for segments where NHDPlus is clearly wrong ----------
# These Hydroseqs will always accept propagated gage flows (force_gage = TRUE)
forced_hseq <- c("550025960", "550020503")  # add more Hydroseq IDs here if needed

flow_order <- flow_order %>%
  mutate(
    force_gage = Hydroseq %in% forced_hseq
  ) 

# Donor gage Hydroseq: segments with a direct gage know their own Hydroseq
flow_order <- flow_order %>%
  mutate(
    gage_hseq = if_else(!is.na(site_no), Hydroseq, NA_character_)
  )

# Precompute downstream index in flow_order for fast propagation
dn_index <- match(flow_order$DnHydroseq, flow_order$Hydroseq)

# Initialize propagation columns
flow_order$gage_used <- flow_order$site_no
for (mm in mon_codes) {
  mcol <- paste0("Qgage_",     mm)
  vcol <- paste0("Qgage_var_", mm)
  
  flow_order[[paste0(mcol, "_prop")]] <- as.numeric(flow_order[[mcol]])
  flow_order[[paste0(vcol, "_prop")]] <- as.numeric(flow_order[[vcol]])
}


# Mark confluences: any Hydroseq that appears as DnHydroseq more than once
flow_order$is_confluence <- flow_order$Hydroseq %in% flow_order$DnHydroseq[duplicated(flow_order$DnHydroseq)]

# Propagate downstream (monthly) where flows are similar
known <- which(rowSums(is.finite(as.matrix(flow_order[, paste0("Qgage_", mon_codes)]))) > 0)
queue <- known

# visited = this row has already been assigned propagated flows
visited <- rep(FALSE, nrow(flow_order))

while (length(queue) > 0) {
  i <- queue[1]
  queue <- queue[-1]
  
  j <- dn_index[i]
  if (is.na(j)) next
  
  # If downstream segment has a direct gage, never overwrite; treat as "done"
  if (!is.na(flow_order$site_no[j])) {
    visited[j] <- TRUE
    next
  }
  
  # If we've already assigned propagated flows to j, don't try again
  if (visited[j]) next
  
  # Similarity check (modeled mean annual flow)
  flow_i <- flow_order$QE_MA[i]
  flow_j <- flow_order$QE_MA[j]
  flows_similar <- is.finite(flow_i) && is.finite(flow_j) &&
    abs(flow_i - flow_j) / pmax(flow_j, 1e-6) < 0.5
  
  allow_propagate <- flows_similar || isTRUE(flow_order$force_gage[j])
  
  if (!allow_propagate) {
    next
  }
  
  # Copy monthly mean + variance from i → j
  for (mm in mon_codes) {
    mcol <- paste0("Qgage_",     mm)
    vcol <- paste0("Qgage_var_", mm)
    
    flow_order[[paste0(mcol, "_prop")]][j] <- flow_order[[paste0(mcol, "_prop")]][i]
    flow_order[[paste0(vcol, "_prop")]][j] <- flow_order[[paste0(vcol, "_prop")]][i]
  }
  
  # Inherit gage provenance
  flow_order$gage_used[j] <- flow_order$gage_used[i]
  if (is.na(flow_order$gage_hseq[j])) {
    flow_order$gage_hseq[j] <- flow_order$gage_hseq[i]
  }
  
  # Now lock j as assigned and continue downstream from it
  visited[j] <- TRUE
  queue <- c(queue, j)
}

# Confluence handling:
# For Hydroseqs with ≥2 upstream branches that each carry gage info,
# sum upstream flows to create a combined gage signal and propagate downstream.
confluence_hseqs <- flow_order %>% filter(is_confluence) %>% pull(Hydroseq)
for (hseq in confluence_hseqs) {
  upstream_idxs <- which(flow_order$DnHydroseq == hseq & !is.na(flow_order$gage_used))
  if (length(upstream_idxs) >= 2) {
    # Sum mean/variance over upstream branches for each month
    summed_means <- sapply(mon_codes, function(mm)
      sum(flow_order[[paste0("Qgage_",     mm, "_prop")]][upstream_idxs], na.rm = TRUE))
    
    summed_vars  <- sapply(mon_codes, function(mm)
      sum(flow_order[[paste0("Qgage_var_", mm, "_prop")]][upstream_idxs], na.rm = TRUE))
    
    # Store IDs of all upstream gages used in the combined branch
    combined_gages <- paste(flow_order$gage_used[upstream_idxs], collapse = "+")
    
    # Walk downstream from the confluence, applying the combined gage until a new gage is encountered
    current_hseq <- hseq
    repeat {
      idx <- which(flow_order$Hydroseq == current_hseq)
      if (length(idx) == 0 || !is.na(flow_order$site_no[idx])) break
      
      for (k in seq_along(mon_codes)) {
        mm <- mon_codes[k]
        flow_order[[paste0("Qgage_",     mm, "_prop")]][idx] <- summed_means[k]
        flow_order[[paste0("Qgage_var_", mm, "_prop")]][idx] <- summed_vars[k]
      }
      
      flow_order$gage_used[idx]  <- combined_gages
      flow_order$gage_hseq[idx]  <- NA_character_  # multiple gages → no single donor
      
      current_hseq <- flow_order$DnHydroseq[idx]
      if (is.na(current_hseq)) break
    }
  }
}

# Derive low/high bands from propagated mean + variance (mean ± 1 SD)
for (mm in mon_codes) {
  mcol  <- paste0("Qgage_",     mm, "_prop")
  vcol  <- paste0("Qgage_var_", mm, "_prop")
  
  sd_vec <- sqrt(pmax(flow_order[[vcol]], 0))
  
  flow_order[[paste0("Qgage_low_",  mm, "_prop")]]  <- pmax(flow_order[[mcol]] - sd_vec, 0)
  flow_order[[paste0("Qgage_high_", mm, "_prop")]]  <- flow_order[[mcol]] + sd_vec
}

# Finalize columns:
# QE_mm_final / QE_mm_low_final / QE_mm_high_final are the gage-based denominators
for (mm in mon_codes) {
  flow_order[[paste0("QE_", mm, "_final")]]      <- flow_order[[paste0("Qgage_", mm, "_prop")]]
  flow_order[[paste0("QE_", mm, "_low_final")]]  <- flow_order[[paste0("Qgage_low_", mm, "_prop")]]
  flow_order[[paste0("QE_", mm, "_high_final")]] <- flow_order[[paste0("Qgage_high_", mm, "_prop")]]
}

# Join assigned gage flows & IDs to flowlines
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
    # Flow source flag: direct gage, propagated gage, or NHDPlus-only
    flow_source = case_when(
      !is.na(site_no)                    ~ "gage_direct",
      is.na(site_no) & !is.na(gage_used) ~ "gage_propagated",
      TRUE                               ~ "nhd_only"
    )
  )

# --- % Wastewater using monthly bands (gage-based + NHDPlus-based) ----------
# Helper: compute %WW, but return NA if denom or ww is NA; cap at 100%.
compute_pct <- function(ww, denom) {
  ifelse(
    is.na(denom) | is.na(ww) | denom <= 0,
    NA_real_,
    pmin(100, pmax(0, 100 * ww / denom))
  )
}

# NHDPlus monthly denominators 
# QE_mm_nhd is the original NHDPlus monthly flow
for (mm in mon_codes) {
  flowlines[[paste0("QE_", mm, "_nhd")]] <- as.numeric(flowlines[[paste0("QE_", mm)]])
}

# Gage-based %WW for each month:
#   - pct_mean_mm: new WW-aware denominator (gage + added WW between gage & segment)
#   - pct_mean_mm_base: legacy %WW = WW / QE_mm_final (no WW added)
#   - pct_min_mm, pct_max_mm: bands using WW_low/WW_high and Q_high/Q_low
for (mm in mon_codes) {
  ww_mean <- paste0("WW_cum_mean_", mm)
  ww_low  <- paste0("WW_cum_low_",  mm)
  ww_high <- paste0("WW_cum_high_", mm)
  
  q_mean  <- paste0("QE_", mm, "_final")
  q_low   <- paste0("QE_", mm, "_low_final")
  q_high  <- paste0("QE_", mm, "_high_final")
  
  pct_mean_col      <- paste0("pct_mean_",      mm)
  pct_min_col       <- paste0("pct_min_",       mm)   # WW_low / Q_high
  pct_max_col       <- paste0("pct_max_",       mm)   # WW_high / Q_low
  pct_mean_base_col <- paste0("pct_mean_", mm, "_base")  # OLD method
  
  WW_seg <- flowlines[[ww_mean]]
  Q_seg  <- flowlines[[q_mean]]         # original denominator (no WW adjustment)
  
  # --- BASELINE: old %WW using Q_seg only -----------------------------------
  flowlines[[pct_mean_base_col]] <- compute_pct(WW_seg, Q_seg)
  
  # --- NEW: WW-aware denominators (mean + bands) -----------------------------
  g_hseq <- flowlines$gage_hseq
  idx_g  <- match(g_hseq, flowlines$Hydroseq)
  valid_g <- !is.na(idx_g)
  
  # WW at segment (mean/low/high)
  WW_seg_mean <- flowlines[[ww_mean]]
  WW_seg_low  <- flowlines[[ww_low]]
  WW_seg_high <- flowlines[[ww_high]]
  
  # WW at donor gage Hydroseq (mean/low/high)
  WW_gage_mean <- rep(NA_real_, length(WW_seg_mean))
  WW_gage_low  <- rep(NA_real_, length(WW_seg_mean))
  WW_gage_high <- rep(NA_real_, length(WW_seg_mean))
  
  WW_gage_mean[valid_g] <- flowlines[[ww_mean]][idx_g[valid_g]]
  WW_gage_low[valid_g]  <- flowlines[[ww_low ]][idx_g[valid_g]]
  WW_gage_high[valid_g] <- flowlines[[ww_high]][idx_g[valid_g]]
  
  # incremental WW between donor gage and segment (mean/low/high), floored at 0
  dWW_mean <- pmax(WW_seg_mean - WW_gage_mean, 0)
  dWW_low  <- pmax(WW_seg_low  - WW_gage_low,  0)
  dWW_high <- pmax(WW_seg_high - WW_gage_high, 0)
  
  dWW_mean[!is.finite(dWW_mean)] <- 0
  dWW_low[ !is.finite(dWW_low) ] <- 0
  dWW_high[!is.finite(dWW_high)] <- 0
  
  # Base gage denominators (unadjusted)
  Q_mean <- flowlines[[q_mean]]
  Q_low  <- flowlines[[q_low]]
  Q_high <- flowlines[[q_high]]
  
  # WW-aware denominators
  Q_tot_mean <- Q_mean + dWW_mean
  Q_tot_low  <- Q_low  + dWW_low
  Q_tot_high <- Q_high + dWW_high
  
  # Use WW-aware denominators only when there is a clean single-donor gage_hseq
  use_adj <- !is.na(g_hseq)
  
  denom_mean <- ifelse(use_adj & is.finite(Q_tot_mean), Q_tot_mean, Q_mean)
  denom_low  <- ifelse(use_adj & is.finite(Q_tot_low),  Q_tot_low,  Q_low)
  denom_high <- ifelse(use_adj & is.finite(Q_tot_high), Q_tot_high, Q_high)
  
  # Mean (WW_mean / denom_mean)
  flowlines[[pct_mean_col]] <- compute_pct(WW_seg_mean, denom_mean)
  
  # Bands consistent with the same denominator logic:
  #   min: WW_low / denom_high   (low numerator, high denominator)
  #   max: WW_high / denom_low   (high numerator, low denominator)
  flowlines[[pct_min_col]]  <- compute_pct(WW_seg_low,  denom_high)
  flowlines[[pct_max_col]]  <- compute_pct(WW_seg_high, denom_low)
}
# NHDPlus-based %WW (parallel to the gage-based %WW, but using NHDPlus denominators)
for (mm in mon_codes) {
  ww_mean <- paste0("WW_cum_mean_", mm)
  ww_low  <- paste0("WW_cum_low_",  mm)
  ww_high <- paste0("WW_cum_high_", mm)
  
  q_nhd   <- paste0("QE_", mm, "_nhd")
  
  flowlines[[paste0("pct_mean_", mm, "_nhd")]] <- compute_pct(flowlines[[ww_mean]], flowlines[[q_nhd]])
  flowlines[[paste0("pct_min_",  mm, "_nhd")]] <- compute_pct(flowlines[[ww_low]],  flowlines[[q_nhd]])
  flowlines[[paste0("pct_max_",  mm, "_nhd")]] <- compute_pct(flowlines[[ww_high]], flowlines[[q_nhd]])
}

# Drop Z/M dimensions from geometry (if present)
flowlines <- st_zm(flowlines, drop = TRUE, what = "ZM")

# --- Save the precomputed flowlines object -----------------------------------
saveRDS(
  flowlines,
  file = file.path(data_dir, "flowlines_precomputed_full.rds")
)

message("Saved precomputed flowlines to data-raw/flowlines_precomputed_full.rds")

# ReSource Tracker: deployed (slim) app
#
# Loads pre-computed tables built by build_app_data.R from the full pipeline
# (app_full.R). To change the data or the flow/wastewater methods, edit
# app_full.R, rerun build_app_data.R locally, and commit the refreshed
# data-raw/*_app.rds files with this file.

# --- Libraries ---------------------------------------------------------------
library(tidyverse)
library(sf)
library(shiny)
library(mapgl)
library(viridisLite)
# Also used via pkg:: calls: scales, sandwich (cluster-robust SEs)

# --- Load pre-computed data --------------------------------------------------
data_dir <- "data-raw"

read_app <- function(name) {
  p <- file.path(data_dir, paste0(name, ".rds"))
  if (!file.exists(p)) {
    stop("Missing ", p, ". Run build_app_data.R locally and commit its outputs.", call. = FALSE)
  }
  readRDS(p)
}

flowlines   <- read_app("flowlines_app")    # sf, EPSG:4326, map columns only
fl_cejst    <- read_app("fl_cejst_app")     # segment x tract table (no geometry)
cejst       <- read_app("cejst_app")        # CEJST + rurality (no geometry)
tracts_map  <- read_app("tracts_map_app")   # tract polygons + popup_html
tracts_info <- read_app("tracts_info_app")  # GEOID, rurality, low_income
dwtp_map    <- read_app("dwtp_map_app")
wwtp_map    <- read_app("wwtp_map_app")
gages_map   <- read_app("gages_map_app")

# --- Constants ---------------------------------------------------------------
mon_codes    <- sprintf("%02d", 1:12)            # "01"–"12"
month_labels <- setNames(month.name, mon_codes)  # "01" -> "January"
month_colors <- setNames(
  c(
    "#3B4F8F", # 01 Jan
    "#4B6CB7", # 02 Feb
    "#3C9D82", # 03 Mar
    "#54B887", # 04 Apr
    "#7BC96F", # 05 May
    "#F4D35E", # 06 Jun
    "#F6B35E", # 07 Jul
    "#F28E52", # 08 Aug
    "#E76F51", # 09 Sep
    "#D95F5F", # 10 Oct
    "#A8558C", # 11 Nov
    "#6D4C9B"  # 12 Dec
  ),
  mon_codes
)
# Consistent colors for rurality categories (Urban / Rural)
rurality_colors <- c(
  "Urban" = "#1b9e77",
  "Rural" = "#7570b3"
)

# Plot theme - consistent across tabs
theme_app <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title    = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 13),
      axis.title.x  = element_text(size = 15, face = "bold"),
      axis.title.y  = element_text(size = 15, face = "bold"),
      axis.text.x   = element_text(size = 13, face = "bold"),
      axis.text.y   = element_text(size = 13, face = "bold"),
      legend.position = "none"
    )
}

safe_round <- function(x) ifelse(is.finite(x), round(x, 1), "NA")
pal <- scales::col_numeric(viridisLite::plasma(256), domain = c(0, 100), na.color = "transparent")

# Boolean CEJST columns are treated like rurality.
# Only the hardcoded columns below are offered (disadvantaged yes/no)
cejst_lgl_keep <- c("Identified as disadvantaged")
cejst_lgl_cols <- intersect(cejst_lgl_keep, names(cejst)[sapply(cejst, is.logical)])
is_cat_var <- function(v) identical(v, "rurality") || isTRUE(v %in% cejst_lgl_cols)

# --- Statistics helpers ------------------------------------------------------
eq_zs <- function(x) as.numeric(scale(x))
eq_zr <- function(x) as.numeric(scale(rank(x)))

# cluster-robust (county) estimate, 95% CI, p for the coefficient `term`
eq_robust <- function(m, term, cl, odds = FALSE) {
  G <- length(unique(cl)); V <- sandwich::vcovCL(m, cluster = cl, type = "HC1")
  b <- unname(stats::coef(m)[term]); se <- sqrt(V[term, term]); tc <- stats::qt(0.975, G - 1)
  r <- c(est = b, lo = b - tc * se, hi = b + tc * se); if (odds) r <- exp(r)
  c(r, p = 2 * stats::pt(-abs(b / se), G - 1))
}

# One indicator -> unadjusted + drainage-area-adjusted effect.
#   Severity  : rank correlation (rho); adjusted = also controls for log10 drainage area
#   Prevalence: odds ratio (Yes vs No, or per +1 SD); adjusted = also controls for log10 drainage area + log(# segments)
eq_fit <- function(tab, var, mode) {
  tryCatch({
    x <- if (identical(var, "rurality")) as.numeric(tab$rurality == "Rural") else as.numeric(tab[[var]])
    covs <- if (identical(mode, "Severity")) "lDA" else c("lDA", "lnseg")
    dd <- data.frame(cl = tab$county, y = as.numeric(tab$metric_value), x = x)
    for (v in covs) dd[[v]] <- as.numeric(tab[[v]])
    dd <- dd[stats::complete.cases(dd) & Reduce(`&`, lapply(dd[-1], is.finite)), , drop = FALSE]
    covs <- covs[vapply(covs, function(v) length(unique(dd[[v]])) > 1, logical(1))]
    G <- length(unique(dd$cl))
    if (nrow(dd) < 50 || G < 15 || length(unique(dd$x)) < 2) return(NULL)
    is_bin <- all(dd$x %in% c(0, 1))
    if (identical(mode, "Severity")) {
      dd$ys <- eq_zr(dd$y); dd$xs <- if (is_bin) eq_zs(dd$x) else eq_zr(dd$x)
      for (v in covs) dd[[v]] <- eq_zr(dd[[v]])
      m0 <- stats::lm(ys ~ xs, dd); m1 <- stats::lm(stats::reformulate(c("xs", covs), "ys"), dd); odds <- FALSE
    } else {
      dd$xs <- if (is_bin) dd$x else eq_zs(dd$x)
      for (v in covs) dd[[v]] <- eq_zs(dd[[v]])
      m0 <- suppressWarnings(stats::glm(y ~ xs, stats::binomial, dd))
      m1 <- suppressWarnings(stats::glm(stats::reformulate(c("xs", covs), "y"), stats::binomial, dd)); odds <- TRUE
    }
    a <- eq_robust(m0, "xs", dd$cl, odds); b <- eq_robust(m1, "xs", dd$cl, odds)
    data.frame(indicator = var, type = if (is_bin) "yes/no" else "continuous", n_tracts = nrow(dd), n_counties = G,
               effect_unadj = a[["est"]], lo_unadj = a[["lo"]], hi_unadj = a[["hi"]], p_unadj = a[["p"]],
               effect_adj   = b[["est"]], lo_adj   = b[["lo"]], hi_adj   = b[["hi"]], p_adj   = b[["p"]],
               stringsAsFactors = FALSE)
  }, error = function(e) NULL)
}

# One-line text used in plot subtitles
eq_stat_line <- function(r, mode) {
  if (is.null(r) || nrow(r) != 1) return(NULL)
  f <- function(x) formatC(x, format = "f", digits = 2)
  nm <- if (identical(mode, "Prevalence")) "OR" else "rho"
  sprintf("Adjusted for drainage area: %s = %s (95%% CI %s to %s), BH q = %s  |  unadjusted %s = %s",
          nm, f(r$effect_adj), f(r$lo_adj), f(r$hi_adj), formatC(r$q_adj, format = "e", digits = 1),
          nm, f(r$effect_unadj))
}

# Paired rank-biserial: +1 = month A higher than month B in every unit, -1 = always lower
eq_rb_paired <- function(a, b) {
  d <- a - b
  d <- d[is.finite(d) & d != 0]
  if (length(d) < 5) return(NA_real_)
  r <- rank(abs(d))
  (sum(r[d > 0]) - sum(r[d < 0])) / sum(r)
}

# Kendall's W (tie-corrected) from a ranks matrix R (units x months)
eq_w_calc <- function(R, Tv, k, idx) {
  n  <- length(idx)
  Rj <- colSums(R[idx, , drop = FALSE])
  S  <- sum((Rj - mean(Rj))^2)
  den <- n^2 * (k^3 - k) - n * sum(Tv[idx])
  if (!is.finite(den) || den <= 0) return(NA_real_)
  12 * S / den
}

# One bootstrap draw of row indices, resampling whole county groups
eq_boot_idx <- function(groups) {
  g <- sample.int(length(groups), replace = TRUE)
  unlist(groups[g], use.names = FALSE)
}

# --- UI ----------------------------------------------------------------------
ui <- tagList(
  # ---- Global style so the tract info panel wraps well ----------------------
  tags$head(
    tags$style(HTML("
      #tract_info {
        white-space: pre-wrap;
        word-break: break-word;
        font-size: 0.9em;
        max-height: 350px;
        overflow-y: auto;
      }
      @media (max-width: 1200px) {
        #tract_info { font-size: 0.85em; max-height: 250px; }
      }
      @media (max-width: 768px) {
        #tract_info { font-size: 0.8em; max-height: 200px; }
      }
    "))
  ),
  
  navbarPage(
    "ReSource Tracker",
    
    # ========================= TAB 1: MAP ====================================
    tabPanel(
      "Map",
      fluidPage(
        fluidRow(
          column(
            width = 3,
            style = "max-height: 900px; overflow-y: auto;",
            
            h4("Map Controls"),
            helpText(
              "Explore how much of streamflow is contributed by upstream ",
              "wastewater discharges. Choose the denominator source and month."
            ),
            radioButtons(
              "src", "Denominator source",
              choices  = c("Gage-based", "NHDPlus-based"),
              selected = "Gage-based", inline = FALSE
            ),
            selectInput(
              "month", "Month (flow conditions)",
              choices  = setNames(mon_codes, month_labels),
              selected = "06"
            ),
            
            actionButton(
              "reset_map",
              label = "Reset Map View",
              icon  = icon("refresh"),
              width = "100%"
            ),
            
            tags$hr(),
            
            h4("Quick guide"),
            tags$small(
              "Hover over a stream and click to see:",
              tags$br(),
              "• Selected month wastewater flow (cfs)",
              tags$br(),
              "• % wastewater for the chosen denominator",
              tags$br(),
              "• Upstream WWTP contributors and gage info",
              tags$br(), tags$br(),
              "For equity analysis using Climate and Economic Justice Screening Tool (CEJST) indicators, use the 'Equity' tab."
            ),
            
            tags$hr(),
            h4("Selected census tract"),
            verbatimTextOutput("tract_info")
          ),
          
          column(
            width = 9,
            maplibreOutput("map", height = "900px")
          )
        )
      )
    ),
    
    # ====================== TAB 2: HOW TO USE ===============================
    tabPanel(
      "How to Use",
      fluidPage(
        fluidRow(
          column(
            width = 8,
            h3("How to use ReSource Tracker"),
            p(
              "This tool estimates the fraction of streamflow composed of ",
              "treated wastewater effluent for each NHDPlus flowline in Colorado."
            ),
            h4("1. Choose a denominator source (Map tab)"),
            tags$ul(
              tags$li(
                strong("Gage-based: "),
                "USGS stream gage data are processed into monthly means ",
                "and propagated downstream along segments of similar drainage area, ",
                "with flow scaled by the drainage-area ratio between segments. ",
                "This is the preferred denominator where available."
              ),
              tags$li(
                strong("NHDPlus-based: "),
                "Uses NHDPlus mean monthly flows (original NHDPlus volumes). ",
                "These provide statewide coverage but can overestimate flows in ",
                "regulated or drought-impacted streams."
              )
            ),
            h4("2. Select a month (Map tab)"),
            p(
              "Use the Month selector to switch between January–December. ",
              "All wastewater flows and percent wastewater values are computed ",
              "for the selected month."
            ),
            h4("3. Interpret the stream colors (Map tab)"),
            p(
              "Stream colors show the estimated percent wastewater for the ",
              "selected month and denominator source."
            ),
            tags$ul(
              tags$li("Dark colors ≈ low % wastewater (near 0%)."),
              tags$li("Bright colors ≈ high % wastewater (approaching 100%).")
            ),
            h4("4. Explore equity impacts (Equity tab)"),
            p(
              "The Equity tab links the stream network to Climate and Economic Justice Screening Tool (CEJST) census tracts. ",
              "For each tract, it summarizes tract-level wastewater prevalence (whether any intersecting segment is impacted) ",
              "and severity (how large % wastewater is among impacted segments), ",
              "and estimates how strongly each tract-level CEJST indicators is associated with wastewater influence, adjusting for drainage area."
            ),
            h4("5. Reading the popups (Map tab)"),
            p("Click on a stream segment to see a popup with:"),
            tags$ul(
              tags$li("The NHDPlus Hydroseq ID (unique segment identifier)."),
              tags$li("Monthly wastewater flow (mean, low, high) in cfs."),
              tags$li("% wastewater (mean/min/max) for the selected month."),
              tags$li("Which gage(s) are used for the denominator."),
              tags$li("Whether flow comes from a direct gage, propagated gage, or NHDPlus only."),
              tags$li("Top contributing wastewater treatment plants on that segment.")
            )
          )
        )
      )
    ),
    
    # ==================== TAB 3: EQUITY =====================================
    tabPanel(
      "Equity",
      fluidPage(
        fluidRow(
          column(
            width = 3,
            h4("CEJST & Wastewater Influence Settings"),
            helpText(
              "Two modes:",
              tags$br(),
              tags$ul(
                tags$li(strong("Prevalence:"), " tract-level (does the tract have ANY impacted segment?)."),
                tags$li(strong("Severity:"), " among impacted segments (%WW > 0), how large the contribution is.")
              )
            ),
            
            radioButtons(
              "cejst_mode", "Analysis mode",
              choices  = c("Prevalence", "Severity"),
              selected = "Prevalence",
              inline   = FALSE
            ),
            
            conditionalPanel(
              condition = "input.cejst_mode == 'Severity'",
              radioButtons(
                "cejst_src", "Denominator source (severity only)",
                choices  = c("Gage-based", "NHDPlus-based"),
                selected = "Gage-based",
                inline   = FALSE
              )
            ),
            
            selectInput(
              "cejst_month", "Month (flow conditions)",
              choices  = setNames(mon_codes, month_labels),
              selected = "06"
            ),
            
            selectInput(
              "rurality",
              "Rurality category",
              choices  = c("All", "Urban", "Rural"),
              selected = "All"
            ),
            
            tags$hr(),
            h4("Download"),
            helpText("Export tract-level data used in the current equity plot."),
            downloadButton("download_cejst_data", "Download equity CSV"),
            
            tags$hr(),
            h4("Indicator"),
            helpText(
              "Choose a CEJST numeric indicator or Rurality.",
              tags$br(), tags$br(),
              strong("To compare Urban/Rural:"),
              tags$br(),
              HTML("&bull; Set <b>Rurality category</b> to <b>All</b>"),
              tags$br(),
              HTML("&bull; Set <b>Indicator</b> to <b>Rurality</b>"),
              tags$br(), tags$br(),
              HTML(
                'Definitions of CEJST parameters: ',
                '<a href="https://public-environmental-data-partners.github.io/j40-cejst-2/en/methodology#3/33.47/-97.5" target="_blank">CEJST methodology &amp; data dictionary</a>.'
              )
            ),
            
            selectInput(
              "cejst_var_box",
              "CEJST variable",
              choices = NULL
            )
          ),
          
          column(
            width = 9,
            h3("Wastewater influence and CEJST indicators"),
            tags$p(
              style = "color:#555;",
              "Co-location patterns, adjusted for drainage area and clustered by county. ",
              "Not causal, and not individual exposure. Prevalence results are descriptive."
            ),
            
            h4("Overview: all indicators"),
            helpText(
              "Each row is one CEJST indicator, adjusted for drainage area. ",
              "Filled = BH q < 0.05, open = not significant. Orange = the indicator selected on the left. ",
              "Click a row to select it."
            ),
            checkboxInput("cejst_forest_all", "Show all indicators (default: the 20 largest effects)", FALSE),
            plotOutput("cejst_forest", height = "auto", click = "cejst_forest_click"),
            
            tags$hr(),
            h4("Selected indicator"),
            uiOutput("cejst_card"),
            plotOutput("cejst_plot", height = "520px")
          )
        )
      )
    ),
    
    #==================  TAB 4: SEASONALITY =================================
    tabPanel(
      "Seasonality",
      fluidPage(
        fluidRow(
          column(
            width = 3,
            h4("Seasonality settings"),
            helpText("Monthly summaries across all tracts (or filtered)."),
            
            radioButtons(
              "season_src", "Denominator source (%WW)",
              choices = c("Gage-based", "NHDPlus-based"),
              selected = "Gage-based",
              inline = FALSE
            ),
            
            selectInput(
              "season_rurality",
              "Rurality category",
              choices  = c("All", "Urban", "Rural"),
              selected = "All"
            ),
            
            checkboxInput(
              "season_use_tracts",
              "Summarize at tract-level (instead of segment-level)",
              TRUE
            ),
            helpText("Tract-level collapses many segments into one value per tract per month."),
            
            tags$hr(),
            
            h4("Download"),
            helpText("Export the data used to draw the seasonality plot."),
            downloadButton("download_season_data", "Download seasonality CSV")
          ),
          
          column(
            width = 9,
            h3("Seasonal patterns in wastewater influence"),
            plotOutput("season_plot_median", height = "420px"),
            tags$hr(),
            verbatimTextOutput("season_text"),
            
            tags$hr(),
            h4("Month-to-month statistics"),
            verbatimTextOutput("season_stats_text")
          )
        )
      )
    ),
    
    # ================== TAB 5: DATA SOURCES =================================
    tabPanel(
      "Data Sources",
      fluidPage(
        fluidRow(
          column(
            width = 8,
            h3("Core input datasets"),
            p(
              "This tab documents the main input datasets used to build the ",
              "ReSource Tracker. Where possible, links point to ",
              "public, externally maintained sources. Processed versions (e.g., ",
              "Colorado-only subsets, network-joined tables) are distributed via ",
              "the accompanying manuscript / repository."
            ),
            
            h4("1. Stream network and base hydrology"),
            tags$ul(
              tags$li(
                strong("NHDPlus flowlines: "),
                "National Hydrography Dataset Plus (NHDPlus) flowlines and ",
                "associated attributes (e.g., COMID, Hydroseq, mean annual flow). ",
                "Used as the base river network and for routing wastewater and ",
                "streamflow downstream."
              ),
              tags$li(
                strong("NHDPlus mean monthly flows: "),
                "NHDPlus-supplied mean monthly flows are used as an alternative ",
                "denominator when USGS gage data are unavailable."
              )
            ),
            
            h4("2. USGS stream gage records"),
            tags$ul(
              tags$li(
                strong("USGS NWIS daily streamflow: "),
                "Daily discharge (cfs) time series for Colorado gages, retrieved ",
                "from the USGS National Water Information System (NWIS). These ",
                "records are aggregated to monthly means and propagated along the ",
                "NHDPlus network to estimate contemporary streamflow conditions."
              ),
              tags$li(
                strong("Gage–COMID linkage: "),
                "Each gage is spatially matched to its nearest NHDPlus COMID; ",
                "only one gage per COMID is retained, favoring the longest and ",
                "most recent record."
              )
            ),
            
            h4("3. Wastewater treatment plants and discharges"),
            tags$ul(
              tags$li(
                strong("WWTP locations: "),
                "Facility coordinates and design flows for municipal wastewater ",
                "treatment plants discharging to surface waters in Colorado. ",
                "Used to place outfalls on the map and assign them to NHDPlus ",
                "flowlines."
              ),
              tags$li(
                strong("Discharge Monitoring Report (DMR) data: "),
                "Monthly-average effluent flows are compiled from publicly ",
                "reported DMR data. Flows are converted to cfs, aggregated by ",
                "facility–COMID–month, and then accumulated downstream along the ",
                "river network."
              )
            ),
            
            h4("4. Drinking water systems and intakes"),
            tags$ul(
              tags$li(
                strong("Drinking water treatment plants (DWTPs): "),
                "Geocoded locations of community drinking water systems, including ",
                "population served and primary source type (e.g., surface water, ",
                "groundwater, mixed). These points are displayed as map markers."
              )
            ),
            
            h4("5. Climate and Economic Justice Screening Tool (CEJST)"),
            tags$ul(
              tags$li(
                strong("CEJST tract-level indicators: "),
                "Census-tract–level CEJST data, including demographic, economic, ",
                "health, climate, and pollution burden indicators. These fields ",
                "are joined to each tract and then intersected with the stream ",
                "network to summarize wastewater influence by community."
              )
            ),
            
            h4("6. Processed intermediate tables"),
            p(
              "The following intermediate tables are generated within the analysis ",
              "workflow and are not standalone external datasets, but are important ",
              "for reproducibility:"
            ),
            tags$ul(
              tags$li(
                strong("Seasonal gage summary: "),
                "Per-gage monthly mean, standard deviation, and low / high flow ",
                "estimates used in the map popups."
              ),
              tags$li(
                strong("WWTP monthly flows: "),
                "Facility–COMID–month tables of effluent discharge (cfs) before ",
                "downstream accumulation."
              ),
              tags$li(
                strong("Tract-level wastewater influence summary: "),
                "For each tract, the median, 90th percentile, and maximum % ",
                "wastewater across all intersecting stream segments for each ",
                "month and denominator source."
              )
            ),
            
            h4("Accessing the inputs"),
            p(
              "Public links and permanent DOIs for the processed inputs (e.g., ",
              "Colorado-only flowlines, gage summaries, CEJST-joined tables) will ",
              "be provided in the associated manuscript, data repository, or ",
              "project website. This tab serves as a living data dictionary for ",
              "interpreting those files."
            )
          )
        )
      )
    ),
    
    # ================== TAB 6: DOWNLOADS & METHODS ===========================
    tabPanel(
      "Downloads & Methods",
      fluidPage(
        fluidRow(
          column(
            width = 8,
            h3("Data & Methods"),
            p(
              "This tab will be updated to allow downloading of data for users to conduct their own analysis."
            ),
            h4("Flow estimation and propagation (summary)"),
            tags$ul(
              tags$li(
                "USGS daily streamflow records (post–2001) are aggregated to ",
                "monthly means, with variability summarized using the interannual ",
                "standard deviation and corresponding mean ± 1 SD bands (low/high) ",
                "for each calendar month."
              ),
              tags$li(
                "Each gage is mapped to its closest NHDPlus flowline (COMID); ",
                "only one gage per COMID is retained, favoring the longest and ",
                "most recent record."
              ),
              tags$li(
                "Monthly flows are propagated downstream one segment at a time, ",
                "scaled by the drainage-area ratio between segments ",
                "(Q_downstream = Q_upstream × (A_downstream / A_upstream)^b, where b is ",
                "a statewide exponent fitted to the gage network). Propagation continues ",
                "only while drainage area changes by less than 50% (relative to the ",
                "downstream segment) from one segment to the next, and stops at larger ",
                "changes or at new gages."
              ),
              tags$li(
                "At confluences, flows from multiple upstream branches that carry ",
                "gage-based estimates are summed (mass balance). Segments downstream of ",
                "a confluence are then subject to the same drainage-area gate as any ",
                "other segment."
              )
            ),
            h4("Wastewater accumulation (summary)"),
            tags$ul(
              tags$li(
                "DMR-reported effluent flows are converted to cfs and averaged ",
                "by facility, COMID, and month, collapsing multiple outfalls per ",
                "facility–COMID combination."
              ),
              tags$li(
                "Wastewater flows are summed for all WWTPs mapped to the same ",
                "COMID and accumulated downstream using the NHDPlus flow network."
              ),
              tags$li(
                "For each segment and month, % wastewater is computed using ",
                "either the gage-based or NHDPlus-based denominator."
              )
            )
          )
        )
      )
    )
  )
)

# --- SERVER ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # helper
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  selected_tract_id <- reactiveVal(NULL)
  
  # ---- Tract polygon clicks on the map --------------------------------------
  observeEvent(input$map_feature_click, {
    click <- input$map_feature_click
    if (is.null(click)) return()
    if (!identical(click$layer, "tracts")) return()
    
    # mapgl generates a numeric feature id, so read GEOID from properties
    gid <- click$properties$GEOID
    if (is.null(gid)) return()
    if (!gid %in% tracts_info$GEOID) return()
    
    selected_tract_id(gid)
  })
  
  contrib_block <- function(x) {
    ifelse(!is.na(x) & nzchar(x),
           paste0(
             "<details><summary style='cursor:pointer; color:#1E90FF; font-weight:bold; text-decoration:underline;'>",
             "Show WWTP Contributors</summary>",
             "<div style='margin-left:10px;'>", x, "</div></details><br>"
           ),
           ""
    )
  }
  
  # Column selector for map: which %WW column to use based on denominator source
  cols_for <- reactive({
    m <- input$month
    if (input$src == "Gage-based") {
      list(pct = paste0("pct_mean_", m))
    } else {
      list(pct = paste0("pct_mean_", m, "_nhd"))
    }
  })
  
  filtered_flowlines <- reactive({ flowlines })
  
  # Populate CEJST variable choices (numeric, non-percentile) + rurality
  observe({
    cejst_num_all <- names(cejst)[sapply(cejst, is.numeric)]
    
    cejst_num_raw <- cejst_num_all[!grepl("percentile", cejst_num_all, ignore.case = TRUE) &
                                     !grepl("pct_", cejst_num_all, ignore.case = TRUE) &
                                     !grepl("_pct$", cejst_num_all, ignore.case = TRUE)]
    cejst_num_raw <- cejst_num_raw[!grepl("wastewater", cejst_num_raw, ignore.case = TRUE)]
    if (length(cejst_num_raw) == 0) cejst_num_raw <- cejst_num_all
    
    if ("rurality" %in% names(cejst)) {
      var_values <- c("rurality", cejst_lgl_cols, cejst_num_raw)
      var_labels <- c("Rurality", paste0(cejst_lgl_cols, " (yes/no)"), cejst_num_raw)
    } else {
      var_values <- c(cejst_lgl_cols, cejst_num_raw)
      var_labels <- c(paste0(cejst_lgl_cols, " (yes/no)"), cejst_num_raw)
    }
    
    updateSelectInput(
      session, "cejst_var_box",
      choices  = stats::setNames(var_values, var_labels),
      selected = var_values[1]
    )
  })
  
  # ---- STREAMS: colors + popups for the current month / denominator ---------
  streams_layer_data <- reactive({
    spec <- cols_for()
    dat  <- filtered_flowlines()
    m    <- input$month
    mlab <- month_labels[m]
    
    vals     <- dat[[spec$pct]]
    ww_color <- ifelse(is.na(vals), "#d9d9d9", pal(vals))
    
    if (input$src == "Gage-based") {
      ww_mean_col <- paste0("WW_cum_mean_", m)
      ww_low_col  <- paste0("WW_cum_low_",  m)
      ww_high_col <- paste0("WW_cum_high_", m)
      sd_col      <- paste0("WW_cum_sd_",   m)
      
      pct_mean_col <- paste0("pct_mean_", m)
      pct_min_col  <- paste0("pct_min_",  m)
      pct_max_col  <- paste0("pct_max_",  m)
      
      popup_html <- paste0(
        "<b>Hydroseq:</b> ", dat$Hydroseq, "<br><br>",
        "<b>", mlab, " WW Flow (mean±sd):</b> ",
        safe_round(dat[[ww_mean_col]]), " cfs",
        " (sd ", safe_round(dat[[sd_col]]),
        "; low ",  safe_round(dat[[ww_low_col]]),
        ", high ", safe_round(dat[[ww_high_col]]), ")<br><br>",
        "<b>", mlab, " % Wastewater (Gage-based):</b><br>",
        "mean/min/max: ",
        safe_round(dat[[pct_mean_col]]), "% / ",
        safe_round(dat[[pct_min_col]]),  "% / ",
        safe_round(dat[[pct_max_col]]),  "%<br><br>",
        ifelse(!is.na(dat$start_year),
               paste0("<i>Gage years: ", dat$start_year, "–", dat$end_year,
                      ", N = ", dat$n_obs, "</i><br>"), ""),
        ifelse(!is.na(dat$gage_used),
               paste0("<b>Gage used:</b> ", dat$gage_used, "<br>"), ""),
        ifelse(!is.na(dat$flow_source),
               paste0("<b>Flow source:</b> ", dat$flow_source, "<br>"), ""),
        contrib_block(dat[[paste0("contributors_html_", m)]])
      )
    } else {
      q_nhd_col    <- paste0("QE_", m, "_nhd")
      pct_mean_nhd <- paste0("pct_mean_", m, "_nhd")
      
      popup_html <- paste0(
        "<b>Hydroseq:</b> ", dat$Hydroseq, "<br><br>",
        "<b>", mlab, " NHDPlus flow:</b> ",
        safe_round(dat[[q_nhd_col]]), " cfs<br><br>",
        "<b>", mlab, " % Wastewater (NHDPlus-based):</b><br>",
        "mean: ", safe_round(dat[[pct_mean_nhd]]), "%<br><br>",
        contrib_block(dat[[paste0("contributors_html_", m)]])
      )
    }
    
    sf::st_sf(
      data.frame(
        hydroseq   = dat$Hydroseq,
        pct        = vals,
        ww_color   = ww_color,
        line_w     = scales::rescale(log1p(dat$QE_MA), to = c(0.2, 4)),
        popup_html = popup_html,
        stringsAsFactors = FALSE
      ),
      geometry = sf::st_geometry(dat)
    )
  })
  
  pct_legend_title <- function(src, m) {
    paste0("% Wastewater (", src, ", ", month_labels[m], ")")
  }
  
  # Custom HTML legend with circle markers
  symbol_legend_html <-
    "<div style='background: white; padding: 6px 8px; font-size: 12px;
               box-shadow: 0 0 10px rgba(0,0,0,0.2);'>
     <b>Map symbols</b><br>
     <div>
       <span style='display:inline-block; width:10px; height:10px;
                    border-radius:50%; background:blue; margin-right:6px;'></span>
       Drinking water treatment plant (DWTP)
     </div>
     <div>
       <span style='display:inline-block; width:10px; height:10px;
                    border-radius:50%; background:brown; margin-right:6px;'></span>
       Wastewater treatment plant (WWTP)
     </div>
     <div>
       <span style='display:inline-block; width:10px; height:10px;
                    border-radius:50%; background:black; margin-right:6px;'></span>
       USGS gage
     </div>
     <div style='margin-top:4px;'>
       <span style='display:inline-block; width:20px; height:3px;
                    background:#d9d9d9; margin-right:6px;'></span>
       Stream with no %WW data
     </div>
   </div>"
  
  # ---- BASE MAP --------------------------------------------------------------
  output$map <- renderMaplibre({
    mapgl::maplibre(
      style = mapgl::openfreemap_style("positron")
    ) %>%
      mapgl::fit_bounds(flowlines, animate = FALSE) %>%
      mapgl::add_navigation_control(position = "top-right") %>%
      mapgl::add_scale_control(position = "bottom-left") %>%
      
      # --- Census tract polygons
      mapgl::add_fill_layer(
        id = "tracts",
        source = tracts_map,
        fill_color = "#CCCCCC",
        fill_opacity = 0.15,
        fill_outline_color = "#666666",
        hover_options = list(fill_color = "#000000", fill_opacity = 0.35),
        popup = "popup_html"
      ) %>%
      
      # --- Streams (isolate() so month/source changes go through the proxy below)
      mapgl::add_line_layer(
        id = "streams",
        source = isolate(streams_layer_data()),
        line_color = mapgl::get_column("ww_color"),
        line_width = mapgl::get_column("line_w"),
        line_opacity = 0.85,
        popup = "popup_html"
      ) %>%
      
      # --- DWTP markers
      mapgl::add_circle_layer(
        id = "dwtp",
        source = dwtp_map,
        circle_color = "blue",
        circle_radius = 6,
        circle_opacity = 0.8,
        popup = "popup_html",
        tooltip = "pws_name"
      ) %>%
      
      # --- WWTP markers
      mapgl::add_circle_layer(
        id = "wwtp",
        source = wwtp_map,
        circle_color = "brown",
        circle_radius = 6,
        circle_opacity = 0.8,
        popup = "popup_html",
        tooltip = "facility"
      ) %>%
      
      # --- USGS gages
      mapgl::add_circle_layer(
        id = "gages",
        source = gages_map,
        circle_color = "black",
        circle_radius = 5,
        circle_opacity = 1,
        circle_stroke_color = "#ffffff",
        circle_stroke_width = 1,
        popup = "popup_str",
        tooltip = "station_nm"
      ) %>%
      
      # --- Controls
      mapgl::add_layers_control(
        position = "top-left",
        layers = list(
          "Streams"       = "streams",
          "DWTP"          = "dwtp",
          "WWTP"          = "wwtp",
          "USGS Gages"    = "gages",
          "Census tracts" = "tracts"
        ),
        collapsible = TRUE
      ) %>%
      mapgl::add_legend(
        legend_title = pct_legend_title(isolate(input$src), isolate(input$month)),
        values = c("0%", "100%"),
        colors = pal(seq(0, 100, length.out = 11)),
        type = "continuous",
        position = "bottom-right",
        unique_id = "pctLegend"
      ) %>%
      mapgl::add_control(
        html = symbol_legend_html,
        position = "bottom-left",
        id = "symbolLegend"
      )
  })
  
  observeEvent(input$reset_map, {
    mapgl::maplibre_proxy("map") %>%
      mapgl::fit_bounds(flowlines, animate = TRUE)
  })
  
  # --- Recolor and repopulate streams when the dropdowns change ---------------
  observeEvent(list(input$src, input$month), {
    mapgl::maplibre_proxy("map") %>%
      mapgl::clear_layer("streams") %>%
      mapgl::add_line_layer(
        id           = "streams",
        source       = streams_layer_data(),
        line_color   = mapgl::get_column("ww_color"),
        line_width   = mapgl::get_column("line_w"),
        line_opacity = 0.85,
        popup        = "popup_html",
        before_id    = "dwtp"   # keep streams under the point layers
      ) %>%
      mapgl::clear_legend("pctLegend") %>%
      mapgl::add_legend(
        legend_title = pct_legend_title(input$src, input$month),
        values       = c("0%", "100%"),
        colors       = pal(seq(0, 100, length.out = 11)),
        type         = "continuous",
        position     = "bottom-right",
        unique_id    = "pctLegend"
      )
  }, ignoreInit = TRUE)
  
  # ================= SEASONALITY TAB LOGIC ====================
  season_cols_for <- reactive({
    src <- input$season_src %||% "Gage-based"
    list(
      pct = function(mm) if (identical(src, "Gage-based")) paste0("pct_mean_", mm) else paste0("pct_mean_", mm, "_nhd"),
      ww  = function(mm) paste0("WW_cum_mean_", mm)
    )
  })
  
  season_base <- reactive({
    dat <- fl_cejst
    if (!is.null(input$season_rurality) && input$season_rurality != "All") {
      if ("rurality" %in% names(dat)) dat <- dplyr::filter(dat, rurality == input$season_rurality) else return(NULL)
    }
    dplyr::filter(dat, !is.na(GEOID), !is.na(Hydroseq))
  })
  
  season_monthly <- reactive({
    dat <- season_base()
    req(dat)
    
    cols <- season_cols_for()
    use_tracts <- isTRUE(input$season_use_tracts)
    
    # long: keep BOTH GEOID and Hydroseq to switch units safely
    long <- dplyr::bind_rows(lapply(mon_codes, function(mm) {
      pct_col <- cols$pct(mm)
      ww_col  <- cols$ww(mm)
      if (!pct_col %in% names(dat) || !ww_col %in% names(dat)) return(NULL)
      
      tibble::tibble(
        GEOID    = dat$GEOID,
        Hydroseq = dat$Hydroseq,
        rurality = dat$rurality,
        mon_code = mm,
        month    = month_labels[mm],
        pct      = suppressWarnings(as.numeric(dat[[pct_col]])),
        ww       = suppressWarnings(as.numeric(dat[[ww_col]]))
      )
    }))
    req(nrow(long) > 0)
    
    long <- long %>%
      dplyr::mutate(unit_id = if (use_tracts) GEOID else as.character(Hydroseq))
    
    # collapse to ONE value per unit_id x month
    long_unit <- long %>%
      dplyr::group_by(unit_id, mon_code, month) %>%
      dplyr::summarise(
        pct = stats::median(pct, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(pct = dplyr::if_else(is.finite(pct), pct, NA_real_))
    
    # ever-impacted membership across months (units that have >0 at least once)
    ever_imp <- long_unit %>%
      dplyr::group_by(unit_id) %>%
      dplyr::summarise(
        ever_impacted = any(!is.na(pct) & pct > 0, na.rm = TRUE),
        .groups = "drop"
      )
    
    long_imp <- long_unit %>%
      dplyr::inner_join(ever_imp, by = "unit_id") %>%
      dplyr::filter(ever_impacted)
    
    # monthly summary (median + IQR) among ever-impacted units, zeros retained
    long_imp %>%
      dplyr::group_by(mon_code, month) %>%
      dplyr::summarise(
        n_units_total = dplyr::n_distinct(unit_id),
        n_units       = sum(!is.na(pct)),
        med_pct = stats::median(pct, na.rm = TRUE),
        lo_iqr  = stats::quantile(pct, 0.25, na.rm = TRUE, names = FALSE),
        hi_iqr  = stats::quantile(pct, 0.75, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        mon_code = factor(mon_code, levels = mon_codes),
        month    = factor(month, levels = unname(month_labels[mon_codes]))
      )
  })
  
  # Cached by inputs: the bootstrap CIs are the slow part
  season_stats <- reactive({
    dat <- season_base()
    req(dat)
    
    cols <- season_cols_for()
    use_tracts <- isTRUE(input$season_use_tracts)
    
    long <- dplyr::bind_rows(lapply(mon_codes, function(mm) {
      pct_col <- cols$pct(mm)
      if (!pct_col %in% names(dat)) return(NULL)
      tibble::tibble(
        GEOID    = dat$GEOID,
        Hydroseq = dat$Hydroseq,
        mon_code = mm,
        pct      = suppressWarnings(as.numeric(dat[[pct_col]]))
      )
    }))
    req(nrow(long) > 0)
    
    long <- long %>%
      dplyr::mutate(unit_id = if (use_tracts) GEOID else as.character(Hydroseq)) %>%
      dplyr::group_by(unit_id, mon_code) %>%
      dplyr::summarise(
        pct = stats::median(pct, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(pct = dplyr::if_else(is.finite(pct), pct, NA_real_))
    
    # ever-impacted membership
    ever_imp <- long %>%
      dplyr::group_by(unit_id) %>%
      dplyr::summarise(ever_impacted = any(!is.na(pct) & pct > 0, na.rm = TRUE), .groups = "drop")
    
    long2 <- long %>%
      dplyr::inner_join(ever_imp, by = "unit_id") %>%
      dplyr::filter(ever_impacted)
    
    wide <- long2 %>% tidyr::pivot_wider(names_from = mon_code, values_from = pct)
    wide_cc <- wide %>% tidyr::drop_na()
    
    if (nrow(wide_cc) < 10) {
      return(list(n = nrow(wide_cc), friedman = NULL, pairwise = NULL))
    }
    
    X <- as.matrix(wide_cc[, mon_codes, drop = FALSE])
    fried <- stats::friedman.test(X)
    
    # county of each unit (tract GEOID digits 1-5; for segments use the first tract it touches)
    cty <- if (use_tracts) {
      substr(as.character(wide_cc$unit_id), 1, 5)
    } else {
      substr(as.character(dat$GEOID[match(wide_cc$unit_id, as.character(dat$Hydroseq))]), 1, 5)
    }
    cty[is.na(cty)] <- "NA"
    groups <- split(seq_along(cty), cty)
    
    # ---- Kendall's W with county-resampled 95% CI ---------------------------
    k  <- ncol(X)
    R  <- t(apply(X, 1, rank))
    Tv <- apply(X, 1, function(r) { tt <- table(r); sum(tt^3 - tt) })
    W  <- eq_w_calc(R, Tv, k, seq_len(nrow(X)))
    W_boot <- replicate(300, eq_w_calc(R, Tv, k, eq_boot_idx(groups)))
    W_ci   <- stats::quantile(W_boot, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
    
    # ---- pairwise paired tests + rank-biserial effect size ------------------
    pairs <- combn(mon_codes, 2, simplify = FALSE)
    pw <- lapply(pairs, function(p) {
      a <- X[, p[1]]; b <- X[, p[2]]
      wt <- stats::wilcox.test(a, b, paired = TRUE, exact = FALSE)
      data.frame(m1 = p[1], m2 = p[2],
                 med_diff = stats::median(a - b, na.rm = TRUE),
                 rb = eq_rb_paired(a, b),
                 p_value = wt$p.value,
                 stringsAsFactors = FALSE)
    }) %>% dplyr::bind_rows()
    
    pw$p_adj_BH <- p.adjust(pw$p_value, method = "BH")
    
    month_meds <- tibble::tibble(
      mon_code = mon_codes,
      month    = unname(month_labels[mon_codes]),
      med      = apply(X, 2, stats::median, na.rm = TRUE)
    ) %>% dplyr::arrange(dplyr::desc(med))
    
    list(n = nrow(wide_cc), n_counties = length(groups),
         friedman = fried, pairwise = pw, month_meds = month_meds,
         W = W, W_lo = W_ci[1], W_hi = W_ci[2],
         X = X, groups = groups)
  }) %>%
    bindCache(input$season_src, input$season_rurality, input$season_use_tracts)
  
  output$season_stats_text <- renderPrint({
    s <- season_stats()
    req(s)
    
    unit_lab <- ifelse(isTRUE(input$season_use_tracts), "tracts", "segments")
    
    if (is.null(s$friedman) || is.null(s$pairwise) || is.null(s$month_meds)) {
      cat("Not enough complete units across all 12 months to run paired month-to-month tests.\n")
      cat("Tip: switch to tract-level, or loosen missingness (but paired tests need complete cases).\n")
      return(invisible(NULL))
    }
    
    # Overall test + how consistent the seasonal pattern is
    cat(sprintf("Complete paired units (n_complete): %d %s in %d counties\n", s$n, unit_lab, s$n_counties))
    cat("Overall seasonality (Friedman):\n")
    cat(sprintf("  chi-squared = %.3f, df = %d, p = %s\n",
                unname(s$friedman$statistic),
                unname(s$friedman$parameter),
                format.pval(s$friedman$p.value, digits = 2, eps = 1e-100)))
    cat(sprintf("  Kendall's W = %.2f (95%% CI %.2f to %.2f)\n\n", s$W, s$W_lo, s$W_hi))
    cat("  W: 0 = no shared seasonal pattern across units, 1 = every unit ranks the months identically.\n\n")
    
    # Ranked months by median
    top_n <- 4
    bot_n <- 4
    cat("Highest months by median %WW :\n")
    print(utils::head(s$month_meds, top_n), row.names = FALSE)
    cat("\nLowest months by median %WW:\n")
    print(utils::tail(s$month_meds, bot_n), row.names = FALSE)
    
    # Only show significant pairwise differences
    pw <- s$pairwise %>%
      dplyr::mutate(
        sig = !is.na(p_adj_BH) & p_adj_BH < 0.05,
        abs_med_diff = abs(med_diff)
      ) %>%
      dplyr::filter(sig) %>%
      dplyr::arrange(p_adj_BH, dplyr::desc(abs_med_diff))
    
    cat("\n")
    
    if (nrow(pw) == 0) {
      cat("No month-to-month pairs are significant after BH correction (alpha = 0.05).\n")
      return(invisible(NULL))
    }
    
    show_n <- min(8, nrow(pw))
    pw <- utils::head(pw, show_n)
    
    cat(sprintf(
      "Top significant month-to-month differences (BH-adjusted p < 0.05; showing %d):\n",
      show_n
    ))
    
    # Bootstrap 95% CI for the rank-biserial, only for the rows shown (resamples whole counties)
    ci <- t(vapply(seq_len(nrow(pw)), function(i) {
      bs <- replicate(200, {
        idx <- eq_boot_idx(s$groups)
        eq_rb_paired(s$X[idx, pw$m1[i]], s$X[idx, pw$m2[i]])
      })
      stats::quantile(bs, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
    }, numeric(2)))
    
    mm_to_abb <- function(mm) month.abb[as.integer(mm)]
    
    out <- pw %>%
      dplyr::mutate(
        month1   = mm_to_abb(m1),
        month2   = mm_to_abb(m2),
        med_diff = round(med_diff, 2),
        rank_biserial = round(rb, 2),
        rb_95CI  = sprintf("%.2f to %.2f", ci[, 1], ci[, 2]),
        p_adj_BH_fmt = ifelse(
          p_adj_BH < 1e-100,
          "< 1e-100",
          formatC(p_adj_BH, format = "e", digits = 2)
        )
      ) %>%
      dplyr::select(month1, month2, med_diff, rank_biserial, rb_95CI, p_adj_BH = p_adj_BH_fmt)
    
    print(as.data.frame(out), row.names = FALSE)
    
    cat("\nNotes:\n")
    cat("  med_diff = median(month1 - month2) across the same units (paired).\n")
    cat("  rank_biserial: -1 to +1 effect size (positive = month1 higher in most units). Roughly: 0.1 small, 0.3 medium, 0.5 large.\n")
    cat("  95% CIs resample whole counties, because tracts in a county are not independent.\n")
    cat("  BH-adjusted p-values control false discovery rate across many pairwise tests.\n")
  })
  
  output$season_plot_median <- renderPlot({
    df <- season_monthly()
    req(df)
    
    src_lab  <- ifelse((input$season_src %||% "Gage-based") == "Gage-based", "gage-based", "NHDPlus-based")
    unit_lab <- ifelse(isTRUE(input$season_use_tracts), "tracts", "segments")
    
    ymax <- max(df$hi_iqr, na.rm = TRUE)
    
    ggplot(df, aes(x = mon_code, y = med_pct, group = 1)) +
      geom_ribbon(
        aes(ymin = lo_iqr, ymax = hi_iqr),
        fill  = "grey85",
        alpha = 0.4,
        color = NA
      ) +
      geom_line(aes(color = mon_code), linewidth = 1.1) +
      geom_point(aes(color = mon_code), size = 2.8) +
      scale_x_discrete(labels = setNames(month.abb, mon_codes)) +
      scale_color_manual(values = month_colors, guide = "none") +
      coord_cartesian(ylim = c(0, ymax * 1.12)) +
      labs(
        x = "Month",
        y = paste0("Median %WW ", unit_lab, "; zeros retained)"),
        title = paste0("Seasonality of wastewater influence (", src_lab, " denominator)"),
        subtitle = paste0("Shaded band: IQR (25–75%) ", unit_lab)
      ) +
      theme_app() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  })
  
  output$season_text <- renderPrint({
    df <- season_monthly()
    req(df)
    
    unit_lab <- ifelse(isTRUE(input$season_use_tracts), "tracts", "segments")
    
    peak_med <- df %>%
      dplyr::filter(med_pct == max(med_pct, na.rm = TRUE)) %>%
      dplyr::slice(1)
    
    cat("Monthly summary (ever-impacted units only; zeros retained):\n")
    print(df %>% dplyr::select(mon_code, month, n_units, med_pct, lo_iqr, hi_iqr))
    
    cat("\nInterpretation notes:\n")
    cat(sprintf("  n_units = number of ever-impacted %s with a value that month.\n", unit_lab))
    cat("  Error bars show IQR (25–75%) of %WW across those units.\n\n")
    
    cat("Peak median month:\n")
    cat(sprintf("  %s (%s): median %.1f%% (IQR %.1f–%.1f%%)\n",
                as.character(peak_med$month), as.character(peak_med$mon_code),
                peak_med$med_pct, peak_med$lo_iqr, peak_med$hi_iqr))
  })
  
  # ---- Download: Seasonality data -----------------
  output$download_season_data <- downloadHandler(
    filename = function() {
      paste0("ReSourceTracker_seasonality_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- season_monthly()
      req(df)
      
      df_out <- df %>%
        mutate(
          denominator_source = input$season_src %||% "Gage-based",
          rurality_filter    = input$season_rurality %||% "All",
          unit_type          = if (isTRUE(input$season_use_tracts)) "tract" else "segment"
        ) %>%
        select(denominator_source, rurality_filter, unit_type, everything())
      
      readr::write_csv(df_out, file)
    }
  )
  
  # ================= CEJST & EQUITY TAB LOGIC =================
  
  cejst_pct_col <- reactive({
    req(input$cejst_month)
    m <- input$cejst_month
    
    # Only used for Severity
    src <- input$cejst_src %||% "Gage-based"
    
    if (identical(src, "Gage-based")) {
      paste0("pct_mean_", m)
    } else {
      paste0("pct_mean_", m, "_nhd")
    }
  })
  
  cejst_summary <- reactive({
    req(input$cejst_mode, input$rurality, input$cejst_month)
    
    m       <- input$cejst_month
    ww_col  <- paste0("WW_cum_mean_", m)   # prevalence uses numerator-only
    pct_col <- cejst_pct_col()            # severity uses denominator-specific %WW
    
    dat <- fl_cejst
    
    # Filter by rurality
    if (!is.null(input$rurality) && input$rurality != "All") {
      if ("rurality" %in% names(dat)) {
        dat <- dplyr::filter(dat, rurality == input$rurality)
      } else {
        return(NULL)
      }
    }
    
    dat <- dplyr::filter(dat, !is.na(GEOID))
    
    # log10 drainage area per segment (non-finite -> NA)
    dat$lDA <- suppressWarnings(log10(as.numeric(dat$TotDASqKM)))
    dat$lDA[!is.finite(dat$lDA)] <- NA_real_
    
    # numeric + boolean CEJST columns from original cejst object
    cejst_num_cols <- c(names(cejst)[sapply(cejst, is.numeric)], cejst_lgl_cols)
    cejst_num_cols <- intersect(cejst_num_cols, names(dat))
    
    # ---- PREVALENCE: tract has ANY impacted segment (0/1) -------------------
    if (identical(input$cejst_mode, "Prevalence")) {
      
      if (!ww_col %in% names(dat)) return(NULL)
      
      dat_prev <- dat %>%
        dplyr::mutate(
          ww = .data[[ww_col]],
          impacted = is.finite(ww) & !is.na(ww) & ww > 0
        )
      
      out <- dat_prev %>%
        dplyr::group_by(GEOID) %>%
        dplyr::summarise(
          any_impacted = as.integer(any(impacted, na.rm = TRUE)),
          metric_value = any_impacted, # 0/1 per tract
          lDA   = suppressWarnings(stats::median(lDA, na.rm = TRUE)),  # all segments in the tract
          lnseg = log(dplyr::n()),
          dplyr::across(
            dplyr::all_of(cejst_num_cols),
            ~ dplyr::first(.x),
            .names = "{.col}"
          ),
          rurality = dplyr::first(rurality),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          lDA    = ifelse(is.finite(lDA), lDA, NA_real_),
          county = substr(as.character(GEOID), 1, 5)
        )
      
      return(out)
    }
    
    # ---- SEVERITY: conditional on %WW > 0 -----------------------------------
    if (!pct_col %in% names(dat)) {
      warning("Missing severity %WW column: ", pct_col)
      return(NULL)
    }
    
    dat_sev <- dat %>%
      dplyr::mutate(
        pct = .data[[pct_col]],
        impacted = is.finite(pct) & !is.na(pct) & pct > 0
      )
    
    tract_base <- dat_sev %>%
      dplyr::group_by(GEOID) %>%
      dplyr::summarise(
        n_segments_total = dplyr::n(),
        n_impacted       = sum(impacted, na.rm = TRUE),
        dplyr::across(
          dplyr::all_of(cejst_num_cols),
          ~ dplyr::first(.x),
          .names = "{.col}"
        ),
        rurality = dplyr::first(rurality),
        .groups = "drop"
      )
    
    sev_vals <- dat_sev %>%
      dplyr::filter(impacted) %>%
      dplyr::group_by(GEOID) %>%
      dplyr::summarise(
        metric_value = stats::median(pct, na.rm = TRUE),
        lDA          = suppressWarnings(stats::median(lDA, na.rm = TRUE)),  # impacted segments only
        .groups = "drop"
      )
    
    tract_base %>%
      dplyr::inner_join(sev_vals, by = "GEOID") %>%
      dplyr::mutate(
        lDA    = ifelse(is.finite(lDA), lDA, NA_real_),
        county = substr(as.character(GEOID), 1, 5)
      )
  })
  
  # Tract table for yes/no and Rurality indicators, with a Yes/No or Urban/Rural group column
  cejst_cat_df <- reactive({
    tab <- cejst_summary()
    var <- input$cejst_var_box
    req(tab, var, is_cat_var(var))
    if (identical(var, "rurality")) {
      tab %>%
        dplyr::filter(is.finite(metric_value), !is.na(rurality)) %>%
        dplyr::transmute(GEOID, metric_value, grp = factor(rurality, levels = c("Urban", "Rural")))
    } else {
      req(var %in% names(tab))
      tab %>%
        dplyr::transmute(GEOID, metric_value, v = .data[[var]]) %>%
        dplyr::filter(is.finite(metric_value), !is.na(v)) %>%
        dplyr::mutate(grp = factor(ifelse(v, "Yes", "No"), levels = c("No", "Yes"))) %>%
        dplyr::select(GEOID, metric_value, grp)
    }
  })
  
  # Indicators offered in the dropdown
  cejst_ind_cols <- function() {
    all_num <- names(cejst)[sapply(cejst, is.numeric)]
    raw <- all_num[!grepl("percentile", all_num, ignore.case = TRUE) &
                     !grepl("pct_", all_num, ignore.case = TRUE) &
                     !grepl("_pct$", all_num, ignore.case = TRUE)]
    raw <- raw[!grepl("wastewater", raw, ignore.case = TRUE)]
    if (length(raw) == 0) raw <- all_num
    c(if ("rurality" %in% names(cejst)) "rurality", cejst_lgl_cols, raw)
  }
  
  # Effect sizes for ALL indicators for the current mode / month / source / rurality filter.
  # Cached by inputs so revisiting a combination is instant.
  eq_all <- reactive({
    tab  <- cejst_summary()
    req(tab, nrow(tab) > 0)
    mode <- input$cejst_mode %||% "Prevalence"
    inds <- cejst_ind_cols()
    inds <- inds[inds == "rurality" | inds %in% names(tab)]
    res  <- dplyr::bind_rows(lapply(inds, function(v) eq_fit(tab, v, mode)))
    req(nrow(res) > 0)
    res$q_unadj <- stats::p.adjust(res$p_unadj, method = "BH")   # BH across indicators
    res$q_adj   <- stats::p.adjust(res$p_adj,   method = "BH")
    res
  }) %>%
    bindCache(input$cejst_mode, input$cejst_month, input$cejst_src, input$rurality)
  
  # Rows shown in the overview plot
  eq_forest_df <- reactive({
    res  <- eq_all()
    mode <- input$cejst_mode %||% "Prevalence"
    sc   <- if (identical(mode, "Prevalence")) function(x) log(pmax(x, 1e-6)) else function(x) x
    res  <- res[order(-abs(sc(res$effect_adj))), , drop = FALSE]
    if (!isTRUE(input$cejst_forest_all)) res <- utils::head(res, 20)
    res <- res[order(sc(res$effect_adj)), , drop = FALSE]
    res$ypos <- seq_len(nrow(res))
    res$lab  <- vapply(
      ifelse(res$indicator == "rurality", "Rurality (rural vs urban)", res$indicator),
      function(s) paste(strwrap(s, 38), collapse = "\n"), character(1)
    )
    res$sel <- res$indicator == input$cejst_var_box
    res$sig <- !is.na(res$q_adj) & res$q_adj < 0.05
    res
  })
  
  output$cejst_forest <- renderPlot({
    df   <- eq_forest_df()
    mode <- input$cejst_mode %||% "Prevalence"
    mlab <- month_labels[input$cejst_month]
    
    p <- ggplot(df, aes(x = effect_adj, y = ypos)) +
      geom_vline(xintercept = if (identical(mode, "Prevalence")) 1 else 0, colour = "#6b7280", linewidth = 0.4) +
      geom_linerange(aes(xmin = lo_adj, xmax = hi_adj, colour = sel), orientation = "y", linewidth = 0.7) +
      geom_point(aes(colour = sel, shape = sig), size = 3, stroke = 1) +
      scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), drop = FALSE, name = NULL,
                         labels = c(`TRUE` = "q < 0.05", `FALSE` = "q ≥ 0.05")) +
      scale_colour_manual(values = c(`TRUE` = "#e07b39", `FALSE` = "#2a78d6"), guide = "none") +
      scale_y_continuous(breaks = df$ypos, labels = df$lab, expand = expansion(add = 0.7)) +
      labs(
        x = if (identical(mode, "Prevalence")) "Adjusted odds ratio of any wastewater impact (log scale)"
        else "Adjusted rank correlation (rho) with %WW among impacted tracts",
        y = NULL,
        title = paste0(mode, " (", mlab, "): effect of each indicator after accounting for drainage area")
      ) +
      theme_app() +
      theme(legend.position = "bottom",
            axis.text.y = element_text(size = 11, face = "plain"),
            axis.text.x = element_text(size = 12, face = "plain"))
    if (identical(mode, "Prevalence")) p <- p + scale_x_log10()
    p
  }, height = function() if (isTRUE(input$cejst_forest_all)) 1150 else 640)
  
  # click a row -> select that indicator
  observeEvent(input$cejst_forest_click, {
    df <- eq_forest_df()
    i  <- round(input$cejst_forest_click$y)
    if (length(i) != 1 || is.na(i) || i < 1 || i > nrow(df)) return()
    updateSelectInput(session, "cejst_var_box", selected = df$indicator[i])
  })
  
  # headline card for the selected indicator
  output$cejst_card <- renderUI({
    var  <- input$cejst_var_box; req(var)
    mode <- input$cejst_mode %||% "Prevalence"
    res  <- eq_all()
    r    <- res[res$indicator == var, , drop = FALSE]
    box  <- "border:1px solid #b8bdc4; border-radius:6px; padding:12px 14px; margin-bottom:12px; background:#fff;"
    
    if (nrow(r) != 1) {
      return(tags$div(style = box, "Effect size not available for this selection ",
                      "(needs at least 50 tracts, 15 counties, and variation in the indicator)."))
    }
    
    f   <- function(x) formatC(x, format = "f", digits = 2)
    fq  <- function(x) formatC(x, format = "e", digits = 1)
    dir <- function(x) if (identical(mode, "Prevalence")) log(x) else x
    dir_a <- dir(r$effect_adj); dir_u <- dir(r$effect_unadj)
    sig_a <- !is.na(r$q_adj) && r$q_adj < 0.05
    sig_u <- !is.na(r$q_unadj) && r$q_unadj < 0.05
    
    badge <- if (sig_a && sign(dir_a) == sign(dir_u)) {
      list("Holds after adjusting for drainage area", "#2a78d6")
    } else if (sig_a) {
      list("Direction reverses after adjusting for drainage area", "#e07b39")
    } else if (sig_u) {
      list("Largely explained by drainage area", "#6b7280")
    } else {
      list("No clear association", "#6b7280")
    }
    
    is_bin <- identical(r$type, "yes/no")
    ci_a <- sprintf("95%% CI %s to %s", f(r$lo_adj), f(r$hi_adj))
    
    sentence <- if (identical(mode, "Prevalence")) {
      if (identical(var, "rurality")) {
        sprintf("Rural tracts have %s times the odds of containing any wastewater-impacted segment compared with urban tracts (adjusted OR, %s).", f(r$effect_adj), ci_a)
      } else if (is_bin) {
        sprintf("Tracts where '%s' is Yes have %s times the odds of containing any wastewater-impacted segment compared with tracts where it is No (adjusted OR, %s).", var, f(r$effect_adj), ci_a)
      } else {
        sprintf("A 1 SD higher '%s' goes with %s times the odds that a tract contains any wastewater-impacted segment (adjusted OR, %s).", var, f(r$effect_adj), ci_a)
      }
    } else {
      hi_lo <- if (r$effect_adj > 0) "higher" else "lower"
      if (identical(var, "rurality")) {
        sprintf("Among impacted tracts, rural tracts tend to have %s %%WW than urban tracts (adjusted rank correlation rho = %s, %s).", hi_lo, f(r$effect_adj), ci_a)
      } else if (is_bin) {
        sprintf("Among impacted tracts, tracts where '%s' is Yes tend to have %s %%WW than tracts where it is No (adjusted rank correlation rho = %s, %s).", var, hi_lo, f(r$effect_adj), ci_a)
      } else {
        sprintf("Among impacted tracts, higher '%s' tends to go with %s %%WW (adjusted rank correlation rho = %s, %s).", var, hi_lo, f(r$effect_adj), ci_a)
      }
    }
    
    nm <- if (identical(mode, "Prevalence")) "OR" else "rho"
    tags$div(
      style = box,
      tags$span(
        style = paste0("display:inline-block; background:", badge[[2]], "; color:#fff; font-weight:bold; ",
                       "padding:3px 10px; border-radius:12px; margin-bottom:8px;"),
        badge[[1]]
      ),
      tags$p(style = "margin:6px 0;", sentence),
      tags$p(
        style = "margin:4px 0; color:#555; font-size:90%;",
        sprintf("Adjusted BH q = %s.  Unadjusted %s = %s (95%% CI %s to %s), BH q = %s.  n = %d tracts in %d counties.",
                fq(r$q_adj), nm, f(r$effect_unadj), f(r$lo_unadj), f(r$hi_unadj), fq(r$q_unadj),
                r$n_tracts, r$n_counties)
      ),
      if (identical(mode, "Prevalence"))
        tags$p(style = "margin:4px 0; color:#555; font-size:90%;",
               tags$em("Prevalence is descriptive: it depends heavily on tract size and how many streams cross the tract."))
    )
  })
  
  # ---- Plotting: Prevalence / Severity ---------------------------------
  output$cejst_plot <- renderPlot({
    var  <- input$cejst_var_box; req(var)
    mode <- input$cejst_mode %||% "Prevalence"
    mlab <- month_labels[input$cejst_month]
    
    r <- tryCatch({ res <- eq_all(); res[res$indicator == var, , drop = FALSE] }, error = function(e) NULL)
    stat_line <- eq_stat_line(r, mode)
    
    # ---------------- yes/no and Rurality ----------------
    if (is_cat_var(var)) {
      df <- cejst_cat_df()
      req(df, nrow(df) > 0)
      
      grp_cols <- if (identical(var, "rurality")) {
        rurality_colors
      } else {
        c(No = "#8a8f98", Yes = "#E07B39")
      }
      x_lab <- if (identical(var, "rurality")) "Rurality category" else var
      ttl   <- ifelse(identical(var, "rurality"), "Rurality", var)
      
      if (identical(mode, "Prevalence")) {
        z <- stats::qnorm(0.975)
        prev_sum <- df %>%
          dplyr::group_by(grp) %>%
          dplyr::summarise(n = dplyr::n(), k = sum(metric_value == 1, na.rm = TRUE), .groups = "drop") %>%
          dplyr::mutate(
            prop   = k / n,
            denom  = 1 + z^2 / n,
            center = (prop + z^2 / (2 * n)) / denom,
            half   = (z * sqrt((prop * (1 - prop) / n) + (z^2 / (4 * n^2)))) / denom,
            lo     = pmax(0, center - half),
            hi     = pmin(1, center + half)
          )
        
        ggplot(prev_sum, aes(x = grp, y = prop, colour = grp)) +
          geom_linerange(aes(ymin = lo, ymax = hi), linewidth = 1.4) +
          geom_point(size = 5) +
          geom_text(
            aes(y = hi, label = paste0(round(100 * prop, 1), "%  (", k, " of ", n, " tracts)")),
            vjust = -0.9, size = 4.4, fontface = "bold", colour = "#1f2328"
          ) +
          scale_colour_manual(values = grp_cols, drop = FALSE) +
          scale_y_continuous(labels = scales::percent, limits = c(0, NA), expand = expansion(mult = c(0.02, 0.18))) +
          labs(
            x = x_lab,
            y = "Share of tracts with any wastewater-impacted segment",
            title = paste0("Prevalence by ", ttl, " (", mlab, ")"),
            subtitle = stat_line,
            caption = "Dots: share of tracts impacted. Bars: 95% confidence interval (Wilson)."
          ) +
          theme_app()
        
      } else {
        src       <- input$cejst_src %||% "Gage-based"
        denom_lab <- ifelse(src == "Gage-based", "gage-based", "NHDPlus-based")
        df <- dplyr::filter(df, metric_value > 0)
        req(nrow(df) > 0)
        
        sev_sum <- df %>%
          dplyr::group_by(grp) %>%
          dplyr::summarise(n = dplyr::n(), med = stats::median(metric_value, na.rm = TRUE),
                           top = max(metric_value, na.rm = TRUE), .groups = "drop") %>%
          dplyr::mutate(label = paste0("Median ", signif(med, 2), "%\n(n = ", n, ")"))
        
        ggplot(df, aes(x = grp, y = metric_value)) +
          geom_jitter(aes(colour = grp), width = 0.18, height = 0, alpha = 0.3, size = 1.4) +
          geom_boxplot(aes(fill = grp), width = 0.38, outlier.shape = NA, alpha = 0.55,
                       colour = "#1f2328", linewidth = 0.6) +
          geom_text(data = sev_sum, aes(x = grp, y = top, label = label),
                    inherit.aes = FALSE, vjust = -0.4, size = 4.2, fontface = "bold", colour = "#1f2328") +
          scale_colour_manual(values = grp_cols, drop = FALSE) +
          scale_fill_manual(values = grp_cols, drop = FALSE) +
          scale_y_log10(labels = scales::label_number(suffix = "%", drop0trailing = TRUE),
                        expand = expansion(mult = c(0.05, 0.2))) +
          labs(
            x = x_lab,
            y = "Severity: median %WW among impacted segments (log scale)",
            title = paste0("Severity by ", ttl, " (", mlab, ", ", denom_lab, " denominator)"),
            subtitle = stat_line,
            caption = "Each point is a tract. Box: median and middle 50% of tracts."
          ) +
          theme_app()
      }
    } else {
      # ---------------- numeric indicator: summary at each distinct value (no smoothing) ----------------
      tab <- cejst_summary()
      req(tab, var %in% names(tab))
      df <- tab %>%
        dplyr::transmute(GEOID, metric_value, x = suppressWarnings(as.numeric(.data[[var]]))) %>%
        dplyr::filter(is.finite(metric_value), is.finite(x))
      req(nrow(df) > 0)
      
      # One group per distinct value; with more than 15 distinct values, use tenths
      n_uniq    <- dplyr::n_distinct(df$x)
      use_vals  <- n_uniq <= 15
      df$g      <- if (use_vals) df$x else stats::ave(df$x, dplyr::ntile(df$x, 10), FUN = mean)
      grp_note  <- if (use_vals) "One summary per distinct value of the indicator." else "Summaries for tenths of the indicator (equal numbers of tracts in each)."
      size_note <- if (use_vals) " Dot size = number of tracts." else ""
      wrap_cap  <- function(s) paste(strwrap(s, 100), collapse = "\n")
      
      # dot layers: size shows tract count only when the groups are distinct values
      pt_layers <- function(colour, data, ycol) {
        if (use_vals) {
          list(
            geom_point(data = data, aes(x = g, y = .data[[ycol]], size = n), colour = colour),
            scale_size_area(max_size = 7, name = "Tracts at this value",
                            breaks = function(lims) unique(round(pretty(lims, n = 4))),
                            labels = scales::label_comma(accuracy = 1))
          )
        } else {
          list(geom_point(data = data, aes(x = g, y = .data[[ycol]]),
                          colour = colour, size = 3.5))
        }
      }
      
      if (identical(mode, "Prevalence")) {
        z <- stats::qnorm(0.975)
        sm <- df %>%
          dplyr::group_by(g) %>%
          dplyr::summarise(n = dplyr::n(), k = sum(metric_value == 1), .groups = "drop") %>%
          dplyr::mutate(
            prop   = k / n,
            denom  = 1 + z^2 / n,
            center = (prop + z^2 / (2 * n)) / denom,
            half   = (z * sqrt((prop * (1 - prop) / n) + (z^2 / (4 * n^2)))) / denom,
            lo     = pmax(0, center - half),
            hi     = pmin(1, center + half)
          )
        
        ggplot(sm, aes(x = g, y = prop)) +
          geom_linerange(aes(ymin = lo, ymax = hi), colour = "#2a78d6", linewidth = 1) +
          pt_layers("#2a78d6", sm, "prop") +
          scale_y_continuous(labels = scales::percent) +
          coord_cartesian(ylim = c(0, 1)) +
          labs(
            x = var,
            y = "Share of tracts with any wastewater-impacted segment",
            title = paste0("Prevalence vs ", var, " (", mlab, ")"),
            subtitle = stat_line,
            caption = wrap_cap(paste0("Dot: share of tracts impacted; bar: 95% CI (Wilson).", size_note, " ",
                                      grp_note, " Raw values; the effect size in the subtitle is adjusted for drainage area."))
          ) +
          theme_app() +
          theme(legend.position = "bottom")
        
      } else {
        src       <- input$cejst_src %||% "Gage-based"
        denom_lab <- ifelse(src == "Gage-based", "gage-based", "NHDPlus-based")
        df <- dplyr::filter(df, metric_value > 0)
        req(nrow(df) > 0)
        
        sm <- df %>%
          dplyr::group_by(g) %>%
          dplyr::summarise(
            n   = dplyr::n(),
            med = stats::median(metric_value),
            q1  = stats::quantile(metric_value, 0.25, names = FALSE),
            q3  = stats::quantile(metric_value, 0.75, names = FALSE),
            .groups = "drop"
          )
        
        # faint points: at the real value (tenths mode) or spread slightly sideways (distinct-value mode)
        df$xp <- if (use_vals) df$g else df$x
        ug <- sort(unique(sm$g))
        jw <- if (!use_vals) 0 else if (length(ug) > 1) 0.25 * min(diff(ug)) else 0.1
        
        ggplot() +
          geom_jitter(data = df, aes(x = xp, y = metric_value), width = jw, height = 0,
                      alpha = 0.2, size = 1.2, colour = "#6b7280") +
          geom_linerange(data = sm, aes(x = g, ymin = q1, ymax = q3), colour = "#2a78d6", linewidth = 1.2) +
          pt_layers("#2a78d6", sm, "med") +
          scale_y_log10(labels = scales::label_number(suffix = "%", drop0trailing = TRUE)) +
          labs(
            x = var,
            y = "Severity: median %WW among impacted segments (log scale)",
            title = paste0("Severity vs ", var, " (", mlab, ", ", denom_lab, " denominator)"),
            subtitle = stat_line,
            caption = wrap_cap(paste0("Faint points: individual tracts. Dot: median; bar: middle 50% of tracts.", size_note, " ",
                                      grp_note, " Raw values; the effect size in the subtitle is adjusted for drainage area."))
          ) +
          theme_app() +
          theme(legend.position = "bottom")
      }
    }
  })
  
  # ---- Download: Equity data (tract-level values) ----------------------------
  output$download_cejst_data <- downloadHandler(
    filename = function() {
      paste0("ReSourceTracker_equity_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tab <- cejst_summary()
      req(tab)
      var <- input$cejst_var_box %||% NA_character_
      
      mode_now    <- input$cejst_mode %||% "Prevalence"
      denom_now   <- if (identical(mode_now, "Severity")) (input$cejst_src %||% "Gage-based") else NA_character_
      month_code  <- input$cejst_month
      month_label <- month_labels[month_code]
      
      df_out <- tab %>%
        dplyr::mutate(
          analysis_mode      = mode_now,
          denominator_source = denom_now,
          month_code         = month_code,
          month_label        = unname(month_label),
          cejst_variable     = var,
          indicator_value    = if (!is.na(var) && var %in% names(tab)) .data[[var]] else NA,
          rurality_filter    = input$rurality %||% "All"
        ) %>%
        dplyr::select(
          analysis_mode, denominator_source, month_code, month_label,
          cejst_variable, rurality_filter, GEOID, indicator_value, metric_value,
          dplyr::everything()
        )
      
      readr::write_csv(df_out, file)
    }
  )
  
  # ---- Census tract info panel (Map tab) ------------------------------------
  output$tract_info <- renderPrint({
    gid <- selected_tract_id()
    if (is.null(gid)) {
      cat("Click a census tract polygon to see \ntract-level wastewater metrics here.\n")
      return(invisible(NULL))
    }
    
    tr_row <- tracts_info[tracts_info$GEOID == gid, , drop = FALSE]
    if (nrow(tr_row) == 0) {
      cat("Selected tract not found in tract table.\n")
      return(invisible(NULL))
    }
    
    # Month and denominator currently selected on the Map tab
    m   <- input$month %||% "06"
    src <- input$src   %||% "Gage-based"
    
    pct_col <- if (identical(src, "Gage-based")) {
      paste0("pct_mean_", m)
    } else {
      paste0("pct_mean_", m, "_nhd")
    }
    ww_col <- paste0("WW_cum_mean_", m)
    
    seg <- fl_cejst %>%
      dplyr::filter(GEOID == gid)
    
    if (!nrow(seg)) {
      cat("No stream segments intersect this tract.\n")
      return(invisible(NULL))
    }
    
    if (!all(c(pct_col, ww_col) %in% names(seg))) {
      cat("Missing wastewater columns for this month / denominator.\n")
      return(invisible(NULL))
    }
    
    seg <- seg %>%
      dplyr::mutate(
        pct = suppressWarnings(as.numeric(.data[[pct_col]])),
        ww  = suppressWarnings(as.numeric(.data[[ww_col]])),
        impacted = is.finite(ww) & ww > 0
      )
    
    n_seg_total <- nrow(seg)
    n_imp       <- sum(seg$impacted, na.rm = TRUE)
    prevalence  <- n_imp > 0
    
    # Severity: %WW among impacted segments
    seg_imp <- seg %>% dplyr::filter(impacted, is.finite(pct))
    if (nrow(seg_imp) > 0) {
      sev_med <- stats::median(seg_imp$pct, na.rm = TRUE)
      sev_p90 <- stats::quantile(seg_imp$pct, 0.9, na.rm = TRUE)
      sev_max <- max(seg_imp$pct, na.rm = TRUE)
    }
    
    cat("Census tract:", gid, "\n")
    cat("Rurality:    ", tr_row$rurality[1] %||% "NA", "\n")
    cat("Low income?: ", ifelse(isTRUE(tr_row$low_income[1]), "Yes",
                                ifelse(isFALSE(tr_row$low_income[1]), "No", "NA")), "\n")
    
    cat("\nMonth:        ", month_labels[m], "\n")
    cat("Denominator:  ", ifelse(identical(src, "Gage-based"),
                                 "Gage-based streamflow",
                                 "NHDPlus-based streamflow"), "\n\n")
    
    cat("Segments intersecting tract: ", n_seg_total, "\n")
    cat("Segments with WW>0 (impacted): ", n_imp, "\n")
    cat("Prevalence this month: ",
        ifelse(prevalence, "Impacted", "Not impacted"),
        "\n\n")
    
    if (nrow(seg_imp) == 0) {
      cat("No impacted segments to compute severity (%WW) \nfor this tract.\n")
    } else {
      cat("Severity among impacted segments (", nrow(seg_imp), " segments):\n", sep = "")
      cat("  Median %WW: ", round(sev_med, 1), "%\n", sep = "")
      cat("  90th pct %WW: ", round(sev_p90, 1), "%\n", sep = "")
      cat("  Max %WW: ", round(sev_max, 1), "%\n", sep = "")
    }
  })
}

# --- Launch App -------------------------------------------------------------
shinyApp(ui = ui, server = server)

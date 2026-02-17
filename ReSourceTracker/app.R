# --- Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(leaflet)
library(sf)
library(shiny)
library(viridisLite)

# --- Relative Paths ----------------------------------------------------------
data_dir <- "data-raw"

paths <- list(
  dwtp_rds      = file.path(data_dir, "geocoded_dwtp_app.rds"),
  cejst_rds     = file.path(data_dir, "cejst_with_rurality.rds"),
  wwtp_rds      = file.path(data_dir, "wwtp_pts_app.rds"),
  gages_rds     = file.path(data_dir, "usgs_gages_sf_app.rds"),
  monthly_flows = file.path(data_dir, "CO_Seasonal_Flow_Summary_Feb2026.csv"), 
  flowlines     = file.path(data_dir, "flowlines_small.rds"),
  fl_cejst      = file.path(data_dir, "flowlines_cejst_table.rds")
)

# Pre-trimmed flowlines (sf, EPSG:4326)
flowlines <- readRDS(paths$flowlines)

# CEJST + rurality (no geometry, small)
cejst <- readRDS(paths$cejst_rds)

# Flowline x CEJST table for Equity + Seasonality (no geometry)
fl_cejst <- readRDS(paths$fl_cejst)

# --- Constants ---------------------------------------------------------------
mon_codes    <- sprintf("%02d", 1:12)          # "01"–"12"
month_labels <- setNames(month.name, mon_codes)  # names "01".."12" → "January"...
month_colors <- setNames(
  c(
    "#3B4F8F", # 01 Jan  - deep cool blue
    "#4B6CB7", # 02 Feb  - cooler blue
    "#3C9D82", # 03 Mar  - teal (early spring)
    "#54B887", # 04 Apr  - fresh green
    "#7BC96F", # 05 May  - brighter green
    "#F4D35E", # 06 Jun  - soft yellow
    "#F6B35E", # 07 Jul  - warm golden
    "#F28E52", # 08 Aug  - orange
    "#E76F51", # 09 Sep  - reddish orange
    "#D95F5F", # 10 Oct  - warm red
    "#A8558C", # 11 Nov  - magenta
    "#6D4C9B"  # 12 Dec  - cool purple
  ),
  mon_codes
)
# Consistent colors for rurality categories (Urban / Rural) 
rurality_colors <- c(
  "Urban"    = "#1b9e77",
  "Rural"    = "#7570b3"
)

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

# --- Load Datasets -----------------------------------------------------------

# DWTPs
geocoded_dwtp <- readRDS(paths$dwtp_rds)

# WWTPs
wwtp_pts <- readRDS(paths$wwtp_rds)

# --- UI ----------------------------------------------------------------------
ui <- navbarPage(
  "ReSource Tracker",
  
  # ========================= TAB 1: MAP ======================================
  tabPanel(
    "Map",
    fluidPage(
      fluidRow(
        column(
          width = 3,
          
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
          )
        ),
        
        column(
          width = 9,
          leafletOutput("map", height = "900px")
        )
      )
    )
  ),
  
  # ====================== TAB 2: HOW TO USE =================================
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
              "and propagated downstream along hydrologically similar segments. ",
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
            "and compares those exposures across bins of tract-level CEJST indicators."
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
  
  # ==================== TAB 3: EQUITY =======================================
  tabPanel(
    "Equity",
    fluidPage(
      fluidRow(
        column(
          width = 3,
          h4("CEJST & Exposure Settings"),
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
          
          ## <-- Download block moved into sidebar here
          tags$hr(),
          h4("Download"),
          helpText("Export tract-level data used in the current equity plot."),
          downloadButton("download_cejst_data", "Download equity CSV"),
          
          tags$hr(),
          h4("Grouping variable (x-axis bins)"),
          helpText(
            "Choose a CEJST numeric indicator (binned) or Rurality (categorical).",
            tags$br(), tags$br(),
            strong("To compare Urban/Rural:"),
            tags$br(),
            HTML("&bull; Set <b>Rurality category</b> to <b>All</b>"),
            tags$br(),
            HTML("&bull; Set <b>Grouping variable</b> to <b>Rurality</b>")
          ),
          
          selectInput(
            "cejst_var_box",
            "CEJST variable",
            choices = NULL
          ),
          
          conditionalPanel(
            condition = "input.cejst_var_box != 'rurality'",
            sliderInput(
              "cejst_nbins",
              "Number of bins (numeric variables only)",
              min   = 2,
              max   = 6,
              value = 4,
              step  = 1,
              round = TRUE,
              ticks = FALSE
            )
          )
        ),
        
        column(
          width = 9,
          h3("Exposure Plot: By CEJST Indicator or Rurality"),
          plotOutput("cejst_plot", height = "520px"),
          tags$hr(),
          h4("Statistical comparison between groups"),
          verbatimTextOutput("cejst_stats")
        )
      )
    )
  ),
  
  #==================  TAB 4: SEASONALITY ===================================
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
  
  # ================== TAB 5: DATA SOURCES ===================================
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
              "network to summarize wastewater exposure by community."
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
              strong("Tract-level exposure summary: "),
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
  
  # ================== TAB 6: DOWNLOADS & METHODS =============================
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
              "Monthly flows are propagated downstream along hydrologically ",
              "similar segments (based on modeled mean annual flow), stopping ",
              "at large changes in flow or at new gages."
            ),
            tags$li(
              "At confluences, flows from multiple upstream gaged branches ",
              "are summed to represent the combined segment."
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

# --- SERVER ------------------------------------------------------------------
server <- function(input, output, session) {
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # ---- GAGES (markers; monthly stats) ---------------------------------------
  safe_round <- function(x) ifelse(is.finite(x), round(x, 1), "NA")
  pal        <- leaflet::colorNumeric(palette = "plasma", domain = c(0, 100), na.color = "transparent")
  
  usgs_gages_sf <- readRDS(paths$gages_rds)
  
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
  
  # ---- Precompute flowlines–CEJST join for equity tab ----------------------
  # Populate CEJST variable choices (numeric, non-percentile) + rurality
  observe({
    cejst_num_all <- names(cejst)[sapply(cejst, is.numeric)]
    
    cejst_num_raw <- cejst_num_all[!grepl("percentile", cejst_num_all, ignore.case = TRUE) &
                                     !grepl("pct_", cejst_num_all, ignore.case = TRUE) &
                                     !grepl("_pct$", cejst_num_all, ignore.case = TRUE)]
    cejst_num_raw <- setdiff(cejst_num_raw, "wastewater_pct")
    if (length(cejst_num_raw) == 0) cejst_num_raw <- cejst_num_all
    
    if ("rurality" %in% names(cejst)) {
      var_values <- c("rurality", cejst_num_raw)
      var_labels <- c("Rurality", cejst_num_raw)
    } else {
      var_values <- cejst_num_raw
      var_labels <- cejst_num_raw
    }
    
    updateSelectInput(
      session, "cejst_var_box",
      choices  = stats::setNames(var_values, var_labels),
      selected = var_values[1]
    )
  })
  
  
  # ---- BASE MAP (Map tab) ----------------------------------------------------
  output$map <- renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Base") %>%
      
      leaflet::addCircleMarkers(
        data  = geocoded_dwtp,
        lng   = ~Longitude, lat = ~Latitude,
        popup = ~paste0("<b>", `PWS Name`, "</b><br>Pop: ", `Population Served`,
                        "<br>Source: ", `Primary Source`),
        radius = 6, color = "blue", fillOpacity = 0.8, group = "DWTP"
      ) %>%
      leaflet::addCircleMarkers(
        data  = wwtp_pts,
        popup = ~paste0("<b>", Facility, "</b><br>Design Flow: ", DesignFlow, " MGD"),
        radius = 6, color = "brown", fillOpacity = 0.8, group = "WWTP"
      ) %>%
      leaflet::addCircleMarkers(
        data   = usgs_gages_sf,
        radius = 5, color = "black", fillOpacity = 1, stroke = TRUE, weight = 1,
        label  = ~station_nm,
        popup  = ~popup_str,
        group  = "USGS Gages"
      ) %>%
      leaflet::addLayersControl(
        overlayGroups = c("Streams","DWTP","WWTP","USGS Gages"),
        options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex = TRUE)
      )
  })
  
  observeEvent(input$reset_map, {
    bb <- sf::st_bbox(flowlines)
    leaflet::leafletProxy("map") %>%
      leaflet::fitBounds(
        lng1 = bb["xmin"], lat1 = bb["ymin"],
        lng2 = bb["xmax"], lat2 = bb["ymax"]
      )
  })
  
  observeEvent(list(input$src, input$month), {
    spec <- cols_for()
    dat  <- filtered_flowlines()
    m    <- input$month
    mlab <- month_labels[m]
    
    # --- identify %WW column + impacted segments --------------------------
    vals <- suppressWarnings(as.numeric(dat[[spec$pct]]))
    
    # impacted = finite %WW > 0
    impacted <- is.finite(vals) & !is.na(vals) & vals > 0
    
    # if nothing is impacted for this month/source, just clear streams + legend
    if (!any(impacted)) {
      proxy <- leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("Streams") %>%
        leaflet::removeControl("pctLegend") %>%
        leaflet::removeControl("symbolLegend")
      
      # (optional) you could add a small control saying "No impacted segments".
      return(invisible(NULL))
    }
    
    # keep ONLY impacted segments
    dat  <- dat[impacted, , drop = FALSE]
    vals <- vals[impacted]
    
    # color only for impacted segments
    dat$._color <- pal(vals)
    
    # --- Build popup text only for impacted segments ----------------------
    if (input$src == "Gage-based") {
      ww_mean_col <- paste0("WW_cum_mean_", m)
      ww_low_col  <- paste0("WW_cum_low_",  m)
      ww_high_col <- paste0("WW_cum_high_", m)
      sd_col      <- paste0("WW_cum_sd_",   m)
      
      pct_mean_col <- paste0("pct_mean_", m)
      pct_min_col  <- paste0("pct_min_",  m)
      pct_max_col  <- paste0("pct_max_",  m)
      
      dat$popup_tmp <- paste0(
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
               paste0("<b>Flow source:</b> ", dat$flow_source, "<br>"), "")
      )
    } else {
      q_nhd_col    <- paste0("QE_", m, "_nhd")
      pct_mean_nhd <- paste0("pct_mean_", m, "_nhd")
      
      dat$popup_tmp <- paste0(
        "<b>Hydroseq:</b> ", dat$Hydroseq, "<br><br>",
        "<b>", mlab, " NHDPlus flow:</b> ",
        safe_round(dat[[q_nhd_col]]), " cfs<br><br>",
        "<b>", mlab, " % Wastewater (NHDPlus-based):</b><br>",
        "mean: ", safe_round(dat[[pct_mean_nhd]]), "%<br><br>"
      )
    }
    
    # --- Strip to just what Leaflet needs (plus geometry) ------------------
    dat_leaf <- dat %>%
      dplyr::select(
        Hydroseq,
        QE_MA,
        ._color,
        popup_tmp
        # geometry carried automatically by sf
      )
    
    proxy <- leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("Streams") %>%
      leaflet::removeControl("pctLegend") %>%
      leaflet::removeControl("symbolLegend") %>%
      leaflet::addPolylines(
        data   = dat_leaf,
        weight = ~scales::rescale(log1p(QE_MA), to = c(0.2, 4)),
        color  = ~._color,
        opacity = 0.85,
        popup   = ~popup_tmp,
        popupOptions = leaflet::popupOptions(maxWidth = 600),
        group   = "Streams"
      )
    
    # Continuous legend for %WW colors (still 0–100%)
    proxy <- proxy %>%
      leaflet::addLegend(
        position = "bottomright",
        pal      = pal,
        values   = c(0, 100),
        title    = paste0("% Wastewater (", input$src, ", ", mlab, ")"),
        opacity  = 1,
        labFormat = leaflet::labelFormat(suffix = "%"),
        layerId  = "pctLegend"
      )
    
    # Updated symbol legend: note that only impacted segments are drawn
    symbol_legend_html <- HTML(
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
         Only stream segments with wastewater contribution (%WW &gt; 0) are shown.
       </div>
     </div>"
    )
    
    proxy %>%
      leaflet::addControl(
        html     = symbol_legend_html,
        position = "bottomleft",
        layerId  = "symbolLegend"
      )
  })
  
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
    unit_lab   <- ifelse(use_tracts, "tracts", "segments")
    
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
    
    # choose unit id
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
    out <- long_imp %>%
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
    
    out
  })
  
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
    
    pairs <- combn(mon_codes, 2, simplify = FALSE)
    pw <- lapply(pairs, function(p) {
      a <- wide_cc[[p[1]]]; b <- wide_cc[[p[2]]]
      wt <- stats::wilcox.test(a, b, paired = TRUE, exact = FALSE)
      data.frame(m1 = p[1], m2 = p[2],
                 med_diff = stats::median(a - b, na.rm = TRUE),
                 p_value = wt$p.value,
                 stringsAsFactors = FALSE)
    }) %>% dplyr::bind_rows()
    
    pw$p_adj_BH <- p.adjust(pw$p_value, method = "BH")
    
    month_meds <- tibble::tibble(
      mon_code = mon_codes,
      month    = unname(month_labels[mon_codes]),
      med      = apply(X, 2, stats::median, na.rm = TRUE)
    ) %>% dplyr::arrange(dplyr::desc(med))
    
    list(n = nrow(wide_cc), friedman = fried, pairwise = pw, month_meds = month_meds)
  })
  
  output$season_stats_text <- renderPrint({
    s <- season_stats()
    req(s)
    
    unit_lab <- ifelse(isTRUE(input$season_use_tracts), "tracts", "segments")
    
    if (is.null(s$friedman) || is.null(s$pairwise) || is.null(s$month_meds)) {
      cat("Not enough complete units across all 12 months to run paired month-to-month tests.\n")
      cat("Tip: switch to tract-level, or loosen missingness (but paired tests need complete cases).\n")
      return(invisible(NULL))
    }
    
    # 1) Overall test
    cat(sprintf("Complete paired units (n_complete): %d\n", s$n))
    cat("Overall seasonality (Friedman):\n")
    cat(sprintf("  chi-squared = %.3f, df = %d, p = %s\n\n",
                unname(s$friedman$statistic),
                unname(s$friedman$parameter),
                format.pval(s$friedman$p.value, digits = 2, eps = 1e-100)))
    
    # 2) Ranked months by median
    top_n <- 4
    bot_n <- 4
    cat("Highest months by median %WW :\n")
    print(utils::head(s$month_meds, top_n), row.names = FALSE)
    cat("\nLowest months by median %WW:\n")
    print(utils::tail(s$month_meds, bot_n), row.names = FALSE)
    
    # 3) Only show “meaningful” significant pairwise differences
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
    
    cat(sprintf(
      "Top significant month-to-month differences (BH-adjusted p < 0.05; showing %d):\n",
      show_n
    ))
    
    # Update month labels (01 -> Jan)
    mm_to_abb <- function(mm) month.abb[as.integer(mm)]
    
    out <- pw %>%
      dplyr::mutate(
        month1   = mm_to_abb(m1),
        month2   = mm_to_abb(m2),
        med_diff = round(med_diff, 2),
        p_adj_BH_fmt = ifelse(
          p_adj_BH < 1e-100,
          "< 1e-100",
          formatC(p_adj_BH, format = "e", digits = 2)
        )
      ) %>%
      dplyr::select(month1, month2, med_diff, p_adj_BH = p_adj_BH_fmt) %>%
      utils::head(show_n)
    
    print(out, row.names = FALSE)
    
    cat("\nNotes:\n")
    cat("  med_diff = median(month1 - month2) across the same units (paired).\n")
    cat("  BH-adjusted p-values control false discovery rate across many pairwise tests.\n")
  })
  
  season_monthly <- shiny::debounce(season_monthly, 400)
  
  output$season_plot_median <- renderPlot({
    df <- season_monthly()
    req(df)
    
    src_lab  <- ifelse((input$season_src %||% "Gage-based") == "Gage-based", "gage-based", "NHDPlus-based")
    unit_lab <- ifelse(isTRUE(input$season_use_tracts), "tracts", "segments")
    
    # y-limit with a bit of headroom
    ymax <- max(df$hi_iqr, na.rm = TRUE)
    
    ggplot(df, aes(x = mon_code, y = med_pct, group = 1)) +
      # IQR ribbon: one neutral fill for all months (no varying aesthetics here)
      geom_ribbon(
        aes(ymin = lo_iqr, ymax = hi_iqr),
        fill  = "grey85",
        alpha = 0.4,
        color = NA
      ) +
      # Seasonal colors for line + points
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
      theme(
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5
        )
      )
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
      
      # Add context columns (denominator, rurality filter, unit type)
      df_out <- df %>%
        mutate(
          denominator_source = input$season_src %||% "Gage-based",
          rurality_filter    = input$season_rurality %||% "All",
          unit_type          = if (isTRUE(input$season_use_tracts)) "tract" else "segment"
        ) %>%
        # move context columns to front
        select(denominator_source, rurality_filter, unit_type, everything())
      
      readr::write_csv(df_out, file)
    }
  )
  
  # ================= CEJST & EQUITY TAB LOGIC =================
  
  cejst_pct_col <- reactive({
    req(input$cejst_month)
    m <- input$cejst_month
    
    # Only used for Severity; default safely if not present
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
    
    # Filter by rurality if requested
    if (!is.null(input$rurality) && input$rurality != "All") {
      if ("rurality" %in% names(dat)) {
        dat <- dplyr::filter(dat, rurality == input$rurality)
      } else {
        return(NULL)
      }
    }
    
    dat <- dplyr::filter(dat, !is.na(GEOID))
    
    # numeric CEJST columns from original cejst object
    cejst_num_cols <- names(cejst)[sapply(cejst, is.numeric)]
    
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
          dplyr::across(
            dplyr::all_of(cejst_num_cols),
            ~ dplyr::first(.x),
            .names = "{.col}"
          ),
          rurality = dplyr::first(rurality),
          .groups = "drop"
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
    
    dat_imp <- dat_sev %>%
      dplyr::filter(impacted)
    
    sev_vals <- dat_imp %>%
      dplyr::group_by(GEOID) %>%
      dplyr::summarise(
        metric_value = stats::median(pct, na.rm = TRUE),
        .groups = "drop"
      )
    
    out <- tract_base %>%
      dplyr::inner_join(sev_vals, by = "GEOID")
    
    out
  })
  
  # Create bins (shared for prevalence/severity)
  cejst_binned_raw <- reactive({
    tab <- cejst_summary()
    req(tab)
    
    var <- input$cejst_var_box
    req(var)
    
    # --- Rurality path (categorical bins) ---
    if (identical(var, "rurality")) {
      req("rurality" %in% names(tab))
      
      df <- tab %>%
        dplyr::select(GEOID, metric_value, rurality) %>%
        dplyr::filter(
          is.finite(metric_value),
          !is.na(rurality),
          rurality %in% c("Urban", "Rural")   # <--- drop anything weird
        )
      
      df$var_bin <- factor(df$rurality, levels = c("Urban", "Rural"))
      return(df)
    }
    
    # --- Numeric CEJST variable path ---
    req(var %in% names(tab))
    
    df <- tab %>%
      dplyr::transmute(
        GEOID,
        metric_value,
        var_value = suppressWarnings(as.numeric(.data[[var]]))
      ) %>%
      dplyr::filter(is.finite(metric_value), is.finite(var_value))
    
    req(nrow(df) > 0)
    
    # Clamp bins to something valid
    cur_nb <- input$cejst_nbins %||% 4L
    nb     <- max(2L, min(6L, as.integer(cur_nb)))
    
    n_uniq <- dplyr::n_distinct(df$var_value)
    if (n_uniq <= 1) {
      df$var_bin <- factor("All tracts")
      return(df)
    }
    
    nb_eff <- min(nb, n_uniq)
    
    # bin by rank -> stable bins even with ties
    df <- df %>%
      dplyr::mutate(
        .r      = dplyr::dense_rank(var_value),
        .bin_id = dplyr::ntile(.r, nb_eff)
      )
    
    # label bins by actual value ranges
    lab_tbl <- df %>%
      dplyr::group_by(.bin_id) %>%
      dplyr::summarise(
        lo = min(var_value, na.rm = TRUE),
        hi = max(var_value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(.bin_id) %>%
      dplyr::mutate(
        label = paste0("[", round(lo, 3), ", ", round(hi, 3), "]"),
        label = dplyr::if_else(.bin_id == 1, paste0(label, " (lowest)"), label),
        label = dplyr::if_else(.bin_id == dplyr::n(), paste0(label, " (highest)"), label)
      )
    
    # Make sure levels are unique (avoid duplicated factor level error)
    lab_levels <- unique(lab_tbl$label)
    
    df <- df %>%
      dplyr::left_join(
        lab_tbl %>% dplyr::select(.bin_id, label),
        by = ".bin_id"
      ) %>%
      dplyr::mutate(
        var_bin = factor(label, levels = lab_levels)
      ) %>%
      dplyr::select(GEOID, metric_value, var_bin)
    
    df
  })
  
  # Debounce to avoid recomputing during rapid slider / selector changes
  cejst_binned <- shiny::debounce(cejst_binned_raw, 400)
  
  # ---- Dynamic max bins for slider (mode-specific, and respects label merging) ----
  observeEvent({
    list(
      input$cejst_var_box,
      input$cejst_mode,
      input$cejst_month,
      input$rurality,
      input$cejst_src
    )
  }, {
    var <- input$cejst_var_box
    req(var)
    
    # If rurality, slider is irrelevant (hidden), but we'll pin it safely anyway
    if (identical(var, "rurality")) {
      updateSliderInput(
        session, "cejst_nbins",
        min   = 2,
        max   = 2,
        value = 2
      )
      return()
    }
    
    tab <- cejst_summary()
    req(tab)
    req("metric_value" %in% names(tab))
    req(var %in% names(tab))
    
    # Same numeric-filtering logic as cejst_binned()
    df <- tab %>%
      dplyr::transmute(
        GEOID,
        metric_value,
        var_value = suppressWarnings(as.numeric(.data[[var]]))
      ) %>%
      dplyr::filter(
        is.finite(metric_value),
        is.finite(var_value)
      )
    
    if (!nrow(df)) return()
    
    # Helper: compute the largest k (<= 6) that yields k DISTINCT labels
    max_feasible_bins <- function(v) {
      n_uniq <- dplyr::n_distinct(v)
      if (n_uniq <= 1) return(1L)
      
      max_try <- min(6L, n_uniq)
      
      for (k in seq(from = max_try, to = 2L, by = -1L)) {
        df_k <- tibble::tibble(var_value = v) %>%
          dplyr::mutate(
            .r      = dplyr::dense_rank(var_value),
            .bin_id = dplyr::ntile(.r, k)
          )
        
        lab_tbl <- df_k %>%
          dplyr::group_by(.bin_id) %>%
          dplyr::summarise(
            lo = min(var_value, na.rm = TRUE),
            hi = max(var_value, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::arrange(.bin_id)
        
        # Key based on numeric range only; rounding to avoid floating slop
        key <- paste0(round(lab_tbl$lo, 6), "_", round(lab_tbl$hi, 6))
        
        if (dplyr::n_distinct(key) == k) {
          return(k)
        }
      }
      
      1L
    }
    
    mx_eff <- max_feasible_bins(df$var_value)
    mx     <- max(2L, min(6L, as.integer(mx_eff)))
    
    updateSliderInput(
      session,
      "cejst_nbins",
      min   = 2,
      max   = mx,
      value = mx  # default to the *largest* feasible number of bins
    )
  }, ignoreInit = FALSE)
  
  # ---- Plotting: Prevalence / Severity ---------------------------------
  output$cejst_plot <- renderPlot({
    df <- cejst_binned()
    req(df)
    df$var_bin <- droplevels(df$var_bin)
    
    var  <- input$cejst_var_box
    mlab <- month_labels[input$cejst_month]
    
    mode <- input$cejst_mode %||% "Prevalence"
    
    if (identical(mode, "Prevalence")) {
      
      prev_sum <- df %>%
        dplyr::group_by(var_bin) %>%
        dplyr::summarise(
          n    = dplyr::n(),
          k    = sum(metric_value == 1, na.rm = TRUE),
          prop = ifelse(n > 0, k / n, NA_real_),
          .groups = "drop"
        )
      
      z <- stats::qnorm(0.975)
      
      prev_sum <- prev_sum %>%
        dplyr::mutate(
          denom  = 1 + z^2 / n,
          center = (prop + z^2 / (2 * n)) / denom,
          half   = (z * sqrt((prop * (1 - prop) / n) + (z^2 / (4 * n^2)))) / denom,
          lo     = pmax(0, center - half),
          hi     = pmin(1, center + half)
        )
      
      title_txt <- paste0(
        "Prevalence by ",
        ifelse(identical(var, "rurality"), "Rurality", var),
        " (", mlab, "; prevalence uses WW>0 and is denominator-independent)"
      )
      
      ggplot(prev_sum, aes(x = var_bin, y = prop, fill = var_bin)) +
        geom_col(width = 0.75, color = "white", linewidth = 0.2) +
        geom_text(
          aes(label = paste0(round(100 * prop, 1), "% (", k, "/", n, ")")),
          vjust = -0.4,
          size = 4.2,
          fontface = "bold"
        ) +
        {
          if (identical(var, "rurality")) {
            scale_fill_manual(values = rurality_colors, drop = FALSE)
          } else {
            scale_fill_viridis_d(option = "C", end = 0.9)
          }
        } +
        labs(
          x = if (identical(var, "rurality")) "Rurality category" else paste0("Bins of ", var),
          y = "Share of tracts with any wastewater-impacted segment",
          title = title_txt
        ) +
        theme_app() +
        theme(
          axis.text.x = element_text(
            angle = if (identical(var, "rurality")) 0 else 25,
            hjust = if (identical(var, "rurality")) 0.5 else 1
          )
        )
      
    } else {
      
      src       <- input$cejst_src %||% "Gage-based"
      denom_lab <- ifelse(src == "Gage-based", "gage-based", "NHDPlus-based")
      
      ymax <- max(df$metric_value, na.rm = TRUE)
      
      sev_sum <- df %>%
        dplyr::group_by(var_bin) %>%
        dplyr::summarise(
          n   = dplyr::n(),
          med = stats::median(metric_value, na.rm = TRUE),
          mx  = max(metric_value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          y_lab = pmin(mx + 0.06 * ymax, ymax * 1.08),
          label = paste0("Median: ", round(med, 1), "%\n(n=", n, ")")
        )
      
      ggplot(df, aes(x = var_bin, y = metric_value, fill = var_bin)) +
        geom_boxplot(outlier.alpha = 0.5) +
        geom_text(
          data = sev_sum,
          aes(x = var_bin, y = y_lab, label = label),
          inherit.aes = FALSE,
          size = 4.2,
          fontface = "bold"
        ) +
        {
          if (identical(var, "rurality")) {
            scale_fill_manual(values = rurality_colors, drop = FALSE)
          } else {
            scale_fill_viridis_d(option = "C", end = 0.9)
          }
        } +
        coord_cartesian(ylim = c(0, ymax * 1.12)) +
        labs(
          x = if (identical(var, "rurality")) "Rurality category" else paste0("Bins of ", var),
          y = "Severity: %WW among impacted segments",
          title = paste0(
            "Severity by ",
            ifelse(identical(var, "rurality"), "Rurality", var),
            " (", mlab, ", ", denom_lab, " denominator)"
          )
        ) +
        theme_app() +
        theme(
          axis.text.x = element_text(
            angle = if (identical(var, "rurality")) 0 else 25,
            hjust = if (identical(var, "rurality")) 0.5 else 1
          )
        )
    }
  })
  
  output$cejst_stats <- renderPrint({
    df <- cejst_binned()
    req(df)
    df$var_bin <- droplevels(df$var_bin)
    
    if (length(levels(df$var_bin)) < 2) {
      cat("Only one bin contains data; cannot compare bins statistically.\n")
      return(invisible(NULL))
    }
    
    cat("Tracts per bin:\n")
    print(table(df$var_bin))
    cat("\n")
    
    if (identical(input$cejst_mode, "Prevalence")) {
      
      # ---------- PREVALENCE STATS ----------
      tab <- table(df$var_bin, df$metric_value)
      cat("Contingency table (bin x any_impacted 0/1):\n")
      print(tab)
      cat("\n")
      
      chi <- try(suppressWarnings(chisq.test(tab)), silent = TRUE)
      if (!inherits(chi, "try-error")) {
        cat("Chi-squared test (overall differences in prevalence across bins):\n")
        cat(sprintf(
          "  X-squared = %.3f, df = %d, p-value = %.3e\n\n",
          unname(chi$statistic),
          unname(chi$parameter),
          chi$p.value
        ))
      } else {
        cat("Could not compute chi-squared test.\n\n")
      }
      
      prev_sum <- df %>%
        dplyr::group_by(var_bin) %>%
        dplyr::summarise(
          n    = dplyr::n(),
          k    = sum(metric_value == 1, na.rm = TRUE),
          prop = k / n,
          .groups = "drop"
        )
      
      cat("Prevalence by bin (k/n):\n")
      print(prev_sum)
      cat("\n")
      
      bins <- as.character(prev_sum$var_bin)
      if (length(bins) >= 2) {
        pairs <- combn(bins, 2, simplify = FALSE)
        pw <- lapply(pairs, function(p) {
          a <- prev_sum[prev_sum$var_bin == p[1], ]
          b <- prev_sum[prev_sum$var_bin == p[2], ]
          tt <- prop.test(
            x = c(a$k, b$k),
            n = c(a$n, b$n),
            correct = FALSE
          )
          data.frame(
            bin1      = p[1],
            bin2      = p[2],
            p_value   = tt$p.value,
            diff_prop = (a$k / a$n) - (b$k / b$n),
            stringsAsFactors = FALSE
          )
        })
        pw <- dplyr::bind_rows(pw)
        pw$p_adj_BH <- p.adjust(pw$p_value, method = "BH")
        
        cat("Pairwise proportion tests (BH-adjusted):\n")
        pw_print <- pw %>%
          dplyr::mutate(
            p_value  = formatC(p_value,  format = "e", digits = 2),
            p_adj_BH = formatC(p_adj_BH, format = "e", digits = 2),
            diff_prop = round(diff_prop, 3)
          )
        print(pw_print, row.names = FALSE)
      }
      
      cat("\nNotes:\n")
      cat("  Prevalence is tract-level: 1 if ANY intersecting segment has WW flow > 0, else 0.\n")
      cat("  Plot shows % of tracts impacted per bin.\n")
      cat("  Denominator selection does not affect prevalence (it only affects severity).\n")
      
    } else {
      
      # ---------- SEVERITY STATS ----------
      kw <- try(stats::kruskal.test(metric_value ~ var_bin, data = df), silent = TRUE)
      if (inherits(kw, "try-error")) {
        cat("Could not compute Kruskal–Wallis test.\n")
        return(invisible(NULL))
      }
      
      cat("Kruskal–Wallis test for differences between bins:\n")
      cat(sprintf(
        "  chi-squared = %.3f, df = %d, p-value = %.3e\n\n",
        kw$statistic,
        kw$parameter,
        kw$p.value
      ))
      
      if (kw$p.value < 0.05) {
        cat("Pairwise comparisons (Wilcoxon, BH-adjusted p-values):\n\n")
        pw <- try(
          stats::pairwise.wilcox.test(df$metric_value, df$var_bin, p.adjust.method = "BH"),
          silent = TRUE
        )
        if (!inherits(pw, "try-error")) {
          pmat     <- pw$p.value
          fmt_pmat <- pmat
          if (!is.null(pmat)) {
            fmt_pmat[] <- ifelse(
              is.na(pmat),
              NA,
              formatC(pmat, format = "e", digits = 2)
            )
          }
          print(fmt_pmat, quote = FALSE)
        } else {
          cat("  Could not compute pairwise Wilcoxon tests.\n")
        }
      } else {
        cat("Result: p ≥ 0.05 → no strong evidence bins differ.\n")
      }
      
      cat("\nNotes:\n")
      cat("  Severity uses %WW for the chosen denominator and is conditional on %WW > 0 (impacted segments only).\n")
      
    }
  })
  
  # ---- Download: Equity data (tract-level values + bins) --------------------
  output$download_cejst_data <- downloadHandler(
    filename = function() {
      paste0("ReSourceTracker_equity_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- cejst_binned()
      req(df)
      
      mode_now    <- input$cejst_mode %||% "Prevalence"
      denom_now   <- if (identical(mode_now, "Severity")) (input$cejst_src %||% "Gage-based") else NA_character_
      month_code  <- input$cejst_month
      month_label <- month_labels[month_code]
      rural_filt  <- input$rurality %||% "All"
      var_name    <- input$cejst_var_box %||% NA_character_
      
      df_out <- df %>%
        mutate(
          analysis_mode      = mode_now,        # "Prevalence" or "Severity"
          denominator_source = denom_now,       # only meaningful for Severity
          month_code         = month_code,
          month_label        = unname(month_label),
          cejst_variable     = var_name,        # e.g., "rurality" or a CEJST metric
          rurality_filter    = rural_filt
        ) %>%
        # put metadata columns first
        select(
          analysis_mode, denominator_source,
          month_code, month_label,
          cejst_variable, rurality_filter,
          GEOID, var_bin, metric_value,
          everything()
        )
      
      readr::write_csv(df_out, file)
    }
  )
  # Only compute these outputs when their tabs are visible
  outputOptions(output, "cejst_plot",          suspendWhenHidden = TRUE)
  outputOptions(output, "cejst_stats",         suspendWhenHidden = TRUE)
  outputOptions(output, "season_plot_median",  suspendWhenHidden = TRUE)
  outputOptions(output, "season_text",         suspendWhenHidden = TRUE)
  outputOptions(output, "season_stats_text",   suspendWhenHidden = TRUE)
}

# --- Launch App -------------------------------------------------------------
shinyApp(ui = ui, server = server)


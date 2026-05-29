#' @keywords internal
app_ui <- function() {
  bslib::page_fluid(
    theme = bslib::bs_theme(
      bg            = "#0f1117",
      fg            = "#e8e8e8",
      primary       = "#4fffb0",
      secondary     = "#1e2130",
      base_font     = bslib::font_google("IBM Plex Mono"),
      heading_font  = bslib::font_google("Space Mono"),
      font_scale    = 0.9,
      bootswatch    = NULL
    ),

    # ── Custom CSS ──────────────────────────────────────────────────────────
    htmltools::tags$head(
      htmltools::tags$style(htmltools::HTML("

        body { background: #0f1117; }

        .sidebar-panel {
          background: #151820;
          border-right: 1px solid #2a2d3a;
          height: 100vh;
          padding: 1.5rem 1.2rem;
          overflow-y: auto;
        }

        .pkg-title {
          font-family: 'Space Mono', monospace;
          font-size: 0.78rem;
          letter-spacing: 0.18em;
          text-transform: uppercase;
          color: #4fffb0;
          margin-bottom: 0.2rem;
        }

        .pkg-subtitle {
          font-family: 'IBM Plex Mono', monospace;
          font-size: 0.65rem;
          color: #555c75;
          margin-bottom: 2rem;
          letter-spacing: 0.06em;
        }

        .section-label {
          font-family: 'Space Mono', monospace;
          font-size: 0.6rem;
          letter-spacing: 0.2em;
          text-transform: uppercase;
          color: #555c75;
          border-bottom: 1px solid #2a2d3a;
          padding-bottom: 0.3rem;
          margin-bottom: 0.8rem;
          margin-top: 1.4rem;
        }

        .stat-box {
          background: #1e2130;
          border: 1px solid #2a2d3a;
          border-radius: 4px;
          padding: 0.7rem 0.9rem;
          margin-bottom: 0.5rem;
        }

        .stat-label {
          font-size: 0.58rem;
          letter-spacing: 0.14em;
          text-transform: uppercase;
          color: #555c75;
          margin-bottom: 0.1rem;
        }

        .stat-value {
          font-family: 'Space Mono', monospace;
          font-size: 1.1rem;
          color: #4fffb0;
          font-weight: 700;
        }

        .map-container {
          position: relative;
          height: 100vh;
        }

        #geo_map {
          height: 100vh !important;
          width: 100% !important;
        }

        .map-overlay {
          position: absolute;
          top: 1rem;
          right: 1rem;
          background: rgba(15,17,23,0.88);
          border: 1px solid #2a2d3a;
          border-radius: 4px;
          padding: 0.6rem 0.9rem;
          font-family: 'IBM Plex Mono', monospace;
          font-size: 0.65rem;
          color: #555c75;
          z-index: 1000;
          pointer-events: none;
        }

        .legend-dot {
          display: inline-block;
          width: 10px;
          height: 10px;
          border-radius: 2px;
          margin-right: 6px;
          vertical-align: middle;
        }

        .form-select, .form-control {
          background: #1e2130 !important;
          border: 1px solid #2a2d3a !important;
          color: #e8e8e8 !important;
          font-family: 'IBM Plex Mono', monospace !important;
          font-size: 0.78rem !important;
        }

        .form-label {
          font-size: 0.65rem;
          letter-spacing: 0.1em;
          text-transform: uppercase;
          color: #777f96;
        }

        .btn-primary {
          background: #4fffb0 !important;
          border: none !important;
          color: #0f1117 !important;
          font-family: 'Space Mono', monospace !important;
          font-size: 0.7rem !important;
          letter-spacing: 0.1em;
          text-transform: uppercase;
          width: 100%;
          padding: 0.55rem;
          border-radius: 3px;
          margin-top: 0.5rem;
        }

        .btn-primary:hover {
          background: #38e89a !important;
        }

        .shiny-input-container { margin-bottom: 0.8rem; }

        /* Leaflet popup styling */
        .leaflet-popup-content-wrapper {
          background: #151820;
          border: 1px solid #2a2d3a;
          border-radius: 4px;
          color: #e8e8e8;
          font-family: 'IBM Plex Mono', monospace;
          font-size: 0.72rem;
        }
        .leaflet-popup-tip { background: #151820; }
        .popup-key { color: #555c75; }
        .popup-val { color: #4fffb0; font-weight: 600; }

      "))
    ),

    # ── Layout ───────────────────────────────────────────────────────────────
    bslib::layout_columns(
      col_widths = c(3, 9),
      gap        = "0px",

      # ── Sidebar ────────────────────────────────────────────────────────────
      htmltools::div(
        class = "sidebar-panel",

        htmltools::div(class = "pkg-title",   "subnatgeoalignR"),
        htmltools::div(class = "pkg-subtitle", "geo alignment viewer"),

        # Filters
        htmltools::div(class = "section-label", "Display"),

        shiny::selectInput(
          "layer_select", "Active layer",
          choices = c(
            "Intersections"   = "intersections",
            "PEPFAR polygons" = "pepfar",
            "Census polygons" = "census"
          )
        ),

        shiny::sliderInput(
          "opacity", "Fill opacity",
          min = 0, max = 1, value = 0.45, step = 0.05
        ),

        shiny::checkboxInput(
          "show_pepfar_outline",
          "Show PEPFAR outlines",
          value = TRUE
        ),

        shiny::checkboxInput(
          "show_census_outline",
          "Show census outlines",
          value = TRUE
        ),

        # Filter by PEPFAR unit
        htmltools::div(class = "section-label", "Filter"),

        shiny::selectInput(
          "pepfar_filter", "PEPFAR unit",
          choices  = NULL,
          multiple = TRUE
        ),

        shiny::actionButton("reset_filter", "Reset filter", class = "btn-primary"),

        # Stats
        htmltools::div(class = "section-label", "Summary"),

        htmltools::div(
          class = "stat-box",
          htmltools::div(class = "stat-label", "Intersection polygons"),
          htmltools::div(class = "stat-value", shiny::textOutput("n_intersections", inline = TRUE))
        ),

        htmltools::div(
          class = "stat-box",
          htmltools::div(class = "stat-label", "PEPFAR units"),
          htmltools::div(class = "stat-value", shiny::textOutput("n_pepfar", inline = TRUE))
        ),

        htmltools::div(
          class = "stat-box",
          htmltools::div(class = "stat-label", "Census units"),
          htmltools::div(class = "stat-value", shiny::textOutput("n_census", inline = TRUE))
        )
      ),

      # ── Map panel ──────────────────────────────────────────────────────────
      htmltools::div(
        class = "map-container",
        leaflet::leafletOutput("geo_map"),

        htmltools::div(
          class = "map-overlay",
          htmltools::tags$span(
            htmltools::tags$span(
              class = "legend-dot",
              style = "background:#4fffb0;"
            ),
            "Intersection"
          ),
          htmltools::tags$br(),
          htmltools::tags$span(
            htmltools::tags$span(
              class = "legend-dot",
              style = "background:#ff6b6b; opacity:0.5;"
            ),
            "PEPFAR outline"
          ),
          htmltools::tags$br(),
          htmltools::tags$span(
            htmltools::tags$span(
              class = "legend-dot",
              style = "background:#4dabf7; opacity:0.5;"
            ),
            "Census outline"
          )
        )
      )
    )
  )
}


#' @keywords internal
app_server <- function(input, output, session, intersection_layer, pepfar_sf, census_sf, pepfar_id_col, census_id_col) {

  # ── Reproject to WGS84 for leaflet ─────────────────────────────────────
  isl  <- sf::st_transform(intersection_layer, 4326)
  pep  <- sf::st_transform(pepfar_sf, 4326)
  cen  <- sf::st_transform(census_sf, 4326)

  # Populate filter choices
  pepfar_choices <- sort(unique(isl[[pepfar_id_col]]))
  shiny::updateSelectInput(session,
                           "pepfar_filter",
                           choices = c("All" = "", pepfar_choices))

  # ── Reactive filtered layer ─────────────────────────────────────────────
  filtered_isl <- shiny::reactive({
    if (is.null(input$pepfar_filter) || length(input$pepfar_filter) == 0) {
      isl
    } else {
      isl[isl[[pepfar_id_col]] %in% input$pepfar_filter, ]
    }
  })

  shiny::observeEvent(input$reset_filter, {
    shiny::updateSelectInput(session, "pepfar_filter", selected = character(0))
  })

  # ── Summary stats ───────────────────────────────────────────────────────
  output$n_intersections <- shiny::renderText({
    nrow(filtered_isl()[!is.na(sf::st_dimension(filtered_isl())), ])
  })

  output$n_pepfar <- shiny::renderText({
    length(unique(filtered_isl()[[pepfar_id_col]]))
  })

  output$n_census <- shiny::renderText({
    # ids <- filtered_isl()[[paste0("census_", census_id_col)]]
    ids <- filtered_isl()[["ADM2_NAME"]]
    length(unique(ids[!is.na(ids)]))
  })

  # ── Base map ─────────────────────────────────────────────────────────────
  output$geo_map <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.DarkMatterNoLabels,
        options = leaflet::tileOptions(opacity = 0.9)
      ) %>%
      leaflet::fitBounds(
        lng1 = sf::st_bbox(isl)[["xmin"]],
        lat1 = sf::st_bbox(isl)[["ymin"]],
        lng2 = sf::st_bbox(isl)[["xmax"]],
        lat2 = sf::st_bbox(isl)[["ymax"]]
      )
  })

  # ── Reactive layer updates ───────────────────────────────────────────────
  shiny::observe({
    filt   <- filtered_isl()
    layer  <- input$layer_select
    opac   <- input$opacity

    # Remove previous dynamic layers
    proxy <- leaflet::leafletProxy("geo_map") %>%
      leaflet::clearGroup("intersections") %>%
      leaflet::clearGroup("pepfar_outline") %>%
      leaflet::clearGroup("census_outline")

    # Census outlines
    if (isTRUE(input$show_census_outline)) {
      proxy <- proxy %>%
        leaflet::addPolygons(
          data        = cen,
          group       = "census_outline",
          fillOpacity = 0,
          color       = "#4dabf7",
          weight      = 1,
          opacity     = 0.5,
          dashArray   = "4,4"
        )
    }

    # PEPFAR outlines
    if (isTRUE(input$show_pepfar_outline)) {
      proxy <- proxy %>%
        leaflet::addPolygons(
          data        = pep,
          group       = "pepfar_outline",
          fillOpacity = 0,
          color       = "#ff6b6b",
          weight      = 1.5,
          opacity     = 0.65
        )
    }

    # Active fill layer
    valid <- filt[!sf::st_is_empty(filt) & !is.na(sf::st_dimension(filt)), ]

    if (nrow(valid) > 0) {
      pep_col    <- pepfar_id_col
      cen_col    <- census_id_col
      popup_html <- paste0(
        "<b class='popup-key'>PEPFAR</b><br>",
        "<span class='popup-val'>", valid[[pep_col]], "</span><br><br>",
        "<b class='popup-key'>Census</b><br>",
        "<span class='popup-val'>", valid[[cen_col]], "</span>"
      )

      proxy <- proxy %>%
        leaflet::addPolygons(
          data        = valid,
          group       = "intersections",
          fillColor   = "#4fffb0",
          fillOpacity = opac,
          color       = "#4fffb0",
          weight      = 0.4,
          opacity     = 0.8,
          popup       = popup_html,
          highlightOptions = leaflet::highlightOptions(
            fillOpacity = min(opac + 0.25, 1),
            weight      = 1.5,
            bringToFront = TRUE
          )
        )
    }

    proxy
  })
}

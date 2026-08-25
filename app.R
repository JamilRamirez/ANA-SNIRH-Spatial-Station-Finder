# v1.1.0-dev — CAMBIO 6: procesamiento multigeometría independiente
# Base funcional: BUILD PUBLICA V17.2
# ============================================================================
# ANA–SNIRH Spatial Station Finder — versión pública
#
# Requiere productos de 01_normalizar_reportes_ana_v2.R:
#   _01_normalizado/01_catalogo_series.csv
#   _01_normalizado/01_disponibilidad_diaria.rds
#
# Funciones principales:
#   - exploración nacional SIN KML
#   - carga OPCIONAL de KML, GPKG o shapefile (partes o ZIP)
#   - procesamiento independiente de múltiples geometrías con ID/nombre conservado
#   - buffer métrico configurable (50 km por defecto) si hay KML
#   - selección espacial de estaciones ANA/SNIRH si hay KML
#   - diagnóstico individual completo por estación/serie
#   - búsqueda type-ahead de estaciones y navegación directa desde el mapa
#   - estado reactivo independiente para clics sucesivos en estaciones
#   - mapa estrictamente filtrado por Precipitación/Caudal
#   - OMM puramente espacial, independiente de filtros temporales
#   - catálogo XLSX como autoridad para enlazar estaciones con series
#   - descarga de copias RAW archivadas + acceso al visor oficial ANA/SNIRH
#   - estaciones sin serie anexadas sin contaminar los station_id temporales
#   - evaluación temporal para un periodo común
#   - búsqueda recursiva de todas las ventanas consecutivas de N años
#   - selección de la mejor serie disponible por estación
#   - diagnóstico exploratorio de densidad OMM/WMO
#
# 50 km NO se etiqueta como criterio OMM. Es solo un radio de búsqueda cuando se usa archivo espacial.
# ============================================================================

pkgs <- c("shiny", "bslib", "data.table", "DT", "sf", "leaflet", "lubridate", "ggplot2", "readxl", "zip", "openxlsx")
miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) {
  stop("Faltan paquetes: ", paste(miss, collapse = ", "),
       "\nInstalar con: install.packages(c(",
       paste(sprintf('"%s"', miss), collapse = ", "), "))")
}

library(shiny)
library(bslib)
library(data.table)
library(DT)
library(sf)
library(leaflet)
library(lubridate)
library(ggplot2)

# ----------------------------------------------------------------------------
# ARCHIVOS DEL REFACTOR MODULAR
# ----------------------------------------------------------------------------
#
# Shiny carga automáticamente los .R de la carpeta R/ antes de app.R.
# Todos esos archivos son ahora "definition-only": no leen datos al cargarse.
# Los volvemos a sourcear de forma explícita después de library(...) para que
# app.R también sea robusto si se ejecuta directamente fuera del cargador
# normal de Shiny.

SUPPORT_FILES <- c(
  "R/core_data.R",
  "R/core_helpers.R",
  "R/core_spatial_reference.R",
  "R/mod_mapa.R",
  "R/mod_diagnostico.R",
  "R/mod_candidatas.R",
  "R/mod_ventana_comun.R",
  "R/mod_descarga_normalizada.R",
  "R/mod_omm.R",
  "R/mod_metodologia.R"
)

missing_support <- SUPPORT_FILES[!file.exists(SUPPORT_FILES)]
if (length(missing_support)) {
  stop(
    "Faltan archivos del refactor modular: ",
    paste(missing_support, collapse = ", ")
  )
}

# IMPORTANTE: capturar UNA sola vez el entorno real de app.R.
# No usar environment() dentro de lapply()/funciones anónimas: en ese caso
# cada archivo quedaría sourceado en un entorno transitorio distinto y los
# módulos no podrían resolver objetos compartidos creados por init_core_data().
APP_ENV <- environment()

for (f in SUPPORT_FILES) {
  sys.source(f, envir = APP_ENV)
}

# La lectura y preparación de CAT/DAY/SERIES/STATIONS/YEAR_OBS/WMO_DENSITY
# se ejecuta recién aquí, cuando data.table, sf, lubridate, etc. ya están
# cargados, y dentro del MISMO entorno que contiene los módulos.
init_core_data(APP_ENV)

# Capa cartográfica fija de referencia (solo visual; no interviene en cálculos).
init_spatial_reference(APP_ENV)

# ----------------------------------------------------------------------------
# UI GENERAL
# ----------------------------------------------------------------------------

ui <- page_sidebar(
  
  title = tags$div(
    class = "app-brand",
    style = "display:flex; align-items:center; gap:10px; font-family:'Lato', sans-serif;",
    
    tags$span(
      "Explorador de estaciones ANA–SNIRH",
      class = "app-brand-title",
      style = "font-weight:600; letter-spacing:0.01em; color:#FFFFFF;"
    ),
    
    tags$span(
      "· by Jamil Ramirez",
      class = "app-brand-byline",
      style = "
        font-size:0.76em;
        font-weight:400;
        color:#FFFFFF;
        opacity:0.8;
      "
    ),

    actionLink(
      "about_app",
      tagList(
        icon("circle-info"),
        tags$span("Acerca de", class = "app-about-label")
      ),
      class = "app-about-link",
      title = "Información, fuentes y créditos"
    )
  ),
  
  window_title = "Explorador de estaciones ANA–SNIRH — Jamil Ramirez",
  
  theme = bs_add_variables(
    bs_theme(
      version = 5,
      bg = "#F4F6F3",
      fg = "#243439",
      primary = "#2F6F73",
      secondary = "#687D79",
      success = "#4F7A65",
      warning = "#7B7459",
      base_font = font_google("Lato"),
      heading_font = font_google("Merriweather"),
      code_font = font_google("Roboto Mono")
    ),
    "border-radius" = "4px",
    "border-color" = "#D7DFDA"
  ),
  
  sidebar = sidebar(
    width = 300,
    open = "desktop",
    
    h5(
      "Área de estudio",
      style = "font-weight:700;"
    ),
    
    fileInput(
      "area_file",
      "Área espacial opcional",
      multiple = TRUE,
      buttonLabel = "Buscar…",
      placeholder = "Ningún archivo seleccionado",
      accept = c(
        ".kml",
        ".kmz",
        ".gpkg",
        ".zip",
        ".shp",
        ".shx",
        ".dbf",
        ".prj",
        ".cpg",
        ".qpj",
        "application/vnd.google-earth.kml+xml",
        "application/vnd.google-earth.kmz",
        "application/zip"
      )
    ),
    
    tags$details(
      class = "filter-help",
      tags$summary("Formatos admitidos"),
      tags$small(
        paste(
          "KML, KMZ, GPKG, ZIP de shapefile o",
          ".shp + .shx + .dbf + .prj seleccionados juntos."
        )
      )
    ),
    
    sliderInput(
      "buffer_km",
      "Buffer de búsqueda (km)",
      0,
      200,
      BUFFER_DEFAULT_KM,
      step = 5
    ),
    
    tags$details(
      class = "filter-help",
      tags$summary("Sobre el buffer"),
      tags$small(
        paste(
          "50 km es un valor exploratorio por defecto cuando se carga un área;",
          "no es una distancia prescrita por la OMM."
        )
      )
    ),
    
    hr(),

    h5(
      "Filtros de análisis",
      style = "font-weight:700;"
    ),
    
    selectInput(
      "tipo",
      "Variable",
      c(
        "Precipitación",
        "Caudal"
      ),
      "Precipitación"
    ),
    
    dateRangeInput(
      "periodo",
      "Periodo requerido",
      start = as.Date(FDEF0),
      end = as.Date(FDEF1),
      min = as.Date(FMIN),
      max = as.Date(FMAX),
      format = "yyyy-mm-dd",
      separator = " a "
    ),
    
    sliderInput(
      "umbral",
      "Completitud mínima (%)",
      50,
      100,
      UMBRAL_DEFAULT,
      step = 1
    ),
    
    checkboxInput(
      "nominal",
      "Exigir cobertura nominal de todo el periodo",
      TRUE
    ),
    
    hr(),
    
    uiOutput(
      "kml_info"
    )
  ),
  
  # --------------------------------------------------------------------------
  # IDENTIDAD VISUAL
  # --------------------------------------------------------------------------
  
  tags$style(
    HTML(
      "
      /* ============================================================
         IDENTIDAD VISUAL — ANA–SNIRH Spatial Station Finder
         Paleta: azul petróleo (#2F6F73) + verde páramo (#8AA49A)
         Tipografía: Lato / Merriweather / Roboto Mono
         ============================================================ */

      :root {
        --brand-ink:         #243439;
        --brand-teal:        #2F6F73;
        --brand-teal-deep:   #123D4A;
        --brand-accent:      #8AA49A;
        --brand-line:        #D7DFDA;
        --brand-surface:     #FFFFFF;
        --brand-bg:          #F4F6F3;
      }

      body { color: var(--brand-ink); }

      .app-brand { min-width: 0; }
      .app-brand-title { white-space: nowrap; }

      .app-about-link {
        align-items: center;
        border: 1px solid rgba(255, 255, 255, 0.34);
        border-radius: 999px;
        display: inline-flex;
        font-size: 0.78rem;
        font-weight: 600;
        gap: 0.35rem;
        margin-left: 0.35rem;
        padding: 0.28rem 0.62rem;
        text-decoration: none !important;
      }

      .app-about-link:hover,
      .app-about-link:focus {
        background: rgba(255, 255, 255, 0.12);
        border-color: rgba(255, 255, 255, 0.65);
      }

      /* Barra superior, con textura de curvas de nivel muy sutil */
      .bslib-page-sidebar > .navbar {
        background-color: var(--brand-teal-deep) !important;
        border-bottom: 3px solid var(--brand-accent) !important;
        box-shadow: 0 1px 4px rgba(0, 0, 0, 0.18);
        background-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='140' height='90' viewBox='0 0 140 90'%3E%3Cg fill='none' stroke='%23FFFFFF' stroke-opacity='0.07' stroke-width='1'%3E%3Cpath d='M-10 15 Q25 -5 60 15 T140 15'/%3E%3Cpath d='M-10 40 Q25 20 60 40 T140 40'/%3E%3Cpath d='M-10 65 Q25 45 60 65 T140 65'/%3E%3C/g%3E%3C/svg%3E\");
        background-repeat: repeat-x;
        background-position: bottom;
      }

      .bslib-page-sidebar > .navbar .navbar-brand,
      .bslib-page-sidebar > .navbar .bslib-page-title,
      .bslib-page-sidebar > .navbar,
      .bslib-page-sidebar > .navbar * {
        color: #FFFFFF !important;
      }

      /* Sidebar tipo ficha técnica */
      .bslib-sidebar-layout > .sidebar {
        background-color: var(--brand-bg);
        border-right: 1px solid var(--brand-line);
      }

      .bslib-sidebar-layout > .sidebar h5 {
        font-size: 0.72rem;
        font-weight: 700;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        color: var(--brand-teal-deep);
        border-left: 3px solid var(--brand-accent);
        padding-left: 8px;
        margin-bottom: 14px;
      }

      .bslib-sidebar-layout > .sidebar hr {
        border-top: 1px solid var(--brand-line);
        margin: 1.25rem 0;
        opacity: 1;
      }

      .bslib-sidebar-layout > .sidebar label {
        font-weight: 600;
        font-size: 0.86rem;
        color: var(--brand-ink);
      }

      .filter-help {
        color: #60777D;
        font-size: 0.8rem;
        margin: -0.3rem 0 1rem;
      }

      .filter-help summary {
        color: var(--brand-teal);
        cursor: pointer;
        font-weight: 600;
        margin-bottom: 0.35rem;
      }

      /* Pestañas principales: subrayado en vez de 'caja' */
      .nav-tabs {
        border-bottom: 1px solid var(--brand-line);
        gap: 4px;
        flex-wrap: nowrap;
      }

      .nav-tabs .nav-item { white-space: nowrap; }

      .nav-tabs .nav-link {
        border: none !important;
        border-radius: 0 !important;
        color: var(--brand-secondary, #6C8B90);
        font-weight: 600;
        font-size: 0.86rem;
        padding: 0.6rem 0.68rem;
        background: transparent !important;
      }

      .nav-tabs .nav-link.active {
        color: var(--brand-teal-deep) !important;
        border-bottom: 2px solid var(--brand-accent) !important;
      }

      .nav-tabs .nav-link:focus-visible,
      .btn:focus-visible,
      summary:focus-visible {
        outline: 3px solid rgba(138, 164, 154, 0.5);
        outline-offset: 2px;
        box-shadow: none;
      }

      .nav-tabs .dropdown-menu {
        border: 1px solid var(--brand-line);
        border-radius: 8px;
        box-shadow: 0 10px 28px rgba(18, 54, 66, 0.14);
      }

      /* Botones primarios */
      .btn-primary {
        background-color: var(--brand-teal);
        border-color: var(--brand-teal);
        font-weight: 600;
        letter-spacing: 0.01em;
      }
      .btn-primary:hover,
      .btn-primary:focus {
        background-color: var(--brand-teal-deep);
        border-color: var(--brand-teal-deep);
      }

      /* Tarjetas bslib::card() usadas por los módulos */
      .card {
        border: 1px solid var(--brand-line);
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(18, 54, 66, 0.055);
      }
      .card-header {
        background-color: var(--brand-surface);
        border-bottom: 1px solid var(--brand-line);
        font-weight: 600;
        color: var(--brand-teal-deep);
      }

      .metric-card {
        background: var(--brand-surface) !important;
        border: 1px solid var(--brand-line) !important;
        border-top: 3px solid var(--brand-teal) !important;
        border-radius: 8px !important;
        box-shadow: 0 2px 8px rgba(18, 54, 66, 0.06);
      }

      .metric-card-title {
        color: #60777D;
        font-size: 0.72rem;
        font-weight: 700;
        letter-spacing: 0.045em;
        text-transform: uppercase;
      }

      .metric-card-value {
        color: var(--brand-teal-deep);
        font-size: 1.8rem;
        font-weight: 700;
        line-height: 1.15;
        margin-top: 6px;
      }

      .metric-card-subtitle {
        color: #60777D;
        font-size: 0.79rem;
        margin-top: 5px;
      }

      .alert-info {
        --bs-alert-color: var(--brand-teal-deep);
        --bs-alert-bg: #EAF1EE;
        --bs-alert-border-color: #C4D5CE;
      }

      .dataTables_wrapper {
        font-size: 0.86rem;
      }

      .card-body:has(.dataTables_wrapper) {
        gap: 0.65rem;
      }

      .dataTables_wrapper .dt-toolbar {
        align-items: center;
        display: flex;
        flex-wrap: wrap;
        gap: 0.35rem;
        justify-content: space-between;
        margin-bottom: 0.5rem;
      }

      .dataTables_wrapper .dt-toolbar .dt-buttons {
        display: inline-flex;
        float: none;
        gap: 0.35rem;
        margin: 0;
      }

      .dataTables_wrapper .dt-buttons .btn,
      .dataTables_wrapper .dt-button {
        background: #EAF1EE !important;
        border: 1px solid #C4D5CE !important;
        border-radius: 5px !important;
        color: var(--brand-teal-deep) !important;
        font-weight: 600;
        font-size: 0.8rem;
        padding: 0.3rem 0.58rem !important;
      }

      .dataTables_wrapper .dt-toolbar .dataTables_filter {
        float: none;
        margin: 0;
        padding-bottom: 0;
      }

      .dataTables_wrapper .dataTables_filter input {
        margin-left: 0.4rem;
        padding: 0.24rem 0.35rem;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.8rem;
        padding-top: 0.45rem !important;
      }

      /* Tablas DT */
      table.dataTable thead th {
        background-color: var(--brand-teal-deep) !important;
        color: #FFFFFF !important;
        font-weight: 600;
        font-size: 0.82rem;
        letter-spacing: 0.02em;
        padding: 0.42rem 0.5rem !important;
      }
      table.dataTable tbody td {
        padding: 0.34rem 0.5rem !important;
      }
      table.dataTable thead input {
        font-size: 0.78rem;
        padding: 0.22rem 0.3rem !important;
      }
      table.dataTable tbody tr:nth-child(even) {
        background-color: #F1F5F2;
      }
      table.dataTable tbody tr:hover {
        background-color: #E4EEEA !important;
      }

      .about-lead {
        color: #48615F;
        font-size: 1rem;
        line-height: 1.55;
        margin-bottom: 1.2rem;
      }

      .about-title-row {
        align-items: center;
        display: flex;
        gap: 0.55rem;
        justify-content: space-between;
        width: 100%;
      }

      .about-version-badge {
        background: #E4EEEA;
        border: 1px solid #C4D5CE;
        border-radius: 999px;
        color: var(--brand-teal-deep);
        font-size: 0.7rem;
        font-weight: 700;
        letter-spacing: 0.04em;
        padding: 0.25rem 0.55rem;
        white-space: nowrap;
      }

      .about-section-title {
        color: var(--brand-teal-deep);
        font-size: 0.92rem;
        font-weight: 700;
        margin: 1.15rem 0 0.65rem;
      }

      .about-stats {
        display: grid;
        gap: 0.55rem;
        grid-template-columns: repeat(4, minmax(0, 1fr));
      }

      .about-stat {
        background: var(--brand-bg);
        border: 1px solid var(--brand-line);
        border-radius: 7px;
        padding: 0.68rem 0.72rem;
      }

      .about-stat strong {
        color: var(--brand-teal-deep);
        display: block;
        font-size: 0.66rem;
        letter-spacing: 0.045em;
        margin-bottom: 0.22rem;
        text-transform: uppercase;
      }

      .about-capabilities {
        display: grid;
        gap: 0.65rem;
        grid-template-columns: repeat(4, minmax(0, 1fr));
      }

      .about-capability {
        border-top: 2px solid var(--brand-accent);
        padding: 0.7rem 0.15rem 0;
      }

      .about-capability .fa-solid {
        color: var(--brand-teal);
        margin-bottom: 0.45rem;
      }

      .about-capability strong {
        display: block;
        font-size: 0.86rem;
        margin-bottom: 0.15rem;
      }

      .about-capability span {
        color: #607572;
        font-size: 0.78rem;
      }

      .about-action-row {
        display: flex;
        flex-wrap: wrap;
        gap: 0.5rem;
      }

      .about-action {
        align-items: center;
        border: 1px solid #AFC4BC;
        border-radius: 6px;
        color: var(--brand-teal-deep);
        display: inline-flex;
        font-size: 0.82rem;
        font-weight: 600;
        gap: 0.35rem;
        padding: 0.5rem 0.68rem;
        text-decoration: none;
      }

      .about-action:hover,
      .about-action:focus {
        background: #EAF1EE;
        color: var(--brand-teal-deep);
      }

      .about-disclaimer {
        background: #EEF4F1;
        border-left: 3px solid var(--brand-accent);
        border-radius: 0 6px 6px 0;
        color: #48615F;
        font-size: 0.84rem;
        line-height: 1.5;
        margin-top: 1.2rem;
        padding: 0.7rem 0.8rem;
      }

      .about-disclaimer strong {
        color: var(--brand-teal-deep);
        display: block;
        margin-bottom: 0.2rem;
      }

      .about-credit {
        color: #647773;
        font-size: 0.76rem;
        margin: 0.85rem 0 0;
        text-align: center;
      }

      .about-close-row {
        display: flex;
        justify-content: flex-end;
        margin-top: 0.65rem;
      }

      .leaflet.html-widget {
        height: clamp(430px, calc(100vh - 230px), 650px) !important;
        min-height: 430px;
      }

      /* Sliders (ionRangeSlider) */
      .irs--shiny .irs-bar,
      .irs--shiny .irs-single,
      .irs--shiny .irs-from,
      .irs--shiny .irs-to {
        background: var(--brand-teal) !important;
        border-color: var(--brand-teal) !important;
      }

      @media (max-width: 767.98px) {
        .app-brand-title {
          font-size: 0.92rem;
          white-space: normal;
        }

        .app-brand-byline { display: none; }

        .app-about-link {
          margin-left: auto;
          padding: 0.32rem 0.5rem;
        }

        .app-about-label { display: none; }

        #main_nav.nav-tabs {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 2px 6px;
        }

        #main_nav.nav-tabs > .nav-item,
        #main_nav.nav-tabs > .nav-item > .nav-link {
          min-width: 0;
          width: 100%;
        }

        #main_nav.nav-tabs .nav-link {
          font-size: 0.84rem;
          overflow: hidden;
          padding: 0.55rem 0.35rem;
          text-align: center;
          text-overflow: ellipsis;
        }

        .leaflet.html-widget {
          height: 55vh !important;
          min-height: 360px;
        }

        .about-stats,
        .about-capabilities { grid-template-columns: repeat(2, minmax(0, 1fr)); }

        .about-title-row { align-items: flex-start; }
      }
      "
    )
  ),

  tags$script(
    HTML(
      "
      document.addEventListener('DOMContentLoaded', function () {
        const tablet = window.matchMedia('(max-width: 991.98px)');
        const closeSidebar = function () {
          const toggle = document.querySelector('.bslib-sidebar-layout .collapse-toggle');
          if (tablet.matches && toggle && toggle.getAttribute('aria-expanded') === 'true') {
            toggle.click();
          }
        };
        closeSidebar();
        window.setTimeout(closeSidebar, 500);
        tablet.addEventListener('change', closeSidebar);
      });
      "
    )
  ),
  
  navset_tab(
    id = "main_nav",
    
    nav_panel(
      "Mapa",
      value = "mapa",
      mod_mapa_ui(
        "mapa_mod"
      )
    ),
    
    nav_panel(
      "Diagnóstico",
      value = "diagnostico",
      mod_diagnostico_ui(
        "diagnostico_mod"
      )
    ),
    
    nav_panel(
      "Candidatas",
      mod_candidatas_ui(
        "candidatas_mod"
      )
    ),

    nav_panel(
      "Ventana común",
      mod_ventana_comun_ui(
        "ventana_mod"
      )
    ),

    nav_panel(
      "Descarga",
      value = "descarga_normalizada",
      mod_descarga_normalizada_ui(
        "descarga_mod"
      )
    ),

    nav_panel(
      "Metodología y OMM",
      value = "metodologia_omm",

      navset_card_tab(

        nav_panel(
          "Metodología",
          mod_metodologia_ui(
            "metodologia_mod"
          )
        ),

        nav_panel(
          "Diagnóstico OMM",
          mod_omm_ui(
            "omm_mod"
          )
        )
      )
    )
  )
)

# ----------------------------------------------------------------------------
# SERVER GENERAL Y ESTADO COMPARTIDO
# ----------------------------------------------------------------------------

server <- function(input, output, session) {
  observeEvent(
    input$about_app,
    {
      showModal(
        modalDialog(
          title = tags$div(
            class = "about-title-row",
            tags$span(
              icon("circle-info"),
              "Acerca del Explorador de estaciones ANA–SNIRH"
            ),
            tags$span("v1.1.0", class = "about-version-badge")
          ),
          easyClose = TRUE,
          size = "xl",
          footer = NULL,
          tags$p(
            class = "about-lead",
            "Herramienta independiente para localizar, evaluar, comparar y descargar series hidrometeorológicas publicadas por ANA/SNIRH en el Perú."
          ),
          tags$div(
            class = "about-stats",
            tags$div(
              class = "about-stat",
              tags$strong("Datos hasta"),
              format_data_freeze_date_es(compact = TRUE)
            ),
            tags$div(class = "about-stat", tags$strong("Actualización"), "Mensual"),
            tags$div(class = "about-stat", tags$strong("Publicación"), "19 ago 2026"),
            tags$div(class = "about-stat", tags$strong("Licencia"), "MIT")
          ),
          tags$div(class = "about-section-title", "Qué permite hacer"),
          tags$div(
            class = "about-capabilities",
            tags$div(class = "about-capability", icon("location-dot"), tags$strong("Localizar"), tags$span("Estaciones por área")),
            tags$div(class = "about-capability", icon("chart-column"), tags$strong("Evaluar"), tags$span("Completitud y continuidad")),
            tags$div(class = "about-capability", icon("right-left"), tags$strong("Comparar"), tags$span("Candidatas y periodos")),
            tags$div(class = "about-capability", icon("download"), tags$strong("Descargar"), tags$span("Series normalizadas"))
          ),
          tags$div(class = "about-section-title", "Datos y recursos"),
          tags$div(
            class = "about-action-row",
            tags$a(
              class = "about-action",
              href = "https://snirh.ana.gob.pe/VisorPorCuenca/",
              target = "_blank",
              rel = "noopener noreferrer",
              "Visor oficial ANA/SNIRH", tags$span("↗", `aria-hidden` = "true")
            ),
            tags$a(
              class = "about-action",
              href = "https://github.com/JamilRamirez/ANA-SNIRH-Official-Reports",
              target = "_blank",
              rel = "noopener noreferrer",
              "Archivo de reportes oficiales", tags$span("↗", `aria-hidden` = "true")
            ),
            tags$a(
              class = "about-action",
              href = "https://github.com/JamilRamirez/ANA-SNIRH-Normalized-Series",
              target = "_blank",
              rel = "noopener noreferrer",
              "Series normalizadas", tags$span("↗", `aria-hidden` = "true")
            )
          ),
          tags$div(class = "about-section-title", "Proyecto"),
          tags$div(
            class = "about-action-row",
            tags$a(
              class = "about-action",
              href = "https://github.com/JamilRamirez/ANA-SNIRH-Spatial-Station-Finder",
              target = "_blank",
              rel = "noopener noreferrer",
              "GitHub", tags$span("↗", `aria-hidden` = "true")
            ),
            tags$a(
              class = "about-action",
              href = "https://jamilramirez.github.io/",
              target = "_blank",
              rel = "noopener noreferrer",
              "Página web del autor", tags$span("↗", `aria-hidden` = "true")
            )
          ),
          tags$div(
            class = "about-disclaimer",
            tags$strong(icon("circle-info"), " Proyecto independiente"),
            "Esta aplicación no pertenece a la Autoridad Nacional del Agua. ANA/SNIRH es la fuente de las observaciones. Para datos incorporados después del corte indicado, consulte el visor oficial."
          ),
          tags$p(
            class = "about-credit",
            "Desarrollado por Jamil Ramirez · 19 ago 2026 · MIT"
          ),
          tags$div(
            class = "about-close-row",
            modalButton("Cerrar")
          )
        )
      )
    },
    ignoreInit = TRUE
  )

  study_area <- reactive({

    req(
      input$area_file
    )

    tryCatch(
      read_uploaded_spatial(
        input$area_file
      ),
      error = function(e) {
        shiny::validate(
          shiny::need(
            FALSE,
            paste(
              "No se pudo leer el área espacial:",
              conditionMessage(e)
            )
          )
        )
      }
    )
  })


  # Preparación geométrica INMUTABLE para el archivo cargado.
  # EPSG UTM, transformación métrica y área no cambian al mover el buffer;
  # se calculan una sola vez por carga espacial.
  spatial_geometry_base <- reactive({
    if (is.null(input$area_file) || !nrow(input$area_file)) {
      return(list(
        has_kml = FALSE,
        area = NULL,
        geometry_info = data.table(),
        parts = list(),
        n_geometries = 0L,
        area_km2 = NA_real_
      ))
    }

    x <- study_area()

    parts <- vector("list", nrow(x))
    info <- vector("list", nrow(x))

    for (i in seq_len(nrow(x))) {
      xi <- x[i, ]
      epsg_i <- utm_epsg(xi)
      target_m_i <- st_transform(xi, epsg_i)
      area_i <- polygon_area_km2(xi, epsg_i)

      parts[[i]] <- list(
        geometry_id = as.character(xi$geometry_id),
        geometry_name = as.character(xi$geometry_name),
        epsg = epsg_i,
        target = xi,
        target_m = target_m_i,
        area_km2 = area_i
      )

      info[[i]] <- data.table(
        geometry_id = as.character(xi$geometry_id),
        geometry_name = as.character(xi$geometry_name),
        source_file = as.character(xi$source_file),
        source_layer = as.character(xi$source_layer),
        source_feature = as.integer(xi$source_feature),
        geometry_type = as.character(xi$geometry_type),
        geometry_label_field = as.character(xi$geometry_label_field),
        epsg = as.integer(epsg_i),
        area_km2 = as.numeric(area_i)
      )
    }

    geometry_info <- rbindlist(info, use.names = TRUE, fill = TRUE)
    area_values <- geometry_info$area_km2
    area_total <- if (any(is.finite(area_values))) {
      sum(area_values[is.finite(area_values)], na.rm = TRUE)
    } else {
      NA_real_
    }

    list(
      has_kml = TRUE,
      area = x,
      geometry_info = geometry_info,
      parts = parts,
      n_geometries = nrow(x),
      area_km2 = area_total
    )
  })


  # Solo el buffer depende del slider. Cambiar 50 -> 60 km ya no vuelve a
  # transformar cada polígono ni recalcula su área.
  spatial <- reactive({
    base <- spatial_geometry_base()

    if (!isTRUE(base$has_kml)) {
      return(list(
        has_kml = FALSE,
        area = NULL,
        buffer = NULL,
        geometry_info = data.table(),
        parts = list(),
        n_geometries = 0L,
        area_km2 = NA_real_
      ))
    }

    parts <- base$parts
    buffers <- vector("list", length(parts))

    for (i in seq_along(parts)) {
      part <- parts[[i]]

      buffer_m_i <- st_buffer(
        part$target_m,
        input$buffer_km * 1000
      )

      buffer_i <- st_transform(buffer_m_i, 4326)
      buffer_i$geometry_id <- part$geometry_id
      buffer_i$geometry_name <- part$geometry_name

      parts[[i]]$buffer <- buffer_i
      buffers[[i]] <- buffer_i[, c("geometry_id", "geometry_name")]
    }

    list(
      has_kml = TRUE,
      area = base$area,
      buffer = do.call(rbind, buffers),
      geometry_info = base$geometry_info,
      parts = parts,
      n_geometries = base$n_geometries,
      area_km2 = base$area_km2
    )
  })

  spatial_stations <- reactive({
    s <- spatial()
    pts <- STATIONS_SF[STATIONS_SF$tipo_dato == input$tipo, ]

    if (!nrow(pts)) {
      return(list(
        sf = pts,
        sf_pairs = pts,
        tab = data.table()
      ))
    }

    # Modo nacional: una fila por estación, sin identidad espacial específica.
    if (!isTRUE(s$has_kml)) {
      pts$geometry_id <- NA_character_
      pts$geometry_name <- "Inventario nacional"
      pts$distancia_km <- NA_real_
      pts$dentro_kml <- NA
      pts$dentro_buffer <- TRUE
      pts$zona_distancia <- "Inventario nacional"
      pts$geometry_names <- "Inventario nacional"
      pts$n_geometrias <- 0L

      return(list(
        sf = pts,
        sf_pairs = pts,
        tab = as.data.table(st_drop_geometry(pts))
      ))
    }

    # Modo multigeometría: cada geometría se procesa de forma independiente.
    pair_tabs <- list()
    pair_sfs <- list()
    k <- 0L

    # Varias geometrías suelen compartir zona UTM. Transformar todas las
    # estaciones una vez por EPSG evita repetir el mismo st_transform().
    pts_m_cache <- new.env(parent = emptyenv())

    for (part in s$parts) {

      epsg_key <- as.character(part$epsg)

      if (!exists(epsg_key, envir = pts_m_cache, inherits = FALSE)) {
        assign(
          epsg_key,
          st_transform(pts, part$epsg),
          envir = pts_m_cache
        )
      }

      pts_m <- get(epsg_key, envir = pts_m_cache, inherits = FALSE)
      dm <- as.numeric(st_distance(pts_m, part$target_m)[, 1])

      keep <- dm <= input$buffer_km * 1000 + 1e-6

      if (!any(keep)) {
        next
      }

      k <- k + 1L

      sf_i <- pts[keep, ]
      dist_i <- dm[keep] / 1000

      sf_i$geometry_id <- part$geometry_id
      sf_i$geometry_name <- part$geometry_name
      sf_i$distancia_km <- dist_i
      sf_i$dentro_kml <- dist_i <= 1e-8
      sf_i$dentro_buffer <- TRUE
      sf_i$zona_distancia <- zone_label(dist_i, input$buffer_km)

      pair_sfs[[k]] <- sf_i
      pair_tabs[[k]] <- as.data.table(st_drop_geometry(sf_i))
    }

    if (!length(pair_tabs)) {
      empty_pts <- pts[0, ]
      return(list(
        sf = empty_pts,
        sf_pairs = empty_pts,
        tab = data.table()
      ))
    }

    tab_pairs <- rbindlist(pair_tabs, use.names = TRUE, fill = TRUE)
    sf_pairs <- do.call(rbind, pair_sfs)

    # Para el mapa dibujamos cada estación física una sola vez. Las asociaciones
    # área-estación permanecen completas en tab/sf_pairs para los análisis.
    map_summary <- tab_pairs[, .(
      distancia_km = min(distancia_km, na.rm = TRUE),
      dentro_kml = any(dentro_kml %in% TRUE),
      dentro_buffer = TRUE,
      geometry_names = paste(unique(geometry_name), collapse = " | "),
      n_geometrias = uniqueN(geometry_id)
    ), by = station_id]

    map_summary[, zona_distancia := zone_label(distancia_km, input$buffer_km)]

    map_pts <- pts[pts$station_id %in% map_summary$station_id, ]
    idx <- match(map_pts$station_id, map_summary$station_id)

    map_pts$distancia_km <- map_summary$distancia_km[idx]
    map_pts$dentro_kml <- map_summary$dentro_kml[idx]
    map_pts$dentro_buffer <- TRUE
    map_pts$zona_distancia <- map_summary$zona_distancia[idx]
    map_pts$geometry_names <- map_summary$geometry_names[idx]
    map_pts$n_geometrias <- map_summary$n_geometrias[idx]

    list(
      sf = map_pts,
      sf_pairs = sf_pairs,
      tab = tab_pairs
    )
  })


  # Métricas temporales costosas. Dependen del conjunto espacial, variable y
  # periodo, pero NO del umbral ni del checkbox de cobertura nominal.
  # Así, cambiar 90 -> 95% no vuelve a consultar DAY ni recalcula rachas.
  candidate_metrics <- reactive({

    sp <- spatial_stations()$tab

    if (!nrow(sp)) {
      return(data.table())
    }

    st_ids_series <- unique(
      sp[
        tiene_serie %in% TRUE,
        station_id
      ]
    )

    sids <- if (length(st_ids_series)) {
      SERIES[
        station_id %chin% st_ids_series &
          tipo_dato == input$tipo,
        unique(series_id)
      ]
    } else {
      character()
    }

    ev <- best_series(
      evaluate_series(
        sids,
        input$periodo[1],
        input$periodo[2]
      )
    )

    if (nrow(ev)) {
      gap_stats <- period_gap_stats(
        ev$series_id,
        input$periodo[1],
        input$periodo[2]
      )

      ev <- merge(
        ev,
        gap_stats,
        by = "series_id",
        all.x = TRUE,
        sort = FALSE
      )
    } else {
      ev[, max_racha_vacia_periodo_dias := integer()]
    }

    out <- merge(
      sp,
      ev,
      by = "station_id",
      all.x = TRUE,
      suffixes = c("", "_serie")
    )

    for (z in c(
      "completitud_obs_pct",
      "cobertura_dias_pct",
      "dias_completos_pct"
    )) {
      if (!z %in% names(out)) {
        out[, (z) := 0]
      } else {
        out[is.na(get(z)), (z) := 0]
      }
    }

    if (!"max_racha_vacia_periodo_dias" %in% names(out)) {
      out[, max_racha_vacia_periodo_dias := NA_integer_]
    }

    for (z in c(
      "n_obs_validas",
      "n_dias_alguna",
      "n_dias_completos",
      "n_dias_parciales"
    )) {
      if (!z %in% names(out)) {
        out[, (z) := 0L]
      } else {
        out[is.na(get(z)), (z) := 0L]
      }
    }

    if (!"cubre_nominalmente" %in% names(out)) {
      out[, cubre_nominalmente := FALSE]
    } else {
      out[is.na(cubre_nominalmente), cubre_nominalmente := FALSE]
    }

    out[is.na(tiene_serie), tiene_serie := FALSE]

    out
  })


  # Clasificación/ranking liviano. Solo esta capa se invalida cuando cambia el
  # umbral o la exigencia de cobertura nominal.
  candidates <- reactive({

    out <- copy(candidate_metrics())

    if (!nrow(out)) {
      return(data.table())
    }

    out[
      ,
      apta :=
        tiene_serie &
        completitud_obs_pct >= input$umbral &
        (
          !input$nominal |
            cubre_nominalmente
        )
    ]

    out <- rank_best_candidates(out)

    if (
      "geometry_id" %in% names(out) &&
      any(!is.na(out$geometry_id))
    ) {
      setorder(
        out,
        geometry_id,
        -mejor_candidata,
        ranking_area,
        -tiene_serie,
        -completitud_obs_pct,
        distancia_km
      )
    } else {
      setorder(
        out,
        -apta,
        -tiene_serie,
        -completitud_obs_pct,
        distancia_km
      )
    }

    out
  })


  output$kml_info <- renderUI({
    s <- spatial()

    if (!isTRUE(s$has_kml)) {
      return(tagList(
        tags$b("Modo nacional"), br(),
        tags$small(
          paste0(
            "Sin archivo espacial: se muestran todas las estaciones de ",
            input$tipo,
            " del inventario de la app, incluidas las estaciones válidas sin serie."
          ),
          style = "color:#6c757d;"
        )
      ))
    }

    info <- s$geometry_info
    n_named <- sum(
      !is.na(info$geometry_label_field) &
        nzchar(info$geometry_label_field)
    )

    tagList(
      tags$b(
        paste(
          input$area_file$name,
          collapse = " + "
        )
      ),
      br(),
      tags$small(
        paste0(
          "Formato detectado: ",
          attr(s$area, "source_format")
        ),
        style = "color:#6c757d;"
      ),
      br(),
      tags$small(
        paste0(
          "Geometrías detectadas: ",
          s$n_geometries,
          ". Nombre/ID detectado en ",
          n_named,
          "; las restantes usan un nombre automático."
        ),
        style = "color:#6c757d;"
      ),
      br(),
      tags$small(
        paste0(
          "Área poligonal total: ",
          if (is.na(s$area_km2)) {
            "no aplicable"
          } else {
            paste0(fmt_num(s$area_km2, 1), " km²")
          }
        ),
        style = "color:#6c757d;"
      )
    )
  })



  # Estado compartido entre mapa y diagnóstico.
  diag_station_state <- reactiveVal(NULL)

  # Solicitud activa para la pestaña de descarga normalizada.
  # Se reemplaza únicamente cuando el usuario pulsa "Descargar normalizadas"
  # desde Candidatas o Ventana común.
  normalized_request_state <- reactiveVal(NULL)

  # Reactivos simples para pasar inputs globales a los módulos sin duplicar lógica.
  tipo_r <- reactive(input$tipo)
  buffer_km_r <- reactive(input$buffer_km)
  periodo_r <- reactive(input$periodo)
  umbral_r <- reactive(input$umbral)
  nominal_r <- reactive(input$nominal)

  map_mod <- mod_mapa_server(
    "mapa_mod",
    spatial = spatial,
    spatial_stations = spatial_stations,
    candidates = candidates,
    tipo = tipo_r,
    buffer_km = buffer_km_r,
    umbral = umbral_r,
    cuencas_ana = CUENCAS_ANA_MAP
  )

  diag_mod <- mod_diagnostico_server(
    "diagnostico_mod",
    spatial_stations = spatial_stations,
    tipo = tipo_r,
    diag_station_state = diag_station_state
  )

  cand_mod <- mod_candidatas_server(
    "candidatas_mod",
    candidates = candidates,
    spatial = spatial,
    tipo = tipo_r,
    periodo = periodo_r,
    umbral = umbral_r,
    nominal = nominal_r
  )

  window_mod <- mod_ventana_comun_server(
    "ventana_mod",
    spatial_stations = spatial_stations,
    spatial = spatial,
    tipo = tipo_r,
    buffer_km = buffer_km_r
  )

  mod_descarga_normalizada_server(
    "descarga_mod",
    request = reactive(
      normalized_request_state()
    )
  )

  mod_omm_server(
    "omm_mod",
    spatial = spatial,
    spatial_stations = spatial_stations,
    tipo = tipo_r
  )

  # --------------------------------------------------------------------------
  # ENVÍO A DESCARGA NORMALIZADA
  # --------------------------------------------------------------------------

  observeEvent(
    cand_mod$normalized_request(),
    {
      req_norm <- cand_mod$normalized_request()

      if (
        is.null(req_norm) ||
        !isTRUE(req_norm$ok)
      ) {
        showNotification(
          if (
            !is.null(req_norm$message)
          ) {
            req_norm$message
          } else {
            "No hay estaciones aptas para enviar a descarga normalizada."
          },
          type = "warning",
          duration = 6
        )
        return()
      }

      normalized_request_state(
        req_norm
      )

      bslib::nav_select(
        id = "main_nav",
        selected = "descarga_normalizada",
        session = session
      )
    },
    ignoreInit = TRUE
  )


  observeEvent(
    window_mod$normalized_request(),
    {
      req_norm <- window_mod$normalized_request()

      if (
        is.null(req_norm) ||
        !isTRUE(req_norm$ok)
      ) {
        showNotification(
          if (
            !is.null(req_norm$message)
          ) {
            req_norm$message
          } else {
            "No hay estaciones en la solución seleccionada."
          },
          type = "warning",
          duration = 6
        )
        return()
      }

      normalized_request_state(
        req_norm
      )

      bslib::nav_select(
        id = "main_nav",
        selected = "descarga_normalizada",
        session = session
      )
    },
    ignoreInit = TRUE
  )


  # --------------------------------------------------------------------------
  # NAVEGACIÓN DIRECTA DESDE EL MAPA
  # Mantiene exactamente el flujo de la versión monolítica:
  # clic -> station_id compartido -> pestaña Diagnóstico.
  # --------------------------------------------------------------------------

  observeEvent(
    map_mod$station_click(),
    {
      click <- map_mod$station_click()

      if (
        is.null(click$id) ||
        is.na(click$id) ||
        !nzchar(as.character(click$id))
      ) {
        return()
      }

      sid_click <- as.character(click$id)

      sc <- diag_mod$station_choices()

      if (
        !nrow(sc$table) ||
        !(sid_click %in% sc$table$station_id)
      ) {
        return()
      }

      diag_station_state(sid_click)

      bslib::nav_select(
        id = "main_nav",
        selected = "diagnostico",
        session = session
      )
    },
    ignoreInit = TRUE
  )
}

shinyApp(ui, server)

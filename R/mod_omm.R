# ============================================================================
# MÓDULO — DIAGNÓSTICO OMM
# v1.1.0-dev: diagnóstico independiente por geometría.
# ============================================================================

mod_omm_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
      card(
        card_header("Contexto fisiográfico"),
        selectInput(ns("wmo_region"), "Región de referencia", WMO_DENSITY$region, "Montañosa"),
        conditionalPanel(
          condition = "input.tipo == 'Precipitación'",
          radioButtons(
            ns("wmo_p_type"),
            "Referencia pluviométrica",
            c("No registradora" = "no_reg", "Registradora" = "reg"),
            "no_reg"
          )
        ),
        tags$small(
          "La densidad se calcula sobre el inventario físico y no usa filtros temporales.",
          style = "color:#6c757d;"
        )
      ),
      card(card_header("Referencia seleccionada"), uiOutput(ns("wmo_ref")))
    ),
    br(), uiOutput(ns("wmo_cards")), br(),
    card(card_header("Interpretación"), uiOutput(ns("wmo_text"))), br(),
    card(
      full_screen = TRUE,
      card_header("Diagnóstico por geometría"),
      DTOutput(ns("wmo_geometry_table"))
    ),
    br(),
    card(card_header("Tabla de referencias utilizada"), DTOutput(ns("wmo_table")))
  )
}

mod_omm_server <- function(id, spatial, spatial_stations, tipo) {
  moduleServer(id, function(input, output, session) {

    wmo <- reactive({

      s <- spatial()

      if (!isTRUE(s$has_kml)) {
        return(list(
          has_kml = FALSE,
          region = input$wmo_region,
          label = NA_character_,
          ref = NA_real_,
          by_geometry = data.table()
        ))
      }

      row <- WMO_DENSITY[region == input$wmo_region]

      if (tipo() == "Caudal") {
        ref <- row$caudal
        label <- "Caudal"
      } else if (input$wmo_p_type == "reg") {
        ref <- row$precip_reg
        label <- "Precipitación registradora"
      } else {
        ref <- row$precip_no_reg
        label <- "Precipitación no registradora"
      }

      info <- copy(s$geometry_info)
      x <- spatial_stations()$tab

      if (nrow(x)) {
        counts <- x[
          dentro_kml %in% TRUE,
          .(n_all = uniqueN(station_id)),
          by = .(geometry_id, geometry_name)
        ]
      } else {
        counts <- data.table(
          geometry_id = character(),
          geometry_name = character(),
          n_all = integer()
        )
      }

      by_geometry <- merge(
        info,
        counts,
        by = c("geometry_id", "geometry_name"),
        all.x = TRUE,
        sort = FALSE
      )

      by_geometry[is.na(n_all), n_all := 0L]

      by_geometry[, dens_all := fifelse(
        !is.na(area_km2) & area_km2 > 0 & n_all > 0,
        area_km2 / n_all,
        NA_real_
      )]

      by_geometry[, n_ref := fifelse(
        !is.na(area_km2) & area_km2 > 0 & ref > 0,
        pmax(1L, ceiling(area_km2 / ref)),
        NA_real_
      )]

      by_geometry[, cumple_ref := fifelse(
        !is.na(dens_all),
        dens_all <= ref,
        NA
      )]

      setorder(by_geometry, geometry_name, geometry_id)

      list(
        has_kml = TRUE,
        region = input$wmo_region,
        label = label,
        ref = ref,
        by_geometry = by_geometry
      )
    })

    output$wmo_ref <- renderUI({
      v <- wmo()

      if (!isTRUE(v$has_kml)) {
        return(div(
          class = "alert alert-secondary",
          paste(
            "El explorador nacional funciona sin archivo espacial.",
            "Cargue un archivo espacial poligonal solo si desea calcular densidad espacial OMM."
          )
        ))
      }

      tagList(
        h3(paste0(fmt_num(v$ref), " km²/estación")),
        p(tags$b(v$label), br(), v$region)
      )
    })

    output$wmo_cards <- renderUI({

      v <- wmo()

      if (!isTRUE(v$has_kml) || !nrow(v$by_geometry)) {
        return(NULL)
      }

      x <- v$by_geometry
      area_total <- if (any(is.finite(x$area_km2))) {
        sum(x$area_km2[is.finite(x$area_km2)], na.rm = TRUE)
      } else {
        NA_real_
      }

      n_valid <- sum(!is.na(x$dens_all))
      n_pass <- sum(x$cumple_ref %in% TRUE, na.rm = TRUE)

      layout_columns(
        col_widths = breakpoints(
          sm = c(12, 12, 12, 12),
          md = c(6, 6, 6, 6),
          lg = c(3, 3, 3, 3)
        ),

        metric_card(
          "Geometrías",
          fmt_num(nrow(x)),
          "Diagnóstico separado por área"
        ),

        metric_card(
          "Área poligonal total",
          if (is.na(area_total)) "—" else paste0(fmt_num(area_total, 1), " km²")
        ),

        metric_card(
          "Estaciones dentro",
          fmt_num(sum(x$n_all, na.rm = TRUE)),
          "Suma de relaciones por geometría"
        ),

        metric_card(
          "Cumplen referencia",
          if (!n_valid) "—" else paste0(n_pass, " / ", n_valid),
          "Solo geometrías con densidad evaluable"
        )
      )
    })

    output$wmo_text <- renderUI({
      v <- wmo()

      if (!isTRUE(v$has_kml)) {
        return(div(
          class = "alert alert-info",
          paste(
            "Modo nacional activo.",
            "El archivo espacial no es necesario para explorar estaciones ni evaluar periodos;",
            "solo se requiere para diagnósticos espaciales de área y densidad."
          )
        ))
      }

      x <- v$by_geometry

      if (!nrow(x)) {
        return(div(class = "alert alert-warning", "No hay geometrías evaluables."))
      }

      valid <- x[!is.na(dens_all)]

      if (!nrow(valid)) {
        return(div(
          class = "alert alert-warning",
          paste(
            "Ninguna geometría tiene simultáneamente área poligonal y estaciones",
            "de la variable seleccionada dentro del área; no se puede calcular km²/estación."
          )
        ))
      }

      n_pass <- sum(valid$cumple_ref %in% TRUE)

      div(
        class = if (n_pass == nrow(valid)) "alert alert-success" else "alert alert-warning",
        tags$b(
          if (nrow(valid) == 1) {
            if (n_pass == 1) {
              "La densidad observada es igual o más densa que la referencia seleccionada."
            } else {
              "La densidad observada es menos densa que la referencia seleccionada."
            }
          } else {
            paste0(
              n_pass,
              " de ",
              nrow(valid),
              " geometrías evaluables cumplen la referencia seleccionada."
            )
          }
        ),
        br(),
        tags$small(
          paste(
            "Cada geometría se calcula de forma independiente.",
            "Este diagnóstico es exclusivamente espacial e independiente",
            "del periodo seleccionado, la completitud y la cobertura temporal.",
            "No sustituye representatividad hidrológica, topográfica, climática",
            "ni fitness for purpose."
          )
        )
      )
    })

    output$wmo_geometry_table <- renderDT({
      v <- wmo()

      if (!isTRUE(v$has_kml) || !nrow(v$by_geometry)) {
        return(datatable(
          data.frame(Mensaje = "Cargue un archivo espacial para obtener el diagnóstico por geometría."),
          rownames = FALSE
        ))
      }

      x <- v$by_geometry

      tab <- x[, .(
        `ID geometría` = geometry_id,
        Área = geometry_name,
        `Tipo geometría` = geometry_type,
        `Archivo fuente` = source_file,
        Capa = source_layer,
        `Área (km²)` = round(area_km2, 2),
        `Estaciones dentro` = n_all,
        `Densidad observada (km²/est.)` = round(dens_all, 2),
        `Referencia (km²/est.)` = v$ref,
        `Equivalente referencia (est.)` = as.integer(n_ref),
        `Cumple referencia` = cumple_ref
      )]

      datatable(
        tab,
        filter = "top",
        extensions = "Buttons",
        options = dt_opts(20),
        rownames = FALSE,
        class = "stripe hover compact"
      )
    }, server = FALSE)

    output$wmo_table <- renderDT({
      x <- copy(WMO_DENSITY)
      setnames(
        x,
        c("region", "precip_no_reg", "precip_reg", "caudal"),
        c(
          "Región fisiográfica",
          "Precip. no registradora (km²/est.)",
          "Precip. registradora (km²/est.)",
          "Caudal (km²/est.)"
        )
      )
      datatable(
        x,
        options = list(paging = FALSE, searching = FALSE, info = FALSE),
        rownames = FALSE,
        class = "stripe hover compact"
      )
    })
  })
}

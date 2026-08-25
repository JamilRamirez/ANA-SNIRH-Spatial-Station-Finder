# VERIFICADO: la leyenda "Disponibilidad anual" se renderiza en AMBOS paneles.
# ============================================================================
# MÓDULO — ESTACIONES CANDIDATAS
# v1.1.0-dev: resultados separados por geometría.
# ============================================================================

mod_candidatas_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(), uiOutput(ns("temporal_msg")), uiOutput(ns("temporal_cards")), br(),

    layout_columns(
      col_widths = breakpoints(
        sm = c(12, 12),
        lg = c(8, 4)
      ),

      div(
        class = "alert alert-light border mb-0",
        tags$b("Descargas de aptas: "),
        "Originales reúne los reportes ANA/SNIRH archivados en un ZIP. ",
        "Normalizadas permite revisar y reducir la selección antes de generar el Excel."
      ),

      div(
        style = "display:grid;grid-template-columns:1fr 1fr;gap:.5rem;align-items:center;height:100%;",
        downloadButton(
          ns("download_raw_aptas"),
          "Originales ZIP",
          class = "btn-outline-primary",
          style = "width:100%;"
        ),
        actionButton(
          ns("send_normalized"),
          "Normalizadas",
          class = "btn-primary",
          width = "100%"
        )
      )
    ),

    br(),

    card(
      full_screen = TRUE,
      card_header("Selección espacial + disponibilidad temporal"),
      tags$small(
        "“Mejor candidata” identifica la mejor serie disponible dentro de cada geometría; no exige superar el umbral configurado.",
        style = "color:#6c757d;"
      ),
      tags$style(
        HTML(
          paste0(
            "#", ns("candidate_table"), " {width:100% !important;}",
            "#", ns("candidate_table"), " .dataTables_wrapper {width:100% !important;}",
            "#", ns("candidate_table"), " table.dataTable {width:100% !important;}",
            "#", ns("candidate_table"), " table.dataTable th,",
            "#", ns("candidate_table"), " table.dataTable td {white-space:nowrap;}"
          )
        )
      ),
      div(
        style = "width:100%;",
        DTOutput(ns("candidate_table"))
      )
    ),
    br(),

    card(
      full_screen = TRUE,
      card_header("Calendario anual de disponibilidad"),

      tags$small(
        paste(
          "Este gráfico se genera únicamente cuando se solicita.",
          "Las estaciones aptas y no aptas se muestran en paneles separados."
        ),
        paste(
          "El selector limita cada grupo por separado (máximo 50 por grupo);",
          "si existen más, se priorizan las mejor posicionadas en Candidatas."
        ),
        style = "color:#6c757d;"
      ),

      br(),

      layout_columns(
        col_widths = breakpoints(
          sm = c(12, 12, 12),
          md = c(6, 6, 12),
          lg = c(5, 3, 4)
        ),

        selectizeInput(
          ns("availability_area"),
          "Área a visualizar",
          choices = character(),
          options = list(
            maxOptions = 100,
            closeAfterSelect = TRUE
          )
        ),

        selectInput(
          ns("availability_n"),
          "Máximo por grupo",
          choices = c(10, 20, 30, 40, 50),
          selected = 30
        ),

        div(
          style = "padding-top:1.95rem;",
          actionButton(
            ns("availability_run"),
            "Generar calendario",
            class = "btn-primary",
            width = "100%"
          )
        )
      ),

      uiOutput(ns("availability_info")),
      uiOutput(ns("availability_plot_ui"))
    ),

    br(),

    layout_columns(
      col_widths = breakpoints(
        sm = c(12, 12),
        lg = c(6, 6)
      ),
      card(card_header("Completitud de observaciones"), plotOutput(ns("comp_plot"), height = "360px")),
      card(card_header("Estaciones por distancia"), plotOutput(ns("dist_plot"), height = "360px"))
    )
  )
}

mod_candidatas_server <- function(id, candidates, spatial, tipo, periodo, umbral, nominal) {
  moduleServer(id, function(input, output, session) {

    output$temporal_msg <- renderUI({

      x <- candidates()
      s <- spatial()

      if (!nrow(x)) {
        return(
          div(
            class = "alert alert-warning",
            "No hay estaciones candidatas."
          )
        )
      }

      n_total <- nrow(x)

      n_comp <- x[
        tiene_serie %in% TRUE &
          completitud_obs_pct >= umbral(),
        .N
      ]

      n_nominal <- x[
        tiene_serie %in% TRUE &
          cubre_nominalmente %in% TRUE,
        .N
      ]

      n_apta <- x[
        apta %in% TRUE,
        .N
      ]

      unit_txt <- if (isTRUE(s$has_kml)) {
        if (s$n_geometries > 1) "relaciones área–estación" else "estaciones"
      } else {
        "estaciones"
      }

      if (isTRUE(nominal())) {

        div(
          class = "alert alert-info",

          tags$b(
            paste0(
              tipo(),
              " — ",
              periodo()[1],
              " a ",
              periodo()[2]
            )
          ),

          br(),

          paste0(
            n_comp,
            " de ",
            n_total,
            " ", unit_txt,
            " alcanzan ≥",
            umbral(),
            "% de completitud; ",
            n_nominal,
            " cubren nominalmente todo el periodo; ",
            n_apta,
            " cumplen ambos criterios."
          )
        )

      } else {

        div(
          class = "alert alert-info",

          tags$b(
            paste0(
              tipo(),
              " — ",
              periodo()[1],
              " a ",
              periodo()[2]
            )
          ),

          br(),

          paste0(
            n_comp,
            " de ",
            n_total,
            " ", unit_txt,
            " alcanzan ≥",
            umbral(),
            "% de completitud."
          )
        )
      }
    })

    output$temporal_cards <- renderUI({

      x <- candidates()
      s <- spatial()

      if (!nrow(x)) {
        return(NULL)
      }

      n_comp <- x[
        tiene_serie %in% TRUE &
          completitud_obs_pct >= umbral(),
        .N
      ]

      n_nominal <- x[
        tiene_serie %in% TRUE &
          cubre_nominalmente %in% TRUE,
        .N
      ]

      n_apta <- x[
        apta %in% TRUE,
        .N
      ]

      n_unique <- uniqueN(x$station_id)

      layout_columns(
        col_widths = breakpoints(
          sm = c(12, 12, 12, 12),
          md = c(6, 6, 6, 6),
          lg = c(3, 3, 3, 3)
        ),

        metric_card(
          if (isTRUE(s$has_kml) && s$n_geometries > 1) {
            "Área–estación"
          } else if (isTRUE(s$has_kml)) {
            "Candidatas espaciales"
          } else {
            "Estaciones nacionales"
          },
          fmt_num(nrow(x)),
          if (isTRUE(s$has_kml) && s$n_geometries > 1) {
            paste0(fmt_num(n_unique), " estaciones únicas en ", s$n_geometries, " geometrías")
          } else {
            paste0(
              fmt_num(sum(x$tiene_serie, na.rm = TRUE)),
              " con serie disponible"
            )
          }
        ),

        metric_card(
          paste0(
            "Completitud ≥ ",
            umbral(),
            "%"
          ),
          fmt_num(n_comp),
          "Solo criterio de completitud"
        ),

        metric_card(
          "Cubren nominalmente",
          fmt_num(n_nominal),
          paste0(
            "Serie inicia ≤ ",
            periodo()[1],
            " y termina ≥ ",
            periodo()[2]
          )
        ),

        metric_card(
          "Aptas finales",
          fmt_num(n_apta),
          if (isTRUE(nominal())) {
            paste0(
              "≥",
              umbral(),
              "% + cobertura nominal"
            )
          } else {
            paste0(
              "≥",
              umbral(),
              "% de completitud"
            )
          }
        )
      )
    })

    # DT cliente: exporta todas las filas filtradas.
    #
    # La tabla se mantiene deliberadamente como una tabla de decisión:
    # contexto espacial + identificación + serie + calidad temporal.
    # Metadata interna/administrativa permanece disponible en los objetos
    # de la app y en futuras exportaciones, pero no se muestra aquí.
    output$candidate_table <- renderDT({

      x <- copy(candidates())
      s <- spatial()

      if (!nrow(x)) {
        return(
          datatable(
            data.frame(
              Mensaje = "Sin estaciones candidatas."
            ),
            rownames = FALSE
          )
        )
      }

      # Variable y unidad se presentan juntas para evitar una columna redundante.
      x[, variable_display := fifelse(
        !is.na(variable) &
          nzchar(trimws(variable)),
        paste0(
          variable,
          fifelse(
            !is.na(unidad) &
              nzchar(trimws(unidad)),
            paste0(
              " (",
              unidad,
              ")"
            ),
            ""
          )
        ),
        fifelse(
          !is.na(unidad) &
            nzchar(trimws(unidad)),
          paste0(
            "Serie (",
            unidad,
            ")"
          ),
          "Serie"
        )
      )]

      if (isTRUE(s$has_kml)) {

        tab <- x[
          ,
          .(
            Área = paste0(
              geometry_id,
              " · ",
              geometry_name
            ),
            Mejor = fifelse(
              mejor_candidata %in% TRUE,
              "★",
              ""
            ),
            Apta = fifelse(
              apta %in% TRUE,
              "Sí",
              "No"
            ),
            `Distancia (km)` = fifelse(
              is.na(distancia_km),
              NA_real_,
              round(
                distancia_km,
                2
              )
            ),
            Estación = nombre_estacion,
            Código = codigo_estacion,
            `Altitud (msnm)` = round(
              altitud_msnm,
              1
            ),
            IDConfig = id_config,
            Variable = variable_display,
            `Primera fecha` = as.Date(
              primera_fecha
            ),
            `Última fecha` = as.Date(
              ultima_fecha
            ),
            `Longitud (años)` = round(
              anios_nominales,
              1
            ),
            `Completitud (%)` = round(
              completitud_obs_pct,
              2
            ),
            `Máx. vacío (días)` = max_racha_vacia_periodo_dias,
            `Cubre periodo` = fifelse(
              cubre_nominalmente %in% TRUE,
              "Sí",
              "No"
            )
          )
        ]

      } else {

        # En modo nacional no existe una geometría de referencia:
        # no se muestran Área, Mejor ni Distancia.
        tab <- x[
          ,
          .(
            Apta = fifelse(
              apta %in% TRUE,
              "Sí",
              "No"
            ),
            Estación = nombre_estacion,
            Código = codigo_estacion,
            `Altitud (msnm)` = round(
              altitud_msnm,
              1
            ),
            IDConfig = id_config,
            Variable = variable_display,
            `Primera fecha` = as.Date(
              primera_fecha
            ),
            `Última fecha` = as.Date(
              ultima_fecha
            ),
            `Longitud (años)` = round(
              anios_nominales,
              1
            ),
            `Completitud (%)` = round(
              completitud_obs_pct,
              2
            ),
            `Máx. vacío (días)` = max_racha_vacia_periodo_dias,
            `Cubre periodo` = fifelse(
              cubre_nominalmente %in% TRUE,
              "Sí",
              "No"
            )
          )
        ]
      }

      candidate_opts <- dt_opts(30)

      # Mantener las columnas legibles es preferible a comprimirlas hasta
      # producir encabezados y filas excesivamente altos.
      candidate_opts$scrollX <- TRUE
      candidate_opts$autoWidth <- TRUE

      datatable(
        tab,
        filter = "top",
        extensions = "Buttons",
        options = candidate_opts,
        rownames = FALSE,
        class = "stripe hover compact",
        width = "100%"
      )
    }, server = FALSE)


    # ------------------------------------------------------------------------
    # CALENDARIO ANUAL DE DISPONIBILIDAD — BAJO DEMANDA
    #
    # Reglas:
    # - no se calcula hasta pulsar "Generar calendario";
    # - máximo visual: 50 estaciones aptas + 50 no aptas;
    # - el selector se aplica por separado a cada grupo;
    # - si hay más, se muestran las mejor posicionadas según la tabla de
    #   Candidatas (prioridad, aptitud, completitud, continuidad, longitud);
    # - en multigeometría se analiza una geometría por vez;
    # - el umbral usado en los colores es el vigente al pulsar el botón.
    # ------------------------------------------------------------------------

    observe({

      x <- candidates()
      s <- spatial()

      if (!nrow(x)) {

        updateSelectizeInput(
          session,
          "availability_area",
          choices = character(),
          selected = character(),
          server = TRUE
        )

        return()
      }

      if (
        isTRUE(s$has_kml) &&
        "geometry_id" %in% names(x)
      ) {

        areas <- unique(
          x[
            !is.na(geometry_id) &
              nzchar(geometry_id),
            .(
              geometry_id,
              geometry_name
            )
          ]
        )

        setorder(
          areas,
          geometry_id
        )

        choices <- setNames(
          areas$geometry_id,
          paste0(
            areas$geometry_id,
            " · ",
            areas$geometry_name
          )
        )

        current <- isolate(
          input$availability_area
        )

        selected <- if (
          !is.null(current) &&
          current %in% areas$geometry_id
        ) {
          current
        } else {
          areas$geometry_id[1]
        }

      } else {

        choices <- c(
          "Inventario nacional" = "__NATIONAL__"
        )

        selected <- "__NATIONAL__"
      }

      updateSelectizeInput(
        session,
        "availability_area",
        choices = choices,
        selected = selected,
        server = TRUE
      )
    })


    availability_result <- eventReactive(
      input$availability_run,
      {

        x <- copy(
          candidates()
        )

        s <- spatial()

        if (!nrow(x)) {
          return(
            list(
              ok = FALSE,
              message = "No hay estaciones candidatas para graficar."
            )
          )
        }

        n_show <- suppressWarnings(
          as.integer(
            input$availability_n
          )
        )

        if (
          is.na(n_show) ||
          !(n_show %in% c(10L, 20L, 30L, 40L, 50L))
        ) {
          n_show <- 30L
        }

        # Tope absoluto por legibilidad.
        n_show <- min(
          n_show,
          50L
        )

        area_id <- input$availability_area

        if (
          isTRUE(s$has_kml) &&
          !is.null(area_id) &&
          nzchar(area_id) &&
          !identical(area_id, "__NATIONAL__")
        ) {

          x <- x[
            geometry_id == area_id
          ]

          if (!nrow(x)) {
            return(
              list(
                ok = FALSE,
                message = "El área seleccionada no contiene estaciones candidatas."
              )
            )
          }

          area_name <- unique(
            paste0(
              x$geometry_id,
              " · ",
              x$geometry_name
            )
          )[1]

          # El ranking por área ya incorpora la mejor candidata y los criterios
          # temporales definidos para la pestaña.
          if ("ranking_area" %in% names(x)) {

            setorder(
              x,
              ranking_area,
              -tiene_serie,
              -completitud_obs_pct,
              distancia_km
            )

          } else {

            setorder(
              x,
              -mejor_candidata,
              -apta,
              -completitud_obs_pct,
              max_racha_vacia_periodo_dias,
              -anios_nominales,
              distancia_km
            )
          }

        } else {

          area_name <- "Inventario nacional"

          setorder(
            x,
            -apta,
            -completitud_obs_pct,
            max_racha_vacia_periodo_dias,
            -anios_nominales,
            nombre_estacion
          )
        }

        # Solo estaciones con serie temporal utilizable.
        x <- x[
          tiene_serie %in% TRUE &
            !is.na(series_id) &
            nzchar(series_id)
        ]

        # Defensa ante posibles duplicados de una misma estación.
        x <- x[
          !duplicated(station_id)
        ]

        n_total <- nrow(x)

        if (!n_total) {
          return(
            list(
              ok = FALSE,
              message = "No hay series disponibles para construir el calendario."
            )
          )
        }

        # --------------------------------------------------------------
        # Separación visual por aptitud.
        #
        # El máximo seleccionado se aplica A CADA GRUPO por separado.
        # Así, una abundancia de estaciones aptas no oculta por completo
        # a las no aptas (y viceversa), manteniendo un máximo absoluto de
        # 50 estaciones por panel.
        # --------------------------------------------------------------
        aptas_pool <- x[
          apta %in% TRUE
        ]

        no_aptas_pool <- x[
          !(apta %in% TRUE)
        ]

        n_total_aptas <- nrow(
          aptas_pool
        )

        n_total_no_aptas <- nrow(
          no_aptas_pool
        )

        shown_aptas <- if (n_total_aptas) {
          aptas_pool[
            seq_len(
              min(
                n_show,
                n_total_aptas
              )
            )
          ]
        } else {
          aptas_pool
        }

        shown_no_aptas <- if (n_total_no_aptas) {
          no_aptas_pool[
            seq_len(
              min(
                n_show,
                n_total_no_aptas
              )
            )
          ]
        } else {
          no_aptas_pool
        }

        shown_aptas[, availability_group := "Aptas"]
        shown_no_aptas[, availability_group := "No aptas"]

        shown <- rbindlist(
          list(
            shown_aptas,
            shown_no_aptas
          ),
          use.names = TRUE,
          fill = TRUE
        )

        threshold_snapshot <- suppressWarnings(
          as.numeric(
            umbral()
          )
        )

        require_nominal_snapshot <- isTRUE(
          nominal()
        )

        if (
          is.na(threshold_snapshot) ||
          threshold_snapshot < 0 ||
          threshold_snapshot > 100
        ) {
          threshold_snapshot <- 90
        }

        # Etiquetas compactas pero inequívocas.
        # El resaltado de la mejor opción se maneja tipográficamente
        # en el gráfico, no mediante símbolos en el nombre.
        shown[, station_label := paste0(
          nombre_estacion,
          fifelse(
            !is.na(codigo_estacion) &
              nzchar(codigo_estacion),
            paste0(
              " [",
              codigo_estacion,
              "]"
            ),
            ""
          )
        )]

        shown[, station_order := seq_len(.N)]
        shown[, station_order_group := seq_len(.N), by = availability_group]

        # En una geometría se conserva la "Mejor candidata" real.
        shown[, calendar_best := mejor_candidata %in% TRUE]

        # En modo nacional no existe una "mejor estación del Perú".
        # Para ayudar a leer la figura, se resalta únicamente la mejor
        # posicionada dentro de la vista/ranking generado.
        if (
          !isTRUE(s$has_kml) ||
          identical(area_id, "__NATIONAL__")
        ) {
          shown[, calendar_best := FALSE]
          shown[1, calendar_best := TRUE]
        }

        # --------------------------------------------------------------
        # El calendario debe respetar EXACTAMENTE el periodo solicitado
        # en Candidatas, no el periodo nominal completo de las estaciones.
        #
        # El snapshot se toma al pulsar "Generar calendario", de modo que
        # cambiar el periodo después no modifica silenciosamente el gráfico.
        # --------------------------------------------------------------
        period_snapshot <- periodo()

        period_start <- as.IDate(
          period_snapshot[1]
        )

        period_end <- as.IDate(
          period_snapshot[2]
        )

        if (
          is.na(period_start) ||
          is.na(period_end) ||
          period_start > period_end
        ) {
          return(
            list(
              ok = FALSE,
              message = "El periodo seleccionado no es válido."
            )
          )
        }

        y0 <- year(
          period_start
        )

        y1 <- year(
          period_end
        )

        years <- seq.int(
          as.integer(y0),
          as.integer(y1)
        )

        grid <- CJ(
          series_id = as.character(
            shown$series_id
          ),
          anio = years,
          unique = TRUE
        )

        meta <- shown[
          ,
          .(
            series_id,
            station_id,
            station_label,
            station_order,
            station_order_group,
            availability_group,
            calendar_best,
            primera_fecha,
            ultima_fecha,
            expected_obs_day
          )
        ]

        grid <- merge(
          grid,
          meta,
          by = "series_id",
          all.x = TRUE,
          sort = FALSE
        )

        # Límites efectivos de cada año dentro del periodo solicitado.
        # Esto hace que un periodo parcial (p.ej. junio 1981–septiembre 2010)
        # no penalice los meses que el usuario nunca pidió.
        grid[, year_start := as.IDate(
          sprintf(
            "%04d-01-01",
            anio
          )
        )]

        grid[, year_end := as.IDate(
          sprintf(
            "%04d-12-31",
            anio
          )
        )]

        grid[, window_start := pmax(
          year_start,
          period_start
        )]

        grid[, window_end := pmin(
          year_end,
          period_end
        )]

        grid[, dias_periodo_anual := as.integer(
          window_end -
            window_start
        ) + 1L]

        # Observaciones únicamente dentro del periodo solicitado.
        obs <- DAY[
          series_id %chin% shown$series_id &
            fecha >= period_start &
            fecha <= period_end,
          .(
            n_obs_validas = sum(
              n_obs_validas,
              na.rm = TRUE
            )
          ),
          by = .(
            series_id,
            anio = year(fecha)
          )
        ]

        grid <- merge(
          grid,
          obs,
          by = c(
            "series_id",
            "anio"
          ),
          all.x = TRUE,
          sort = FALSE
        )

        grid[
          is.na(expected_obs_day) |
            expected_obs_day < 1,
          expected_obs_day := 1L
        ]

        # "Fuera del registro" significa que el tramo solicitado de ese año
        # no intersecta en absoluto el periodo nominal de la serie.
        grid[, dentro_registro := (
          !is.na(primera_fecha) &
            !is.na(ultima_fecha) &
            ultima_fecha >= window_start &
            primera_fecha <= window_end
        )]

        grid[
          dentro_registro %in% TRUE &
            is.na(n_obs_validas),
          n_obs_validas := 0
        ]

        grid[, obs_esperadas := (
          dias_periodo_anual *
            expected_obs_day
        )]

        grid[, completitud_anual_pct := fifelse(
          dentro_registro %in% TRUE &
            obs_esperadas > 0,
          100 *
            n_obs_validas /
            obs_esperadas,
          NA_real_
        )]

        # Protección ante sobreconteos anómalos.
        grid[
          !is.na(completitud_anual_pct),
          completitud_anual_pct := pmin(
            100,
            completitud_anual_pct
          )
        ]

        threshold_label <- paste0(
          "≥",
          format(
            threshold_snapshot,
            trim = TRUE,
            scientific = FALSE
          ),
          "%"
        )

        below_label <- paste0(
          "<",
          format(
            threshold_snapshot,
            trim = TRUE,
            scientific = FALSE
          ),
          "% con datos"
        )

        grid[, estado := fcase(
          !dentro_registro,
          "Fuera del registro",
          completitud_anual_pct >= threshold_snapshot,
          threshold_label,
          completitud_anual_pct > 0,
          below_label,
          default = "Sin datos"
        )]

        grid[, estado := factor(
          estado,
          levels = c(
            threshold_label,
            below_label,
            "Sin datos",
            "Fuera del registro"
          )
        )]

        # Mantener el mismo orden de prioridad usado al seleccionar las filas.
        label_levels <- shown[
          order(
            station_order
          ),
          station_label
        ]

        grid[, station_label := factor(
          station_label,
          levels = rev(
            label_levels
          )
        )]

        setorder(
          grid,
          station_order,
          anio
        )

        list(
          ok = TRUE,
          data = grid,
          n_total = n_total,
          n_shown = nrow(shown),
          n_total_aptas = n_total_aptas,
          n_total_no_aptas = n_total_no_aptas,
          n_shown_aptas = nrow(shown_aptas),
          n_shown_no_aptas = nrow(shown_no_aptas),
          n_requested = n_show,
          area_name = area_name,
          y0 = y0,
          y1 = y1,
          period_start = period_start,
          period_end = period_end,
          threshold = threshold_snapshot,
          require_nominal = require_nominal_snapshot,
          threshold_label = threshold_label,
          below_label = below_label
        )
      },
      ignoreInit = TRUE
    )


    output$availability_info <- renderUI({

      # Antes del primer clic no mostramos un error: solo una instrucción.
      if (
        is.null(
          input$availability_run
        ) ||
        input$availability_run == 0
      ) {
        return(
          div(
            class = "alert alert-light border mt-3",
            paste(
              "Seleccione el área y el número máximo por grupo,",
              "y pulse «Generar calendario»."
            )
          )
        )
      }

      r <- availability_result()

      if (!isTRUE(r$ok)) {
        return(
          div(
            class = "alert alert-warning mt-3",
            r$message
          )
        )
      }

      limited_aptas <- (
        r$n_total_aptas >
          r$n_shown_aptas
      )

      limited_no_aptas <- (
        r$n_total_no_aptas >
          r$n_shown_no_aptas
      )

      limited <- (
        limited_aptas ||
          limited_no_aptas
      )

      bold_note <- if (
        identical(
          r$area_name,
          "Inventario nacional"
        )
      ) {
        "Negrita = mejor posicionada entre las estaciones mostradas."
      } else {
        "Negrita = mejor candidata disponible para el área."
      }

      div(
        class = if (limited) {
          "alert alert-warning mt-3"
        } else {
          "alert alert-info mt-3"
        },

        tags$b(
          paste0(
            r$area_name,
            " — ",
            as.character(
              r$period_start
            ),
            " a ",
            as.character(
              r$period_end
            )
          )
        ),

        br(),

        paste0(
          "Aptas: ",
          r$n_shown_aptas,
          " de ",
          r$n_total_aptas,
          "; no aptas: ",
          r$n_shown_no_aptas,
          " de ",
          r$n_total_no_aptas,
          ". Máximo configurado: ",
          r$n_requested,
          " estaciones por grupo. "
        ),

        paste0(
          "La separación apta/no apta usa los filtros vigentes al generar el gráfico: ",
          "completitud ≥",
          r$threshold,
          "%",
          if (isTRUE(r$require_nominal)) {
            " y cobertura nominal completa"
          } else {
            ""
          },
          ". Los colores muestran la disponibilidad anual respecto del mismo umbral."
        ),

        tags$small(
          bold_note,
          style = "display:block;margin-top:.35rem;"
        ),

        tags$small(
          "El calendario no se actualiza automáticamente: si cambia filtros, área o umbral, vuelva a pulsar «Generar calendario».",
          style = "display:block;margin-top:.15rem;"
        )
      )
    })


    output$availability_plot_ui <- renderUI({

      if (
        is.null(
          input$availability_run
        ) ||
        input$availability_run == 0
      ) {
        return(NULL)
      }

      r <- availability_result()

      if (!isTRUE(r$ok)) {
        return(NULL)
      }

      # Cada panel conserva su propia altura dinámica. El máximo seleccionado
      # se aplica por grupo, por lo que cada panel puede tener hasta 50 filas.
      plot_height <- function(n) {
        max(
          260L,
          min(
            1250L,
            120L +
              22L *
                as.integer(n)
          )
        )
      }

      blocks <- list()

      if (r$n_shown_aptas > 0L) {
        blocks[[length(blocks) + 1L]] <- tagList(
          div(
            style = "display:flex;align-items:baseline;gap:.5rem;margin-top:1rem;margin-bottom:.35rem;",
            tags$h6(
              "Estaciones aptas",
              style = "margin:0;font-weight:700;"
            ),
            tags$small(
              paste0(
                r$n_shown_aptas,
                " mostradas de ",
                r$n_total_aptas
              ),
              style = "color:#6c757d;"
            )
          ),
          plotOutput(
            session$ns(
              "availability_plot_aptas"
            ),
            height = paste0(
              plot_height(
                r$n_shown_aptas
              ),
              "px"
            )
          )
        )
      }

      if (
        r$n_shown_aptas > 0L &&
          r$n_shown_no_aptas > 0L
      ) {
        blocks[[length(blocks) + 1L]] <- div(
          style = "height:18px;border-top:1px solid #DCE3E3;margin-top:6px;"
        )
      }

      if (r$n_shown_no_aptas > 0L) {
        blocks[[length(blocks) + 1L]] <- tagList(
          div(
            style = "display:flex;align-items:baseline;gap:.5rem;margin-top:1rem;margin-bottom:.35rem;",
            tags$h6(
              "Estaciones no aptas",
              style = "margin:0;font-weight:700;"
            ),
            tags$small(
              paste0(
                r$n_shown_no_aptas,
                " mostradas de ",
                r$n_total_no_aptas
              ),
              style = "color:#6c757d;"
            )
          ),
          plotOutput(
            session$ns(
              "availability_plot_no_aptas"
            ),
            height = paste0(
              plot_height(
                r$n_shown_no_aptas
              ),
              "px"
            )
          )
        )
      }

      do.call(
        tagList,
        blocks
      )
    })


    availability_group_plot <- function(group_name) {

      renderPlot({

        r <- availability_result()

        shiny::validate(
          shiny::need(
            isTRUE(r$ok),
            if (!is.null(r$message)) {
              r$message
            } else {
              "Genere el calendario."
            }
          )
        )

        x <- r$data[
          availability_group == group_name
        ]

        shiny::validate(
          shiny::need(
            nrow(x) > 0,
            paste0(
              "No hay estaciones ",
              tolower(group_name),
              " para mostrar."
            )
          )
        )

        best_labels <- unique(
          as.character(
            x[
              calendar_best %in% TRUE,
              station_label
            ]
          )
        )

        # Repetir la leyenda en ambos paneles para que cada gráfico
        # sea interpretable de forma independiente.
        legend_position <- "bottom"

        ggplot(
          x,
          aes(
            x = anio,
            y = station_label,
            fill = estado
          )
        ) +
          geom_tile(
            linewidth = 0.15
          ) +
          scale_fill_manual(
            values = setNames(
              c(
                "#2ca25f",
                "#fdae6b",
                "#de2d26",
                "#e5e5e5"
              ),
              c(
                r$threshold_label,
                r$below_label,
                "Sin datos",
                "Fuera del registro"
              )
            ),
            drop = FALSE,
            name = "Disponibilidad anual"
          ) +
          scale_x_continuous(
            limits = c(
              r$y0 - 0.5,
              r$y1 + 0.5
            ),
            breaks = pretty(
              c(
                r$y0,
                r$y1
              ),
              n = 12
            ),
            expand = c(
              0,
              0
            )
          ) +
          scale_y_discrete(
            labels = function(labs) {

              expr_txt <- vapply(
                labs,
                function(lbl) {

                  quoted <- encodeString(
                    lbl,
                    quote = "'"
                  )

                  if (lbl %in% best_labels) {
                    paste0(
                      "bold(",
                      quoted,
                      ")"
                    )
                  } else {
                    paste0(
                      "plain(",
                      quoted,
                      ")"
                    )
                  }
                },
                character(1)
              )

              parse(
                text = expr_txt
              )
            }
          ) +
          labs(
            x = "Año",
            y = NULL
          ) +
          guides(
            fill = guide_legend(
              title.position = "left",
              title.hjust = 0
            )
          ) +
          theme_minimal(
            base_size = 11
          ) +
          theme(
            panel.grid = element_blank(),
            axis.text.y = element_text(
              size = 8
            ),
            legend.position = legend_position
          )
      })
    }


    output$availability_plot_aptas <- availability_group_plot(
      "Aptas"
    )

    output$availability_plot_no_aptas <- availability_group_plot(
      "No aptas"
    )


    # ------------------------------------------------------------------------
    # DESCARGAR REPORTES ORIGINALES DE LAS ESTACIONES APTAS
    # ------------------------------------------------------------------------

    output$download_raw_aptas <- downloadHandler(

      filename = function() {
        paste0(
          "ANA_SNIRH_candidatas_aptas_",
          format(as.Date(periodo()[1]), "%Y%m%d"),
          "_",
          format(as.Date(periodo()[2]), "%Y%m%d"),
          ".zip"
        )
      },

      content = function(file) {

        x <- copy(candidates())[
          apta %in% TRUE &
            !is.na(id_config) &
            nzchar(as.character(id_config))
        ]

        # Una estación puede aparecer en varias geometrías, pero su reporte
        # original es único por IDConfig.
        x <- x[
          !duplicated(id_config)
        ]

        req(
          nrow(x) > 0
        )

        tmpdir <- tempfile(
          "candidate_raw_"
        )

        dir.create(
          tmpdir,
          recursive = TRUE,
          showWarnings = FALSE
        )

        on.exit(
          unlink(
            tmpdir,
            recursive = TRUE,
            force = TRUE
          ),
          add = TRUE
        )

        manifest <- x[
          ,
          .(
            Estacion = nombre_estacion,
            Codigo = codigo_estacion,
            IDConfig = id_config,
            URL = NA_character_,
            Estado = "No disponible"
          )
        ]

        downloaded <- character()

        withProgress(
          message = "Descargando reportes originales de estaciones aptas...",
          value = 0,
          {
            for (i in seq_len(nrow(x))) {

              url <- raw_report_url(
                x$id_config[i]
              )

              manifest$URL[i] <- url

              if (
                length(url) &&
                  !is.na(url) &&
                  nzchar(url)
              ) {

                safe_station <- gsub(
                  "[^A-Za-z0-9_-]+",
                  "_",
                  iconv(
                    paste0(
                      x$nombre_estacion[i],
                      "_",
                      x$codigo_estacion[i]
                    ),
                    to = "ASCII//TRANSLIT"
                  )
                )

                fn <- paste0(
                  sprintf("%03d", i),
                  "_",
                  safe_station,
                  "_IDConfig_",
                  x$id_config[i],
                  ".xlsx"
                )

                dest <- file.path(
                  tmpdir,
                  fn
                )

                ok <- tryCatch(
                  {
                    utils::download.file(
                      url,
                      destfile = dest,
                      mode = "wb",
                      quiet = TRUE
                    )

                    file.exists(dest) &&
                      file.info(dest)$size > 0
                  },
                  error = function(e) {
                    FALSE
                  }
                )

                if (isTRUE(ok)) {
                  manifest$Estado[i] <- "Descargado"
                  downloaded <- c(
                    downloaded,
                    fn
                  )
                } else {
                  manifest$Estado[i] <- "Error de descarga"
                }
              }

              incProgress(
                1 / nrow(x)
              )
            }
          }
        )

        fwrite(
          manifest,
          file.path(tmpdir, "manifest.csv"),
          bom = TRUE
        )

        zip::zipr(
          zipfile = file,
          files = c(downloaded, "manifest.csv"),
          root = tmpdir
        )
      }
    )


    # ------------------------------------------------------------------------
    # ENVIAR A DESCARGA NORMALIZADA
    # ------------------------------------------------------------------------

    normalized_request <- eventReactive(
      input$send_normalized,
      {

        x <- copy(
          candidates()
        )

        s <- spatial()

        x <- x[
          apta %in% TRUE &
            tiene_serie %in% TRUE &
            !is.na(series_id) &
            nzchar(series_id)
        ]

        if (!nrow(x)) {
          return(
            list(
              ok = FALSE,
              message = paste0(
                "No hay estaciones aptas con los filtros actuales (completitud ≥",
                umbral(),
                "%",
                if (isTRUE(nominal())) {
                  " y cobertura nominal"
                } else {
                  ""
                },
                ")."
              )
            )
          )
        }

        # Una misma estación puede pertenecer a varios buffers. Para la descarga
        # normalizada debe existir una sola columna por serie; se conservan todas
        # las geometrías asociadas como metadata.
        if (isTRUE(s$has_kml)) {

          geo <- x[
            ,
            .(
              geometry_id = paste(
                unique(
                  na.omit(
                    geometry_id
                  )
                ),
                collapse = " | "
              ),
              geometry_name = paste(
                unique(
                  na.omit(
                    geometry_name
                  )
                ),
                collapse = " | "
              )
            ),
            by = series_id
          ]

        } else {

          geo <- unique(
            x[
              ,
              .(
                series_id,
                geometry_id = "NACIONAL",
                geometry_name = "Inventario nacional"
              )
            ]
          )
        }

        selected <- unique(
          x[
            ,
            .(
              series_id
            )
          ]
        )

        selected <- merge(
          selected,
          geo,
          by = "series_id",
          all.x = TRUE,
          sort = FALSE
        )

        list(
          ok = TRUE,
          request_id = paste0(
            "CAND_",
            as.numeric(
              Sys.time()
            )
          ),
          source = "Estaciones candidatas",
          source_detail = paste0(
            "Aptas finales | completitud ≥",
            umbral(),
            "%",
            if (isTRUE(nominal())) {
              " + cobertura nominal"
            } else {
              ""
            }
          ),
          tipo = tipo(),
          period_start = as.IDate(
            periodo()[1]
          ),
          period_end = as.IDate(
            periodo()[2]
          ),
          stations = selected
        )
      },
      ignoreInit = TRUE
    )


    output$comp_plot <- renderPlot({

      x <- candidates()

      shiny::validate(
        shiny::need(nrow(x) > 0, "Sin estaciones.")
      )

      ggplot(
        x,
        aes(completitud_obs_pct)
      ) +
        geom_histogram(
          binwidth = 5,
          boundary = 0
        ) +
        geom_vline(
          xintercept = umbral(),
          linetype = 2
        ) +
        scale_x_continuous(
          limits = c(0, 100),
          breaks = seq(0, 100, 10)
        ) +
        labs(
          x = "Completitud de observaciones (%)",
          y = "Estaciones / relaciones",
          subtitle = paste0(
            "Línea discontinua = ",
            umbral(),
            "%. La cobertura nominal se evalúa por separado."
          )
        ) +
        theme_minimal(base_size = 11)
    })

    output$dist_plot <- renderPlot({
      x <- candidates()
      shiny::validate(shiny::need(nrow(x) > 0, "Sin estaciones."))

      d <- x[, .N, by = zona_distancia]

      ggplot(d, aes(reorder(zona_distancia, -N), N)) +
        geom_col() +
        geom_text(aes(label = N), vjust = -.25) +
        labs(x = NULL, y = "Número de estaciones / relaciones") +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(angle = 20, hjust = 1))
    })

    list(
      normalized_request = normalized_request
    )
  })
}

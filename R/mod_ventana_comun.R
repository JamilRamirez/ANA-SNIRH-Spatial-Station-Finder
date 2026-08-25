# ============================================================================
# MÓDULO — VENTANAS COMUNES OPTIMIZADAS
# v1.1.0 — tres soluciones distintas + calendario + descarga masiva.
# ============================================================================

mod_ventana_comun_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),

    tags$style(
      HTML(
        paste0(
          ".rw-compact-card{display:block !important;height:auto !important;min-height:0 !important;}",
          ".rw-compact-card>.card-body,.rw-compact-card-body{display:block !important;flex:none !important;height:auto !important;min-height:0 !important;overflow:visible !important;padding:.75rem 1rem .65rem 1rem !important;}",
          ".rw-compact-card .shiny-bound-output{height:auto !important;min-height:0 !important;}",
          ".rw-compact-card .dataTables_wrapper{height:auto !important;min-height:0 !important;margin:0 !important;padding:0 !important;}",
          ".rw-compact-card .dt-buttons{margin:0 0 .35rem 0 !important;}",
          ".rw-compact-card table.dataTable{margin-top:0 !important;margin-bottom:0 !important;}",
          ".rw-detail-toolbar{display:grid;grid-template-columns:minmax(0,1fr) auto;align-items:center;column-gap:1rem;row-gap:.25rem;margin:0 0 .45rem 0 !important;}",
          ".rw-detail-summary{min-width:0;display:flex;flex-direction:column;gap:.12rem;}",
          ".rw-detail-note{color:#6c757d;display:block;line-height:1.25;}",
          ".rw-detail-status{color:#6c757d;display:block;line-height:1.2;margin:0 !important;}",
          ".rw-detail-download{margin-left:auto;white-space:nowrap;}",
          ".rw-detail-actions{display:flex;align-items:center;justify-content:flex-end;gap:.5rem;flex-wrap:nowrap;}",
          ".rw-detail-actions .btn{margin:0 !important;white-space:nowrap;}",
          "@media (max-width: 1050px){.rw-detail-toolbar{grid-template-columns:1fr;}.rw-detail-download{margin-left:0;}.rw-detail-actions{justify-content:flex-start;flex-wrap:wrap;}}"
        )
      )
    ),

    layout_columns(
      col_widths = breakpoints(
        sm = c(12, 12, 12, 12),
        md = c(6, 6, 6, 6),
        lg = c(3, 3, 3, 3)
      ),

      card(
        card_header("Longitud mínima"),
        numericInput(
          ns("rw_years"),
          "Mínimo de años consecutivos",
          value = 30,
          min = 1,
          max = 150,
          step = 1
        ),
        tags$small(
          "La búsqueda puede devolver ventanas más largas que este mínimo.",
          style = "color:#6c757d;"
        )
      ),

      card(
        card_header("Inicio mínimo"),
        numericInput(
          ns("rw_start_year"),
          "Año inicial mínimo",
          value = 1914,
          min = 1800,
          max = 2100,
          step = 1
        ),
        tags$small(
          "Solo se evaluarán ventanas cuyo año de inicio sea igual o posterior a este valor.",
          style = "color:#6c757d;"
        )
      ),

      card(
        card_header("Completitud"),
        sliderInput(
          ns("rw_threshold"),
          "Completitud mínima (%)",
          min = 50,
          max = 100,
          value = 90,
          step = 1
        )
      ),

      card(
        card_header("Cobertura nominal"),
        checkboxInput(
          ns("rw_nominal"),
          "Exigir que la serie cubra toda la ventana",
          value = TRUE
        ),
        tags$small(
          paste(
            "Con múltiples geometrías, cada área se evalúa de forma",
            "completamente independiente."
          ),
          style = "color:#6c757d;"
        )
      )
    ),

    actionButton(
      ns("rw_run"),
      "Buscar 3 mejores combinaciones",
      class = "btn-primary"
    ),

    br(),

    uiOutput(ns("rw_message")),
    uiOutput(ns("rw_cards")),

    br(),

    # Bootstrap card manual: evita que bslib convierta este bloque pequeño
    # en un contenedor fillable y reserve altura vertical innecesaria.
    div(
      id = ns("rw_solutions_card_compact"),
      class = "card rw-compact-card",

      div(
        class = "card-header",
        "Tres soluciones de compromiso"
      ),

      div(
        class = "card-body rw-compact-card-body",

        uiOutput(ns("rw_area_ui")),

        tags$small(
          paste(
            "Máxima cobertura prioriza estaciones; Máxima longitud prioriza años;",
            "Equilibrada busca el mejor compromiso entre ambos objetivos."
          ),
          style = "color:#6c757d;display:block;margin:0 0 .45rem 0;"
        ),

        DTOutput(ns("rw_solutions"))
      )
    ),

    br(),

    card(
      full_screen = TRUE,
      fill = FALSE,
      card_header("Estaciones aptas por año inicial"),

      uiOutput(ns("rw_curve_caption")),

      plotOutput(
        ns("rw_curve_plot"),
        height = "420px"
      )
    ),

    br(),

    uiOutput(ns("rw_solution_cards")),

    br(),

    card(
      full_screen = TRUE,
      card_header("Disponibilidad anual de la solución seleccionada"),

      layout_columns(
        col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),

        selectInput(
          ns("rw_calendar_n"),
          "Máximo de estaciones en el gráfico",
          choices = c(10, 20, 30, 40, 50),
          selected = 30
        ),

        uiOutput(ns("rw_calendar_info"))
      ),

      plotOutput(
        ns("rw_calendar_plot"),
        height = "620px"
      )
    ),

    br(),

    # Igual que el bloque de soluciones: tarjeta Bootstrap normal para que
    # la altura dependa exclusivamente de su contenido real.
    div(
      id = ns("rw_detail_card_compact"),
      class = "card rw-compact-card",

      div(
        class = "card-header",
        "Estaciones de la solución seleccionada"
      ),

      div(
        class = "card-body rw-compact-card-body",

        div(
          class = "rw-detail-toolbar",

          div(
            class = "rw-detail-summary",

            tags$small(
              paste(
                "La tabla contiene únicamente las estaciones que cumplen la",
                "solución seleccionada."
              ),
              class = "rw-detail-note"
            ),

            uiOutput(ns("rw_download_status_text"))
          ),

          div(
            class = "rw-detail-download",
            uiOutput(ns("rw_download_buttons"))
          )
        ),

        DTOutput(ns("rw_detail"))
      )
    )
  )
}


mod_ventana_comun_server <- function(
  id,
  spatial_stations,
  spatial,
  tipo,
  buffer_km
) {

  moduleServer(
    id,
    function(input, output, session) {

      empty_result <- function() {
        list(
          solutions = data.table(),
          detail = data.table(),
          by_geometry = data.table(),
          all_windows = data.table(),
          eligible_stations = character(),
          n_eligible = 0L,
          multi = FALSE,
          min_years = NA_integer_,
          min_start_year = NA_integer_,
          threshold = NA_real_,
          require_nominal = NA
        )
      }


      # ----------------------------------------------------------------------
      # BÚSQUEDA BAJO DEMANDA
      # ----------------------------------------------------------------------

      rw_result <- eventReactive(
        input$rw_run,
        {

          esp <- spatial_stations()$tab
          s <- spatial()

          if (!nrow(esp)) {
            return(
              empty_result()
            )
          }

          min_years_snapshot <- suppressWarnings(
            as.integer(
              input$rw_years
            )
          )

          start_year_snapshot <- suppressWarnings(
            as.integer(
              input$rw_start_year
            )
          )

          threshold_snapshot <- suppressWarnings(
            as.numeric(
              input$rw_threshold
            )
          )

          nominal_snapshot <- isTRUE(
            input$rw_nominal
          )

          if (
            is.na(min_years_snapshot) ||
            min_years_snapshot < 1L
          ) {
            min_years_snapshot <- 30L
          }

          if (
            is.na(start_year_snapshot) ||
            start_year_snapshot < 1800L ||
            start_year_snapshot > 2100L
          ) {
            start_year_snapshot <- 1914L
          }

          if (
            is.na(threshold_snapshot) ||
            threshold_snapshot < 0 ||
            threshold_snapshot > 100
          ) {
            threshold_snapshot <- 90
          }

          withProgress(
            message = "Buscando combinaciones de ventanas...",
            value = 0.05,
            {

              # --------------------------------------------------------------
              # MODO NACIONAL
              # --------------------------------------------------------------
              if (!isTRUE(s$has_kml)) {

                ans <- search_optimized_windows(
                  station_ids = unique(
                    esp[
                      tiene_serie %in% TRUE,
                      station_id
                    ]
                  ),
                  tipo = tipo(),
                  min_years = min_years_snapshot,
                  threshold = threshold_snapshot,
                  require_nominal = nominal_snapshot,
                  min_start_year = start_year_snapshot
                )

                if (nrow(ans$solutions)) {
                  ans$solutions[, `:=`(
                    geometry_id = "NACIONAL",
                    geometry_name = "Inventario nacional"
                  )]
                }

                if (nrow(ans$solution_detail)) {
                  ans$solution_detail[, `:=`(
                    geometry_id = "NACIONAL",
                    geometry_name = "Inventario nacional"
                  )]
                }

                if (nrow(ans$all_windows)) {
                  ans$all_windows[, `:=`(
                    geometry_id = "NACIONAL",
                    geometry_name = "Inventario nacional"
                  )]
                }

                ans$by_geometry <- data.table(
                  geometry_id = "NACIONAL",
                  geometry_name = "Inventario nacional",
                  n_eligible = ans$n_eligible,
                  n_solutions = nrow(
                    ans$solutions
                  ),
                  n_combinations = ans$n_combinations
                )

                incProgress(
                  0.90
                )

                return(
                  list(
                    solutions = ans$solutions,
                    detail = ans$solution_detail,
                    by_geometry = ans$by_geometry,
                    all_windows = ans$all_windows,
                    eligible_stations = ans$eligible_stations,
                    n_eligible = ans$n_eligible,
                    multi = FALSE,
                    min_years = min_years_snapshot,
                    min_start_year = start_year_snapshot,
                    threshold = threshold_snapshot,
                    require_nominal = nominal_snapshot
                  )
                )
              }

              # --------------------------------------------------------------
              # MODO MULTIGEOMETRÍA
              # --------------------------------------------------------------

              scopes <- unique(
                esp[
                  !is.na(geometry_id) &
                    nzchar(geometry_id),
                  .(
                    geometry_id,
                    geometry_name
                  )
                ]
              )

              setorder(
                scopes,
                geometry_name,
                geometry_id
              )

              sol_list <- vector(
                "list",
                nrow(scopes)
              )

              det_list <- vector(
                "list",
                nrow(scopes)
              )

              win_list <- vector(
                "list",
                nrow(scopes)
              )

              geo_list <- vector(
                "list",
                nrow(scopes)
              )

              for (i in seq_len(nrow(scopes))) {

                gid <- scopes$geometry_id[i]
                gname <- scopes$geometry_name[i]

                station_ids_i <- unique(
                  esp[
                    geometry_id == gid &
                      tiene_serie %in% TRUE,
                    station_id
                  ]
                )

                ans_i <- search_optimized_windows(
                  station_ids = station_ids_i,
                  tipo = tipo(),
                  min_years = min_years_snapshot,
                  threshold = threshold_snapshot,
                  require_nominal = nominal_snapshot,
                  min_start_year = start_year_snapshot
                )

                if (nrow(ans_i$solutions)) {

                  ans_i$solutions[, `:=`(
                    geometry_id = gid,
                    geometry_name = gname
                  )]

                  sol_list[[i]] <- ans_i$solutions
                }

                if (nrow(ans_i$solution_detail)) {

                  ans_i$solution_detail[, `:=`(
                    geometry_id = gid,
                    geometry_name = gname
                  )]

                  det_list[[i]] <- ans_i$solution_detail
                }

                if (nrow(ans_i$all_windows)) {

                  ans_i$all_windows[, `:=`(
                    geometry_id = gid,
                    geometry_name = gname
                  )]

                  win_list[[i]] <- ans_i$all_windows
                }

                geo_list[[i]] <- data.table(
                  geometry_id = gid,
                  geometry_name = gname,
                  n_eligible = ans_i$n_eligible,
                  n_solutions = nrow(
                    ans_i$solutions
                  ),
                  n_combinations = ans_i$n_combinations
                )

                incProgress(
                  0.90 /
                    max(
                      1,
                      nrow(scopes)
                    )
                )
              }

              solutions <- rbindlist(
                sol_list,
                use.names = TRUE,
                fill = TRUE
              )

              detail <- rbindlist(
                det_list,
                use.names = TRUE,
                fill = TRUE
              )

              all_windows <- rbindlist(
                win_list,
                use.names = TRUE,
                fill = TRUE
              )

              by_geometry <- rbindlist(
                geo_list,
                use.names = TRUE,
                fill = TRUE
              )

              if (nrow(solutions)) {
                setorder(
                  solutions,
                  geometry_name,
                  solution_order
                )
              }

              if (nrow(detail)) {
                setorder(
                  detail,
                  geometry_name,
                  solution_order,
                  -apta,
                  -completitud_obs_pct,
                  station_id
                )
              }

              list(
                solutions = solutions,
                detail = detail,
                by_geometry = by_geometry,
                all_windows = all_windows,
                eligible_stations = unique(
                  esp[
                    tiene_serie %in% TRUE,
                    station_id
                  ]
                ),
                n_eligible = sum(
                  by_geometry$n_eligible,
                  na.rm = TRUE
                ),
                multi = nrow(scopes) > 1,
                min_years = min_years_snapshot,
                min_start_year = start_year_snapshot,
                threshold = threshold_snapshot,
                require_nominal = nominal_snapshot
              )
            }
          )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )


      # ----------------------------------------------------------------------
      # MENSAJE / RESUMEN
      # ----------------------------------------------------------------------

      output$rw_message <- renderUI({

        r <- rw_result()

        if (!nrow(r$by_geometry)) {
          return(
            div(
              class = "alert alert-warning",
              "No hay estaciones disponibles para evaluar."
            )
          )
        }

        if (!nrow(r$solutions)) {
          return(
            div(
              class = "alert alert-warning",
              paste0(
                "No se encontró ninguna combinación con al menos ",
                r$min_years,
                " años, inicio ≥",
                r$min_start_year,
                " y completitud ≥",
                r$threshold,
                "%."
              )
            )
          )
        }

        n_geo <- nrow(
          r$by_geometry
        )

        n_geo_three <- sum(
          r$by_geometry$n_solutions >= 3,
          na.rm = TRUE
        )

        n_geo_any <- sum(
          r$by_geometry$n_solutions > 0,
          na.rm = TRUE
        )

        class_msg <- if (
          n_geo_three == n_geo
        ) {
          "alert alert-success"
        } else {
          "alert alert-info"
        }

        div(
          class = class_msg,

          tags$b(
            paste0(
              "Búsqueda completada: mínimo ",
              r$min_years,
              " años | inicio ≥",
              r$min_start_year,
              " | completitud ≥",
              r$threshold,
              "%."
            )
          ),

          br(),

          if (n_geo == 1) {
            paste0(
              r$by_geometry$n_solutions[1],
              " solución(es) distinta(s) obtenida(s) a partir de ",
              fmt_num(
                r$by_geometry$n_combinations[1]
              ),
              " combinaciones estaciones–longitud."
            )
          } else {
            paste0(
              n_geo_any,
              " de ",
              n_geo,
              " geometrías tienen soluciones; ",
              n_geo_three,
              " permiten mostrar las tres alternativas distintas."
            )
          },

          if (
            any(
              r$by_geometry$n_solutions < 3,
              na.rm = TRUE
            )
          ) {
            tags$small(
              paste(
                "Si un área muestra menos de tres soluciones, no existen tres",
                "combinaciones estaciones–longitud distintas bajo los criterios actuales."
              ),
              style = "display:block;margin-top:.35rem;"
            )
          }
        )
      })


      output$rw_cards <- renderUI({

        r <- rw_result()

        if (!nrow(r$solutions)) {
          return(NULL)
        }

        layout_columns(
          col_widths = breakpoints(
            sm = c(12, 12, 12, 12),
            md = c(6, 6, 6, 6),
            lg = c(3, 3, 3, 3)
          ),

          metric_card(
            "Longitud mínima",
            paste0(
              r$min_years,
              " años"
            ),
            paste0(
              "Inicio mínimo: ",
              r$min_start_year
            )
          ),

          metric_card(
            "Completitud",
            paste0(
              "≥",
              r$threshold,
              "%"
            ),
            if (isTRUE(r$require_nominal)) {
              "Con cobertura nominal exigida"
            } else {
              "Sin exigir cobertura nominal"
            }
          ),

          metric_card(
            "Geometrías con solución",
            paste0(
              sum(
                r$by_geometry$n_solutions > 0,
                na.rm = TRUE
              ),
              " / ",
              nrow(
                r$by_geometry
              )
            )
          ),

          metric_card(
            "Combinaciones evaluadas",
            fmt_num(
              sum(
                r$by_geometry$n_combinations,
                na.rm = TRUE
              )
            ),
            "Compromisos estaciones–longitud"
          )
        )
      })


      # ----------------------------------------------------------------------
      # SELECTOR DE ÁREA
      # ----------------------------------------------------------------------

      observe({

        r <- rw_result()

        if (!nrow(r$solutions)) {

          updateSelectizeInput(
            session,
            "rw_area",
            choices = character(),
            selected = character(),
            server = TRUE
          )

          return()
        }

        areas <- unique(
          r$solutions[
            ,
            .(
              geometry_id,
              geometry_name
            )
          ]
        )

        setorder(
          areas,
          geometry_name,
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
          input$rw_area
        )

        selected <- if (
          !is.null(current) &&
          current %in% areas$geometry_id
        ) {
          current
        } else {
          areas$geometry_id[1]
        }

        updateSelectizeInput(
          session,
          "rw_area",
          choices = choices,
          selected = selected,
          server = TRUE
        )
      })


      output$rw_area_ui <- renderUI({

        r <- rw_result()

        if (!nrow(r$solutions)) {
          return(NULL)
        }

        areas <- unique(
          r$solutions$geometry_id
        )

        if (length(areas) <= 1L) {
          return(NULL)
        }

        selectizeInput(
          session$ns("rw_area"),
          "Área a revisar",
          choices = character(),
          options = list(
            maxOptions = 100,
            closeAfterSelect = TRUE
          )
        )
      })


      rw_area_solutions <- reactive({

        r <- rw_result()

        req(
          nrow(r$solutions) > 0
        )

        gid <- input$rw_area

        if (
          is.null(gid) ||
          !nzchar(gid) ||
          !(gid %in% r$solutions$geometry_id)
        ) {
          gid <- r$solutions$geometry_id[1]
        }

        x <- copy(
          r$solutions[
            geometry_id == gid
          ]
        )

        setorder(
          x,
          solution_order
        )

        x
      })


      # ----------------------------------------------------------------------
      # TRES SOLUCIONES
      # ----------------------------------------------------------------------

      output$rw_solutions <- renderDT({

        x <- rw_area_solutions()

        if (!nrow(x)) {
          return(
            datatable(
              data.frame(
                Mensaje = "Sin soluciones para el área seleccionada."
              ),
              rownames = FALSE
            )
          )
        }

        tab <- x[
          ,
          .(
            Solución = solution_name,
            Periodo = paste0(
              anio_inicio,
              "–",
              anio_fin
            ),
            Años = n_years,
            Estaciones = paste0(
              n_cumplen,
              " / ",
              n_elegibles
            ),
            `Cobertura (%)` = round(
              pct_cumplen,
              2
            ),
            `Comp. mediana (%)` = round(
              completitud_mediana_aptas,
              2
            )
          )
        ]

        opts <- dt_opts(5)
        opts$scrollX <- FALSE
        opts$autoWidth <- FALSE
        opts$searching <- FALSE
        opts$paging <- FALSE
        opts$info <- FALSE

        datatable(
          tab,
          selection = "single",
          extensions = "Buttons",
          options = opts,
          rownames = FALSE,
          class = "stripe hover compact",
          width = "100%"
        )
      }, server = FALSE)


      output$rw_curve_caption <- renderUI({

        r <- rw_result()
        x <- rw_area_curve_data()

        if (!isTRUE(x$ok)) {
          return(NULL)
        }

        tags$small(
          paste0(
            r$min_years,
            " años | completitud ≥",
            r$threshold,
            "%",
            if (isTRUE(r$require_nominal)) " | cobertura nominal exigida" else "",
            " | línea discontinua = todas las elegibles"
          ),
          style = "color:#6c757d;display:block;margin-bottom:.5rem;"
        )
      })


      rw_area_curve_data <- reactive({

        r <- rw_result()
        s <- rw_area_solutions()

        req(nrow(s) > 0)

        gid <- s$geometry_id[1]

        aw <- copy(
          r$all_windows[
            geometry_id == gid &
              n_years >= r$min_years &
              n_cumplen > 0
          ]
        )

        if (!nrow(aw)) {
          return(list(
            ok = FALSE,
            message = "Sin ventanas para construir la curva."
          ))
        }

        setorder(
          aw,
          anio_inicio,
          -n_cumplen,
          -n_years,
          -completitud_mediana_aptas,
          -anio_fin
        )

        curve <- aw[, .SD[1], by = anio_inicio]

        sol_pts <- copy(s)[, .(
          solution_name,
          anio_inicio,
          n_cumplen
        )]

        list(
          ok = TRUE,
          curve = curve,
          solutions = sol_pts,
          n_eligible = unique(curve$n_elegibles)[1]
        )
      })


      output$rw_curve_plot <- renderPlot({

        r <- rw_area_curve_data()

        shiny::validate(
          shiny::need(
            isTRUE(r$ok),
            r$message
          )
        )

        curve <- r$curve
        sol <- r$solutions

        ggplot(
          curve,
          aes(
            x = anio_inicio,
            y = n_cumplen
          )
        ) +
          geom_hline(
            yintercept = r$n_eligible,
            linetype = "dashed",
            linewidth = 0.45,
            color = "black"
          ) +
          geom_line(
            linewidth = 0.55,
            color = "black"
          ) +
          geom_point(
            size = 1.8,
            color = "black"
          ) +
          geom_point(
            data = sol,
            aes(
              x = anio_inicio,
              y = n_cumplen,
              color = solution_name
            ),
            size = 3.2,
            inherit.aes = FALSE
          ) +
          scale_color_manual(
            values = c(
              "Máxima cobertura" = "#1F5F73",
              "Equilibrada" = "#C97A2B",
              "Máxima longitud" = "#2C7A59"
            ),
            drop = FALSE,
            name = "Soluciones elegidas"
          ) +
          scale_x_continuous(
            breaks = pretty(curve$anio_inicio, n = 10)
          ) +
          scale_y_continuous(
            breaks = pretty(c(0, curve$n_cumplen, r$n_eligible), n = 8),
            expand = expansion(mult = c(0.02, 0.04))
          ) +
          labs(
            x = "Año inicial de la ventana",
            y = "Estaciones que cumplen",
            title = "Relación entre año inicial y estaciones aptas",
            subtitle = "Para cada año inicial se muestra la mejor ventana posible bajo los criterios actuales"
          ) +
          theme_minimal(base_size = 11) +
          theme(
            panel.grid.minor = element_blank(),
            legend.position = "none"
          )
      })


      rw_active_solution <- reactive({

        x <- rw_area_solutions()

        req(
          nrow(x) > 0
        )

        sel <- input$rw_solutions_rows_selected

        row_i <- if (
          !length(sel) ||
          sel[1] > nrow(x)
        ) {
          1L
        } else {
          sel[1]
        }

        x[
          row_i
        ]
      })


      rw_active_detail_all <- reactive({

        r <- rw_result()
        s <- rw_active_solution()

        req(
          nrow(s) == 1
        )

        copy(
          r$detail[
            geometry_id == s$geometry_id[1] &
              solution_id == s$solution_id[1]
          ]
        )
      })


      rw_active_detail <- reactive({

        x <- rw_active_detail_all()

        x[
          apta %in% TRUE
        ]
      })


      output$rw_solution_cards <- renderUI({

        s <- rw_active_solution()

        if (!nrow(s)) {
          return(NULL)
        }

        layout_columns(
          col_widths = breakpoints(
            sm = c(12, 12, 12, 12),
            md = c(6, 6, 6, 6),
            lg = c(3, 3, 3, 3)
          ),

          metric_card(
            "Solución",
            s$solution_name,
            paste0(
              s$geometry_id,
              " · ",
              s$geometry_name
            )
          ),

          metric_card(
            "Periodo",
            paste0(
              s$anio_inicio,
              "–",
              s$anio_fin
            ),
            paste0(
              s$n_years,
              " años consecutivos"
            )
          ),

          metric_card(
            "Estaciones",
            paste0(
              s$n_cumplen,
              " / ",
              s$n_elegibles
            ),
            fmt_pct(
              s$pct_cumplen,
              1
            )
          ),

          metric_card(
            "Completitud mediana",
            fmt_pct(
              s$completitud_mediana_aptas,
              1
            ),
            "Entre las estaciones que cumplen"
          )
        )
      })


      # ----------------------------------------------------------------------
      # CALENDARIO DE LA SOLUCIÓN SELECCIONADA
      # ----------------------------------------------------------------------

      rw_calendar_data <- reactive({

        s <- rw_active_solution()
        d <- rw_active_detail()

        req(
          nrow(s) == 1
        )

        if (!nrow(d)) {
          return(
            list(
              ok = FALSE,
              message = "La solución seleccionada no contiene estaciones aptas."
            )
          )
        }

        n_show <- suppressWarnings(
          as.integer(
            input$rw_calendar_n
          )
        )

        if (
          is.na(n_show) ||
          !(n_show %in% c(
            10L,
            20L,
            30L,
            40L,
            50L
          ))
        ) {
          n_show <- 30L
        }

        n_show <- min(
          50L,
          n_show
        )

        setorder(
          d,
          -completitud_obs_pct,
          nombre_estacion,
          station_id
        )

        n_total <- nrow(
          d
        )

        shown <- d[
          seq_len(
            min(
              n_show,
              .N
            )
          )
        ]

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

        years <- seq.int(
          as.integer(
            s$anio_inicio
          ),
          as.integer(
            s$anio_fin
          )
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
            station_label,
            station_order,
            expected_obs_day,
            primera_fecha,
            ultima_fecha
          )
        ]

        grid <- merge(
          grid,
          meta,
          by = "series_id",
          all.x = TRUE,
          sort = FALSE
        )

        obs <- YEAR_OBS[
          series_id %chin% shown$series_id &
            anio >= s$anio_inicio &
            anio <= s$anio_fin,
          .(
            series_id,
            anio,
            n_obs_validas
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

        grid[, dentro_registro := (
          !is.na(primera_fecha) &
            !is.na(ultima_fecha) &
            ultima_fecha >= year_start &
            primera_fecha <= year_end
        )]

        grid[
          dentro_registro %in% TRUE &
            is.na(n_obs_validas),
          n_obs_validas := 0
        ]

        grid[, dias_calendario := fifelse(
          leap_year(anio),
          366L,
          365L
        )]

        grid[, obs_esperadas := (
          dias_calendario *
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

        grid[
          !is.na(completitud_anual_pct),
          completitud_anual_pct := pmin(
            100,
            completitud_anual_pct
          )
        ]

        threshold_snapshot <- rw_result()$threshold

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

        list(
          ok = TRUE,
          data = grid,
          n_total = n_total,
          n_shown = nrow(
            shown
          ),
          threshold_label = threshold_label,
          below_label = below_label,
          solution = s
        )
      })


      output$rw_calendar_info <- renderUI({

        r <- rw_calendar_data()

        if (!isTRUE(r$ok)) {
          return(
            div(
              class = "alert alert-warning mb-0",
              r$message
            )
          )
        }

        limited <- r$n_total > r$n_shown

        div(
          class = if (limited) {
            "alert alert-warning mb-0"
          } else {
            "alert alert-light border mb-0"
          },

          if (limited) {
            paste0(
              "Se muestran ",
              r$n_shown,
              " de ",
              r$n_total,
              " estaciones por legibilidad."
            )
          } else {
            paste0(
              "Se muestran las ",
              r$n_shown,
              " estaciones de la solución."
            )
          }
        )
      })


      output$rw_calendar_plot <- renderPlot({

        r <- rw_calendar_data()

        shiny::validate(
          shiny::need(
            isTRUE(r$ok),
            r$message
          )
        )

        x <- r$data
        s <- r$solution

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
              s$anio_inicio - 0.5,
              s$anio_fin + 0.5
            ),
            breaks = pretty(
              c(
                s$anio_inicio,
                s$anio_fin
              ),
              n = 12
            ),
            expand = c(
              0,
              0
            )
          ) +
          labs(
            x = "Año",
            y = NULL,
            title = paste0(
              s$solution_name,
              " — ",
              s$anio_inicio,
              "–",
              s$anio_fin
            ),
            subtitle = paste0(
              s$n_cumplen,
              " estaciones | ",
              s$n_years,
              " años | completitud de ventana ≥",
              rw_result()$threshold,
              "%"
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
            legend.position = "bottom"
          )
      })


      # ----------------------------------------------------------------------
      # DETALLE SIMPLIFICADO
      # ----------------------------------------------------------------------

      output$rw_detail <- renderDT({

        x <- rw_active_detail()

        if (!nrow(x)) {
          return(
            datatable(
              data.frame(
                Mensaje = "Sin estaciones para la solución seleccionada."
              ),
              rownames = FALSE
            )
          )
        }

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
          "Serie"
        )]

        setorder(
          x,
          -completitud_obs_pct,
          nombre_estacion
        )

        tab <- x[
          ,
          .(
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
            `Completitud (%)` = round(
              completitud_obs_pct,
              2
            ),
            `Cubre ventana` = fifelse(
              cubre_nominalmente %in% TRUE,
              "Sí",
              "No"
            )
          )
        ]

        opts <- dt_opts(30)
        opts$scrollX <- FALSE
        opts$autoWidth <- FALSE

        datatable(
          tab,
          filter = "top",
          extensions = "Buttons",
          options = opts,
          rownames = FALSE,
          class = "stripe hover compact",
          width = "100%"
        )
      }, server = FALSE)


      # ----------------------------------------------------------------------
      # DESCARGA MASIVA DE LOS XLSX RAW DE LA SOLUCIÓN
      # ----------------------------------------------------------------------

      rw_download_status <- reactive({

        x <- rw_active_detail()

        if (!nrow(x)) {
          return(
            list(
              n_total = 0L,
              n_available = 0L
            )
          )
        }

        urls <- vapply(
          x$id_config,
          raw_report_url,
          character(1)
        )

        available <- !is.na(urls) &
          nzchar(urls)

        list(
          n_total = nrow(x),
          n_available = sum(
            available
          )
        )
      })


      output$rw_download_status_text <- renderUI({

        st <- rw_download_status()

        if (!st$n_total) {
          return(NULL)
        }

        tags$small(
          paste0(
            st$n_available,
            " de ",
            st$n_total,
            " estaciones tienen reporte RAW archivado. ",
            "La descarga normalizada usa los RDS preparados por IDConfig."
          ),
          class = "rw-detail-status"
        )
      })


      output$rw_download_buttons <- renderUI({

        st <- rw_download_status()

        if (!st$n_total) {
          return(NULL)
        }

        div(
          class = "rw-detail-actions",
          downloadButton(
            session$ns("rw_download_bulk"),
            "Descargar XLSX originales (.zip)",
            class = "btn-outline-primary"
          ),
          actionButton(
            session$ns("rw_send_normalized"),
            "Descargar normalizadas",
            class = "btn-primary"
          )
        )
      })


      output$rw_download_bulk <- downloadHandler(

        filename = function() {

          s <- rw_active_solution()

          safe_area <- gsub(
            "[^A-Za-z0-9_-]+",
            "_",
            as.character(
              s$geometry_name
            )
          )

          safe_sol <- gsub(
            "[^A-Za-z0-9_-]+",
            "_",
            iconv(
              s$solution_name,
              to = "ASCII//TRANSLIT"
            )
          )

          paste0(
            "ANA_SNIRH_",
            safe_area,
            "_",
            safe_sol,
            "_",
            s$anio_inicio,
            "_",
            s$anio_fin,
            ".zip"
          )
        },

        content = function(file) {

          x <- copy(
            rw_active_detail()
          )

          req(
            nrow(x) > 0
          )

          tmpdir <- tempfile(
            "rw_bulk_"
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
            message = "Preparando descarga masiva...",
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
                    sprintf(
                      "%03d",
                      i
                    ),
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
                  1 /
                    nrow(x)
                )
              }
            }
          )

          manifest_path <- file.path(
            tmpdir,
            "manifest.csv"
          )

          fwrite(
            manifest,
            manifest_path,
            bom = TRUE
          )

          files_zip <- c(
            downloaded,
            "manifest.csv"
          )

          zip::zipr(
            zipfile = file,
            files = files_zip,
            root = tmpdir
          )
        }
      )


      # ----------------------------------------------------------------------
      # ENVIAR SOLUCIÓN ACTIVA A DESCARGA NORMALIZADA
      # ----------------------------------------------------------------------

      normalized_request <- eventReactive(
        input$rw_send_normalized,
        {

          s <- rw_active_solution()
          x <- copy(
            rw_active_detail()
          )

          if (
            !nrow(s) ||
            !nrow(x)
          ) {
            return(
              list(
                ok = FALSE,
                message = "La solución seleccionada no contiene estaciones aptas."
              )
            )
          }

          selected <- unique(
            x[
              ,
              .(
                series_id,
                geometry_id = as.character(
                  s$geometry_id[1]
                ),
                geometry_name = as.character(
                  s$geometry_name[1]
                )
              )
            ]
          )

          list(
            ok = TRUE,
            request_id = paste0(
              "RW_",
              as.numeric(
                Sys.time()
              )
            ),
            source = paste0(
              "Ventana común — ",
              s$solution_name[1]
            ),
            source_detail = paste0(
              s$geometry_id[1],
              " · ",
              s$geometry_name[1],
              " | ",
              s$anio_inicio[1],
              "–",
              s$anio_fin[1],
              " | ",
              nrow(selected),
              " estaciones"
            ),
            tipo = tipo(),
            period_start = as.IDate(
              s$fecha_inicio[1]
            ),
            period_end = as.IDate(
              s$fecha_fin[1]
            ),
            stations = selected
          )
        },
        ignoreInit = TRUE
      )


      list(
        result = rw_result,
        active_solution = rw_active_solution,
        selected_stations = rw_active_detail,
        normalized_request = normalized_request
      )
    }
  )
}

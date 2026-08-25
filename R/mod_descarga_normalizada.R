# ============================================================================
# MÓDULO — DESCARGA NORMALIZADA POR PERIODO
# v1.1.1
#
# Recibe selecciones desde:
#   - Estaciones candidatas
#   - Ventana común optimizada
#
# La revisión temporal usa DAY/SERIES ya cargados en memoria.
# Los RDS largos se descargan desde GitHub SOLO al generar el Excel.
# ============================================================================

mod_descarga_normalizada_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),

    uiOutput(
      ns("request_banner")
    ),

    layout_columns(
      col_widths = breakpoints(
        sm = c(12, 12, 12),
        md = c(6, 6, 12),
        lg = c(4, 4, 4)
      ),

      div(
        class = "card rw-download-compact-card",
        style = "align-self:start;width:100%;height:auto;min-height:0;",

        div(
          class = "card-header",
          "Periodo de exportación"
        ),

        div(
          class = "card-body rw-download-compact-body",

          dateRangeInput(
            ns("export_period"),
            NULL,
            start = as.Date(FDEF0),
            end = as.Date(FDEF1),
            min = as.Date(FMIN),
            max = as.Date(FMAX),
            format = "yyyy-mm-dd",
            separator = " a "
          ),

          tags$small(
            "Modificar el periodo no elimina la selección de estaciones.",
            style = "color:#6c757d;display:block;margin-top:.2rem;"
          )
        )
      ),

      div(
        class = "card rw-download-compact-card",
        style = "align-self:start;width:100%;height:auto;min-height:0;",

        div(
          class = "card-header",
          "Selección"
        ),

        div(
          class = "card-body rw-download-compact-body",

          div(
            style = "display:flex;flex-direction:column;gap:.55rem;",

            actionButton(
              ns("select_all"),
              "Incluir todas",
              class = "btn-outline-primary",
              width = "100%"
            ),

            actionButton(
              ns("exclude_subdaily"),
              "Excluir no compatibles",
              class = "btn-outline-secondary",
              width = "100%"
            )
          ),

          tags$small(
            "Las filas resaltadas de la tabla serán exportadas.",
            style = "color:#6c757d;display:block;margin-top:.55rem;"
          )
        )
      ),

      div(
        class = "card rw-download-compact-card",
        style = "align-self:start;width:100%;height:auto;min-height:0;",

        div(
          class = "card-header",
          "Salida"
        ),

        div(
          class = "card-body rw-download-compact-body",

          tags$b(
            "Excel normalizado",
            style = "display:block;margin-bottom:.35rem;"
          ),

          tags$small(
            "Hoja Datos: formato ancho sobre un único calendario diario.",
            style = "color:#6c757d;display:block;margin-bottom:.25rem;"
          ),

          tags$small(
            "Hoja Metadata: trazabilidad de estación, IDConfig, cobertura y fuentes.",
            style = "color:#6c757d;display:block;margin-bottom:.7rem;"
          ),

          actionButton(
            ns("prepare_excel"),
            "Preparar Excel",
            class = "btn-primary",
            width = "100%"
          ),

          uiOutput(
            ns("prepare_status")
          ),

          uiOutput(
            ns("prepared_download_ui")
          )
        )
      )
    ),

    tags$style(
      HTML(
        paste0(
          "#", ns("export_period"), " {margin-bottom:0 !important;}",
          ".rw-download-compact-card {width:100% !important;height:auto !important;min-height:0 !important;}",
          ".rw-download-compact-body {padding-bottom:.85rem !important;}"
        )
      )
    ),

    br(),

    uiOutput(
      ns("summary_cards")
    ),

    br(),

    card(
      full_screen = TRUE,
      card_header("Revisión de estaciones"),

      tags$small(
        paste(
          "Seleccione o deseleccione filas para decidir qué estaciones entrarán al archivo.",
          "La precipitación de 12 h usa P(D, 19:00) + P(D + 1, 07:00); otras frecuencias subdiarias se excluyen."
        ),
        style = "color:#6c757d;"
      ),

      br(),

      tags$style(
        HTML(
          paste0(
            "#", ns("review_table"), " {width:100% !important;}",
            "#", ns("review_table"), " .dataTables_wrapper {width:100% !important;}",
            "#", ns("review_table"), " table.dataTable {width:100% !important;}"
          )
        )
      ),

      DTOutput(
        ns("review_table")
      )
    )
  )
}


mod_descarga_normalizada_server <- function(
  id,
  request
) {

  moduleServer(
    id,
    function(input, output, session) {

      NORMALIZED_BASE_URL <- paste0(
        "https://raw.githubusercontent.com/",
        "JamilRamirez/ANA-SNIRH-Normalized-Series/",
        "main/series_long"
      )

      included_ids <- reactiveVal(
        character()
      )

      request_key <- reactiveVal(
        NULL
      )


      # ----------------------------------------------------------------------
      # HELPERS
      # ----------------------------------------------------------------------

      safe_data_column <- function(
        station,
        code,
        id_config
      ) {

        station <- iconv(
          as.character(
            station
          ),
          from = "UTF-8",
          to = "ASCII//TRANSLIT"
        )

        missing_station <- is.na(
          station
        ) |
          station == ""

        station[
          missing_station
        ] <- "Estacion"

        station <- gsub(
          "[^A-Za-z0-9]+",
          "_",
          station
        )

        station <- gsub(
          "^_+|_+$",
          "",
          station
        )

        code <- as.character(
          code
        )

        missing_code <- is.na(
          code
        ) |
          code == ""

        code[
          missing_code
        ] <- paste0(
          "IDConfig",
          id_config[
            missing_code
          ]
        )

        out <- paste0(
          station,
          "_",
          code
        )

        make.unique(
          out,
          sep = "_"
        )
      }


      normalized_series_url <- function(
        carpeta_cuenca,
        id_config
      ) {

        if (
          !length(
            carpeta_cuenca
          ) ||
          is.na(
            carpeta_cuenca
          ) ||
          !nzchar(
            carpeta_cuenca
          ) ||
          !length(
            id_config
          ) ||
          is.na(
            id_config
          ) ||
          !nzchar(
            as.character(
              id_config
            )
          )
        ) {
          return(
            NA_character_
          )
        }

        paste0(
          NORMALIZED_BASE_URL,
          "/",
          encode_path_segments(
            as.character(
              carpeta_cuenca
            )
          ),
          "/IDConfig_",
          as.character(
            id_config
          ),
          ".rds"
        )
      }


      download_normalized_rds <- function(
        url
      ) {

        tmp <- tempfile(
          fileext = ".rds"
        )

        on.exit(
          unlink(
            tmp,
            force = TRUE
          ),
          add = TRUE
        )

        old_timeout <- getOption(
          "timeout"
        )

        on.exit(
          options(
            timeout = old_timeout
          ),
          add = TRUE
        )

        options(
          timeout = max(
            120,
            old_timeout
          )
        )

        utils::download.file(
          url,
          destfile = tmp,
          mode = "wb",
          quiet = TRUE
        )

        if (
          !file.exists(
            tmp
          ) ||
          file.info(
            tmp
          )$size <= 0
        ) {
          stop(
            "El RDS descargado está vacío."
          )
        }

        as.data.table(
          readRDS(
            tmp
          )
        )
      }


      # ----------------------------------------------------------------------
      # SOLICITUD / METADATA BASE
      # ----------------------------------------------------------------------

      base_selection <- reactive({

        req_norm <- request()

        if (
          is.null(
            req_norm
          ) ||
          !isTRUE(
            req_norm$ok
          ) ||
          is.null(
            req_norm$stations
          ) ||
          !nrow(
            req_norm$stations
          )
        ) {
          return(
            data.table()
          )
        }

        req_st <- as.data.table(
          copy(
            req_norm$stations
          )
        )

        req_st[, series_id := as.character(
          series_id
        )]

        req_st <- req_st[
          !is.na(
            series_id
          ) &
            nzchar(
              series_id
            )
        ]

        if (!nrow(req_st)) {
          return(
            data.table()
          )
        }

        if (!"geometry_id" %in% names(req_st)) {
          req_st[, geometry_id := "NACIONAL"]
        }

        if (!"geometry_name" %in% names(req_st)) {
          req_st[, geometry_name := "Inventario nacional"]
        }

        # Colapsar geometrías si la misma serie llegó desde varios buffers.
        req_geo <- req_st[
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

        meta <- SERIES[
          series_id %chin% req_geo$series_id,
          .(
            series_id,
            station_id,
            id_config,
            codigo_estacion,
            nombre_estacion,
            tipo_dato,
            variable,
            unidad,
            expected_obs_day,
            primera_fecha,
            ultima_fecha,
            latitud,
            longitud,
            altitud_msnm,
            carpeta_cuenca,
            unidad_hidrografica,
            aaa,
            ala
          )
        ]

        meta <- merge(
          meta,
          req_geo,
          by = "series_id",
          all.x = TRUE,
          sort = FALSE
        )

        meta[, normalized_url := vapply(
          seq_len(.N),
          function(i) {
            normalized_series_url(
              carpeta_cuenca[i],
              id_config[i]
            )
          },
          character(1)
        )]

        setorder(
          meta,
          nombre_estacion,
          codigo_estacion,
          id_config
        )

        meta
      })


      # Nueva solicitud: reiniciar selección y precargar periodo.
      observeEvent(
        request(),
        {

          req_norm <- request()

          if (
            is.null(
              req_norm
            ) ||
            !isTRUE(
              req_norm$ok
            )
          ) {
            return()
          }

          key <- as.character(
            req_norm$request_id
          )

          if (
            length(
              key
            ) &&
            !is.na(
              key
            ) &&
            identical(
              key,
              request_key()
            )
          ) {
            return()
          }

          request_key(
            key
          )

          x <- base_selection()

          included_ids(
            unique(
              x$series_id
            )
          )

          updateDateRangeInput(
            session,
            "export_period",
            start = as.Date(
              req_norm$period_start
            ),
            end = as.Date(
              req_norm$period_end
            ),
            min = as.Date(
              FMIN
            ),
            max = as.Date(
              FMAX
            )
          )
        },
        ignoreInit = FALSE
      )


      # ----------------------------------------------------------------------
      # EVALUACIÓN DEL PERIODO ACTUAL
      # ----------------------------------------------------------------------

      review_data <- reactive({

        base <- copy(
          base_selection()
        )

        if (!nrow(base)) {
          return(
            data.table()
          )
        }

        req(
          input$export_period
        )

        f0 <- as.IDate(
          input$export_period[1]
        )

        f1 <- as.IDate(
          input$export_period[2]
        )

        if (
          is.na(
            f0
          ) ||
          is.na(
            f1
          ) ||
          f0 > f1
        ) {
          return(
            data.table()
          )
        }

        ev <- evaluate_series(
          base$series_id,
          f0,
          f1
        )

        ev <- ev[
          ,
          .(
            series_id,
            completitud_obs_pct,
            cobertura_dias_pct,
            cubre_nominalmente,
            n_dias_alguna,
            dias_esperados
          )
        ]

        out <- merge(
          base,
          ev,
          by = "series_id",
          all.x = TRUE,
          sort = FALSE
        )

        out[, frecuencia := fcase(
          expected_obs_day == 1L,
          "1 observación/día",
          expected_obs_day == 2L,
          "2 observaciones/día (12 h)",
          !is.na(expected_obs_day),
          paste0(
            expected_obs_day,
            " observaciones/día"
          ),
          default = "Frecuencia no identificada"
        )]

        # Compatibles con la salida diaria:
        #   - 1 obs/día: se exporta directamente;
        #   - precipitación de 12 h: usa el día pluviométrico ANA.
        out[, compatible_diaria := (
          !is.na(
            expected_obs_day
          ) &
            (
              expected_obs_day == 1L |
                is_ana_precip_12h(
                  tipo_dato,
                  expected_obs_day
                )
            )
        )]

        out[, requiere_agregacion_12h := is_ana_precip_12h(
          tipo_dato,
          expected_obs_day
        )]

        out[, estado_exportacion := fcase(
          !compatible_diaria,
          "Subdiaria no compatible: excluir",
          is.na(
            normalized_url
          ) |
            !nzchar(
              normalized_url
            ),
          "Sin ruta RDS",
          !cubre_nominalmente,
          "Cobertura parcial",
          completitud_obs_pct < 100,
          "Con vacíos",
          requiere_agregacion_12h,
          "12 h → día ANA",
          default = "Completa"
        )]

        out[, disponible_txt := paste0(
          fifelse(
            is.na(
              primera_fecha
            ),
            "—",
            as.character(
              primera_fecha
            )
          ),
          " → ",
          fifelse(
            is.na(
              ultima_fecha
            ),
            "—",
            as.character(
              ultima_fecha
            )
          )
        )]

        setorder(
          out,
          nombre_estacion,
          codigo_estacion,
          id_config
        )

        out
      })


      # ----------------------------------------------------------------------
      # SELECCIÓN INDIVIDUAL
      # ----------------------------------------------------------------------

      output$review_table <- renderDT({

        x <- review_data()

        if (!nrow(x)) {
          return(
            datatable(
              data.frame(
                Mensaje = "Reciba una selección desde Candidatas o Ventana común."
              ),
              rownames = FALSE
            )
          )
        }

        selected_idx <- which(
          x$series_id %chin%
            included_ids()
        )

        tab <- x[
          ,
          .(
            Estación = nombre_estacion,
            Código = codigo_estacion,
            IDConfig = id_config,
            Variable = paste0(
              variable,
              fifelse(
                !is.na(
                  unidad
                ) &
                  nzchar(
                    unidad
                  ),
                paste0(
                  " (",
                  unidad,
                  ")"
                ),
                ""
              )
            ),
            Frecuencia = frecuencia,
            Disponible = disponible_txt,
            `Completitud (%)` = round(
              completitud_obs_pct,
              2
            ),
            `Cubre periodo` = fifelse(
              cubre_nominalmente %in% TRUE,
              "Sí",
              "No"
            ),
            Área = geometry_name,
            Estado = estado_exportacion
          )
        ]

        opts <- dt_opts(
          25
        )

        opts$scrollX <- FALSE
        opts$autoWidth <- FALSE

        datatable(
          tab,
          selection = list(
            mode = "multiple",
            selected = selected_idx,
            target = "row"
          ),
          filter = "top",
          extensions = "Buttons",
          options = opts,
          rownames = FALSE,
          class = "stripe hover compact",
          width = "100%"
        )
      }, server = FALSE)


      observeEvent(
        input$review_table_rows_selected,
        {

          x <- review_data()

          if (!nrow(x)) {
            included_ids(
              character()
            )
            return()
          }

          idx <- input$review_table_rows_selected

          idx <- idx[
            idx >= 1L &
              idx <= nrow(
                x
              )
          ]

          included_ids(
            unique(
              x$series_id[
                idx
              ]
            )
          )
        },
        ignoreInit = TRUE
      )


      proxy <- dataTableProxy(
        "review_table",
        session = session
      )


      observeEvent(
        input$select_all,
        {

          x <- review_data()

          if (!nrow(x)) {
            return()
          }

          included_ids(
            x$series_id
          )

          selectRows(
            proxy,
            seq_len(
              nrow(
                x
              )
            )
          )
        }
      )


      observeEvent(
        input$exclude_subdaily,
        {

          x <- review_data()

          if (!nrow(x)) {
            return()
          }

          keep <- which(
            x$compatible_diaria %in% TRUE
          )

          included_ids(
            x$series_id[
              keep
            ]
          )

          selectRows(
            proxy,
            keep
          )
        }
      )


      selected_review <- reactive({

        x <- review_data()

        if (!nrow(x)) {
          return(
            data.table()
          )
        }

        x[
          series_id %chin%
            included_ids()
        ]
      })


      # ----------------------------------------------------------------------
      # MENSAJES Y RESUMEN
      # ----------------------------------------------------------------------

      output$request_banner <- renderUI({

        req_norm <- request()

        if (
          is.null(
            req_norm
          ) ||
          !isTRUE(
            req_norm$ok
          )
        ) {
          return(
            div(
              class = "alert alert-secondary",
              tags$b("Sin selección recibida. "),
              paste(
                "Use «Descargar normalizadas» desde Estaciones candidatas",
                "o desde una solución de Ventana común."
              )
            )
          )
        }

        x <- selected_review()

        n_sel <- nrow(
          x
        )

        f0 <- if (
          !is.null(
            input$export_period
          )
        ) {
          as.character(
            input$export_period[1]
          )
        } else {
          as.character(
            req_norm$period_start
          )
        }

        f1 <- if (
          !is.null(
            input$export_period
          )
        ) {
          as.character(
            input$export_period[2]
          )
        } else {
          as.character(
            req_norm$period_end
          )
        }

        n_partial <- if (n_sel) {
          sum(
            !x$cubre_nominalmente |
              x$completitud_obs_pct < 100,
            na.rm = TRUE
          )
        } else {
          0L
        }

        n_incompatible <- if (n_sel) {
          sum(
            !x$compatible_diaria,
            na.rm = TRUE
          )
        } else {
          0L
        }

        n_12h <- if (n_sel) {
          sum(
            x$requiere_agregacion_12h %in% TRUE,
            na.rm = TRUE
          )
        } else {
          0L
        }

        div(
          class = if (
            n_partial > 0 ||
            n_incompatible > 0
          ) {
            "alert alert-warning"
          } else {
            "alert alert-success"
          },

          tags$b(
            paste0(
              req_norm$source,
              " — ",
              n_sel,
              " estación(es) seleccionada(s)"
            )
          ),

          br(),

          paste0(
            "Estás descargando ",
            n_sel,
            " estaciones para el periodo ",
            f0,
            " a ",
            f1,
            " en formato ancho."
          ),

          if (
            n_partial > 0
          ) {
            tags$small(
              paste0(
                n_partial,
                " estación(es) no cubren completamente el periodo o contienen vacíos."
              ),
              style = "display:block;margin-top:.35rem;"
            )
          },

          if (
            n_incompatible > 0
          ) {
            tags$small(
              paste0(
                n_incompatible,
                " serie(s) tienen frecuencia subdiaria no compatible y deben excluirse."
              ),
              style = "display:block;margin-top:.2rem;"
            )
          },

          if (
            n_12h > 0
          ) {
            tags$small(
              paste0(
                n_12h,
                " serie(s) de precipitación de 12 h usarán P(D, 19:00) + P(D + 1, 07:00)."
              ),
              style = "display:block;margin-top:.2rem;"
            )
          },

          tags$small(
            req_norm$source_detail,
            style = "display:block;margin-top:.35rem;color:#6c757d;"
          )
        )
      })


      output$summary_cards <- renderUI({

        x <- selected_review()

        if (!nrow(base_selection())) {
          return(NULL)
        }

        n_sel <- nrow(
          x
        )

        n_nominal <- if (n_sel) {
          sum(
            x$cubre_nominalmente %in% TRUE,
            na.rm = TRUE
          )
        } else {
          0L
        }

        n_complete <- if (n_sel) {
          sum(
            x$cubre_nominalmente %in% TRUE &
              x$completitud_obs_pct >= 99.999,
            na.rm = TRUE
          )
        } else {
          0L
        }

        n_12h <- if (n_sel) {
          sum(
            x$requiere_agregacion_12h %in% TRUE,
            na.rm = TRUE
          )
        } else {
          0L
        }

        layout_columns(
          col_widths = breakpoints(
            sm = c(12, 12, 12, 12),
            md = c(6, 6, 6, 6),
            lg = c(3, 3, 3, 3)
          ),

          metric_card(
            "Incluidas",
            fmt_num(
              n_sel
            ),
            paste0(
              fmt_num(
                nrow(
                  base_selection()
                )
              ),
              " recibidas del origen"
            )
          ),

          metric_card(
            "Cubren periodo",
            fmt_num(
              n_nominal
            ),
            "Cobertura nominal completa"
          ),

          metric_card(
            "100% completas",
            fmt_num(
              n_complete
            ),
            "Sin observaciones faltantes"
          ),

          metric_card(
            "12 h → diario",
            fmt_num(
              n_12h
            ),
            if (n_12h) {
              "19:00 de D + 07:00 de D+1"
            } else {
              "Sin conversión necesaria"
            }
          )
        )
      })


      # ----------------------------------------------------------------------
      # EXCEL FINAL — PREPARACIÓN SEPARADA DE LA DESCARGA
      #
      # El trabajo pesado (descargar RDS + ensamblar + escribir XLSX) ocurre
      # al pulsar "Preparar Excel". El navegador solo inicia una descarga
      # cuando el archivo ya existe y ha sido validado como XLSX.
      # ----------------------------------------------------------------------

      download_status <- reactive({

        x <- selected_review()

        list(
          n = nrow(
            x
          ),
          n_incompatible = if (nrow(x)) {
            sum(
              !x$compatible_diaria,
              na.rm = TRUE
            )
          } else {
            0L
          },
          n_12h = if (nrow(x)) {
            sum(
              x$requiere_agregacion_12h %in% TRUE,
              na.rm = TRUE
            )
          } else {
            0L
          },
          n_without_url = if (nrow(x)) {
            sum(
              is.na(
                x$normalized_url
              ) |
                !nzchar(
                  x$normalized_url
                ),
              na.rm = TRUE
            )
          } else {
            0L
          }
        )
      })


      prepared_state <- reactiveVal(
        list(
          status = "idle",
          path = NULL,
          filename = NULL,
          message = NULL,
          n = 0L
        )
      )


      clear_prepared_file <- function() {

        st <- prepared_state()

        if (
          !is.null(
            st$path
          ) &&
          length(
            st$path
          ) &&
          file.exists(
            st$path
          )
        ) {
          unlink(
            st$path,
            force = TRUE
          )
        }

        prepared_state(
          list(
            status = "idle",
            path = NULL,
            filename = NULL,
            message = NULL,
            n = 0L
          )
        )

        invisible(
          TRUE
        )
      }


      session$onSessionEnded(
        function() {

          # onSessionEnded() se ejecuta cuando el contexto reactivo de la
          # sesión ya está cerrándose. Leer un reactiveVal directamente aquí
          # provoca "Operation not allowed without an active reactive context".
          # isolate() permite consultar el último estado sin registrar
          # dependencias reactivas.
          st <- isolate(
            prepared_state()
          )

          if (
            !is.null(
              st$path
            ) &&
            length(
              st$path
            ) &&
            file.exists(
              st$path
            )
          ) {
            unlink(
              st$path,
              force = TRUE
            )
          }
        }
      )


      # Si el usuario cambia la selección o el periodo, un Excel ya preparado
      # deja de representar el estado actual y se invalida.
      observeEvent(
        list(
          included_ids(),
          input$export_period,
          request()
        ),
        {
          st <- prepared_state()

          if (
            identical(
              st$status,
              "ready"
            ) ||
            identical(
              st$status,
              "error"
            )
          ) {
            clear_prepared_file()
          }
        },
        ignoreInit = TRUE
      )


      build_excel_file <- function(
        output_path
      ) {

        req_norm <- request()

        x <- copy(
          selected_review()
        )

        if (!nrow(x)) {
          stop(
            "No hay estaciones seleccionadas."
          )
        }

        if (
          any(
            !x$compatible_diaria,
            na.rm = TRUE
          )
        ) {
          stop(
            "La selección contiene series subdiarias no compatibles. Solo se aceptan series diarias y precipitación de 12 h."
          )
        }

        if (
          any(
            is.na(
              x$normalized_url
            ) |
              !nzchar(
                x$normalized_url
              )
          )
        ) {
          stop(
            "Una o más series no tienen ruta RDS normalizada."
          )
        }

        f0 <- as.IDate(
          input$export_period[1]
        )

        f1 <- as.IDate(
          input$export_period[2]
        )

        if (
          is.na(
            f0
          ) ||
          is.na(
            f1
          ) ||
          f0 > f1
        ) {
          stop(
            "El periodo de exportación no es válido."
          )
        }

        x[, columna_datos := safe_data_column(
          nombre_estacion,
          codigo_estacion,
          id_config
        )]

        datos <- data.table(
          Fecha = seq(
            as.Date(
              f0
            ),
            as.Date(
              f1
            ),
            by = "day"
          )
        )

        failures <- character()

        # Trazabilidad de incidencias durante la conversión a salida diaria.
        # No se modifica el RDS remoto: toda resolución/agregación ocurre
        # únicamente al construir la salida ancha.
        incidencias_lista <- list()

        x[, `:=`(
          fechas_multi_registro = 0L,
          fechas_valores_distintos = 0L,
          dias_12h_agregados = 0L,
          dias_12h_anomalos = 0L
        )]

        withProgress(
          message = "Preparando Excel normalizado...",
          detail = "Descargando series por IDConfig",
          value = 0,
          {

            for (i in seq_len(nrow(x))) {

              incProgress(
                0,
                detail = paste0(
                  "Serie ",
                  i,
                  " de ",
                  nrow(
                    x
                  ),
                  ": ",
                  x$nombre_estacion[i]
                )
              )

              url <- x$normalized_url[i]

              obs <- tryCatch(
                download_normalized_rds(
                  url
                ),
                error = function(e) {
                  failures <<- c(
                    failures,
                    paste0(
                      x$nombre_estacion[i],
                      " (IDConfig ",
                      x$id_config[i],
                      "): ",
                      conditionMessage(
                        e
                      )
                    )
                  )
                  NULL
                }
              )

              if (is.null(obs)) {
                incProgress(
                  1 /
                    nrow(
                      x
                    )
                )
                next
              }

              required_cols <- c(
                "fecha",
                "valor"
              )

              missing_cols <- setdiff(
                required_cols,
                names(
                  obs
                )
              )

              if (length(missing_cols)) {

                failures <- c(
                  failures,
                  paste0(
                    x$nombre_estacion[i],
                    " (IDConfig ",
                    x$id_config[i],
                    "): RDS sin columnas ",
                    paste(
                      missing_cols,
                      collapse = ", "
                    )
                  )
                )

                incProgress(
                  1 /
                    nrow(
                      x
                    )
                )
                next
              }

              obs[, fecha := as.IDate(
                fecha
              )]

              obs <- obs[
                fecha >= f0 &
                  fecha <= if (isTRUE(
                    x$requiere_agregacion_12h[i]
                  )) f1 + 1L else f1
              ]

              # ------------------------------------------------------------
              # CONVERSIÓN A UNA FILA DIARIA
              #
              # expected_obs_day == 1:
              #   - una fila por fecha;
              #   - si aparecen registros extra, conservar la última
              #     observación no NA y documentar la incidencia.
              #
              # precipitación de 12 h:
              #   - P_D = P(D, 19:00) + P(D + 1, 07:00);
              #   - si el par diario no está completo, exportar NA;
              #   - los dos registros esperados NO se consideran duplicados;
              #   - solo los días incompletos/ambiguos van a Incidencias.
              # ------------------------------------------------------------

              obs[, valor_num := suppressWarnings(
                as.numeric(
                  valor
                )
              )]

              # Los RDS normalizados ya vienen ordenados por fecha/hora, pero
              # reforzamos el orden si la columna hora está disponible.
              if ("hora" %in% names(obs)) {
                setorder(
                  obs,
                  fecha,
                  hora,
                  na.last = TRUE
                )
              } else {
                setorder(
                  obs,
                  fecha
                )
              }

              if (isTRUE(
                x$requiere_agregacion_12h[i]
              )) {

                # ----------------------------------------------------------
                # PRECIPITACIÓN 12 h -> DÍA PLUVIOMÉTRICO ANA
                # ----------------------------------------------------------

                resumen_12h <- aggregate_ana_precip_12h(obs)[
                  fecha >= f0 & fecha <= f1
                ]

                x$dias_12h_agregados[i] <- resumen_12h[
                  Par_12h_completo %in% TRUE,
                  .N
                ]

                x$dias_12h_anomalos[i] <- resumen_12h[
                  !Par_12h_completo %in% TRUE,
                  .N
                ]

                inc <- resumen_12h[
                  !Par_12h_completo %in% TRUE
                ]

                inc[, N_obs_validas_diagnostico := NULL]

                if (nrow(inc)) {

                  inc[, `:=`(
                    Estacion = x$nombre_estacion[i],
                    Codigo = x$codigo_estacion[i],
                    IDConfig = x$id_config[i],
                    Regla = paste(
                      "Precipitación 12 h: P_D = P(D, 19:00) +",
                      "P(D + 1, 07:00);",
                      "en otro caso se exporta NA"
                    )
                  )]

                  setcolorder(
                    inc,
                    c(
                      "Estacion",
                      "Codigo",
                      "IDConfig",
                      "fecha",
                      "N_registros",
                      "N_valores_validos",
                      "N_valores_distintos",
                      "N_horas_distintas",
                      "Valores_observados",
                      "Fechas_observadas",
                      "Horas_observadas",
                      "Valor_exportado",
                      "Regla"
                    )
                  )

                  incidencias_lista[[
                    length(
                      incidencias_lista
                    ) + 1L
                  ]] <- inc
                }

                d <- resumen_12h[
                  ,
                  .(
                    fecha,
                    valor = Valor_exportado
                  )
                ]

              } else {

                # ----------------------------------------------------------
                # SERIE DIARIA -> UNA FILA POR FECHA
                # ----------------------------------------------------------

                dup_dates <- obs[
                  ,
                  .(
                    n_registros = .N,
                    n_valores_validos = sum(
                      !is.na(
                        valor_num
                      )
                    ),
                    n_valores_distintos = uniqueN(
                      valor_num[
                        !is.na(
                          valor_num
                        )
                      ]
                    )
                  ),
                  by = fecha
                ][
                  n_registros > 1
                ]

                if (nrow(dup_dates)) {

                  x$fechas_multi_registro[i] <- nrow(
                    dup_dates
                  )

                  x$fechas_valores_distintos[i] <- dup_dates[
                    n_valores_distintos > 1,
                    .N
                  ]

                  inc <- obs[
                    fecha %in% dup_dates$fecha,
                    {
                      valid_idx <- which(
                        !is.na(
                          valor_num
                        )
                      )

                      pick <- if (
                        length(
                          valid_idx
                        )
                      ) {
                        valid_idx[
                          length(
                            valid_idx
                          )
                        ]
                      } else {
                        .N
                      }

                      vals_txt <- paste(
                        unique(
                          as.character(
                            valor_num[
                              !is.na(
                                valor_num
                              )
                            ]
                          )
                        ),
                        collapse = " | "
                      )

                      horas_txt <- if (
                        "hora" %in% names(.SD)
                      ) {
                        paste(
                          unique(
                            na.omit(
                              as.character(
                                hora
                              )
                            )
                          ),
                          collapse = " | "
                        )
                      } else {
                        ""
                      }

                      .(
                        N_registros = .N,
                        N_valores_validos = sum(
                          !is.na(
                            valor_num
                          )
                        ),
                        N_valores_distintos = uniqueN(
                          valor_num[
                            !is.na(
                              valor_num
                            )
                          ]
                        ),
                        Valores_observados = vals_txt,
                        Horas_observadas = horas_txt,
                        Valor_exportado = valor_num[
                          pick
                        ]
                      )
                    },
                    by = fecha
                  ]

                  inc[, `:=`(
                    Estacion = x$nombre_estacion[i],
                    Codigo = x$codigo_estacion[i],
                    IDConfig = x$id_config[i],
                    Regla = paste(
                      "Última observación no NA del día",
                      "según el orden FECHA/HORA del RDS"
                    )
                  )]

                  setcolorder(
                    inc,
                    c(
                      "Estacion",
                      "Codigo",
                      "IDConfig",
                      "fecha",
                      "N_registros",
                      "N_valores_validos",
                      "N_valores_distintos",
                      "Valores_observados",
                      "Horas_observadas",
                      "Valor_exportado",
                      "Regla"
                    )
                  )

                  incidencias_lista[[
                    length(
                      incidencias_lista
                    ) + 1L
                  ]] <- inc
                }

                # Una fila final por fecha. Si existen varias observaciones,
                # se conserva la última no NA; si todas son NA, queda NA.
                d <- obs[
                  ,
                  {
                    valid_idx <- which(
                      !is.na(
                        valor_num
                      )
                    )

                    pick <- if (
                      length(
                        valid_idx
                      )
                    ) {
                      valid_idx[
                        length(
                          valid_idx
                        )
                      ]
                    } else {
                      .N
                    }

                    .(
                      valor = valor_num[
                        pick
                      ]
                    )
                  },
                  by = fecha
                ]
              }

              d[, Fecha := as.Date(
                fecha
              )]

              d[, fecha := NULL]

              setcolorder(
                d,
                c(
                  "Fecha",
                  "valor"
                )
              )

              setnames(
                d,
                "valor",
                x$columna_datos[i]
              )

              datos <- merge(
                datos,
                d,
                by = "Fecha",
                all.x = TRUE,
                sort = FALSE
              )

              incProgress(
                1 /
                  nrow(
                    x
                  )
              )
            }
          }
        )

        if (length(failures)) {
          stop(
            paste0(
              "No se pudo preparar el conjunto completo. ",
              paste(
                failures,
                collapse = " | "
              )
            )
          )
        }

        setorder(
          datos,
          Fecha
        )

        metadata <- x[
          ,
          .(
            Columna_datos = columna_datos,
            Estacion = nombre_estacion,
            Codigo = codigo_estacion,
            IDConfig = id_config,
            Tipo_dato = tipo_dato,
            Variable = variable,
            Unidad = unidad,
            Frecuencia = frecuencia,
            Latitud = latitud,
            Longitud = longitud,
            Altitud_msnm = altitud_msnm,
            Fecha_inicial_disponible = as.Date(
              primera_fecha
            ),
            Fecha_final_disponible = as.Date(
              ultima_fecha
            ),
            Periodo_exportado_inicio = as.Date(
              f0
            ),
            Periodo_exportado_fin = as.Date(
              f1
            ),
            Completitud_periodo_pct = round(
              completitud_obs_pct,
              3
            ),
            Cubre_periodo_nominal = cubre_nominalmente,
            Dias_con_dato_periodo = n_dias_alguna,
            Dias_periodo = dias_esperados,
            Fechas_con_multiples_registros = fechas_multi_registro,
            Fechas_con_valores_distintos = fechas_valores_distintos,
            Dias_12h_agregados = dias_12h_agregados,
            Dias_12h_anomalos = dias_12h_anomalos,
            Transformacion_a_diario = fcase(
              requiere_agregacion_12h %in% TRUE,
              paste(
                "Día pluviométrico ANA: P_D = P(D, 19:00) +",
                "P(D + 1, 07:00); si el par no está completo, NA"
              ),
              default = "Serie diaria; sin agregación temporal"
            ),
            Regla_registros_multiples = fifelse(
              fechas_multi_registro > 0,
              paste(
                "Última observación no NA del día",
                "según el orden FECHA/HORA del RDS;"
              ),
              "No aplicó"
            ),
            ID_geometria = geometry_id,
            Area = geometry_name,
            Unidad_hidrografica = unidad_hidrografica,
            AAA = aaa,
            ALA = ala,
            URL_serie_normalizada = normalized_url,
            URL_reporte_original_ANA = vapply(
              id_config,
              raw_report_url,
              character(1)
            ),
            URL_visor_ANA_SNIRH = url_visor_ana,
            Origen_seleccion = req_norm$source
          )
        ]

        incidencias <- if (
          length(
            incidencias_lista
          )
        ) {
          rbindlist(
            incidencias_lista,
            fill = TRUE,
            use.names = TRUE
          )
        } else {
          data.table()
        }

        if (nrow(incidencias)) {
          incidencias[, Fecha := as.Date(
            fecha
          )]
          incidencias[, fecha := NULL]
          orden_incidencias <- c(
            "Estacion",
            "Codigo",
            "IDConfig",
            "Fecha",
            "N_registros",
            "N_valores_validos",
            "N_valores_distintos",
            "N_horas_distintas",
            "Valores_observados",
            "Fechas_observadas",
            "Horas_observadas",
            "Valor_exportado",
            "Par_12h_completo",
            "Regla"
          )

          setcolorder(
            incidencias,
            orden_incidencias[
              orden_incidencias %in% names(
                incidencias
              )
            ]
          )
          setorder(
            incidencias,
            Estacion,
            Fecha
          )
        }

        withProgress(
          message = "Escribiendo archivo Excel...",
          value = 0.15,
          {

            wb <- openxlsx::createWorkbook()

            openxlsx::addWorksheet(
              wb,
              "Datos"
            )

            openxlsx::addWorksheet(
              wb,
              "Metadata"
            )

            if (nrow(incidencias)) {
              openxlsx::addWorksheet(
                wb,
                "Incidencias"
              )
            }

            openxlsx::writeData(
              wb,
              "Datos",
              datos,
              keepNA = FALSE
            )

            incProgress(
              0.3
            )

            openxlsx::writeData(
              wb,
              "Metadata",
              metadata,
              keepNA = FALSE
            )

            if (nrow(incidencias)) {
              openxlsx::writeData(
                wb,
                "Incidencias",
                incidencias,
                keepNA = FALSE
              )
            }

            openxlsx::freezePane(
              wb,
              "Datos",
              firstRow = TRUE,
              firstCol = TRUE
            )

            openxlsx::freezePane(
              wb,
              "Metadata",
              firstRow = TRUE
            )

            if (nrow(incidencias)) {
              openxlsx::freezePane(
                wb,
                "Incidencias",
                firstRow = TRUE
              )
            }

            openxlsx::setColWidths(
              wb,
              "Datos",
              cols = 1,
              widths = 13
            )

            if (ncol(datos) > 1) {
              openxlsx::setColWidths(
                wb,
                "Datos",
                cols = 2:ncol(
                  datos
                ),
                widths = 18
              )
            }

            # No usar widths = "auto" sobre toda Metadata cuando hay URLs
            # largas: es innecesario y puede ralentizar mucho el archivo.
            openxlsx::setColWidths(
              wb,
              "Metadata",
              cols = 1:ncol(
                metadata
              ),
              widths = 18
            )

            if (nrow(incidencias)) {
              openxlsx::setColWidths(
                wb,
                "Incidencias",
                cols = 1:ncol(
                  incidencias
                ),
                widths = 18
              )
            }

            metadata_cols_anchas <- match(
              c(
                "Columna_datos",
                "Estacion",
                "Variable",
                "Transformacion_a_diario",
                "URL_serie_normalizada",
                "URL_reporte_original_ANA",
                "URL_visor_ANA_SNIRH"
              ),
              names(
                metadata
              )
            )

            metadata_cols_anchas <- metadata_cols_anchas[
              !is.na(
                metadata_cols_anchas
              )
            ]

            if (length(
              metadata_cols_anchas
            )) {
              openxlsx::setColWidths(
                wb,
                "Metadata",
                cols = metadata_cols_anchas,
                widths = 28
              )
            }

            incProgress(
              0.35
            )

            openxlsx::saveWorkbook(
              wb,
              output_path,
              overwrite = TRUE
            )

            repair_xlsx_orphan_relationships(
              output_path
            )

            incProgress(
              0.2
            )
          }
        )

        if (
          !file.exists(
            output_path
          ) ||
          file.info(
            output_path
          )$size <= 0
        ) {
          stop(
            "El archivo Excel no fue creado correctamente."
          )
        }

        # XLSX es un contenedor ZIP y debe empezar con la firma PK.
        sig <- readBin(
          output_path,
          what = "raw",
          n = 2
        )

        if (
          length(
            sig
          ) < 2 ||
          !identical(
            as.integer(
              sig
            ),
            c(
              80L,
              75L
            )
          )
        ) {
          stop(
            "El archivo generado no tiene una firma XLSX válida."
          )
        }

        list(
          n_stations = nrow(
            x
          ),
          n_days = nrow(
            datos
          ),
          size = file.info(
            output_path
          )$size,
          f0 = f0,
          f1 = f1,
          n_duplicate_days = sum(
            x$fechas_multi_registro,
            na.rm = TRUE
          ),
          n_conflicting_days = sum(
            x$fechas_valores_distintos,
            na.rm = TRUE
          ),
          n_12h_days = sum(
            x$dias_12h_agregados,
            na.rm = TRUE
          ),
          n_12h_anomalous_days = sum(
            x$dias_12h_anomalos,
            na.rm = TRUE
          )
        )
      }


      observeEvent(
        input$prepare_excel,
        {

          st <- download_status()

          if (!st$n) {
            showNotification(
              "No hay estaciones seleccionadas.",
              type = "warning",
              duration = 5
            )
            return()
          }

          if (st$n_incompatible > 0) {
            showNotification(
              paste0(
                "Hay ",
                st$n_incompatible,
                " serie(s) subdiarias no compatibles. Solo se aceptan diarias y precipitación de 12 h."
              ),
              type = "warning",
              duration = 7
            )
            return()
          }

          if (st$n_without_url > 0) {
            showNotification(
              paste0(
                "Hay ",
                st$n_without_url,
                " serie(s) sin ruta RDS normalizada."
              ),
              type = "warning",
              duration = 7
            )
            return()
          }

          clear_prepared_file()

          req_norm <- request()

          f0_txt <- as.character(
            input$export_period[1]
          )

          f1_txt <- as.character(
            input$export_period[2]
          )

          safe_type <- gsub(
            "[^A-Za-z0-9]+",
            "_",
            iconv(
              req_norm$tipo,
              to = "ASCII//TRANSLIT"
            )
          )

          final_name <- paste0(
            "ANA_SNIRH_normalizado_",
            safe_type,
            "_",
            f0_txt,
            "_",
            f1_txt,
            ".xlsx"
          )

          tmp_xlsx <- tempfile(
            pattern = "ANA_SNIRH_normalizado_",
            fileext = ".xlsx"
          )

          prepared_state(
            list(
              status = "building",
              path = NULL,
              filename = final_name,
              message = "Preparando el archivo...",
              n = st$n
            )
          )

          result <- tryCatch(
            build_excel_file(
              tmp_xlsx
            ),
            error = function(e) {
              e
            }
          )

          if (inherits(
            result,
            "error"
          )) {

            if (file.exists(
              tmp_xlsx
            )) {
              unlink(
                tmp_xlsx,
                force = TRUE
              )
            }

            prepared_state(
              list(
                status = "error",
                path = NULL,
                filename = NULL,
                message = conditionMessage(
                  result
                ),
                n = st$n
              )
            )

            showNotification(
              paste0(
                "No se pudo preparar el Excel: ",
                conditionMessage(
                  result
                )
              ),
              type = "error",
              duration = 12
            )

            return()
          }

          prepared_state(
            list(
              status = "ready",
              path = tmp_xlsx,
              filename = final_name,
              message = paste0(
                "Archivo listo: ",
                result$n_stations,
                " estaciones × ",
                format(
                  result$n_days,
                  big.mark = ","
                ),
                " días (",
                round(
                  result$size /
                    1024^2,
                  2
                ),
                " MB).",
                if (
                  result$n_12h_days > 0
                ) {
                  paste0(
                    " Se agregaron con la regla ANA ",
                    result$n_12h_days,
                    " día(s) de precipitación de 12 h."
                  )
                } else {
                  ""
                },
                if (
                  result$n_duplicate_days > 0 ||
                    result$n_12h_anomalous_days > 0
                ) {
                  paste0(
                    " Revise Incidencias: ",
                    result$n_duplicate_days,
                    " fecha(s) con registros múltiples en series diarias y ",
                    result$n_12h_anomalous_days,
                    " día(s) 12 h sin un par válido completo."
                  )
                } else {
                  ""
                }
              ),
              n = result$n_stations
            )
          )

          showNotification(
            if (
              result$n_duplicate_days > 0 ||
                result$n_12h_anomalous_days > 0
            ) {
              paste0(
                "Excel preparado. La precipitación de 12 h usó el día pluviométrico ANA; ",
                "hay incidencias que conviene revisar."
              )
            } else if (
              result$n_12h_days > 0
            ) {
              paste0(
                "Excel preparado. Se agregaron ",
                result$n_12h_days,
                " día(s) de precipitación de 12 h con P(D, 19:00) + P(D + 1, 07:00)."
              )
            } else {
              "Excel preparado correctamente. Ya puede descargarlo."
            },
            type = "message",
            duration = 8
          )
        },
        ignoreInit = TRUE
      )


      output$prepare_status <- renderUI({

        st <- prepared_state()
        ds <- download_status()

        if (!ds$n) {
          return(
            tags$small(
              "Seleccione al menos una estación.",
              style = "color:#6c757d;display:block;margin-top:.45rem;"
            )
          )
        }

        if (
          ds$n_incompatible > 0 ||
          ds$n_without_url > 0
        ) {

          reason <- c(
            if (ds$n_incompatible > 0) {
              paste0(
                ds$n_incompatible,
                " subdiaria(s) no compatible(s)"
              )
            },
            if (ds$n_without_url > 0) {
              paste0(
                ds$n_without_url,
                " sin ruta RDS"
              )
            }
          )

          return(
            tags$small(
              paste(
                "Antes de preparar:",
                paste(
                  reason,
                  collapse = " y "
                )
              ),
              style = "color:#b45309;display:block;margin-top:.45rem;"
            )
          )
        }

        if (identical(
          st$status,
          "building"
        )) {
          return(
            tags$small(
              "Preparando archivo...",
              style = "color:#6c757d;display:block;margin-top:.45rem;"
            )
          )
        }

        if (identical(
          st$status,
          "error"
        )) {
          return(
            div(
              class = "alert alert-danger py-2 mt-2 mb-0",
              tags$b("No se pudo preparar el Excel. "),
              st$message
            )
          )
        }

        if (identical(
          st$status,
          "ready"
        )) {
          return(
            div(
              class = "alert alert-success py-2 mt-2 mb-0",
              st$message
            )
          )
        }

        tags$small(
          paste0(
            "Listo para preparar ",
            ds$n,
            " estación(es)",
            if (ds$n_12h > 0) {
              paste0(
                "; ",
                ds$n_12h,
                " serie(s) de precipitación de 12 h usarán la regla ANA"
              )
            } else {
              ""
            },
            "."
          ),
          style = "color:#6c757d;display:block;margin-top:.45rem;"
        )
      })


      output$prepared_download_ui <- renderUI({

        st <- prepared_state()

        if (
          !identical(
            st$status,
            "ready"
          ) ||
          is.null(
            st$path
          ) ||
          !file.exists(
            st$path
          )
        ) {
          return(
            NULL
          )
        }

        tagList(
          downloadButton(
            session$ns(
              "download_prepared_excel"
            ),
            "Descargar Excel preparado",
            class = "btn-success",
            style = "width:100%;margin-top:.55rem;"
          )
        )
      })


      output$download_prepared_excel <- downloadHandler(

        filename = function() {

          st <- prepared_state()

          req(
            identical(
              st$status,
              "ready"
            ),
            !is.null(
              st$filename
            )
          )

          st$filename
        },

        content = function(file) {

          st <- prepared_state()

          req(
            identical(
              st$status,
              "ready"
            ),
            !is.null(
              st$path
            ),
            file.exists(
              st$path
            )
          )

          ok <- file.copy(
            st$path,
            file,
            overwrite = TRUE
          )

          if (!isTRUE(
            ok
          )) {
            stop(
              "No se pudo copiar el Excel preparado al archivo de descarga."
            )
          }
        },

        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      )

    }
  )
}

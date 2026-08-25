# ============================================================================
# MÓDULO — DIAGNÓSTICO DE ESTACIÓN
# Refactor estructural de BUILD PUBLICA V17.2. Sin cambios funcionales.
# ============================================================================

mod_diagnostico_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),

    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),

      card(
        card_header(
          tags$span(
            icon("magnifying-glass"),
            " Buscar estación"
          )
        ),

        selectizeInput(
          ns("diag_station"),
          NULL,
          choices = NULL,
          width = "100%",
          options = list(
            placeholder = "Escriba nombre, código o unidad hidrográfica...",
            openOnFocus = FALSE,
            maxOptions = 80,
            selectOnTab = TRUE,
            closeAfterSelect = TRUE,
            plugins = list("clear_button")
          )
        ),

        tags$small(
          paste(
            "No es necesario recorrer la lista:",
            "escriba cualquier parte del nombre, código o unidad hidrográfica."
          ),
          style = "color:#6c757d;"
        )
      ),

      card(
        card_header("Serie / IDConfig"),

        selectizeInput(
          ns("diag_series"),
          NULL,
          choices = character(),
          width = "100%",
          options = list(
            placeholder = "Seleccione una serie / IDConfig...",
            maxOptions = 50,
            closeAfterSelect = TRUE
          )
        ),

        tags$small(
          paste(
            "Si una estación tiene varias series, aquí se diagnostican por separado;",
            "no se fusionan."
          ),
          style = "color:#6c757d;"
        )
      )
    ),

    uiOutput(ns("diag_selected_banner")),
    uiOutput(ns("diag_no_series")),

    uiOutput(ns("ana_download_ui")),

    br(),

    uiOutput(ns("diag_cards")),

    br(),

    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),

      card(
        card_header("Completitud anual"),
        plotOutput(ns("diag_annual_plot"), height = "390px")
      ),

      card(
        card_header("Completitud mensual"),
        plotOutput(ns("diag_monthly_heatmap"), height = "390px")
      )
    ),

    br(),

    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),

      card(
        full_screen = TRUE,
        card_header("Mayores rachas de días completamente vacíos"),
        DTOutput(ns("diag_gap_table"))
      ),

      card(
        card_header("Calidad temporal para análisis"),
        uiOutput(ns("diag_continuity"))
      )
    ),

    br(),

    card(
      full_screen = TRUE,
      card_header("Resumen anual"),
      DTOutput(ns("diag_annual_table"))
    ),

    br(),

    card(
      full_screen = TRUE,
      card_header(
        uiOutput(ns("diag_series_plot_header"))
      ),
      plotOutput(
        ns("diag_series_plot"),
        height = "430px"
      ),
      uiOutput(ns("diag_series_plot_note"))
    )
  )
}


mod_diagnostico_server <- function(id, spatial_stations, tipo, diag_station_state) {
  moduleServer(id, function(input, output, session) {

    diag_series_state <- reactiveVal(NULL)

    fmt_year_block <- function(n, y0, y1) {

      if (
        !length(n) ||
        is.na(n) ||
        n <= 0 ||
        !length(y0) ||
        !length(y1) ||
        is.na(y0) ||
        is.na(y1)
      ) {
        return("0 años")
      }

      paste0(
        n,
        if (n == 1) " año (" else " años (",
        y0,
        "–",
        y1,
        ")"
      )
    }

    normalize_raw_text <- function(x) {

      x <- as.character(x)

      y <- iconv(
        x,
        from = "UTF-8",
        to = "ASCII//TRANSLIT"
      )

      y[is.na(y)] <- x[is.na(y)]

      tolower(
        trimws(y)
      )
    }


    parse_raw_value <- function(x) {

      z <- trimws(
        as.character(x)
      )

      z[
        is.na(z) |
          z == "" |
          tolower(z) %in% c(
            "na",
            "nan",
            "null",
            "s/d",
            "sd",
            "-"
          )
      ] <- NA_character_

      ambos <- !is.na(z) &
        grepl(",", z, fixed = TRUE) &
        grepl(".", z, fixed = TRUE)

      if (any(ambos)) {
        z[ambos] <- gsub(
          ",",
          "",
          z[ambos],
          fixed = TRUE
        )
      }

      solo_coma <- !is.na(z) &
        grepl(",", z, fixed = TRUE) &
        !grepl(".", z, fixed = TRUE)

      if (any(solo_coma)) {
        z[solo_coma] <- gsub(
          ",",
          ".",
          z[solo_coma],
          fixed = TRUE
        )
      }

      z <- gsub(
        "\\s+",
        "",
        z
      )

      suppressWarnings(
        as.numeric(z)
      )
    }


    parse_raw_date <- function(x) {

      z <- trimws(
        as.character(x)
      )

      out <- rep(
        as.Date(NA),
        length(z)
      )

      num <- suppressWarnings(
        as.numeric(z)
      )

      idx_excel <- !is.na(num) &
        num >= 10000 &
        num <= 100000 &
        grepl(
          "^\\d+(\\.\\d+)?$",
          z
        )

      if (any(idx_excel)) {
        out[idx_excel] <- as.Date(
          floor(
            num[idx_excel]
          ),
          origin = "1899-12-30"
        )
      }

      idx <- which(
        is.na(out) &
          !is.na(z) &
          z != ""
      )

      if (length(idx)) {

        p <- suppressWarnings(
          lubridate::parse_date_time(
            z[idx],
            orders = c(
              "dmy HMS",
              "dmy HM",
              "dmy",
              "ymd HMS",
              "ymd HM",
              "ymd"
            ),
            tz = "America/Lima",
            quiet = TRUE
          )
        )

        out[idx] <- as.Date(
          p
        )
      }

      out
    }


    parse_raw_time <- function(x) {

      z <- trimws(
        as.character(x)
      )

      out <- rep(
        NA_character_,
        length(z)
      )

      idx_txt <- grepl(
        "^\\d{1,2}:\\d{2}(:\\d{2})?$",
        z
      )

      if (any(idx_txt, na.rm = TRUE)) {

        zz <- z[idx_txt]

        zz <- ifelse(
          nchar(zz) == 5,
          paste0(
            zz,
            ":00"
          ),
          zz
        )

        out[idx_txt] <- zz
      }

      num <- suppressWarnings(
        as.numeric(z)
      )

      idx_excel <- is.na(out) &
        !is.na(num) &
        num >= 0 &
        num < 1

      if (any(idx_excel)) {

        secs <- round(
          num[idx_excel] * 86400
        )

        hh <- secs %/% 3600
        mm <- (secs %% 3600) %/% 60
        ss <- secs %% 60

        out[idx_excel] <- sprintf(
          "%02d:%02d:%02d",
          hh,
          mm,
          ss
        )
      }

      out
    }


    read_raw_series_for_plot <- function(
      url,
      expected_obs_day = 1L
    ) {

      tmp <- tempfile(
        fileext = ".xlsx"
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
          60,
          old_timeout
        )
      )

      utils::download.file(
        url,
        destfile = tmp,
        mode = "wb",
        quiet = TRUE
      )

      hojas <- readxl::excel_sheets(
        tmp
      )

      if (!length(hojas)) {
        stop(
          "El XLSX no contiene hojas."
        )
      }

      raw <- suppressWarnings(
        readxl::read_excel(
          tmp,
          sheet = hojas[1],
          col_names = FALSE,
          col_types = "text",
          .name_repair = "minimal"
        )
      )

      if (!nrow(raw)) {
        stop(
          "La hoja principal del XLSX está vacía."
        )
      }

      mat <- as.matrix(
        raw
      )

      norm <- matrix(
        normalize_raw_text(
          mat
        ),
        nrow = nrow(mat),
        ncol = ncol(mat)
      )

      fila_header <- NA_integer_
      col_fecha <- NA_integer_
      col_hora <- NA_integer_
      col_valor <- NA_integer_

      for (r in seq_len(nrow(norm))) {

        vals <- norm[r, ]

        f <- which(
          vals == "fecha"
        )

        h <- which(
          vals == "hora"
        )

        v <- which(
          grepl(
            "^valor",
            vals
          )
        )

        if (
          length(f) > 0 &&
          length(v) > 0
        ) {

          fila_header <- r
          col_fecha <- f[1]

          col_hora <- if (
            length(h) > 0
          ) {
            h[1]
          } else {
            NA_integer_
          }

          col_valor <- v[1]

          break
        }
      }

      if (is.na(fila_header)) {
        stop(
          "No se encontró el encabezado FECHA/HORA/VALOR."
        )
      }

      idx <- seq.int(
        fila_header + 1L,
        nrow(raw)
      )

      fecha_raw <- as.character(
        raw[
          idx,
          col_fecha
        ][[1]]
      )

      hora_raw <- if (
        !is.na(col_hora)
      ) {
        as.character(
          raw[
            idx,
            col_hora
          ][[1]]
        )
      } else {
        rep(
          NA_character_,
          length(idx)
        )
      }

      valor_raw <- as.character(
        raw[
          idx,
          col_valor
        ][[1]]
      )

      x <- data.table(
        fecha = as.IDate(
          parse_raw_date(
            fecha_raw
          )
        ),
        hora = parse_raw_time(
          hora_raw
        ),
        valor = parse_raw_value(
          valor_raw
        )
      )

      x <- x[
        !is.na(fecha) &
          !is.na(valor)
      ]

      if (!nrow(x)) {
        stop(
          "El XLSX no contiene valores observados graficables."
        )
      }

      x[
        ,
        datetime := as.POSIXct(
          ifelse(
            is.na(hora),
            paste0(
              as.character(fecha),
              " 00:00:00"
            ),
            paste(
              as.character(fecha),
              hora
            )
          ),
          tz = "America/Lima"
        )
      ]

      x <- unique(
        x,
        by = c(
          "datetime",
          "valor"
        )
      )

      setorder(
        x,
        datetime
      )

      exp_day <- suppressWarnings(
        as.integer(
          expected_obs_day
        )
      )

      if (
        is.na(exp_day) ||
        exp_day < 1
      ) {
        exp_day <- 1L
      }

      expected_gap_hours <- 24 / exp_day

      x[
        ,
        gap_hours := c(
          NA_real_,
          as.numeric(
            diff(datetime),
            units = "hours"
          )
        )
      ]

      x[
        ,
        segment := cumsum(
          is.na(gap_hours) |
            gap_hours >
              expected_gap_hours * 1.75
        )
      ]

      x
    }


  station_choices <- reactive({

    sp <- spatial_stations()$tab

    if (!nrow(sp)) {
      return(
        list(
          choices = character(),
          table = data.table()
        )
      )
    }

    x <- unique(
      sp[
        ,
        .(
          station_id,
          nombre_estacion,
          codigo_estacion,
          unidad_hidrografica,
          tiene_serie
        )
      ]
    )

    x[, etiqueta := paste0(
      nombre_estacion,
      ifelse(
        is.na(codigo_estacion) | codigo_estacion == "",
        "",
        paste0(" [", codigo_estacion, "]")
      ),
      ifelse(
        is.na(unidad_hidrografica) | unidad_hidrografica == "",
        "",
        paste0(" — ", unidad_hidrografica)
      ),
      ifelse(tiene_serie %in% TRUE, "", " — SIN SERIE")
    )]

    setorder(
      x,
      nombre_estacion,
      codigo_estacion
    )

    list(
      choices = setNames(
        x$station_id,
        x$etiqueta
      ),
      table = x
    )
  })


  # Búsqueda manual -> estado interno.
  observeEvent(
    input$diag_station,
    {
      sid <- input$diag_station

      if (
        !is.null(sid) &&
        length(sid) == 1 &&
        !is.na(sid) &&
        nzchar(as.character(sid))
      ) {
        diag_station_state(
          as.character(sid)
        )
      }
    },
    ignoreInit = FALSE
  )



  # --------------------------------------------------------------------------
  # DIAGNÓSTICO INDIVIDUAL DE ESTACIÓN
  # --------------------------------------------------------------------------

  observe({

    sc <- station_choices()

    if (!nrow(sc$table)) {

      diag_station_state(
        NULL
      )

      updateSelectizeInput(
        session,
        "diag_station",
        choices = character(),
        selected = character(),
        server = TRUE
      )

      return()
    }

    current_state <- diag_station_state()

    selected <- if (
      !is.null(current_state) &&
      current_state %in% sc$table$station_id
    ) {
      current_state
    } else {
      sc$table$station_id[1]
    }

    # Mantener coherencia cuando cambia variable, archivo espacial o buffer.
    if (
      is.null(current_state) ||
      !(current_state %in% sc$table$station_id)
    ) {
      diag_station_state(
        selected
      )
    }

    updateSelectizeInput(
      session,
      "diag_station",
      choices = sc$choices,
      selected = selected,
      server = TRUE
    )
  })


  observe({

    sid <- diag_station_state()
    req(sid)

    s <- copy(
      SERIES[
        station_id == sid &
          tipo_dato == tipo()
      ]
    )

    if (!nrow(s)) {
      diag_series_state(NULL)

      updateSelectizeInput(
        session,
        "diag_series",
        choices = character(),
        selected = character(),
        server = TRUE
      )

      return()
    }

    setorder(
      s,
      -anios_nominales,
      -completitud_propia_pct,
      variable,
      id_config
    )

    labels <- paste0(
      ifelse(
        is.na(s$variable) | s$variable == "",
        "Serie",
        s$variable
      ),
      " | IDConfig ",
      s$id_config,
      " | ",
      ifelse(
        is.na(s$primera_fecha),
        "—",
        as.character(s$primera_fecha)
      ),
      " → ",
      ifelse(
        is.na(s$ultima_fecha),
        "—",
        as.character(s$ultima_fecha)
      ),
      " | ",
      ifelse(
        is.na(s$completitud_propia_pct),
        "—",
        paste0(
          round(s$completitud_propia_pct, 1),
          "%"
        )
      )
    )

    choices <- setNames(
      s$series_id,
      labels
    )

    current <- isolate(input$diag_series)

    selected <- if (
      !is.null(current) &&
      current %in% s$series_id
    ) {
      current
    } else {
      s$series_id[1]
    }

    # Estado lógico primero.
    diag_series_state(
      as.character(selected)
    )

    updateSelectizeInput(
      session,
      "diag_series",
      choices = choices,
      selected = selected,
      server = TRUE
    )
  })


  observeEvent(
    input$diag_series,
    {
      sid_series <- input$diag_series

      if (
        !is.null(sid_series) &&
        length(sid_series) == 1 &&
        !is.na(sid_series) &&
        nzchar(as.character(sid_series))
      ) {
        diag_series_state(
          as.character(sid_series)
        )
      }
    },
    ignoreInit = FALSE
  )


  # Cuando cambia la estación lógica, escoger inmediatamente una serie válida,
  # sin esperar a que el selectize del navegador termine de actualizarse.
  observeEvent(
    diag_station_state(),
    {
      sid <- diag_station_state()
      req(sid)

      s <- copy(
        SERIES[
          station_id == sid &
            tipo_dato == tipo()
        ]
      )

      if (!nrow(s)) {
        diag_series_state(NULL)
        return()
      }

      setorder(
        s,
        -anios_nominales,
        -completitud_propia_pct,
        variable,
        id_config
      )

      diag_series_state(
        as.character(
          s$series_id[1]
        )
      )
    },
    ignoreInit = FALSE
  )


  diag_result <- reactive({

    sid_station <- diag_station_state()
    sid_series <- diag_series_state()

    req(
      sid_station,
      sid_series
    )

    # Protección extra: la serie debe pertenecer a la estación activa.
    valid <- SERIES[
      series_id == sid_series &
        station_id == sid_station &
        tipo_dato == tipo(),
      .N
    ]

    if (!valid) {
      return(NULL)
    }

    tryCatch(
      diagnosticar_serie(
        sid_series
      ),
      error = function(e) {
        showNotification(
          paste0(
            "No se pudo calcular el diagnóstico: ",
            conditionMessage(e)
          ),
          type = "error",
          duration = 8
        )
        NULL
      }
    )
  })


  output$ana_download_ui <- renderUI({

    sid_series <- diag_series_state()

    if (is.null(sid_series)) {
      return(NULL)
    }

    x <- SERIES[
      series_id == sid_series
    ][1]

    if (!nrow(x)) {
      return(NULL)
    }

    cuenca_txt <- if (
      "unidad_hidrografica" %in% names(x) &&
      !is.na(x$unidad_hidrografica) &&
      nzchar(x$unidad_hidrografica)
    ) {
      as.character(x$unidad_hidrografica)
    } else {
      "la unidad hidrográfica indicada en la ficha"
    }

    estacion_txt <- if (
      !is.na(x$nombre_estacion) &&
      nzchar(x$nombre_estacion)
    ) {
      as.character(x$nombre_estacion)
    } else {
      "la estación seleccionada"
    }

    codigo_txt <- if (
      !is.na(x$codigo_estacion) &&
      nzchar(x$codigo_estacion)
    ) {
      paste0(
        " [",
        x$codigo_estacion,
        "]"
      )
    } else {
      ""
    }

    capa_txt <- if (
      identical(
        tipo(),
        "Caudal"
      )
    ) {
      "Hidrometría"
    } else {
      "Pluviometría"
    }

    raw_url <- raw_report_url(
      x$id_config
    )

    raw_available <- (
      length(raw_url) &&
        !is.na(raw_url) &&
        nzchar(raw_url)
    )

    exp_day <- suppressWarnings(
      as.integer(x$expected_obs_day)
    )

    normalized_compatible <- (
      !is.na(exp_day) &&
        (
          exp_day == 1L ||
            is_ana_precip_12h(
              x$tipo_dato,
              exp_day
            )
        ) &&
        "carpeta_cuenca" %in% names(x) &&
        !is.na(x$carpeta_cuenca) &&
        nzchar(as.character(x$carpeta_cuenca))
    )

    card(
      card_header("Acceso a la serie"),

      layout_columns(
        col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),

        div(
          class = "h-100 border rounded p-3",
          style = "background:#F8FAFC;border-color:#DCE3EA !important;",

          tags$b("Reporte original"),

          br(),

          if (raw_available) {
            tagList(
              tags$a(
                href = raw_url,
                target = "_blank",
                rel = "noopener noreferrer",
                class = "btn btn-primary mb-2",
                icon("file-excel"),
                " Descargar reporte original ANA/SNIRH (.xlsx)"
              ),

              br(),

              tags$small(
                paste(
                  "Copia archivada sin modificaciones del reporte XLSX oficial",
                  "obtenido de ANA/SNIRH. Conserva el formato y metadatos originales."
                ),
                style = "color:#6c757d;"
              )
            )
          } else {
            div(
              class = "alert alert-warning py-2 mb-0",
              paste(
                "No se encontró una copia RAW archivada para este IDConfig.",
                "Puede consultar la serie en el visor oficial."
              )
            )
          },

          tags$hr(),

          tags$b("Serie normalizada"),

          br(),

          if (isTRUE(normalized_compatible)) {
            tagList(
              dateRangeInput(
                session$ns("diag_export_period"),
                "Periodo de descarga",
                start = as.Date(x$primera_fecha),
                end = as.Date(x$ultima_fecha),
                min = as.Date(x$primera_fecha),
                max = as.Date(x$ultima_fecha),
                format = "yyyy-mm-dd",
                separator = " a "
              ),

              tags$small(
                paste(
                  "Por defecto se exporta todo el periodo nominal.",
                  "Puede reducir el intervalo antes de descargar."
                ),
                style = "color:#6c757d;display:block;margin-bottom:.75rem;"
              ),

              downloadButton(
                session$ns("download_normalized_single"),
                "Descargar serie normalizada (.xlsx)",
                class = "btn-success",
                style = "width:100%;"
              ),

              tags$small(
                if (is_ana_precip_12h(x$tipo_dato, exp_day)) {
                  paste(
                    "Día pluviométrico ANA: P_D = P(D, 19:00) +",
                    "P(D + 1, 07:00). Si falta una componente, el día queda sin dato."
                  )
                } else {
                  "La salida conserva un calendario diario continuo y los vacíos como celdas sin dato."
                },
                style = "color:#6c757d;display:block;margin-top:.55rem;"
              )
            )
          } else {
            div(
              class = "alert alert-warning py-2 mb-0",
              paste(
                "Esta serie no es compatible con la descarga normalizada diaria directa.",
                "Actualmente se admiten series diarias y precipitación de 12 h con la regla ANA."
              )
            )
          }
        ),

        div(
          class = "h-100 border rounded p-3",
          style = "background:#F8FAFC;border-color:#DCE3EA !important;",

          tags$b("Visor oficial ANA/SNIRH"),

          br(),

          div(
            class = "alert alert-info py-2",
            tags$b(
              paste0(
                "Fecha de corte de esta aplicación: ",
                format_data_freeze_date_es(),
                ". "
              )
            ),
            paste(
              "La base integrada corresponde a una copia congelada de ANA/SNIRH",
              "y se actualizará con frecuencia mensual. Para consultar registros",
              "incorporados después de esa fecha, revise la información más reciente",
              "disponible en el visor oficial ANA/SNIRH."
            )
          ),

          tags$a(
            href = url_visor_ana,
            target = "_blank",
            rel = "noopener noreferrer",
            class = "btn btn-outline-primary mb-2",
            icon("arrow-up-right-from-square"),
            " Abrir visor oficial ANA/SNIRH"
          ),

          tags$ol(
            style = "margin-top:.45rem;margin-bottom:.35rem;",
            tags$li(
              paste0(
                "Busque la cuenca: ",
                cuenca_txt,
                "."
              )
            ),
            tags$li(
              paste0(
                "Active la capa «",
                capa_txt,
                "»."
              )
            ),
            tags$li(
              paste0(
                "Ubique ",
                estacion_txt,
                codigo_txt,
                " y abra su ficha."
              )
            )
          ),

          tags$small(
            paste0(
              "IDConfig de la serie: ",
              x$id_config,
              "."
            ),
            style = "color:#6c757d;"
          )
        )
      )
    )
  })


  output$download_normalized_single <- downloadHandler(

    filename = function() {

      sid_series <- diag_series_state()
      req(sid_series)

      x <- SERIES[
        series_id == sid_series
      ][1]

      req(nrow(x))
      req(input$diag_export_period)

      f0 <- as.Date(input$diag_export_period[1])
      f1 <- as.Date(input$diag_export_period[2])

      station_part <- safe_export_name(
        x$nombre_estacion,
        fallback = "Estacion"
      )

      code_part <- if (
        !is.na(x$codigo_estacion) &&
        nzchar(as.character(x$codigo_estacion))
      ) {
        safe_export_name(
          x$codigo_estacion,
          fallback = paste0("IDConfig_", x$id_config)
        )
      } else {
        paste0(
          "IDConfig_",
          x$id_config
        )
      }

      paste0(
        "ANA_SNIRH_normalizado_",
        station_part,
        "_",
        code_part,
        "_",
        as.character(f0),
        "_",
        as.character(f1),
        ".xlsx"
      )
    },

    content = function(file) {

      sid_series <- diag_series_state()

      req(
        sid_series,
        input$diag_export_period
      )

      f0 <- as.IDate(
        input$diag_export_period[1]
      )

      f1 <- as.IDate(
        input$diag_export_period[2]
      )

      withProgress(
        message = "Generando serie normalizada...",
        detail = "Descargando y preparando la serie seleccionada",
        value = 0.15,
        {
          incProgress(0.15)

          build_single_normalized_excel(
            series_id = sid_series,
            f0 = f0,
            f1 = f1,
            output_path = file
          )

          incProgress(0.70)
        }
      )
    },

    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )



  output$diag_selected_banner <- renderUI({

    sid <- diag_station_state()

    if (is.null(sid)) {
      return(NULL)
    }

    x <- STATIONS[
      station_id == sid &
        tipo_dato == tipo()
    ][1]

    if (!nrow(x)) {
      return(NULL)
    }

    div(
      class = "alert alert-light border",
      style = "padding:.55rem .8rem;margin-bottom:0;",
      tags$b("Estación activa: "),
      x$nombre_estacion,
      if (
        !is.na(x$codigo_estacion) &&
        nzchar(x$codigo_estacion)
      ) {
        paste0(
          " [",
          x$codigo_estacion,
          "]"
        )
      }
    )
  })


  output$diag_no_series <- renderUI({

    sid <- diag_station_state()
    if (is.null(sid)) return(NULL)

    x <- STATIONS[
      station_id == sid &
        tipo_dato == tipo()
    ][1]

    if (!nrow(x) || isTRUE(x$tiene_serie)) {
      return(NULL)
    }

    div(
      class = "alert alert-warning",
      tags$b("Estación válida sin serie disponible. "),
      paste(
        "ANA incluye esta estación en su inventario,",
        "pero su ficha no expone actualmente ningún IDConfig/serie descargable."
      )
    )
  })


  output$diag_cards <- renderUI({
    r <- diag_result()
    if (is.null(r)) return(NULL)

    x <- r$summary[1]

    layout_columns(
      col_widths = breakpoints(
        sm = c(12, 12, 12, 12),
        md = c(6, 6, 6, 6),
        lg = c(3, 3, 3, 3)
      ),

      metric_card(
        "Periodo nominal",
        paste0(
          as.character(x$primera_fecha),
          " → ",
          as.character(x$ultima_fecha)
        ),
        paste0(
          round(x$anios_nominales, 1),
          " años"
        )
      ),

      metric_card(
        "Completitud propia",
        fmt_pct(
          x$completitud_propia_pct,
          2
        ),
        paste0(
          fmt_num(x$observaciones_validas),
          " / ",
          fmt_num(x$observaciones_esperadas),
          " observaciones"
        )
      ),

      metric_card(
        "Mayor racha vacía",
        paste0(
          fmt_num(x$max_racha_vacia_dias),
          " días"
        ),
        paste0(
          fmt_num(x$n_rachas_vacias),
          " rachas | P95 ",
          round(x$p95_racha_vacia_dias, 1),
          " días"
        )
      ),

      metric_card(
        "Mayor bloque con datos",
        paste0(
          fmt_num(x$max_racha_con_dato_dias),
          " días"
        ),
        tagList(
          div(
            paste0(
              "Años 100% completos: ",
              fmt_year_block(
                x$max_anios_100_completos,
                x$max_anios_100_inicio,
                x$max_anios_100_fin
              )
            )
          ),
          div(
            paste0(
              "Años ≥90%: ",
              fmt_year_block(
                x$max_anios_ge90,
                x$max_anios_ge90_inicio,
                x$max_anios_ge90_fin
              )
            )
          )
        )
      )
    )
  })


  output$diag_annual_plot <- renderPlot({
    r <- diag_result()
    shiny::validate(
      shiny::need(!is.null(r), "Sin serie disponible."),
      shiny::need(!is.null(r) && nrow(r$annual) > 0, "Sin datos anuales.")
    )

    ggplot(
      r$annual,
      aes(
        x = anio,
        y = completitud_pct
      )
    ) +
      geom_col() +
      geom_hline(
        yintercept = c(90, 95),
        linetype = c(2, 3)
      ) +
      scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 10)
      ) +
      labs(
        x = "Año",
        y = "Completitud de observaciones (%)"
      ) +
      theme_minimal(
        base_size = 11
      )
  })


  output$diag_monthly_heatmap <- renderPlot({
    r <- diag_result()
    shiny::validate(
      shiny::need(!is.null(r), "Sin serie disponible."),
      shiny::need(!is.null(r) && nrow(r$monthly) > 0, "Sin datos mensuales.")
    )

    x <- copy(r$monthly)

    x[, mes_f := factor(
      mes,
      levels = 1:12,
      labels = c(
        "Ene", "Feb", "Mar", "Abr",
        "May", "Jun", "Jul", "Ago",
        "Sep", "Oct", "Nov", "Dic"
      )
    )]

    ggplot(
      x,
      aes(
        x = mes_f,
        y = factor(anio),
        fill = completitud_pct
      )
    ) +
      geom_tile() +
      scale_fill_viridis_c(
        limits = c(0, 100),
        name = "%"
      ) +
      labs(
        x = NULL,
        y = "Año"
      ) +
      theme_minimal(
        base_size = 10
      ) +
      theme(
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 7)
      )
  })


  output$diag_gap_table <- renderDT({
    r <- diag_result()

    if (is.null(r)) {
      return(
        datatable(
          data.frame(Mensaje = "Estación válida, pero ANA no expone una serie descargable."),
          rownames = FALSE
        )
      )
    }

    x <- copy(r$gaps)

    if (!nrow(x)) {
      return(
        datatable(
          data.frame(
            Mensaje = "No se detectaron días completamente vacíos dentro del periodo nominal."
          ),
          rownames = FALSE
        )
      )
    }

    setorder(x, -n_dias, inicio)

    tab <- x[
      1:min(100, .N),
      .(
        Inicio = as.Date(inicio),
        Fin = as.Date(fin),
        `Días consecutivos` = n_dias
      )
    ]

    datatable(
      tab,
      extensions = "Buttons",
      options = dt_opts(20),
      rownames = FALSE,
      class = "stripe hover compact"
    )
  }, server = FALSE)


  output$diag_continuity <- renderUI({
    r <- diag_result()

    if (is.null(r)) {
      return(
        div(
          class = "alert alert-secondary",
          "Sin diagnóstico temporal: la estación no tiene serie expuesta por ANA."
        )
      )
    }

    x <- r$summary[1]
    annual <- copy(r$annual)
    monthly <- copy(r$monthly)
    gaps <- copy(r$gaps)

    # --------------------------------------------------------------
    # Años calendario realmente comparables.
    # Los años parciales del inicio/fin nominal no entran en los conteos.
    # --------------------------------------------------------------
    annual_full <- annual[
      anio_calendario_completo %in% TRUE
    ]

    n_years_full <- nrow(annual_full)

    n_ge90 <- annual_full[
      completitud_pct >= 90,
      .N
    ]

    n_ge95 <- annual_full[
      completitud_pct >= 95,
      .N
    ]

    n_100 <- annual_full[
      anio_100_completo %in% TRUE,
      .N
    ]

    # Integridad intradía: entre los días donde existe al menos una
    # observación, proporción que alcanzó la frecuencia diaria esperada.
    intraday_integrity <- if (
      !is.na(x$dias_con_alguna_obs) &&
      x$dias_con_alguna_obs > 0
    ) {
      100 * x$dias_completos / x$dias_con_alguna_obs
    } else {
      NA_real_
    }

    # Interrupciones largas y concentración del faltante.
    n_gap_30 <- if (nrow(gaps)) gaps[n_dias >= 30, .N] else 0L
    n_gap_90 <- if (nrow(gaps)) gaps[n_dias >= 90, .N] else 0L
    n_gap_365 <- if (nrow(gaps)) gaps[n_dias >= 365, .N] else 0L

    gap_total_days <- if (nrow(gaps)) {
      sum(gaps$n_dias, na.rm = TRUE)
    } else {
      0
    }

    gap_90_days <- if (nrow(gaps)) {
      sum(gaps[n_dias >= 90, n_dias], na.rm = TRUE)
    } else {
      0
    }

    gap_concentration <- if (gap_total_days > 0) {
      100 * gap_90_days / gap_total_days
    } else {
      0
    }

    # Estabilidad interanual: mediana e IQR solo sobre años calendario
    # completos, para no penalizar el primer/último año parcial.
    annual_median <- if (n_years_full) {
      median(
        annual_full$completitud_pct,
        na.rm = TRUE
      )
    } else {
      NA_real_
    }

    annual_q25 <- if (n_years_full) {
      as.numeric(
        quantile(
          annual_full$completitud_pct,
          probs = 0.25,
          na.rm = TRUE,
          names = FALSE
        )
      )
    } else {
      NA_real_
    }

    annual_q75 <- if (n_years_full) {
      as.numeric(
        quantile(
          annual_full$completitud_pct,
          probs = 0.75,
          na.rm = TRUE,
          names = FALSE
        )
      )
    } else {
      NA_real_
    }

    # Mes climatológico más débil: completitud media por mes, usando
    # únicamente meses calendario completos. La mediana saturaba en 100%
    # para muchas series y el desempate terminaba eligiendo enero.
    weakest_month <- NULL

    if (nrow(monthly)) {
      monthly[, month_start := as.Date(
        paste0(
          ym,
          "-01"
        )
      )]

      monthly[, dias_calendario_mes := lubridate::days_in_month(
        month_start
      )]

      monthly_full <- monthly[
        dias_periodo == dias_calendario_mes
      ]

      if (nrow(monthly_full)) {
        month_summary <- monthly_full[
          ,
          .(
            completitud_media = mean(
              completitud_pct,
              na.rm = TRUE
            )
          ),
          by = mes
        ]

        setorder(
          month_summary,
          completitud_media,
          mes
        )

        min_completitud <- month_summary$completitud_media[1]
        weakest_month <- month_summary[
          abs(completitud_media - min_completitud) < 1e-9
        ]
      }
    }

    month_names <- c(
      "Enero", "Febrero", "Marzo", "Abril",
      "Mayo", "Junio", "Julio", "Agosto",
      "Septiembre", "Octubre", "Noviembre", "Diciembre"
    )

    weakest_month_txt <- if (
      !is.null(weakest_month) &&
      nrow(weakest_month) &&
      all(!is.na(weakest_month$mes)) &&
      all(weakest_month$mes >= 1) &&
      all(weakest_month$mes <= 12)
    ) {
      if (
        nrow(weakest_month) == 12L &&
        setequal(weakest_month$mes, 1:12)
      ) {
        paste0(
          "Sin diferencias entre meses — media ",
          fmt_pct(
            weakest_month$completitud_media[1],
            1
          )
        )
      } else {
        paste0(
          if (nrow(weakest_month) > 1L) "Empate: " else "",
          paste(
            month_names[weakest_month$mes],
            collapse = ", "
          ),
          " — media ",
          fmt_pct(
            weakest_month$completitud_media[1],
            1
          )
        )
      }
    } else {
      "—"
    }

    div(
      tags$h5(
        paste0(
          x$nombre_estacion,
          " — ",
          x$variable
        )
      ),

      tags$small(
        paste0(
          "Frecuencia esperada: ",
          x$expected_obs_day,
          " observación(es)/día.",
          if (is_ana_precip_12h(r$meta$tipo_dato, x$expected_obs_day)) {
            " Para 12 h: 19:00 de D + 07:00 de D+1."
          } else {
            ""
          }
        ),
        style = "color:#6c757d;display:block;margin-bottom:.65rem;"
      ),

      tags$ul(
        tags$li(
          tags$b("Años calendario utilizables: "),
          "≥90%: ",
          fmt_num(n_ge90),
          " / ",
          fmt_num(n_years_full),
          "; ≥95%: ",
          fmt_num(n_ge95),
          " / ",
          fmt_num(n_years_full),
          "; 100%: ",
          fmt_num(n_100),
          " / ",
          fmt_num(n_years_full),
          "."
        ),

        tags$li(
          tags$b("Integridad intradía: "),
          fmt_pct(
            intraday_integrity,
            2
          ),
          " de los días con datos alcanzan la frecuencia esperada; ",
          fmt_num(x$dias_parciales),
          " día(s) parciales."
        ),

        tags$li(
          tags$b("Interrupciones largas: "),
          "≥30 días: ",
          fmt_num(n_gap_30),
          "; ≥90 días: ",
          fmt_num(n_gap_90),
          "; ≥365 días: ",
          fmt_num(n_gap_365),
          "."
        ),

        tags$li(
          tags$b("Concentración del faltante: "),
          fmt_pct(
            gap_concentration,
            1
          ),
          " de los días completamente vacíos pertenece a interrupciones de ≥90 días."
        ),

        tags$li(
          tags$b("Estabilidad entre años: "),
          "mediana ",
          fmt_pct(
            annual_median,
            1
          ),
          "; IQR ",
          fmt_pct(
            annual_q25,
            1
          ),
          "–",
          fmt_pct(
            annual_q75,
            1
          ),
          " sobre años calendario completos."
        ),

        tags$li(
          tags$b("Mes climatológico más débil: "),
          weakest_month_txt,
          "."
        )
      )
    )
  })



  output$diag_annual_table <- renderDT({
    r <- diag_result()

    if (is.null(r) || !nrow(r$annual)) {
      return(
        datatable(
          data.frame(Mensaje = "Sin serie disponible."),
          rownames = FALSE
        )
      )
    }

    x <- copy(r$annual)

    tab <- x[
      ,
      .(
        Año = anio,
        `Días periodo` = dias_periodo,
        `Obs. válidas` = observaciones_validas,
        `Obs. esperadas` = observaciones_esperadas,
        `Completitud (%)` = round(completitud_pct, 2),
        `Año calendario completo` = fifelse(
          is.na(anio_calendario_completo),
          "—",
          fifelse(anio_calendario_completo, "Sí", "No")
        ),
        `Año 100% completo` = fifelse(
          is.na(anio_100_completo),
          "—",
          fifelse(anio_100_completo, "Sí", "No")
        ),
        `Año ≥90%` = fifelse(
          is.na(anio_ge90),
          "—",
          fifelse(anio_ge90, "Sí", "No")
        ),
        `Días con algún dato` = dias_con_alguna_obs,
        `Días completos` = dias_completos,
        `Días parciales` = dias_parciales,
        `Días vacíos` = dias_vacios,
        `Cobertura días (%)` = round(cobertura_dias_pct, 2)
      )
    ]

    datatable(
      tab,
      extensions = "Buttons",
      options = dt_opts(25),
      rownames = FALSE,
      class = "stripe hover compact"
    )
  }, server = FALSE)


  # --------------------------------------------------------------------------
  # Serie observada real desde el XLSX RAW de la serie seleccionada.
  # Se carga bajo demanda; no incrementa el tiempo de arranque de la app.
  # --------------------------------------------------------------------------

  diag_observed_series <- reactive({

    sid_series <- diag_series_state()

    if (
      is.null(sid_series) ||
      !nzchar(
        as.character(
          sid_series
        )
      )
    ) {
      return(
        list(
          ok = FALSE,
          message = "Seleccione una serie."
        )
      )
    }

    meta <- SERIES[
      series_id == sid_series
    ][1]

    if (!nrow(meta)) {
      return(
        list(
          ok = FALSE,
          message = "No se encontró metadata para la serie seleccionada."
        )
      )
    }

    raw_url <- raw_report_url(
      meta$id_config
    )

    if (
      !length(raw_url) ||
      is.na(raw_url) ||
      !nzchar(raw_url)
    ) {
      return(
        list(
          ok = FALSE,
          message = "No existe una copia RAW archivada para graficar esta serie."
        )
      )
    }

    x <- tryCatch(
      read_raw_series_for_plot(
        raw_url,
        expected_obs_day = meta$expected_obs_day
      ),
      error = function(e) {
        e
      }
    )

    if (inherits(x, "error")) {
      return(
        list(
          ok = FALSE,
          message = paste0(
            "No se pudo cargar el gráfico desde el XLSX RAW: ",
            conditionMessage(
              x
            )
          )
        )
      )
    }

    list(
      ok = TRUE,
      data = x,
      meta = meta
    )
  })


  output$diag_series_plot_header <- renderUI({

    r <- diag_observed_series()

    if (!isTRUE(r$ok)) {
      return(
        "Serie observada ANA/SNIRH"
      )
    }

    paste0(
      r$meta$nombre_estacion,
      " — ",
      r$meta$variable
    )
  })


  output$diag_series_plot <- renderPlot({

    r <- diag_observed_series()

    shiny::validate(
      shiny::need(
        isTRUE(r$ok),
        r$message
      )
    )

    x <- r$data
    meta <- r$meta

    y_label <- if (
      !is.na(meta$unidad) &&
      nzchar(
        as.character(
          meta$unidad
        )
      )
    ) {
      paste0(
        meta$variable,
        " (",
        meta$unidad,
        ")"
      )
    } else {
      as.character(
        meta$variable
      )
    }

    ggplot(
      x,
      aes(
        x = datetime,
        y = valor,
        group = segment
      )
    ) +
      geom_line(
        linewidth = 0.35,
        na.rm = TRUE
      ) +
      labs(
        x = "Fecha",
        y = y_label
      ) +
      theme_minimal(
        base_size = 11
      ) +
      theme(
        panel.grid.minor = element_blank()
      )
  })


  output$diag_series_plot_note <- renderUI({

    r <- diag_observed_series()

    if (!isTRUE(r$ok)) {
      return(NULL)
    }

    x <- r$data
    meta <- r$meta

    tags$small(
      paste0(
        "Serie RAW del IDConfig ",
        meta$id_config,
        ": ",
        format(
          min(
            as.Date(
              x$datetime
            ),
            na.rm = TRUE
          ),
          "%Y-%m-%d"
        ),
        " → ",
        format(
          max(
            as.Date(
              x$datetime
            ),
            na.rm = TRUE
          ),
          "%Y-%m-%d"
        ),
        " | ",
        format(
          nrow(x),
          big.mark = ","
        ),
        " observaciones válidas. Los vacíos temporales se mantienen como cortes en la línea."
      ),
      style = "color:#6c757d;"
    )
  })


    list(station_choices = station_choices)
  })
}

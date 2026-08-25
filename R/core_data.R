# ============================================================================
# CORE DATA — inicialización diferida
#
# IMPORTANTE: Shiny carga automáticamente los archivos .R ubicados en R/
# antes de evaluar app.R. Por eso este archivo NO debe leer datos ni llamar
# fread() en el nivel superior. Solo define una función; app.R la ejecuta
# después de cargar los paquetes.
# ============================================================================

cache_source_signature <- function(paths) {

  signature_one <- function(path) {

    if (!file.exists(path)) {
      return(list(size = NA_real_, md5 = NA_character_))
    }

    if (!grepl("\\.csv$", path, ignore.case = TRUE)) {
      return(list(
        size = as.numeric(file.info(path)$size),
        md5 = unname(tools::md5sum(path))
      ))
    }

    bytes <- readBin(
      path,
      what = "raw",
      n = file.info(path)$size
    )

    if (length(bytes) > 1L) {
      is_crlf <- bytes == as.raw(13L) &
        c(bytes[-1L] == as.raw(10L), FALSE)
      bytes <- bytes[!is_crlf]
    }

    canonical_file <- tempfile(fileext = ".csv")
    on.exit(unlink(canonical_file), add = TRUE)
    writeBin(bytes, canonical_file)

    list(
      size = as.numeric(length(bytes)),
      md5 = unname(tools::md5sum(canonical_file))
    )
  }

  signatures <- lapply(paths, signature_one)

  data.frame(
    file = basename(paths),
    exists = file.exists(paths),
    size = vapply(signatures, `[[`, numeric(1), "size"),
    md5 = vapply(signatures, `[[`, character(1), "md5"),
    stringsAsFactors = FALSE
  )
}

init_core_data <- function(target_env = parent.frame()) {

  # --------------------------------------------------------------------------
  # FAST STARTUP CACHE v3 — DAY reducido y fragmentado
  #
  # Se genera una sola vez con build_startup_cache_v3.R.
  # Son RDS SIN compresión. DAY se divide en fragmentos menores de 50 MiB para
  # poder versionarlos en GitHub sin sacrificar velocidad de lectura.
  # --------------------------------------------------------------------------

  .profile_startup_outer <- isTRUE(
    getOption(
      "ana.profile.startup",
      FALSE
    )
  )

  .startup_outer_t0 <- proc.time()[["elapsed"]]

  .cache_dir <- file.path(
    "data",
    "startup_cache_v3"
  )

  .cache_manifest_file <- file.path(
    .cache_dir,
    "manifest.rds"
  )

  .ignore_cache <- isTRUE(
    getOption(
      "ana.ignore.startup.cache",
      FALSE
    )
  )

  .source_files <- c(
    file.path(
      "data",
      "01_catalogo_series.csv"
    ),
    file.path(
      "data",
      "01_disponibilidad_diaria.rds"
    ),
    file.path(
      "data",
      "01_inventario_estaciones_validado.csv"
    ),
    file.path(
      "data",
      "01_estaciones_sin_serie.csv"
    ),
    file.path(
      "data",
      "02_indice_raw_xlsx.csv"
    )
  )

  if (
    !.ignore_cache &&
    file.exists(.cache_manifest_file)
  ) {

    .cache_manifest <- tryCatch(
      readRDS(.cache_manifest_file),
      error = function(e) {
        NULL
      }
    )

    .current_signature <- cache_source_signature(
      .source_files
    )

    .cache_valid <- (
      !is.null(
          .cache_manifest
        ) &&
        identical(
          .cache_manifest$version,
          "startup-cache-v3-sharded"
        ) &&
        identical(
          .cache_manifest$source_signature,
          .current_signature
        ) &&
        file.exists(
          file.path(
            .cache_dir,
            .cache_manifest$objects_file
          )
        ) &&
        all(
          file.exists(
            file.path(
              .cache_dir,
              .cache_manifest$day_files
            )
          )
        )
    )

    .cache_objects <- if (isTRUE(.cache_valid)) {
      tryCatch(
        {
          objects <- readRDS(
            file.path(
              .cache_dir,
              .cache_manifest$objects_file
            )
          )

          day_parts <- lapply(
            file.path(
              .cache_dir,
              .cache_manifest$day_files
            ),
            readRDS
          )

          objects$DAY <- data.table::rbindlist(
            day_parts,
            use.names = TRUE
          )

          data.table::setkeyv(
            objects$DAY,
            c("series_id", "fecha")
          )

          objects
        },
        error = function(e) {
          NULL
        }
      )
    } else {
      NULL
    }

    .cache_valid <- (
      is.list(.cache_objects) &&
        identical(
          names(.cache_objects$DAY),
          .cache_manifest$day_columns
        ) &&
        identical(
          nrow(.cache_objects$DAY),
          .cache_manifest$day_rows
        )
    )

    if (
      isTRUE(
        .cache_valid
      )
    ) {

      list2env(
        .cache_objects,
        envir = target_env
      )

      if (
        .profile_startup_outer
      ) {

        message(
          sprintf(
            "[startup] %-28s %7.3f s",
            "FAST CACHE cargado",
            proc.time()[["elapsed"]] -
              .startup_outer_t0
          )
        )

        message(
          sprintf(
            "[startup] %-28s %7.3f s",
            "TOTAL init_core_data",
            proc.time()[["elapsed"]] -
              .startup_outer_t0
          )
        )
      }

      return(
        invisible(
          NULL
        )
      )
    } else if (
      .profile_startup_outer
    ) {

      message(
        "[startup] Caché ausente/desactualizado; usando inicialización completa."
      )
    }
  }

  evalq({
    # Perfilador liviano de arranque. Activar antes de shiny::runApp() con:
    # options(ana.profile.startup = TRUE)
    .profile_startup <- isTRUE(getOption("ana.profile.startup", FALSE))
    .startup_t0 <- proc.time()[["elapsed"]]
    .startup_mark <- function(label) {
      if (.profile_startup) {
        now <- proc.time()[["elapsed"]]
        message(
          sprintf(
            "[startup] %-28s %7.3f s",
            label,
            now - .startup_t0
          )
        )
      }
      invisible(NULL)
    }

    .repair_legacy_utf8 <- function(x) {
      x <- gsub(
        "PrecipitaciC3n",
        "Precipitación",
        as.character(x),
        fixed = TRUE
      )
      Encoding(x) <- "UTF-8"
      x
    }

    # 1. CONFIGURACIÓN
    # ----------------------------------------------------------------------------

    # La app pública es autocontenida.
    # Todos los datos derivados necesarios se encuentran en ./data
    DIR_NORM <- "data"

    F_CATALOGO <- file.path(DIR_NORM, "01_catalogo_series.csv")
    F_DIARIO <- file.path(DIR_NORM, "01_disponibilidad_diaria.rds")
    F_ESTACIONES <- file.path(DIR_NORM, "01_inventario_estaciones_validado.csv")
    F_SIN_SERIE <- file.path(DIR_NORM, "01_estaciones_sin_serie.csv")
    F_RAW_INDEX <- file.path(DIR_NORM, "02_indice_raw_xlsx.csv")

    BUFFER_DEFAULT_KM <- 50
    UMBRAL_DEFAULT <- 90

    if (!file.exists(F_CATALOGO)) stop("No existe: ", F_CATALOGO)
    if (!file.exists(F_DIARIO)) stop("No existe: ", F_DIARIO)
    if (!file.exists(F_ESTACIONES)) stop("No existe: ", F_ESTACIONES)

    # ----------------------------------------------------------------------------
    # 2. DATOS NORMALIZADOS
    # ----------------------------------------------------------------------------

    CAT <- fread(F_CATALOGO, encoding = "UTF-8", na.strings = c("", "NA", "NaN"))

    required_catalog_cols <- c(
      "series_id",
      "station_id",
      "id_config",
      "altitud_msnm",
      "codigo_estacion",
      "nombre_estacion",
      "tipo_dato",
      "variable",
      "unidad",
      "latitud",
      "longitud",
      "expected_obs_day"
    )

    missing_catalog_cols <- setdiff(
      required_catalog_cols,
      names(CAT)
    )

    if (length(missing_catalog_cols)) {
      stop(
        "El catálogo normalizado no contiene columnas requeridas: ",
        paste(missing_catalog_cols, collapse = ", ")
      )
    }

    DAY <- as.data.table(readRDS(F_DIARIO))
    .startup_mark("catálogo + diario")

    INV_ST <- fread(
      F_ESTACIONES,
      encoding = "UTF-8",
      na.strings = c("", "NA", "NaN")
    )

    SIN_SERIE <- if (file.exists(F_SIN_SERIE)) {
      fread(
        F_SIN_SERIE,
        encoding = "UTF-8",
        na.strings = c("", "NA", "NaN")
      )
    } else {
      data.table()
    }


    RAW_INDEX <- if (file.exists(F_RAW_INDEX)) {
      fread(
        F_RAW_INDEX,
        encoding = "UTF-8",
        na.strings = c("", "NA", "NaN")
      )
    } else {
      data.table()
    }

    if (nrow(RAW_INDEX)) {

      required_raw_cols <- c(
        "IDConfig",
        "ruta_relativa"
      )

      missing_raw_cols <- setdiff(
        required_raw_cols,
        names(RAW_INDEX)
      )

      if (length(missing_raw_cols)) {
        stop(
          "Faltan columnas en 02_indice_raw_xlsx.csv: ",
          paste(
            missing_raw_cols,
            collapse = ", "
          )
        )
      }

      RAW_INDEX[, IDConfig := as.character(IDConfig)]
      RAW_INDEX[, ruta_relativa := gsub(
        "\\\\",
        "/",
        as.character(ruta_relativa)
      )]

      if (anyDuplicated(RAW_INDEX$IDConfig)) {
        stop(
          "02_indice_raw_xlsx.csv contiene IDConfig duplicados."
        )
      }

      setkey(
        RAW_INDEX,
        IDConfig
      )
    }


    CAT[, `:=`(
      series_id = as.character(series_id),
      station_id = as.character(station_id),
      codigo_estacion = as.character(codigo_estacion),
      nombre_estacion = as.character(nombre_estacion),
      tipo_dato = as.character(tipo_dato),
      variable = as.character(variable),
      unidad = as.character(unidad),
      id_config = as.character(id_config),
      altitud_msnm = suppressWarnings(as.numeric(altitud_msnm)),
      latitud = suppressWarnings(as.numeric(latitud)),
      longitud = suppressWarnings(as.numeric(longitud)),
      expected_obs_day = suppressWarnings(as.integer(expected_obs_day))
    )]

    DAY[, `:=`(
      series_id = .repair_legacy_utf8(series_id),
      station_id = .repair_legacy_utf8(station_id),
      tipo_dato = .repair_legacy_utf8(tipo_dato),
      fecha = as.IDate(fecha),
      n_obs_validas = suppressWarnings(as.integer(n_obs_validas)),
      expected_obs_day = suppressWarnings(as.integer(expected_obs_day))
    )]

    DAY[
      is.na(expected_obs_day) | expected_obs_day < 1L,
      expected_obs_day := 1L
    ]
    DAY[
      ,
      n_obs_validas := pmin(
        pmax(fcoalesce(n_obs_validas, 0L), 0L),
        expected_obs_day
      )
    ]

    CAT <- CAT[
      tipo_dato %chin% c("Precipitación", "Caudal") &
        !is.na(latitud) & !is.na(longitud) &
        between(latitud, -90, 90) & between(longitud, -180, 180)
    ]
    DAY <- DAY[
      tipo_dato %chin% c(
        "Precipitación",
        "Caudal"
      ) &
        !is.na(
          fecha
        ),
      .(
        series_id,
        fecha,
        n_obs_validas,
        expected_obs_day
      )
    ]

    # DAY es disponibilidad temporal. La metadata repetida por cada día
    # ya existe en SERIES y no vuelve a ser utilizada por la aplicación.
    setkey(
      DAY,
      series_id,
      fecha
    )

    SPAN <- DAY[n_obs_validas > 0,
                .(primera_fecha = min(fecha),
                  ultima_fecha = max(fecha),
                  n_dias_con_dato_total = uniqueN(fecha)),
                by = series_id]

    SERIES <- merge(CAT, SPAN, by = "series_id", all.x = TRUE)
    .startup_mark("SPAN + SERIES base")


    # Resumen propio por serie para etiquetas y diagnóstico individual.
    SERIES_OBS_SUM <- DAY[, .(
      n_obs_validas_total = sum(n_obs_validas, na.rm = TRUE),
      expected_obs_day_day = {
        z <- expected_obs_day[!is.na(expected_obs_day) & expected_obs_day >= 1]
        if (length(z)) z[1] else 1L
      }
    ), by = series_id]

    SERIES <- merge(
      SERIES,
      SERIES_OBS_SUM,
      by = "series_id",
      all.x = TRUE
    )

    SERIES[
      is.na(expected_obs_day) | expected_obs_day < 1,
      expected_obs_day := expected_obs_day_day
    ]

    SERIES[
      is.na(expected_obs_day) | expected_obs_day < 1,
      expected_obs_day := 1L
    ]

    SERIES[, dias_nominales := fifelse(
      !is.na(primera_fecha) & !is.na(ultima_fecha),
      as.integer(ultima_fecha - primera_fecha) + 1L,
      NA_integer_
    )]

    SERIES[, anios_nominales := dias_nominales / 365.2425]

    SERIES[, completitud_propia_pct := fifelse(
      !is.na(dias_nominales) & dias_nominales > 0,
      100 * n_obs_validas_total / (dias_nominales * expected_obs_day),
      NA_real_
    )]

    SERIES[, expected_obs_day_day := NULL]

    # Índices secundarios para consultas repetidas sin reordenar físicamente.
    setindexv(SERIES, c("station_id", "tipo_dato"))
    setindexv(SERIES, "id_config")
    .startup_mark("resumen por serie")

    # ----------------------------------------------------------------------------
    # INVENTARIO ESPACIAL DE LA APP
    #
    # REGLA:
    # - Estaciones CON serie: metadata del catálogo normalizado/XLSX oficial.
    # - Estaciones SIN serie: se anexan desde 01_estaciones_sin_serie.csv.
    #
    # No se usa codigo_estacion del inventario bruto como clave para enlazar
    # estaciones con SERIES.
    # ----------------------------------------------------------------------------

    ST_CON_SERIE <- SERIES[
      tipo_dato %chin% c("Precipitación", "Caudal") &
        !is.na(latitud) &
        !is.na(longitud) &
        between(latitud, -90, 90) &
        between(longitud, -180, 180),
      .(
        codigo_estacion = first(codigo_estacion),
        nombre_estacion = first(nombre_estacion),
        latitud = first(latitud),
        longitud = first(longitud),
        tipo_estacion = paste(
          unique(na.omit(tipo_estacion[tipo_estacion != ""])),
          collapse = " | "
        ),
        unidad_hidrografica = paste(
          unique(na.omit(unidad_hidrografica[unidad_hidrografica != ""])),
          collapse = " | "
        ),
        aaa = paste(
          unique(na.omit(aaa[aaa != ""])),
          collapse = " | "
        ),
        ala = paste(
          unique(na.omit(ala[ala != ""])),
          collapse = " | "
        ),
        estado_tipo_ana = NA_character_,
        CodigoUH = NA_character_,
        cuenca_ana = NA_character_,
        n_series = uniqueN(series_id),
        tiene_serie = TRUE
      ),
      by = .(
        tipo_dato,
        station_id
      )
    ]


    # ---------------------------------------------------------------------
    # Las estaciones sin serie son pocas y no necesitan enlazarse con SERIES.
    # Se les asigna un station_id propio basado en los identificadores backend
    # de ANA para evitar cualquier colisión con códigos de estación.
    # ---------------------------------------------------------------------

    if (
      nrow(SIN_SERIE) > 0
    ) {

      # Crear columnas faltantes de forma defensiva.
      for (nm in c(
        "CodigoUH",
        "pIdVariable",
        "pIDMapa",
        "IDRegistro",
        "codigo_estacion",
        "nombre_estacion",
        "latitud",
        "longitud",
        "estado_tipo",
        "cuenca"
      )) {
        if (!nm %in% names(SIN_SERIE)) {
          SIN_SERIE[, (nm) := NA]
        }
      }

      SIN_SERIE[, `:=`(
        CodigoUH = as.character(CodigoUH),
        pIdVariable = suppressWarnings(as.integer(pIdVariable)),
        pIDMapa = suppressWarnings(as.integer(pIDMapa)),
        IDRegistro = as.character(IDRegistro),
        codigo_estacion = as.character(codigo_estacion),
        nombre_estacion = as.character(nombre_estacion),
        latitud = suppressWarnings(as.numeric(latitud)),
        longitud = suppressWarnings(as.numeric(longitud)),
        estado_tipo = as.character(estado_tipo),
        cuenca = as.character(cuenca)
      )]

      SIN_SERIE[, tipo_dato := fifelse(
        pIdVariable == 22L,
        "Precipitación",
        fifelse(
          pIdVariable == 21L,
          "Caudal",
          NA_character_
        )
      )]

      SIN_SERIE[, station_id := paste0(
        tipo_dato,
        "|SIN_SERIE|",
        CodigoUH,
        "|",
        pIDMapa,
        "|",
        IDRegistro
      )]

      ST_SIN_SERIE <- SIN_SERIE[
        tipo_dato %chin% c("Precipitación", "Caudal") &
          !is.na(latitud) &
          !is.na(longitud) &
          between(latitud, -90, 90) &
          between(longitud, -180, 180),
        .(
          tipo_dato,
          station_id,
          codigo_estacion,
          nombre_estacion,
          latitud,
          longitud,
          tipo_estacion = estado_tipo,
          unidad_hidrografica = cuenca,
          aaa = NA_character_,
          ala = NA_character_,
          estado_tipo_ana = estado_tipo,
          CodigoUH,
          cuenca_ana = cuenca,
          n_series = 0L,
          tiene_serie = FALSE
        )
      ]

    } else {

      ST_SIN_SERIE <- data.table(
        tipo_dato = character(),
        station_id = character(),
        codigo_estacion = character(),
        nombre_estacion = character(),
        latitud = double(),
        longitud = double(),
        tipo_estacion = character(),
        unidad_hidrografica = character(),
        aaa = character(),
        ala = character(),
        estado_tipo_ana = character(),
        CodigoUH = character(),
        cuenca_ana = character(),
        n_series = integer(),
        tiene_serie = logical()
      )
    }


    STATIONS <- rbindlist(
      list(
        ST_CON_SERIE,
        ST_SIN_SERIE
      ),
      use.names = TRUE,
      fill = TRUE
    )

    # Protección contra duplicados exactos de identidad interna.
    STATIONS <- unique(
      STATIONS,
      by = c(
        "tipo_dato",
        "station_id"
      )
    )

    STATIONS_SF <- st_as_sf(
      STATIONS,
      coords = c("longitud", "latitud"),
      crs = 4326,
      remove = FALSE
    )

    setindexv(STATIONS, c("tipo_dato", "station_id"))
    .startup_mark("inventario espacial")

    FMIN <- min(DAY$fecha, na.rm = TRUE)
    FMAX <- max(DAY$fecha, na.rm = TRUE)
    FDEF0 <- max(FMIN, as.IDate("1981-01-01"))
    FDEF1 <- min(FMAX, as.IDate("2025-12-31"))


    # Resumen anual por serie para búsquedas recursivas de ventanas N-años.
    # Se calcula una sola vez al iniciar la app. El año se deriva dentro del
    # agrupamiento para no conservar una columna redundante en DAY.
    YEAR_OBS <- DAY[, .(
      n_obs_validas = sum(n_obs_validas, na.rm = TRUE),
      n_dias_alguna = sum(n_obs_validas > 0, na.rm = TRUE),
      n_dias_completos = sum(n_obs_validas >= expected_obs_day, na.rm = TRUE),
      n_dias_parciales = sum(n_obs_validas > 0 & n_obs_validas < expected_obs_day, na.rm = TRUE),
      expected_obs_day = {
        z <- expected_obs_day[!is.na(expected_obs_day) & expected_obs_day >= 1]
        if (length(z)) z[1] else 1L
      }
    ), by = .(
      series_id,
      anio = year(fecha)
    )]

    # DAY ya fue adelgazado y no conserva metadata repetida por día.
    # Recuperar station_id y tipo_dato desde SERIES, donde existen una sola
    # vez por series_id. Esto mantiene YEAR_OBS idéntico para el resto de la app.
    YEAR_META <- SERIES[
      ,
      .(
        station_id = first(station_id),
        tipo_dato = first(tipo_dato)
      ),
      by = series_id
    ]

    YEAR_OBS <- merge(
      YEAR_OBS,
      YEAR_META,
      by = "series_id",
      all.x = TRUE,
      sort = FALSE
    )

    setcolorder(
      YEAR_OBS,
      c(
        "series_id",
        "station_id",
        "tipo_dato",
        "anio",
        "n_obs_validas",
        "n_dias_alguna",
        "n_dias_completos",
        "n_dias_parciales",
        "expected_obs_day"
      )
    )

    setkey(YEAR_OBS, series_id, anio)
    setindexv(YEAR_OBS, c("station_id", "tipo_dato"))
    .startup_mark("YEAR_OBS")

    # ----------------------------------------------------------------------------
    # 3. REFERENCIAS OMM/WMO: km² por estación
    # ----------------------------------------------------------------------------

    WMO_DENSITY <- data.table(
      region = c("Costera", "Montañosa", "Llanura interior", "Ondulada / colinosa", "Islas pequeñas", "Polar / árida"),
      precip_no_reg = c(900, 250, 575, 575, 25, 10000),
      precip_reg = c(9000, 2500, 5750, 5750, 250, 100000),
      caudal = c(2750, 1000, 1875, 1875, 300, 20000)
    )

    .startup_mark("TOTAL init_core_data")
    rm(.startup_mark, .startup_t0, .profile_startup)

  }, envir = target_env)

  invisible(NULL)
}

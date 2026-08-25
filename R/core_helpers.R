# 4. HELPERS
# ----------------------------------------------------------------------------

url_visor_ana <- "https://snirh.ana.gob.pe/VisorPorCuenca/"

url_repo_raw <- "https://github.com/JamilRamirez/ANA-SNIRH-Official-Reports"
url_raw_base <- "https://raw.githubusercontent.com/JamilRamirez/ANA-SNIRH-Official-Reports/main"

encode_path_segments <- function(path) {

  if (
    !length(path) ||
    is.na(path) ||
    !nzchar(path)
  ) {
    return(NA_character_)
  }

  path <- gsub(
    "\\\\",
    "/",
    path
  )

  pieces <- strsplit(
    path,
    "/",
    fixed = TRUE
  )[[1]]

  encoded <- vapply(
    pieces,
    utils::URLencode,
    character(1),
    reserved = TRUE
  )

  paste(
    encoded,
    collapse = "/"
  )
}

raw_report_url <- function(id_config) {

  if (
    !nrow(RAW_INDEX) ||
    !length(id_config) ||
    is.na(id_config)
  ) {
    return(NA_character_)
  }

  id_txt <- as.character(
    id_config
  )

  hit <- RAW_INDEX[
    IDConfig == id_txt
  ]

  if (!nrow(hit)) {
    return(NA_character_)
  }

  paste0(
    url_raw_base,
    "/",
    encode_path_segments(
      hit$ruta_relativa[1]
    )
  )
}


fmt_num <- function(x, digits = 0) {
  if (!length(x) || is.na(x)) return("—")
  format(round(x, digits), big.mark = ",", nsmall = digits)
}

fmt_pct <- function(x, digits = 1) {
  if (!length(x) || is.na(x)) return("—")
  paste0(format(round(x, digits), nsmall = digits), "%")
}

metric_card <- function(title, value, subtitle = NULL) {
  div(
    class = "metric-card p-3 h-100",
    div(title, class = "metric-card-title"),
    div(value, class = "metric-card-value"),
    if (!is.null(subtitle)) div(subtitle, class = "metric-card-subtitle")
  )
}

dt_opts <- function(n = 25) {

  export_all <- list(
    modifier = list(
      page = "all",
      search = "applied"
    )
  )

  list(
    dom = '<"dt-toolbar"Bf>rtip',
    buttons = list(
      list(
        extend = "copy",
        text = "Copiar",
        exportOptions = export_all
      ),
      list(
        extend = "csv",
        exportOptions = export_all
      ),
      list(
        extend = "excel",
        exportOptions = export_all
      )
    ),
    pageLength = n,
    scrollX = TRUE,
    autoWidth = TRUE,
    language = list(
      search = "Buscar:",
      lengthMenu = "Mostrar _MENU_ registros",
      info = "Mostrando _START_ a _END_ de _TOTAL_",
      zeroRecords = "Sin resultados",
      paginate = list(
        previous = "Anterior",
        `next` = "Siguiente"
      )
    )
  )
}

# ----------------------------------------------------------------------------
# Lectura espacial multigeometría
# ----------------------------------------------------------------------------

normalize_field_name <- function(x) {
  x <- iconv(
    as.character(x),
    from = "",
    to = "ASCII//TRANSLIT"
  )
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

geometry_label_field <- function(x) {

  sf_col <- attr(x, "sf_column")
  cols <- setdiff(names(x), sf_col)

  if (!length(cols)) {
    return(NA_character_)
  }

  usable <- vapply(
    cols,
    function(nm) {
      z <- x[[nm]]
      if (is.list(z)) return(FALSE)
      z <- trimws(as.character(z))
      any(!is.na(z) & nzchar(z))
    },
    logical(1)
  )

  cols <- cols[usable]

  if (!length(cols)) {
    return(NA_character_)
  }

  norm <- normalize_field_name(cols)

  # Primero nombres semánticos; luego identificadores.
  exact_priority <- c(
    "nombre", "name", "label", "title", "nombre_cuenca", "nom_cuenca",
    "nom", "nomb", "nom_uh", "nombre_uh",
    "cuenca", "subcuenca", "basin", "watershed",
    "unidad_hidrografica", "unidad_hidrologica", "uh",
    "codigo", "code", "id", "id_geom", "geometry_id", "fid"
  )

  for (candidate in exact_priority) {
    hit <- which(norm == candidate)
    if (length(hit)) return(cols[hit[1]])
  }

  pattern_priority <- c(
    "nombre", "name", "(^|_)nom($|_)", "^nom_", "nomb", "label", "title",
    "subcuenca", "cuenca", "basin", "watershed",
    "unidad.*hidro", "codigo", "code", "(^|_)id($|_)"
  )

  for (pat in pattern_priority) {
    hit <- grep(pat, norm)
    if (length(hit)) return(cols[hit[1]])
  }

  NA_character_
}

normalize_spatial_layer <- function(x, path, layer_name = NA_character_) {

  if (is.null(x) || !inherits(x, "sf") || !nrow(x)) {
    return(NULL)
  }

  if (is.na(st_crs(x))) {
    stop(
      "La capa ",
      ifelse(is.na(layer_name), basename(path), layer_name),
      " no tiene CRS definido."
    )
  }

  x <- suppressWarnings(st_make_valid(x))
  x <- x[!st_is_empty(x), ]

  if (!nrow(x)) {
    return(NULL)
  }

  label_field <- geometry_label_field(x)

  labels <- if (!is.na(label_field)) {
    trimws(as.character(x[[label_field]]))
  } else {
    rep(NA_character_, nrow(x))
  }

  labels[is.na(labels) | !nzchar(labels)] <- NA_character_

  x <- st_transform(x, 4326)

  st_sf(
    geometry_name_raw = labels,
    geometry_label_field = if (is.na(label_field)) NA_character_ else label_field,
    source_file = basename(path),
    source_layer = if (is.na(layer_name) || !nzchar(layer_name)) {
      tools::file_path_sans_ext(basename(path))
    } else {
      as.character(layer_name)
    },
    source_feature = seq_len(nrow(x)),
    geometry_type = as.character(st_geometry_type(x)),
    geometry = st_geometry(x),
    crs = 4326
  )
}

read_vector_source <- function(path) {

  ext <- tolower(tools::file_ext(path))

  # SHP = una capa directa.
  if (ext == "shp") {

    x <- st_read(
      path,
      quiet = TRUE,
      stringsAsFactors = FALSE
    )

    return(
      normalize_spatial_layer(
        x,
        path = path,
        layer_name = tools::file_path_sans_ext(basename(path))
      )
    )
  }

  # KML/GPKG pueden contener varias capas.
  layers <- tryCatch(
    st_layers(path)$name,
    error = function(e) character()
  )

  if (!length(layers)) {

    x <- tryCatch(
      st_read(
        path,
        quiet = TRUE,
        stringsAsFactors = FALSE
      ),
      error = function(e) NULL
    )

    return(
      normalize_spatial_layer(
        x,
        path = path,
        layer_name = tools::file_path_sans_ext(basename(path))
      )
    )
  }

  xs <- lapply(
    layers,
    function(lyr) {
      x <- tryCatch(
        st_read(
          path,
          layer = lyr,
          quiet = TRUE,
          stringsAsFactors = FALSE
        ),
        error = function(e) NULL
      )

      normalize_spatial_layer(
        x,
        path = path,
        layer_name = lyr
      )
    }
  )

  xs <- Filter(
    function(x) !is.null(x) && nrow(x) > 0,
    xs
  )

  if (!length(xs)) {
    return(NULL)
  }

  do.call(rbind, xs)
}


read_uploaded_spatial <- function(file_df) {

  if (
    is.null(file_df) ||
    !nrow(file_df)
  ) {
    stop("No se recibió archivo espacial.")
  }

  tmpdir <- tempfile("area_")
  dir.create(
    tmpdir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  copied <- character(nrow(file_df))

  for (i in seq_len(nrow(file_df))) {

    nm <- basename(as.character(file_df$name[i]))
    dst <- file.path(tmpdir, nm)

    ok <- file.copy(
      file_df$datapath[i],
      dst,
      overwrite = TRUE
    )

    if (!ok) {
      stop("No se pudo copiar el archivo temporal: ", nm)
    }

    copied[i] <- dst
  }

  # ------------------------------------------------------------------
  # ZIP genérico:
  # puede contener un shapefile completo, GPKG, KML o incluso KMZ.
  # ------------------------------------------------------------------
  zip_files <- copied[
    tolower(tools::file_ext(copied)) == "zip"
  ]

  zip_extracted <- character()

  if (length(zip_files)) {

    for (i in seq_along(zip_files)) {

      z <- zip_files[i]

      unzip_dir <- file.path(
        tmpdir,
        paste0("_zip_", i)
      )

      dir.create(
        unzip_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )

      utils::unzip(
        z,
        exdir = unzip_dir
      )

      zip_extracted <- c(
        zip_extracted,
        list.files(
          unzip_dir,
          recursive = TRUE,
          full.names = TRUE
        )
      )
    }
  }

  # ------------------------------------------------------------------
  # KMZ:
  # KMZ es un contenedor ZIP de Google Earth. Se descomprime localmente
  # y se procesa el KML interno con el mismo lector usado para .kml.
  #
  # Si existe doc.kml se toma como documento principal. Si no existe,
  # se procesan todos los .kml encontrados dentro del KMZ.
  # ------------------------------------------------------------------
  pre_kmz_files <- unique(
    c(
      copied,
      zip_extracted
    )
  )

  pre_kmz_exts <- tolower(
    tools::file_ext(
      pre_kmz_files
    )
  )

  kmz_files <- pre_kmz_files[
    pre_kmz_exts == "kmz"
  ]

  kmz_kml_files <- character()
  kmz_origin <- character()

  if (length(kmz_files)) {

    for (i in seq_along(kmz_files)) {

      z <- kmz_files[i]

      kmz_dir <- file.path(
        tmpdir,
        paste0("_kmz_", i)
      )

      dir.create(
        kmz_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )

      unzip_ok <- tryCatch(
        {
          utils::unzip(
            z,
            exdir = kmz_dir
          )
          TRUE
        },
        error = function(e) {
          stop(
            "No se pudo descomprimir el KMZ ",
            basename(z),
            ": ",
            conditionMessage(e)
          )
        }
      )

      if (!isTRUE(unzip_ok)) {
        next
      }

      inside <- list.files(
        kmz_dir,
        recursive = TRUE,
        full.names = TRUE
      )

      kml_inside <- inside[
        tolower(
          tools::file_ext(
            inside
          )
        ) == "kml"
      ]

      if (!length(kml_inside)) {
        stop(
          "El archivo KMZ ",
          basename(z),
          " no contiene ningún archivo KML utilizable."
        )
      }

      # doc.kml es el documento raíz convencional de un KMZ.
      doc_idx <- which(
        tolower(
          basename(
            kml_inside
          )
        ) == "doc.kml"
      )

      if (length(doc_idx)) {
        selected_kml <- kml_inside[doc_idx[1]]
      } else {
        selected_kml <- kml_inside
      }

      kmz_kml_files <- c(
        kmz_kml_files,
        selected_kml
      )

      kmz_origin <- c(
        kmz_origin,
        rep(
          basename(z),
          length(selected_kml)
        )
      )
    }
  }

  # Mapa path KML extraído -> nombre del KMZ original.
  kmz_source_map <- if (length(kmz_kml_files)) {
    setNames(
      kmz_origin,
      normalizePath(
        kmz_kml_files,
        winslash = "/",
        mustWork = FALSE
      )
    )
  } else {
    character()
  }

  all_files <- unique(
    c(
      copied,
      zip_extracted,
      kmz_kml_files
    )
  )

  exts <- tolower(
    tools::file_ext(
      all_files
    )
  )

  # ------------------------------------------------------------------
  # Validación de shapefile cuando se cargan componentes sueltos.
  # ------------------------------------------------------------------
  shp_files <- all_files[
    exts == "shp"
  ]

  if (length(shp_files)) {

    for (shp in shp_files) {

      stem <- tools::file_path_sans_ext(shp)
      dbf <- paste0(stem, ".dbf")
      shx <- paste0(stem, ".shx")
      prj <- paste0(stem, ".prj")

      faltan <- c(
        if (!file.exists(dbf)) ".dbf",
        if (!file.exists(shx)) ".shx",
        if (!file.exists(prj)) ".prj"
      )

      if (length(faltan)) {
        stop(
          "Shapefile incompleto. Para ",
          basename(shp),
          " faltan: ",
          paste(faltan, collapse = ", "),
          ". Puede seleccionar juntos .shp + .shx + .dbf + .prj o subir un ZIP."
        )
      }
    }
  }

  # Los .kmz no se pasan directamente a sf: ya fueron descomprimidos.
  sources <- unique(
    c(
      all_files[exts == "gpkg"],
      all_files[exts == "kml"],
      shp_files
    )
  )

  if (!length(sources)) {
    stop(
      paste(
        "Formato no reconocido.",
        "Use KML, KMZ, GPKG, ZIP con shapefile o seleccione juntos",
        ".shp + .shx + .dbf + .prj."
      )
    )
  }

  xs <- lapply(
    sources,
    function(path) {

      ans <- tryCatch(
        read_vector_source(path),
        error = function(e) {
          structure(
            list(
              error = conditionMessage(e),
              path = path
            ),
            class = "spatial_read_error"
          )
        }
      )

      if (
        !inherits(ans, "spatial_read_error") &&
        !is.null(ans) &&
        inherits(ans, "sf") &&
        nrow(ans) > 0
      ) {

        key <- normalizePath(
          path,
          winslash = "/",
          mustWork = FALSE
        )

        if (key %in% names(kmz_source_map)) {
          ans$source_file <- unname(
            kmz_source_map[[key]]
          )
        }
      }

      ans
    }
  )

  errores <- vapply(
    xs,
    inherits,
    logical(1),
    what = "spatial_read_error"
  )

  if (all(errores)) {
    msgs <- vapply(
      xs,
      function(e) paste0(basename(e$path), ": ", e$error),
      character(1)
    )

    stop(
      "No se pudo leer ninguna geometría: ",
      paste(msgs, collapse = " | ")
    )
  }

  xs <- xs[!errores]

  xs <- Filter(
    function(x) {
      !is.null(x) &&
        inherits(x, "sf") &&
        nrow(x) > 0
    },
    xs
  )

  if (!length(xs)) {
    stop("No se encontraron geometrías vectoriales utilizables.")
  }

  out <- do.call(
    rbind,
    xs
  )

  if (!nrow(out)) {
    stop("Las capas no contienen geometrías no vacías.")
  }

  out <- suppressWarnings(
    st_make_valid(
      out
    )
  )

  out <- out[
    !st_is_empty(out),
  ]

  # Identidad global estable durante la sesión de carga.
  out$geometry_id <- sprintf(
    "G%03d",
    seq_len(nrow(out))
  )

  fallback <- paste0(
    "Polígono ",
    seq_len(nrow(out))
  )

  nm <- trimws(
    as.character(
      out$geometry_name_raw
    )
  )

  missing_nm <- is.na(nm) | !nzchar(nm)
  nm[missing_nm] <- fallback[missing_nm]

  # Evita nombres ambiguos cuando el atributo elegido se repite.
  out$geometry_name <- make.unique(
    nm,
    sep = " #"
  )

  # Orden de columnas deliberado: identidad primero, geometría al final.
  out <- out[
    ,
    c(
      "geometry_id",
      "geometry_name",
      "geometry_name_raw",
      "geometry_label_field",
      "source_file",
      "source_layer",
      "source_feature",
      "geometry_type",
      attr(out, "sf_column")
    )
  ]

  attr(
    out,
    "uploaded_names"
  ) <- paste(
    file_df$name,
    collapse = " + "
  )

  # El formato informado corresponde a lo que cargó el usuario,
  # no al KML temporal extraído desde un KMZ.
  uploaded_fmt <- unique(
    toupper(
      tools::file_ext(
        as.character(
          file_df$name
        )
      )
    )
  )

  attr(
    out,
    "source_format"
  ) <- paste(
    uploaded_fmt,
    collapse = " + "
  )

  out
}


utm_epsg <- function(x) {
  ctd <- suppressWarnings(st_centroid(st_union(x)))
  xy <- st_coordinates(st_transform(ctd, 4326))[1, ]
  zone <- max(1, min(60, floor((xy["X"] + 180) / 6) + 1))
  if (xy["Y"] >= 0) 32600 + zone else 32700 + zone
}

polygon_area_km2 <- function(x, epsg) {
  typ <- as.character(st_geometry_type(x))
  if (!any(grepl("POLYGON", typ))) return(NA_real_)
  pol <- suppressWarnings(st_collection_extract(x, "POLYGON"))
  if (!nrow(pol)) return(NA_real_)
  as.numeric(sum(st_area(st_transform(pol, epsg)))) / 1e6
}

zone_label <- function(d, b) {
  cut1 <- min(25, b)
  ifelse(d <= 1e-8, "Dentro del área",
         ifelse(d <= cut1, paste0("0–", cut1, " km"),
                ifelse(d <= b, paste0(cut1, "–", b, " km"), "Fuera")))
}

empty_series_evaluation <- function() {

  data.table(
    series_id = character(),
    station_id = character(),
    id_config = character(),
    altitud_msnm = numeric(),
    codigo_estacion = character(),
    nombre_estacion = character(),
    tipo_dato = character(),
    variable = character(),
    unidad = character(),
    expected_obs_day = integer(),
    primera_fecha = as.IDate(character()),
    ultima_fecha = as.IDate(character()),
    dias_nominales = integer(),
    anios_nominales = numeric(),
    n_obs_validas = integer(),
    n_dias_alguna = integer(),
    n_dias_completos = integer(),
    n_dias_parciales = integer(),
    dias_esperados = integer(),
    obs_esperadas = integer(),
    completitud_obs_pct = numeric(),
    cobertura_dias_pct = numeric(),
    dias_completos_pct = numeric(),
    cubre_nominalmente = logical()
  )
}


evaluate_series <- function(ids, f0, f1) {

  ids <- unique(
    as.character(ids)
  )

  fi <- as.IDate(f0)
  ff <- as.IDate(f1)

  if (
    !length(ids) ||
    is.na(fi) ||
    is.na(ff) ||
    fi > ff
  ) {
    return(
      empty_series_evaluation()
    )
  }

  nday <- as.integer(
    ff - fi
  ) + 1L

  base <- SERIES[
    series_id %chin% ids,
    .(
      series_id,
      station_id,
      id_config,
      altitud_msnm,
      codigo_estacion,
      nombre_estacion,
      tipo_dato,
      variable,
      unidad,
      expected_obs_day,
      primera_fecha,
      ultima_fecha,
      dias_nominales,
      anios_nominales
    )
  ]

  if (!nrow(base)) {
    return(
      empty_series_evaluation()
    )
  }

  d <- DAY[
    series_id %chin% ids &
      fecha >= fi &
      fecha <= ff
  ]

  if (nrow(d)) {

    obs <- d[
      ,
      .(
        n_obs_validas = sum(
          n_obs_validas,
          na.rm = TRUE
        ),
        n_dias_alguna = sum(
          n_obs_validas > 0,
          na.rm = TRUE
        ),
        n_dias_completos = sum(
          n_obs_validas >= expected_obs_day,
          na.rm = TRUE
        ),
        n_dias_parciales = sum(
          n_obs_validas > 0 &
            n_obs_validas < expected_obs_day,
          na.rm = TRUE
        )
      ),
      by = series_id
    ]

  } else {

    obs <- data.table(
      series_id = character(),
      n_obs_validas = integer(),
      n_dias_alguna = integer(),
      n_dias_completos = integer(),
      n_dias_parciales = integer()
    )
  }

  out <- merge(
    base,
    obs,
    by = "series_id",
    all.x = TRUE
  )

  for (z in c(
    "n_obs_validas",
    "n_dias_alguna",
    "n_dias_completos",
    "n_dias_parciales"
  )) {
    out[
      is.na(get(z)),
      (z) := 0L
    ]
  }

  out[
    is.na(expected_obs_day) |
      expected_obs_day < 1,
    expected_obs_day := 1L
  ]

  out[, dias_esperados := nday]
  out[, obs_esperadas := nday * expected_obs_day]

  out[
    ,
    completitud_obs_pct :=
      100 *
      n_obs_validas /
      obs_esperadas
  ]

  out[
    ,
    cobertura_dias_pct :=
      100 *
      n_dias_alguna /
      nday
  ]

  out[
    ,
    dias_completos_pct :=
      100 *
      n_dias_completos /
      nday
  ]

  out[
    ,
    cubre_nominalmente :=
      !is.na(primera_fecha) &
      !is.na(ultima_fecha) &
      primera_fecha <= fi &
      ultima_fecha >= ff
  ]

  out
}


best_series <- function(x) {
  if (!nrow(x)) return(x)
  y <- copy(x)
  setorder(y, station_id, -cubre_nominalmente, -completitud_obs_pct,
           -dias_completos_pct, -cobertura_dias_pct, -n_obs_validas)
  y[, .SD[1], by = station_id]
}


# ----------------------------------------------------------------------------
# CONTINUIDAD DENTRO DEL PERIODO SOLICITADO
# ----------------------------------------------------------------------------
#
# La continuidad se resume con la mayor racha de días sin ninguna observación.
# Se calcula sin construir un calendario completo por serie:
#   - vacío inicial;
#   - vacíos entre fechas con dato;
#   - vacío final.
#
# Para series con frecuencia >1 obs/día, un día con al menos una observación
# rompe la racha vacía. La completitud de observaciones sigue evaluándose aparte.
# ----------------------------------------------------------------------------

period_gap_stats <- function(ids, f0, f1) {

  ids <- unique(
    as.character(ids)
  )

  fi <- as.IDate(f0)
  ff <- as.IDate(f1)

  if (
    !length(ids) ||
    is.na(fi) ||
    is.na(ff) ||
    fi > ff
  ) {
    return(
      data.table(
        series_id = character(),
        max_racha_vacia_periodo_dias = integer()
      )
    )
  }

  nday <- as.integer(
    ff - fi
  ) + 1L

  d <- DAY[
    series_id %chin% ids &
      fecha >= fi &
      fecha <= ff &
      n_obs_validas > 0,
    .(
      fecha = sort(
        unique(
          fecha
        )
      )
    ),
    by = series_id
  ]

  # Resultado base: si una serie no tiene ningún día con dato dentro del
  # intervalo, toda la ventana solicitada es una única racha vacía.
  out <- data.table(
    series_id = ids,
    max_racha_vacia_periodo_dias = as.integer(nday)
  )

  if (!nrow(d)) {
    return(out)
  }

  gaps <- d[
    ,
    {
      z <- sort(
        unique(
          fecha
        )
      )

      lead_gap <- as.integer(
        z[1] - fi
      )

      tail_gap <- as.integer(
        ff - z[length(z)]
      )

      internal_gap <- if (length(z) > 1L) {
        max(
          pmax(
            0L,
            as.integer(
              diff(z)
            ) - 1L
          ),
          na.rm = TRUE
        )
      } else {
        0L
      }

      .(
        max_racha_vacia_periodo_dias = as.integer(
          max(
            c(
              lead_gap,
              internal_gap,
              tail_gap
            ),
            na.rm = TRUE
          )
        )
      )
    },
    by = series_id
  ]

  out[
    gaps,
    on = "series_id",
    max_racha_vacia_periodo_dias := i.max_racha_vacia_periodo_dias
  ]

  out
}


# ----------------------------------------------------------------------------
# MEJOR CANDIDATA POR GEOMETRÍA
# ----------------------------------------------------------------------------
#
# No existe criterio absoluto de exclusión por completitud.
# Toda relación área-estación con serie disponible puede competir.
#
# Orden jerárquico:
#   1. mayor completitud en el periodo solicitado;
#   2. menor mayor-racha-vacía dentro del periodo;
#   3. apta según los filtros actuales (solo desempate);
#   4. mayor longitud nominal de la serie;
#   5. menor distancia al área;
#   6. nombre / station_id para desempate determinista.
#
# En modo nacional no se asigna "mejor candidata", porque sin geometría de
# referencia una única estación nacional destacada sería poco interpretable.
# ----------------------------------------------------------------------------

rank_best_candidates <- function(x) {

  y <- copy(x)

  if (!nrow(y)) {
    return(y)
  }

  if (!"mejor_candidata" %in% names(y)) {
    y[, mejor_candidata := FALSE]
  } else {
    y[, mejor_candidata := FALSE]
  }

  if (!"ranking_area" %in% names(y)) {
    y[, ranking_area := NA_integer_]
  } else {
    y[, ranking_area := NA_integer_]
  }

  # Solo tiene sentido priorizar si existe identidad de geometría.
  if (
    !"geometry_id" %in% names(y) ||
    !any(
      !is.na(y$geometry_id) &
        nzchar(y$geometry_id)
    )
  ) {
    return(y)
  }

  # Valores defensivos para ordenar.
  y[, prioridad_comp := fifelse(
    is.na(completitud_obs_pct),
    -Inf,
    completitud_obs_pct
  )]

  y[, prioridad_gap := fifelse(
    is.na(max_racha_vacia_periodo_dias),
    .Machine$integer.max,
    as.integer(max_racha_vacia_periodo_dias)
  )]

  y[, prioridad_apta := fifelse(
    apta %in% TRUE,
    1L,
    0L
  )]

  y[, prioridad_anios := fifelse(
    is.na(anios_nominales),
    -Inf,
    anios_nominales
  )]

  y[, prioridad_dist := fifelse(
    is.na(distancia_km),
    Inf,
    distancia_km
  )]

  eligible <- y[
    tiene_serie %in% TRUE &
      !is.na(series_id) &
      nzchar(series_id) &
      !is.na(geometry_id) &
      nzchar(geometry_id)
  ]

  if (!nrow(eligible)) {

    y[
      ,
      c(
        "prioridad_comp",
        "prioridad_gap",
        "prioridad_apta",
        "prioridad_anios",
        "prioridad_dist"
      ) := NULL
    ]

    return(y)
  }

  setorder(
    eligible,
    geometry_id,
    -prioridad_comp,
    prioridad_gap,
    -prioridad_apta,
    -prioridad_anios,
    prioridad_dist,
    nombre_estacion,
    station_id
  )

  eligible[
    ,
    ranking_area := seq_len(.N),
    by = geometry_id
  ]

  eligible[
    ranking_area == 1L,
    mejor_candidata := TRUE
  ]

  # Actualizar por clave área-estación. En la estructura actual existe una
  # única relación por geometry_id + station_id.
  y[
    eligible[
      ,
      .(
        geometry_id,
        station_id,
        ranking_area,
        mejor_candidata
      )
    ],
    on = .(
      geometry_id,
      station_id
    ),
    `:=`(
      ranking_area = i.ranking_area,
      mejor_candidata = i.mejor_candidata
    )
  ]

  y[
    ,
    c(
      "prioridad_comp",
      "prioridad_gap",
      "prioridad_apta",
      "prioridad_anios",
      "prioridad_dist"
    ) := NULL
  ]

  y
}


search_recursive_windows <- function(
  station_ids,
  tipo,
  n_years = 30L,
  threshold = 90,
  require_nominal = TRUE
) {
  n_years <- as.integer(n_years)
  station_ids <- unique(as.character(station_ids))

  if (!length(station_ids) || is.na(n_years) || n_years < 1) {
    return(list(
      windows = data.table(),
      detail = data.table(),
      eligible_stations = character(),
      n_eligible = 0L
    ))
  }

  ser <- copy(
    SERIES[
      station_id %chin% station_ids &
        tipo_dato == tipo &
        !is.na(primera_fecha) &
        !is.na(ultima_fecha),
      .(
        series_id,
        station_id,
        id_config,
        altitud_msnm,
        codigo_estacion,
        nombre_estacion,
        variable,
        unidad,
        expected_obs_day,
        primera_fecha,
        ultima_fecha
      )
    ]
  )

  if (!nrow(ser)) {
    return(list(
      windows = data.table(),
      detail = data.table(),
      eligible_stations = character(),
      n_eligible = 0L
    ))
  }

  ser[is.na(expected_obs_day) | expected_obs_day < 1, expected_obs_day := 1L]
  ser[, nominal_years := (as.numeric(ultima_fecha - primera_fecha) + 1) / 365.2425]

  eligible_ids <- ser[
    ,
    .(eligible = any(nominal_years >= n_years, na.rm = TRUE)),
    by = station_id
  ][eligible == TRUE, station_id]

  if (!length(eligible_ids)) {
    return(list(
      windows = data.table(),
      detail = data.table(),
      eligible_stations = character(),
      n_eligible = 0L
    ))
  }

  ser <- ser[station_id %chin% eligible_ids]

  y_min <- min(year(ser$primera_fecha), na.rm = TRUE)
  y_max <- max(year(ser$ultima_fecha), na.rm = TRUE)

  if (!is.finite(y_min) || !is.finite(y_max) || (y_max - y_min + 1L) < n_years) {
    return(list(
      windows = data.table(),
      detail = data.table(),
      eligible_stations = eligible_ids,
      n_eligible = length(eligible_ids)
    ))
  }

  years <- seq.int(y_min, y_max)
  ids_series <- unique(ser$series_id)

  grid <- CJ(
    series_id = ids_series,
    anio = years,
    unique = TRUE
  )

  grid <- merge(
    grid,
    ser[, .(
      series_id,
      station_id,
      id_config,
      altitud_msnm,
      codigo_estacion,
      nombre_estacion,
      variable,
      unidad,
      expected_obs_day,
      primera_fecha,
      ultima_fecha
    )],
    by = "series_id",
    all.x = TRUE,
    sort = FALSE
  )

  obs <- YEAR_OBS[
    series_id %chin% ids_series,
    .(
      series_id,
      anio,
      n_obs_validas,
      n_dias_alguna,
      n_dias_completos,
      n_dias_parciales
    )
  ]

  grid <- merge(
    grid,
    obs,
    by = c("series_id", "anio"),
    all.x = TRUE,
    sort = FALSE
  )

  for (z in c(
    "n_obs_validas",
    "n_dias_alguna",
    "n_dias_completos",
    "n_dias_parciales"
  )) {
    grid[is.na(get(z)), (z) := 0]
  }

  grid[, dias_calendario := ifelse(leap_year(anio), 366L, 365L)]
  grid[, obs_esperadas_anio := dias_calendario * expected_obs_day]

  setorder(grid, series_id, anio)

  grid[, `:=`(
    obs_window = frollsum(n_obs_validas, n = n_years, align = "right", fill = NA_real_),
    expected_window = frollsum(obs_esperadas_anio, n = n_years, align = "right", fill = NA_real_),
    days_any_window = frollsum(n_dias_alguna, n = n_years, align = "right", fill = NA_real_),
    days_full_window = frollsum(n_dias_completos, n = n_years, align = "right", fill = NA_real_),
    days_calendar_window = frollsum(dias_calendario, n = n_years, align = "right", fill = NA_real_)
  ), by = series_id]

  grid[, anio_inicio := anio - n_years + 1L]
  grid <- grid[!is.na(obs_window)]

  grid[, fecha_inicio := as.IDate(sprintf("%04d-01-01", anio_inicio))]
  grid[, fecha_fin := as.IDate(sprintf("%04d-12-31", anio))]

  grid[, completitud_obs_pct := 100 * obs_window / expected_window]
  grid[, cobertura_dias_pct := 100 * days_any_window / days_calendar_window]
  grid[, dias_completos_pct := 100 * days_full_window / days_calendar_window]

  grid[, cubre_nominalmente :=
         primera_fecha <= fecha_inicio &
         ultima_fecha >= fecha_fin]

  grid[, apta :=
         completitud_obs_pct >= threshold &
         (!require_nominal | cubre_nominalmente)]

  # Para cada estación y ventana escogemos su mejor serie, sin fusionar IDConfig.
  setorder(
    grid,
    anio_inicio,
    station_id,
    -cubre_nominalmente,
    -completitud_obs_pct,
    -dias_completos_pct,
    -cobertura_dias_pct
  )

  best <- grid[, .SD[1], by = .(anio_inicio, station_id)]

  # Añadir metadata espacial/general de estación.
  st_meta <- STATIONS[
    station_id %chin% eligible_ids,
    .(
      station_id,
      tipo_estacion,
      unidad_hidrografica,
      aaa,
      ala,
      latitud,
      longitud
    )
  ]

  best <- merge(
    best,
    st_meta,
    by = "station_id",
    all.x = TRUE,
    sort = FALSE
  )

  n_eligible <- length(eligible_ids)

  windows <- best[, .(
    n_elegibles = n_eligible,
    n_cubren_nominal = sum(cubre_nominalmente, na.rm = TRUE),
    n_cumplen = sum(apta, na.rm = TRUE),
    pct_cumplen = 100 * sum(apta, na.rm = TRUE) / n_eligible,
    completitud_mediana = median(completitud_obs_pct, na.rm = TRUE),
    completitud_mediana_aptas = if (any(apta, na.rm = TRUE)) {
      median(completitud_obs_pct[apta], na.rm = TRUE)
    } else {
      NA_real_
    }
  ), by = .(
    anio_inicio,
    anio_fin = anio,
    fecha_inicio,
    fecha_fin
  )]

  windows[, universal := n_cumplen == n_elegibles]

  setorder(
    windows,
    -n_cumplen,
    -completitud_mediana_aptas,
    -anio_fin
  )

  windows[, ranking := seq_len(.N)]
  setcolorder(
    windows,
    c("ranking", setdiff(names(windows), "ranking"))
  )

  # Asociar ranking a detalle.
  best <- merge(
    best,
    windows[, .(anio_inicio, ranking)],
    by = "anio_inicio",
    all.x = TRUE,
    sort = FALSE
  )

  list(
    windows = windows,
    detail = best,
    eligible_stations = eligible_ids,
    n_eligible = n_eligible
  )
}


# ----------------------------------------------------------------------------
# TRES COMBINACIONES OPTIMIZADAS DE VENTANAS COMUNES
# ----------------------------------------------------------------------------
#
# A diferencia de search_recursive_windows(), esta función NO fija una única
# longitud. El usuario define un mínimo de años y se evalúan todas las ventanas
# calendario con longitud >= ese mínimo.
#
# El universo de estaciones elegibles se fija con el mínimo solicitado para
# que el número de estaciones sea comparable entre ventanas de distinta
# longitud.
#
# Se generan tres soluciones REPRESENTATIVAS y distintas cuando existen al
# menos tres combinaciones (número de estaciones, longitud) diferentes:
#   1) Máxima cobertura  -> prioriza más estaciones.
#   2) Equilibrada       -> compromiso entre estaciones y longitud mediante
#                           media armónica de ambos objetivos normalizados.
#   3) Máxima longitud   -> prioriza más años.
#
# La completitud mediana se utiliza como criterio de desempate, no como un peso
# arbitrario dentro del compromiso estaciones-longitud.
# ----------------------------------------------------------------------------

search_optimized_windows <- function(
  station_ids,
  tipo,
  min_years = 30L,
  threshold = 90,
  require_nominal = TRUE,
  min_start_year = 1914L
) {

  min_years <- suppressWarnings(
    as.integer(min_years)
  )

  threshold <- suppressWarnings(
    as.numeric(threshold)
  )

  min_start_year <- suppressWarnings(
    as.integer(min_start_year)
  )

  station_ids <- unique(
    as.character(station_ids)
  )

  empty <- function() {
    list(
      solutions = data.table(),
      solution_detail = data.table(),
      pareto = data.table(),
      all_windows = data.table(),
      eligible_stations = character(),
      n_eligible = 0L,
      n_combinations = 0L
    )
  }

  if (
    !length(station_ids) ||
    is.na(min_years) ||
    min_years < 1L ||
    is.na(threshold) ||
    is.na(min_start_year)
  ) {
    return(
      empty()
    )
  }

  ser <- copy(
    SERIES[
      station_id %chin% station_ids &
        tipo_dato == tipo &
        !is.na(primera_fecha) &
        !is.na(ultima_fecha),
      .(
        series_id,
        station_id,
        id_config,
        altitud_msnm,
        codigo_estacion,
        nombre_estacion,
        variable,
        unidad,
        expected_obs_day,
        primera_fecha,
        ultima_fecha
      )
    ]
  )

  if (!nrow(ser)) {
    return(
      empty()
    )
  }

  ser[
    is.na(expected_obs_day) |
      expected_obs_day < 1,
    expected_obs_day := 1L
  ]

  ser[, nominal_years := (
    as.numeric(
      ultima_fecha -
        primera_fecha
    ) + 1
  ) / 365.2425]

  eligible_ids <- ser[
    ,
    .(
      eligible = any(
        nominal_years >= min_years,
        na.rm = TRUE
      )
    ),
    by = station_id
  ][
    eligible %in% TRUE,
    station_id
  ]

  if (!length(eligible_ids)) {
    return(
      empty()
    )
  }

  ser <- ser[
    station_id %chin% eligible_ids
  ]

  y_min <- min(
    year(
      ser$primera_fecha
    ),
    na.rm = TRUE
  )

  y_max <- max(
    year(
      ser$ultima_fecha
    ),
    na.rm = TRUE
  )

  if (
    !is.finite(y_min) ||
    !is.finite(y_max) ||
    (
      y_max -
        y_min +
        1L
    ) < min_years
  ) {

    ans <- empty()
    ans$eligible_stations <- eligible_ids
    ans$n_eligible <- length(
      eligible_ids
    )

    return(ans)
  }

  years <- seq.int(
    as.integer(y_min),
    as.integer(y_max)
  )

  ids_series <- unique(
    ser$series_id
  )

  # ------------------------------------------------------------------
  # Resumen anual completo por serie.
  # ------------------------------------------------------------------

  grid <- CJ(
    series_id = ids_series,
    anio = years,
    unique = TRUE
  )

  obs <- YEAR_OBS[
    series_id %chin% ids_series,
    .(
      series_id,
      anio,
      n_obs_validas,
      n_dias_alguna,
      n_dias_completos,
      n_dias_parciales
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

  for (z in c(
    "n_obs_validas",
    "n_dias_alguna",
    "n_dias_completos",
    "n_dias_parciales"
  )) {
    grid[
      is.na(
        get(z)
      ),
      (z) := 0
    ]
  }

  grid <- merge(
    grid,
    ser[
      ,
      .(
        series_id,
        expected_obs_day
      )
    ],
    by = "series_id",
    all.x = TRUE,
    sort = FALSE
  )

  grid[
    is.na(expected_obs_day) |
      expected_obs_day < 1,
    expected_obs_day := 1L
  ]

  grid[, dias_calendario := fifelse(
    leap_year(anio),
    366L,
    365L
  )]

  grid[, obs_esperadas_anio := (
    dias_calendario *
      expected_obs_day
  )]

  setorder(
    grid,
    series_id,
    anio
  )

  # Acumulados: permiten evaluar TODAS las longitudes sin repetir la
  # construcción del grid para cada N de años.
  grid[
    ,
    `:=`(
      c_obs = cumsum(
        n_obs_validas
      ),
      c_expected = cumsum(
        obs_esperadas_anio
      ),
      c_days_any = cumsum(
        n_dias_alguna
      ),
      c_days_full = cumsum(
        n_dias_completos
      ),
      c_days_calendar = cumsum(
        dias_calendario
      )
    ),
    by = series_id
  ]

  cum_tbl <- grid[
    ,
    .(
      series_id,
      anio,
      c_obs,
      c_expected,
      c_days_any,
      c_days_full,
      c_days_calendar
    )
  ]

  # Fila base para poder restar el acumulado del año anterior al inicio.
  base0 <- data.table(
    series_id = ids_series,
    anio = as.integer(y_min) - 1L,
    c_obs = 0,
    c_expected = 0,
    c_days_any = 0,
    c_days_full = 0,
    c_days_calendar = 0
  )

  cum_tbl <- rbindlist(
    list(
      base0,
      cum_tbl
    ),
    use.names = TRUE
  )

  # ------------------------------------------------------------------
  # Todas las ventanas >= min_years.
  # ------------------------------------------------------------------

  win <- CJ(
    anio_inicio = years,
    anio_fin = years,
    unique = TRUE
  )

  win <- win[
    anio_inicio >= min_start_year &
      anio_fin >= (
        anio_inicio +
          min_years -
          1L
      )
  ]

  if (!nrow(win)) {

    ans <- empty()
    ans$eligible_stations <- eligible_ids
    ans$n_eligible <- length(
      eligible_ids
    )

    return(ans)
  }

  win[, n_years := (
    anio_fin -
      anio_inicio +
      1L
  )]

  win[, window_id := seq_len(.N)]

  win[, fecha_inicio := as.IDate(
    sprintf(
      "%04d-01-01",
      anio_inicio
    )
  )]

  win[, fecha_fin := as.IDate(
    sprintf(
      "%04d-12-31",
      anio_fin
    )
  )]

  # Serie x ventana. Para el rango histórico actual de la app esto es
  # manejable y evita repetir cálculos por cada longitud posible.
  ev <- CJ(
    series_id = ids_series,
    window_id = win$window_id,
    unique = TRUE
  )

  ev <- merge(
    ev,
    win,
    by = "window_id",
    all.x = TRUE,
    sort = FALSE
  )

  # Acumulado al final.
  ev <- merge(
    ev,
    cum_tbl,
    by.x = c(
      "series_id",
      "anio_fin"
    ),
    by.y = c(
      "series_id",
      "anio"
    ),
    all.x = TRUE,
    sort = FALSE
  )

  setnames(
    ev,
    c(
      "c_obs",
      "c_expected",
      "c_days_any",
      "c_days_full",
      "c_days_calendar"
    ),
    c(
      "end_obs",
      "end_expected",
      "end_days_any",
      "end_days_full",
      "end_days_calendar"
    )
  )

  ev[, anio_prev := (
    anio_inicio -
      1L
  )]

  # Acumulado justo antes del inicio.
  ev <- merge(
    ev,
    cum_tbl,
    by.x = c(
      "series_id",
      "anio_prev"
    ),
    by.y = c(
      "series_id",
      "anio"
    ),
    all.x = TRUE,
    sort = FALSE
  )

  setnames(
    ev,
    c(
      "c_obs",
      "c_expected",
      "c_days_any",
      "c_days_full",
      "c_days_calendar"
    ),
    c(
      "start_obs",
      "start_expected",
      "start_days_any",
      "start_days_full",
      "start_days_calendar"
    )
  )

  ev[, `:=`(
    obs_window = end_obs - start_obs,
    expected_window = end_expected - start_expected,
    days_any_window = end_days_any - start_days_any,
    days_full_window = end_days_full - start_days_full,
    days_calendar_window = end_days_calendar - start_days_calendar
  )]

  # Metadata de serie/estación.
  ev <- merge(
    ev,
    ser[
      ,
      .(
        series_id,
        station_id,
        id_config,
        altitud_msnm,
        codigo_estacion,
        nombre_estacion,
        variable,
        unidad,
        expected_obs_day,
        primera_fecha,
        ultima_fecha
      )
    ],
    by = "series_id",
    all.x = TRUE,
    sort = FALSE
  )

  ev[, completitud_obs_pct := fifelse(
    expected_window > 0,
    100 *
      obs_window /
      expected_window,
    NA_real_
  )]

  ev[, cobertura_dias_pct := fifelse(
    days_calendar_window > 0,
    100 *
      days_any_window /
      days_calendar_window,
    NA_real_
  )]

  ev[, dias_completos_pct := fifelse(
    days_calendar_window > 0,
    100 *
      days_full_window /
      days_calendar_window,
    NA_real_
  )]

  ev[, cubre_nominalmente := (
    primera_fecha <= fecha_inicio &
      ultima_fecha >= fecha_fin
  )]

  ev[, apta := (
    completitud_obs_pct >= threshold &
      (
        !require_nominal |
          cubre_nominalmente
      )
  )]

  # Mejor serie por estación dentro de cada ventana. No se fusionan IDConfig.
  setorder(
    ev,
    window_id,
    station_id,
    -cubre_nominalmente,
    -completitud_obs_pct,
    -dias_completos_pct,
    -cobertura_dias_pct,
    series_id
  )

  best <- ev[
    ,
    .SD[1],
    by = .(
      window_id,
      station_id
    )
  ]

  st_meta <- STATIONS[
    station_id %chin% eligible_ids,
    .(
      station_id,
      tipo_estacion,
      unidad_hidrografica,
      aaa,
      ala,
      latitud,
      longitud
    )
  ]

  best <- merge(
    best,
    st_meta,
    by = "station_id",
    all.x = TRUE,
    sort = FALSE
  )

  n_eligible <- length(
    eligible_ids
  )

  windows <- best[
    ,
    .(
      n_elegibles = n_eligible,
      n_cubren_nominal = sum(
        cubre_nominalmente,
        na.rm = TRUE
      ),
      n_cumplen = sum(
        apta,
        na.rm = TRUE
      ),
      pct_cumplen = 100 *
        sum(
          apta,
          na.rm = TRUE
        ) /
        n_eligible,
      completitud_mediana = median(
        completitud_obs_pct,
        na.rm = TRUE
      ),
      completitud_mediana_aptas = if (
        any(
          apta,
          na.rm = TRUE
        )
      ) {
        median(
          completitud_obs_pct[
            apta
          ],
          na.rm = TRUE
        )
      } else {
        NA_real_
      }
    ),
    by = .(
      window_id,
      anio_inicio,
      anio_fin,
      n_years,
      fecha_inicio,
      fecha_fin
    )
  ]

  windows[, universal := (
    n_cumplen ==
      n_elegibles
  )]

  windows[, quality := fifelse(
    is.na(
      completitud_mediana_aptas
    ),
    0,
    completitud_mediana_aptas
  )]

  # Solo ventanas que aportan al menos una estación.
  candidates <- windows[
    n_cumplen > 0
  ]

  if (!nrow(candidates)) {

    ans <- empty()
    ans$all_windows <- windows
    ans$eligible_stations <- eligible_ids
    ans$n_eligible <- n_eligible

    return(ans)
  }

  # Una representación por combinación estaciones-años. Si existen varios
  # periodos con el mismo compromiso, se conserva el de mayor completitud
  # mediana; después el más reciente.
  setorder(
    candidates,
    n_cumplen,
    n_years,
    -quality,
    -anio_fin,
    anio_inicio
  )

  combos <- candidates[
    ,
    .SD[1],
    by = .(
      n_cumplen,
      n_years
    )
  ]

  n_combinations <- nrow(
    combos
  )

  # Frontera de Pareto en los dos objetivos interpretables:
  # número de estaciones y longitud de ventana.
  dominated <- vapply(
    seq_len(
      nrow(combos)
    ),
    function(i) {

      any(
        (
          combos$n_cumplen >=
            combos$n_cumplen[i]
        ) &
          (
            combos$n_years >=
              combos$n_years[i]
          ) &
          (
            (
              combos$n_cumplen >
                combos$n_cumplen[i]
            ) |
              (
                combos$n_years >
                  combos$n_years[i]
              )
          )
      )
    },
    logical(1)
  )

  pareto <- combos[
    !dominated
  ]

  if (!nrow(pareto)) {
    pareto <- copy(
      combos
    )
  }

  # --------------------------------------------------------------
  # Selección de soluciones distintas.
  # --------------------------------------------------------------

  pick_coverage <- copy(
    pareto
  )

  setorder(
    pick_coverage,
    -n_cumplen,
    -n_years,
    -quality,
    -anio_fin
  )

  sol_coverage <- pick_coverage[
    1
  ]

  remaining_after_coverage <- pareto[
    window_id !=
      sol_coverage$window_id[1]
  ]

  if (!nrow(remaining_after_coverage)) {
    remaining_after_coverage <- combos[
      window_id !=
        sol_coverage$window_id[1]
    ]
  }

  sol_length <- data.table()

  if (nrow(remaining_after_coverage)) {

    pick_length <- copy(
      remaining_after_coverage
    )

    setorder(
      pick_length,
      -n_years,
      -n_cumplen,
      -quality,
      -anio_fin
    )

    sol_length <- pick_length[
      1
    ]
  }

  selected_ids <- c(
    sol_coverage$window_id,
    sol_length$window_id
  )

  selected_ids <- selected_ids[
    !is.na(
      selected_ids
    )
  ]

  balance_pool <- pareto[
    !window_id %in%
      selected_ids
  ]

  if (!nrow(balance_pool)) {
    balance_pool <- combos[
      !window_id %in%
        selected_ids
    ]
  }

  sol_balance <- data.table()

  if (nrow(balance_pool)) {

    max_st <- max(
      combos$n_cumplen,
      na.rm = TRUE
    )

    max_yr <- max(
      combos$n_years,
      na.rm = TRUE
    )

    # max_st y max_yr son escalares; no usar fifelse() aquí porque
    # data.table::fifelse exige que test y yes/no tengan longitudes compatibles.
    # Si el máximo es válido, la operación vectorizada se aplica a todas las filas.
    if (
      is.finite(max_st) &&
      max_st > 0
    ) {
      balance_pool[, station_rel := n_cumplen / max_st]
    } else {
      balance_pool[, station_rel := 0]
    }

    if (
      is.finite(max_yr) &&
      max_yr > 0
    ) {
      balance_pool[, years_rel := n_years / max_yr]
    } else {
      balance_pool[, years_rel := 0]
    }

    balance_pool[, balance_score := fifelse(
      (
        station_rel +
          years_rel
      ) > 0,
      2 *
        station_rel *
        years_rel /
        (
          station_rel +
            years_rel
        ),
      0
    )]

    setorder(
      balance_pool,
      -balance_score,
      -quality,
      -n_cumplen,
      -n_years,
      -anio_fin
    )

    sol_balance <- balance_pool[
      1
    ]
  }

  sol_list <- list()

  if (nrow(sol_coverage)) {
    sol_coverage[, `:=`(
      solution_id = "S1",
      solution_order = 1L,
      solution_name = "Máxima cobertura"
    )]
    sol_list[[length(sol_list) + 1L]] <- sol_coverage
  }

  if (nrow(sol_balance)) {
    sol_balance[, `:=`(
      solution_id = "S2",
      solution_order = 2L,
      solution_name = "Equilibrada"
    )]
    sol_list[[length(sol_list) + 1L]] <- sol_balance
  }

  if (nrow(sol_length)) {
    sol_length[, `:=`(
      solution_id = "S3",
      solution_order = 3L,
      solution_name = "Máxima longitud"
    )]
    sol_list[[length(sol_list) + 1L]] <- sol_length
  }

  solutions <- rbindlist(
    sol_list,
    use.names = TRUE,
    fill = TRUE
  )

  if (nrow(solutions)) {

    # Si por alguna razón extrema dos categorías terminaran sobre la misma
    # combinación, conservar solo una. Normalmente no ocurre porque la selección
    # se hace sobre combinaciones estaciones-años distintas.
    solutions <- unique(
      solutions,
      by = c(
        "n_cumplen",
        "n_years"
      )
    )

    setorder(
      solutions,
      solution_order
    )
  }

  solution_detail <- data.table()

  if (nrow(solutions)) {

    sol_map <- solutions[
      ,
      .(
        window_id,
        solution_id,
        solution_order,
        solution_name
      )
    ]

    solution_detail <- merge(
      best[
        window_id %in%
          solutions$window_id
      ],
      sol_map,
      by = "window_id",
      all.x = TRUE,
      sort = FALSE
    )

    setorder(
      solution_detail,
      solution_order,
      -apta,
      -completitud_obs_pct,
      station_id
    )
  }

  windows[, quality := NULL]

  list(
    solutions = solutions,
    solution_detail = solution_detail,
    pareto = pareto,
    all_windows = windows,
    eligible_stations = eligible_ids,
    n_eligible = n_eligible,
    n_combinations = n_combinations
  )
}



# ----------------------------------------------------------------------------
# 4B. DIAGNÓSTICO INDIVIDUAL DE UNA SERIE
# ----------------------------------------------------------------------------

longest_consecutive_year_block <- function(years, ok) {

  x <- data.table(
    anio = suppressWarnings(as.integer(years)),
    ok = as.logical(ok)
  )

  x <- x[
    !is.na(anio)
  ]

  if (!nrow(x)) {
    return(
      list(
        n_anios = 0L,
        anio_inicio = NA_integer_,
        anio_fin = NA_integer_
      )
    )
  }

  setorder(
    x,
    anio
  )

  # Una nueva racha empieza si cambia el estado o si los años dejan de ser
  # consecutivos. Esto mantiene el cálculo correcto incluso si faltara algún
  # año en la tabla anual por cualquier motivo.
  breaks <- rep(
    FALSE,
    nrow(x)
  )

  breaks[1] <- TRUE

  if (nrow(x) > 1) {
    breaks[2:nrow(x)] <- (
      diff(x$anio) != 1L |
        x$ok[2:nrow(x)] != x$ok[1:(nrow(x) - 1L)]
    )
  }

  x[, grp := cumsum(breaks)]

  runs <- x[
    ok %in% TRUE,
    .(
      n_anios = .N,
      anio_inicio = min(anio),
      anio_fin = max(anio)
    ),
    by = grp
  ]

  if (!nrow(runs)) {
    return(
      list(
        n_anios = 0L,
        anio_inicio = NA_integer_,
        anio_fin = NA_integer_
      )
    )
  }

  # Desempate estable: si hay dos bloques de igual longitud,
  # se informa el primero cronológicamente.
  setorder(
    runs,
    -n_anios,
    anio_inicio
  )

  list(
    n_anios = as.integer(runs$n_anios[1]),
    anio_inicio = as.integer(runs$anio_inicio[1]),
    anio_fin = as.integer(runs$anio_fin[1])
  )
}


is_ana_precip_12h <- function(tipo_dato, expected_obs_day) {
  !is.na(tipo_dato) &
    tipo_dato == "Precipitación" &
    !is.na(expected_obs_day) &
    expected_obs_day == 2L
}


aggregate_ana_precip_12h <- function(obs) {

  x <- copy(as.data.table(obs))

  required_cols <- c("fecha", "hora")
  missing_cols <- setdiff(required_cols, names(x))

  if (length(missing_cols)) {
    stop(
      paste0(
        "La serie de precipitación de 12 h no contiene: ",
        paste(missing_cols, collapse = ", "),
        "."
      )
    )
  }

  if (!"valor_num" %in% names(x)) {
    if (!"valor" %in% names(x)) {
      stop("La serie de precipitación de 12 h no contiene la columna valor.")
    }

    x[, valor_num := suppressWarnings(as.numeric(valor))]
  }

  x[, fecha := as.IDate(fecha)]
  x <- x[!is.na(fecha)]

  if (!nrow(x)) {
    return(
      data.table(
        fecha = as.IDate(character()),
        N_registros = integer(),
        N_valores_validos = integer(),
        N_valores_distintos = integer(),
        N_horas_distintas = integer(),
        Valores_observados = character(),
        Fechas_observadas = character(),
        Horas_observadas = character(),
        Par_12h_completo = logical(),
        N_obs_validas_diagnostico = integer(),
        Valor_exportado = numeric()
      )
    )
  }

  x[, hora_fuente := trimws(as.character(hora))]
  x[, fecha_fuente := fecha]
  x[, componente_ana := fcase(
    hora_fuente %chin% c("19:00", "19:00:00"),
    "D_19",
    hora_fuente %chin% c("7:00", "7:00:00", "07:00", "07:00:00"),
    "D1_07",
    default = "OTRA"
  )]

  # Día pluviométrico ANA:
  # P_D = P(D, 19:00) + P(D + 1, 07:00).
  x[, fecha_dia := fecha]
  x[componente_ana == "D1_07", fecha_dia := fecha - 1L]

  setorder(x, fecha_dia, fecha, hora_fuente, na.last = TRUE)

  out <- x[
    ,
    {
      valid <- !is.na(valor_num)
      n_19 <- sum(componente_ana == "D_19")
      n_07 <- sum(componente_ana == "D1_07")
      n_otras <- sum(componente_ana == "OTRA")
      n_19_valid <- sum(componente_ana == "D_19" & valid)
      n_07_valid <- sum(componente_ana == "D1_07" & valid)

      par_completo <- (
        .N == 2L &
          n_19 == 1L &
          n_07 == 1L &
          n_otras == 0L &
          n_19_valid == 1L &
          n_07_valid == 1L
      )

      n_componentes_validos <- as.integer(
        n_19 == 1L && n_19_valid == 1L
      ) + as.integer(
        n_07 == 1L && n_07_valid == 1L
      )

      .(
        N_registros = .N,
        N_valores_validos = sum(valid),
        N_valores_distintos = uniqueN(valor_num[valid]),
        N_horas_distintas = uniqueN(hora_fuente[!is.na(hora_fuente) & nzchar(hora_fuente)]),
        Valores_observados = paste(as.character(valor_num[valid]), collapse = " | "),
        Fechas_observadas = paste(as.character(fecha_fuente), collapse = " | "),
        Horas_observadas = paste(hora_fuente, collapse = " | "),
        Par_12h_completo = par_completo,
        N_obs_validas_diagnostico = if (par_completo) {
          2L
        } else {
          min(1L, n_componentes_validos)
        },
        Valor_exportado = if (par_completo) sum(valor_num) else NA_real_
      )
    },
    by = fecha_dia
  ]

  setnames(out, "fecha_dia", "fecha")
  out[]
}


diagnosticar_serie <- function(series_id) {

  sid <- as.character(series_id)[1]

  meta <- copy(
    SERIES[
      series_id == sid
    ][1]
  )

  if (!nrow(meta) || is.na(meta$primera_fecha) || is.na(meta$ultima_fecha)) {
    return(NULL)
  }

  fi <- as.IDate(meta$primera_fecha)
  ff <- as.IDate(meta$ultima_fecha)

  if (is.na(fi) || is.na(ff) || fi > ff) {
    return(NULL)
  }

  exp_day <- meta$expected_obs_day
  if (is.na(exp_day) || exp_day < 1) exp_day <- 1L

  # Calendario continuo del periodo nominal de la serie.
  cal <- data.table(
    fecha = seq(
      fi,
      ff,
      by = "day"
    )
  )

  if (is_ana_precip_12h(meta$tipo_dato, exp_day)) {
    norm_url <- normalized_series_url(
      meta$carpeta_cuenca,
      meta$id_config
    )

    obs_12h <- download_normalized_rds(norm_url)
    resumen_12h <- aggregate_ana_precip_12h(obs_12h)

    d <- resumen_12h[
      fecha >= fi & fecha <= ff,
      .(
        fecha,
        n_obs_validas = N_obs_validas_diagnostico
      )
    ]
  } else {
    d <- DAY[
      series_id == sid,
      .(
        fecha,
        n_obs_validas,
        expected_obs_day
      )
    ]
  }

  # Por seguridad: una fila diaria por serie.
  if (nrow(d)) {
    d <- d[, .(
      n_obs_validas = sum(n_obs_validas, na.rm = TRUE)
    ), by = fecha]
  }

  cal <- merge(
    cal,
    d,
    by = "fecha",
    all.x = TRUE,
    sort = TRUE
  )

  cal[is.na(n_obs_validas), n_obs_validas := 0L]
  cal[, expected_obs_day := exp_day]

  cal[, estado := fifelse(
    n_obs_validas <= 0,
    "VACIO",
    fifelse(
      n_obs_validas >= expected_obs_day,
      "COMPLETO",
      "PARCIAL"
    )
  )]

  cal[, completitud_dia_pct :=
        pmin(100, 100 * n_obs_validas / expected_obs_day)]

  cal[, `:=`(
    anio = year(fecha),
    mes = month(fecha),
    ym = format(as.Date(fecha), "%Y-%m")
  )]

  n_days <- nrow(cal)
  obs_validas <- sum(cal$n_obs_validas, na.rm = TRUE)
  expected_obs <- n_days * exp_day

  comp_own <- if (expected_obs > 0) {
    100 * obs_validas / expected_obs
  } else {
    NA_real_
  }

  # -----------------------------
  # Agregación anual
  # -----------------------------
  anual <- cal[, .(
    dias_periodo = .N,
    observaciones_validas = sum(n_obs_validas, na.rm = TRUE),
    observaciones_esperadas = .N * exp_day,
    dias_con_alguna_obs = sum(n_obs_validas > 0),
    dias_completos = sum(n_obs_validas >= exp_day),
    dias_parciales = sum(n_obs_validas > 0 & n_obs_validas < exp_day),
    dias_vacios = sum(n_obs_validas == 0)
  ), by = anio]

  anual[, completitud_pct :=
          100 * observaciones_validas / observaciones_esperadas]

  anual[, cobertura_dias_pct :=
          100 * dias_con_alguna_obs / dias_periodo]

  # -----------------------------
  # Bloques máximos de años consecutivos
  #
  # Importante:
  # - un año parcial al inicio o final de la serie NO cuenta;
  # - 100% completo exige todos los días calendario con la frecuencia
  #   esperada completa;
  # - >=90% usa la completitud anual ya definida arriba, pero únicamente
  #   sobre años calendario completos.
  # -----------------------------
  anual[, dias_calendario := ifelse(
    leap_year(anio),
    366L,
    365L
  )]

  anual[, anio_calendario_completo :=
          dias_periodo == dias_calendario]

  anual[, anio_100_completo :=
          anio_calendario_completo &
          dias_completos == dias_calendario]

  anual[, anio_ge90 :=
          anio_calendario_completo &
          completitud_pct >= 90]

  block_100 <- longest_consecutive_year_block(
    anual$anio,
    anual$anio_100_completo
  )

  block_90 <- longest_consecutive_year_block(
    anual$anio,
    anual$anio_ge90
  )

  # -----------------------------
  # Agregación mensual
  # -----------------------------
  mensual <- cal[, .(
    dias_periodo = .N,
    observaciones_validas = sum(n_obs_validas, na.rm = TRUE),
    observaciones_esperadas = .N * exp_day,
    dias_con_alguna_obs = sum(n_obs_validas > 0),
    dias_completos = sum(n_obs_validas >= exp_day),
    dias_parciales = sum(n_obs_validas > 0 & n_obs_validas < exp_day),
    dias_vacios = sum(n_obs_validas == 0)
  ), by = .(anio, mes, ym)]

  mensual[, completitud_pct :=
            100 * observaciones_validas / observaciones_esperadas]

  mensual[, cobertura_dias_pct :=
            100 * dias_con_alguna_obs / dias_periodo]

  # -----------------------------
  # Rachas diarias
  # -----------------------------
  cal[, grp_estado := rleid(estado)]

  runs <- cal[, .(
    estado = first(estado),
    inicio = min(fecha),
    fin = max(fecha),
    n_dias = .N
  ), by = grp_estado][, grp_estado := NULL]

  gaps <- runs[estado == "VACIO"]
  partial_runs <- runs[estado == "PARCIAL"]
  full_runs <- runs[estado == "COMPLETO"]

  # Rachas de días con al menos algún dato: vacío rompe la continuidad.
  cal[, tiene_dato := n_obs_validas > 0]
  cal[, grp_dato := rleid(tiene_dato)]

  runs_data <- cal[, .(
    tiene_dato = first(tiene_dato),
    inicio = min(fecha),
    fin = max(fecha),
    n_dias = .N
  ), by = grp_dato][, grp_dato := NULL]

  max_data_run <- runs_data[
    tiene_dato == TRUE,
    if (.N) max(n_dias) else 0L
  ]

  # -----------------------------
  # Rachas de meses completamente vacíos
  # -----------------------------
  mensual[, mes_vacio := observaciones_validas == 0]
  mensual[, month_index := anio * 12L + mes]
  setorder(mensual, month_index)

  mensual[, grp_vacio := rleid(mes_vacio)]

  month_runs <- mensual[
    ,
    .(
      mes_vacio = first(mes_vacio),
      inicio_idx = min(month_index),
      fin_idx = max(month_index),
      n_meses = .N
    ),
    by = grp_vacio
  ][, grp_vacio := NULL]

  max_empty_month_run <- month_runs[
    mes_vacio == TRUE,
    if (.N) max(n_meses) else 0L
  ]

  # -----------------------------
  # Rachas de años completamente vacíos
  # -----------------------------
  anual[, anio_vacio := observaciones_validas == 0]
  setorder(anual, anio)
  anual[, grp_vacio := rleid(anio_vacio)]

  year_runs <- anual[
    ,
    .(
      anio_vacio = first(anio_vacio),
      anio_inicio = min(anio),
      anio_fin = max(anio),
      n_anios = .N
    ),
    by = grp_vacio
  ][, grp_vacio := NULL]

  max_empty_year_run <- year_runs[
    anio_vacio == TRUE,
    if (.N) max(n_anios) else 0L
  ]

  # -----------------------------
  # Resumen
  # -----------------------------
  summary <- data.table(
    series_id = sid,
    station_id = meta$station_id,
    nombre_estacion = meta$nombre_estacion,
    codigo_estacion = meta$codigo_estacion,
    variable = meta$variable,
    unidad = meta$unidad,
    id_config = meta$id_config,
    primera_fecha = fi,
    ultima_fecha = ff,
    dias_nominales = n_days,
    anios_nominales = n_days / 365.2425,
    expected_obs_day = exp_day,
    observaciones_validas = obs_validas,
    observaciones_esperadas = expected_obs,
    completitud_propia_pct = comp_own,
    dias_con_alguna_obs = sum(cal$n_obs_validas > 0),
    dias_completos = sum(cal$n_obs_validas >= exp_day),
    dias_parciales = sum(cal$n_obs_validas > 0 & cal$n_obs_validas < exp_day),
    dias_vacios = sum(cal$n_obs_validas == 0),
    cobertura_dias_pct = 100 * sum(cal$n_obs_validas > 0) / n_days,
    dias_completos_pct = 100 * sum(cal$n_obs_validas >= exp_day) / n_days,
    max_racha_vacia_dias = if (nrow(gaps)) max(gaps$n_dias) else 0L,
    n_rachas_vacias = nrow(gaps),
    mediana_racha_vacia_dias = if (nrow(gaps)) median(gaps$n_dias) else 0,
    p95_racha_vacia_dias = if (nrow(gaps)) {
      as.numeric(quantile(gaps$n_dias, 0.95, na.rm = TRUE, names = FALSE))
    } else {
      0
    },
    max_racha_parcial_dias = if (nrow(partial_runs)) max(partial_runs$n_dias) else 0L,
    max_racha_completa_dias = if (nrow(full_runs)) max(full_runs$n_dias) else 0L,
    max_racha_con_dato_dias = max_data_run,
    max_racha_meses_vacios = max_empty_month_run,
    max_racha_anios_vacios = max_empty_year_run,
    max_anios_100_completos = block_100$n_anios,
    max_anios_100_inicio = block_100$anio_inicio,
    max_anios_100_fin = block_100$anio_fin,
    max_anios_ge90 = block_90$n_anios,
    max_anios_ge90_inicio = block_90$anio_inicio,
    max_anios_ge90_fin = block_90$anio_fin,
    completitud_anual_min = min(anual$completitud_pct, na.rm = TRUE),
    completitud_anual_mediana = median(anual$completitud_pct, na.rm = TRUE),
    completitud_anual_max = max(anual$completitud_pct, na.rm = TRUE),
    completitud_mensual_min = min(mensual$completitud_pct, na.rm = TRUE),
    completitud_mensual_mediana = median(mensual$completitud_pct, na.rm = TRUE),
    completitud_mensual_max = max(mensual$completitud_pct, na.rm = TRUE)
  )

  list(
    meta = meta,
    summary = summary,
    calendar = cal,
    annual = anual,
    monthly = mensual,
    gaps = gaps,
    partial_runs = partial_runs,
    full_runs = full_runs,
    month_runs = month_runs,
    year_runs = year_runs
  )
}


# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# 4C. EXPORTACIÓN NORMALIZADA INDIVIDUAL DESDE DIAGNÓSTICO
# ----------------------------------------------------------------------------
#
# Reutiliza la misma fuente de RDS normalizados publicada por la aplicación.
# La salida es diaria:
#   - expected_obs_day == 1: conserva una observación por fecha;
#     si existen registros múltiples, usa la última observación no NA y
#     documenta la incidencia.
#   - precipitación con expected_obs_day == 2: aplica el día pluviométrico ANA,
#     P_D = P(D, 19:00) + P(D + 1, 07:00); si el par no está completo,
#     exporta NA y documenta la incidencia.
#
# Otras frecuencias subdiarias no se convierten automáticamente.
# ----------------------------------------------------------------------------

NORMALIZED_SERIES_BASE_URL <- paste0(
  "https://raw.githubusercontent.com/",
  "JamilRamirez/ANA-SNIRH-Normalized-Series/",
  "main/series_long"
)

DATA_SNAPSHOT_FILE <- file.path(
  "data",
  "00_data_snapshot.csv"
)

read_data_freeze_date <- function(
  fallback = as.Date("2026-08-15")
) {
  if (!file.exists(DATA_SNAPSHOT_FILE)) {
    return(fallback)
  }

  x <- tryCatch(
    utils::read.csv(
      DATA_SNAPSHOT_FILE,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(e) NULL
  )

  if (
    is.null(x) ||
    !nrow(x) ||
    !"snapshot_date" %in% names(x)
  ) {
    return(fallback)
  }

  d <- suppressWarnings(
    as.Date(
      x$snapshot_date[1]
    )
  )

  if (is.na(d)) fallback else d
}

format_data_freeze_date_es <- function(
  x = DATA_FREEZE_DATE,
  compact = FALSE
) {
  x <- as.Date(x)

  if (is.na(x)) {
    return("fecha no disponible")
  }

  meses <- c(
    "enero",
    "febrero",
    "marzo",
    "abril",
    "mayo",
    "junio",
    "julio",
    "agosto",
    "septiembre",
    "octubre",
    "noviembre",
    "diciembre"
  )

  if (isTRUE(compact)) {
    return(
      paste(
        lubridate::day(x),
        substr(meses[lubridate::month(x)], 1L, 3L),
        lubridate::year(x)
      )
    )
  }

  paste(
    lubridate::day(x),
    "de",
    meses[lubridate::month(x)],
    "de",
    lubridate::year(x)
  )
}

DATA_FREEZE_DATE <- read_data_freeze_date()


normalized_series_url <- function(carpeta_cuenca, id_config) {

  if (
    !length(carpeta_cuenca) ||
    is.na(carpeta_cuenca) ||
    !nzchar(as.character(carpeta_cuenca)) ||
    !length(id_config) ||
    is.na(id_config) ||
    !nzchar(as.character(id_config))
  ) {
    return(NA_character_)
  }

  paste0(
    NORMALIZED_SERIES_BASE_URL,
    "/",
    encode_path_segments(as.character(carpeta_cuenca)),
    "/IDConfig_",
    as.character(id_config),
    ".rds"
  )
}


download_normalized_rds <- function(url) {

  if (
    !length(url) ||
    is.na(url) ||
    !nzchar(as.character(url))
  ) {
    stop("La serie no tiene una ruta RDS normalizada disponible.")
  }

  tmp <- tempfile(fileext = ".rds")

  on.exit(
    unlink(tmp, force = TRUE),
    add = TRUE
  )

  old_timeout <- getOption("timeout")

  on.exit(
    options(timeout = old_timeout),
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
    !file.exists(tmp) ||
    file.info(tmp)$size <= 0
  ) {
    stop("El RDS normalizado descargado está vacío.")
  }

  as.data.table(
    readRDS(tmp)
  )
}


safe_export_name <- function(x, fallback = "serie") {

  y <- iconv(
    as.character(x),
    from = "UTF-8",
    to = "ASCII//TRANSLIT"
  )

  if (!length(y) || is.na(y) || !nzchar(y)) {
    y <- fallback
  }

  y <- gsub("[^A-Za-z0-9_-]+", "_", y)
  y <- gsub("_+", "_", y)
  y <- gsub("^_+|_+$", "", y)

  if (!nzchar(y)) fallback else y
}


repair_xlsx_orphan_relationships <- function(path) {

  stopifnot(
    length(path) == 1L,
    file.exists(path)
  )

  unpack_dir <- tempfile("xlsx_rels_")
  repaired_path <- tempfile(fileext = ".xlsx")

  dir.create(
    unpack_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  on.exit(
    unlink(
      c(unpack_dir, repaired_path),
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  zip::unzip(
    path,
    exdir = unpack_dir
  )

  root_path <- normalizePath(
    unpack_dir,
    winslash = "/",
    mustWork = TRUE
  )

  rel_files <- list.files(
    unpack_dir,
    pattern = "[.]rels$",
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE
  )

  removed <- 0L

  for (rel_file in rel_files) {

    rel_text <- paste(
      readLines(
        rel_file,
        warn = FALSE,
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )

    rel_tags <- regmatches(
      rel_text,
      gregexpr(
        "<Relationship\\b[^>]*/>",
        rel_text,
        perl = TRUE
      )
    )[[1]]

    if (!length(rel_tags)) {
      next
    }

    rel_relative <- substring(
      normalizePath(
        rel_file,
        winslash = "/",
        mustWork = TRUE
      ),
      nchar(root_path) + 2L
    )

    source_part <- if (identical(rel_relative, "_rels/.rels")) {
      "."
    } else {
      sub(
        "(^|/)_rels/([^/]+)[.]rels$",
        "\\1\\2",
        rel_relative
      )
    }

    for (rel_tag in rel_tags) {

      if (grepl(
        "TargetMode=\"External\"",
        rel_tag,
        fixed = TRUE
      )) {
        next
      }

      target <- sub(
        ".*\\bTarget=\"([^\"]+)\".*",
        "\\1",
        rel_tag,
        perl = TRUE
      )

      if (identical(target, rel_tag)) {
        next
      }

      target <- utils::URLdecode(target)

      if (startsWith(target, "/")) {
        target_path <- file.path(
          root_path,
          sub("^/+", "", target)
        )
      } else {
        target_path <- file.path(
          root_path,
          dirname(source_part),
          target
        )
      }

      target_path <- normalizePath(
        target_path,
        winslash = "/",
        mustWork = FALSE
      )

      inside_archive <- startsWith(
        tolower(target_path),
        paste0(tolower(root_path), "/")
      )

      if (!inside_archive || !file.exists(target_path)) {
        rel_text <- sub(
          rel_tag,
          "",
          rel_text,
          fixed = TRUE
        )
        removed <- removed + 1L
      }
    }

    writeLines(
      rel_text,
      rel_file,
      useBytes = TRUE
    )
  }

  if (!removed) {
    return(invisible(0L))
  }

  archive_members <- list.files(
    unpack_dir,
    recursive = TRUE,
    all.files = TRUE,
    full.names = FALSE,
    include.dirs = FALSE,
    no.. = TRUE
  )

  zip::zip(
    repaired_path,
    archive_members,
    root = unpack_dir,
    mode = "mirror",
    include_directories = FALSE
  )

  if (
    !file.copy(
      repaired_path,
      path,
      overwrite = TRUE
    )
  ) {
    stop("No se pudo reemplazar el XLSX por su versión reparada.")
  }

  invisible(removed)
}


build_single_normalized_excel <- function(
  series_id,
  f0,
  f1,
  output_path
) {

  sid <- as.character(series_id)[1]
  fi <- as.IDate(f0)
  ff <- as.IDate(f1)

  if (
    !length(sid) ||
    is.na(sid) ||
    !nzchar(sid)
  ) {
    stop("No se recibió una serie válida.")
  }

  if (
    is.na(fi) ||
    is.na(ff) ||
    fi > ff
  ) {
    stop("El periodo de exportación no es válido.")
  }

  meta <- copy(
    SERIES[
      series_id == sid
    ][1]
  )

  if (!nrow(meta)) {
    stop("La serie seleccionada no existe en el catálogo cargado.")
  }

  if (
    is.na(meta$primera_fecha) ||
    is.na(meta$ultima_fecha)
  ) {
    stop("La serie no tiene un periodo nominal definido.")
  }

  if (
    fi < as.IDate(meta$primera_fecha) ||
    ff > as.IDate(meta$ultima_fecha)
  ) {
    stop(
      paste0(
        "El periodo solicitado debe estar dentro del periodo nominal ",
        as.character(meta$primera_fecha),
        " a ",
        as.character(meta$ultima_fecha),
        "."
      )
    )
  }

  exp_day <- suppressWarnings(
    as.integer(meta$expected_obs_day)
  )

  precip_12h_ana <- is_ana_precip_12h(
    meta$tipo_dato,
    exp_day
  )

  if (
    is.na(exp_day) ||
    !(
      exp_day == 1L ||
        precip_12h_ana
    )
  ) {
    stop(
      paste(
        "La descarga normalizada individual solo admite series diarias",
        "y precipitación de 12 h compatible con la regla diaria ANA."
      )
    )
  }

  if (!"carpeta_cuenca" %in% names(meta)) {
    stop(
      "El catálogo no contiene carpeta_cuenca, necesaria para localizar el RDS normalizado."
    )
  }

  norm_url <- normalized_series_url(
    meta$carpeta_cuenca,
    meta$id_config
  )

  if (
    is.na(norm_url) ||
    !nzchar(norm_url)
  ) {
    stop("No se pudo construir la ruta de la serie normalizada.")
  }

  obs <- download_normalized_rds(
    norm_url
  )

  required_cols <- c(
    "fecha",
    "valor"
  )

  missing_cols <- setdiff(
    required_cols,
    names(obs)
  )

  if (length(missing_cols)) {
    stop(
      paste0(
        "El RDS normalizado no contiene las columnas requeridas: ",
        paste(missing_cols, collapse = ", "),
        "."
      )
    )
  }

  obs[, fecha := as.IDate(fecha)]
  obs <- obs[
    !is.na(fecha) &
      fecha >= fi &
      fecha <= if (precip_12h_ana) ff + 1L else ff
  ]

  obs[, valor_num := suppressWarnings(
    as.numeric(valor)
  )]

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

  incidencias <- data.table()
  n_multi <- 0L
  n_distinct <- 0L
  n_12h_ok <- 0L
  n_12h_bad <- 0L

  if (precip_12h_ana) {

    resumen <- aggregate_ana_precip_12h(obs)[
      fecha >= fi & fecha <= ff
    ]

    n_12h_ok <- resumen[
      Par_12h_completo %in% TRUE,
      .N
    ]

    n_12h_bad <- resumen[
      !Par_12h_completo %in% TRUE,
      .N
    ]

    incidencias <- resumen[
      !Par_12h_completo %in% TRUE
    ]

    incidencias[, N_obs_validas_diagnostico := NULL]

    if (nrow(incidencias)) {
      incidencias[, `:=`(
        Estacion = meta$nombre_estacion,
        Codigo = meta$codigo_estacion,
        IDConfig = meta$id_config,
        Regla = paste(
          "Precipitación 12 h: P_D = P(D, 19:00) +",
          "P(D + 1, 07:00);",
          "en otro caso se exporta NA"
        )
      )]
    }

    diario <- resumen[
      ,
      .(
        fecha,
        valor = Valor_exportado
      )
    ]

  } else {

    if (nrow(obs)) {

      dup <- obs[
        ,
        .(
          N_registros = .N,
          N_valores_validos = sum(!is.na(valor_num)),
          N_valores_distintos = uniqueN(
            valor_num[
              !is.na(valor_num)
            ]
          )
        ),
        by = fecha
      ][
        N_registros > 1L
      ]

      n_multi <- nrow(dup)
      n_distinct <- dup[
        N_valores_distintos > 1L,
        .N
      ]

      if (nrow(dup)) {

        incidencias <- obs[
          fecha %in% dup$fecha,
          {
            valid_idx <- which(
              !is.na(valor_num)
            )

            pick <- if (length(valid_idx)) {
              valid_idx[length(valid_idx)]
            } else {
              .N
            }

            .(
              N_registros = .N,
              N_valores_validos = sum(!is.na(valor_num)),
              N_valores_distintos = uniqueN(
                valor_num[
                  !is.na(valor_num)
                ]
              ),
              Valores_observados = paste(
                unique(
                  as.character(
                    valor_num[
                      !is.na(valor_num)
                    ]
                  )
                ),
                collapse = " | "
              ),
              Horas_observadas = if ("hora" %in% names(.SD)) {
                paste(
                  unique(
                    na.omit(
                      as.character(hora)
                    )
                  ),
                  collapse = " | "
                )
              } else {
                ""
              },
              Valor_exportado = valor_num[pick]
            )
          },
          by = fecha
        ]

        incidencias[, `:=`(
          Estacion = meta$nombre_estacion,
          Codigo = meta$codigo_estacion,
          IDConfig = meta$id_config,
          Regla = paste(
            "Última observación no NA del día",
            "según el orden FECHA/HORA del RDS"
          )
        )]
      }

      diario <- obs[
        ,
        {
          valid_idx <- which(
            !is.na(valor_num)
          )

          pick <- if (length(valid_idx)) {
            valid_idx[length(valid_idx)]
          } else {
            .N
          }

          .(
            valor = valor_num[pick]
          )
        },
        by = fecha
      ]

    } else {

      diario <- data.table(
        fecha = as.IDate(character()),
        valor = numeric()
      )
    }
  }

  calendario <- data.table(
    Fecha = seq(
      as.Date(fi),
      as.Date(ff),
      by = "day"
    )
  )

  diario[, Fecha := as.Date(fecha)]
  diario[, fecha := NULL]

  datos <- merge(
    calendario,
    diario,
    by = "Fecha",
    all.x = TRUE,
    sort = FALSE
  )

  setorder(
    datos,
    Fecha
  )

  col_data <- safe_export_name(
    paste(
      meta$nombre_estacion,
      if (
        !is.na(meta$codigo_estacion) &&
        nzchar(as.character(meta$codigo_estacion))
      ) {
        meta$codigo_estacion
      } else {
        paste0("IDConfig_", meta$id_config)
      },
      sep = "_"
    ),
    fallback = paste0("IDConfig_", meta$id_config)
  )

  setnames(
    datos,
    "valor",
    col_data
  )

  ev <- evaluate_series(
    sid,
    fi,
    ff
  )

  ev <- if (nrow(ev)) {
    ev[1]
  } else {
    data.table(
      completitud_obs_pct = NA_real_,
      cobertura_dias_pct = NA_real_,
      cubre_nominalmente = FALSE,
      n_dias_alguna = 0L,
      dias_esperados = as.integer(ff - fi) + 1L
    )
  }

  frecuencia <- if (exp_day == 1L) {
    "1 observación/día"
  } else {
    "2 observaciones/día (12 h)"
  }

  metadata <- data.table(
    Columna_datos = col_data,
    Estacion = meta$nombre_estacion,
    Codigo = meta$codigo_estacion,
    IDConfig = meta$id_config,
    Tipo_dato = meta$tipo_dato,
    Variable = meta$variable,
    Unidad = meta$unidad,
    Frecuencia = frecuencia,
    Latitud = meta$latitud,
    Longitud = meta$longitud,
    Altitud_msnm = if (
      "altitud_msnm" %in% names(meta)
    ) {
      meta$altitud_msnm
    } else {
      NA_real_
    },
    Fecha_inicial_disponible = as.Date(meta$primera_fecha),
    Fecha_final_disponible = as.Date(meta$ultima_fecha),
    Periodo_exportado_inicio = as.Date(fi),
    Periodo_exportado_fin = as.Date(ff),
    Completitud_periodo_pct = round(
      ev$completitud_obs_pct,
      3
    ),
    Cobertura_dias_periodo_pct = round(
      ev$cobertura_dias_pct,
      3
    ),
    Cubre_periodo_nominal = ifelse(
      isTRUE(ev$cubre_nominalmente),
      "Sí",
      "No"
    ),
    Dias_con_dato_periodo = ev$n_dias_alguna,
    Dias_periodo = ev$dias_esperados,
    Fechas_con_multiples_registros = n_multi,
    Fechas_con_valores_distintos = n_distinct,
    Dias_12h_agregados = n_12h_ok,
    Dias_12h_anomalos = n_12h_bad,
    Transformacion_a_diario = if (precip_12h_ana) {
      paste(
        "Día pluviométrico ANA: P_D = P(D, 19:00) +",
        "P(D + 1, 07:00); si el par no está completo, NA"
      )
    } else {
      "Serie diaria; sin agregación temporal"
    },
    Regla_registros_multiples = if (n_multi > 0L) {
      paste(
        "Última observación no NA del día",
        "según el orden FECHA/HORA del RDS"
      )
    } else {
      "No aplicó"
    },
    Unidad_hidrografica = if (
      "unidad_hidrografica" %in% names(meta)
    ) {
      meta$unidad_hidrografica
    } else {
      NA_character_
    },
    AAA = if (
      "aaa" %in% names(meta)
    ) {
      meta$aaa
    } else {
      NA_character_
    },
    ALA = if (
      "ala" %in% names(meta)
    ) {
      meta$ala
    } else {
      NA_character_
    },
    Fuente_observaciones = "ANA/SNIRH",
    Fecha_corte_base_app = DATA_FREEZE_DATE,
    Frecuencia_actualizacion_app = "Mensual",
    URL_serie_normalizada = norm_url,
    URL_reporte_original_ANA = raw_report_url(
      meta$id_config
    ),
    URL_visor_ANA_SNIRH = url_visor_ana,
    Origen_exportacion = "Diagnóstico de estación"
  )

  if (nrow(incidencias)) {
    incidencias[, Fecha := as.Date(fecha)]
    incidencias[, fecha := NULL]

    cols_inc <- c(
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
      cols_inc[
        cols_inc %in% names(incidencias)
      ]
    )

    if (
      "Par_12h_completo" %in% names(incidencias)
    ) {
      incidencias[, Par_12h_completo := fifelse(
        is.na(Par_12h_completo),
        "—",
        fifelse(
          Par_12h_completo,
          "Sí",
          "No"
        )
      )]
    }
  }

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

  if (ncol(datos) > 1L) {
    openxlsx::setColWidths(
      wb,
      "Datos",
      cols = 2:ncol(datos),
      widths = 20
    )
  }

  openxlsx::setColWidths(
    wb,
    "Metadata",
    cols = 1:ncol(metadata),
    widths = 20
  )

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
    names(metadata)
  )

  metadata_cols_anchas <- metadata_cols_anchas[
    !is.na(metadata_cols_anchas)
  ]

  if (length(metadata_cols_anchas)) {
    openxlsx::setColWidths(
      wb,
      "Metadata",
      cols = metadata_cols_anchas,
      widths = 30
    )
  }

  if (nrow(incidencias)) {
    openxlsx::setColWidths(
      wb,
      "Incidencias",
      cols = 1:ncol(incidencias),
      widths = 18
    )
  }

  date_style <- openxlsx::createStyle(
    numFmt = "yyyy-mm-dd"
  )

  openxlsx::addStyle(
    wb,
    "Datos",
    style = date_style,
    rows = 2:(nrow(datos) + 1L),
    cols = 1,
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::saveWorkbook(
    wb,
    output_path,
    overwrite = TRUE
  )

  repair_xlsx_orphan_relationships(
    output_path
  )

  if (
    !file.exists(output_path) ||
    file.info(output_path)$size <= 0
  ) {
    stop("No se pudo generar el Excel normalizado.")
  }

  invisible(
    list(
      n_days = nrow(datos),
      size = file.info(output_path)$size,
      normalized_url = norm_url,
      n_incidencias = nrow(incidencias)
    )
  )
}

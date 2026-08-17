# BUILD PUBLICA V17 — descarga directa de reportes RAW archivados en GitHub
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

pkgs <- c("shiny", "bslib", "data.table", "DT", "sf", "leaflet", "lubridate", "ggplot2")
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
DAY <- as.data.table(readRDS(F_DIARIO))

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
  latitud = suppressWarnings(as.numeric(latitud)),
  longitud = suppressWarnings(as.numeric(longitud)),
  expected_obs_day = suppressWarnings(as.integer(expected_obs_day))
)]

DAY[, `:=`(
  series_id = as.character(series_id),
  station_id = as.character(station_id),
  tipo_dato = as.character(tipo_dato),
  fecha = as.IDate(fecha),
  n_obs_validas = suppressWarnings(as.integer(n_obs_validas)),
  expected_obs_day = suppressWarnings(as.integer(expected_obs_day))
)]

CAT <- CAT[
  tipo_dato %chin% c("Precipitación", "Caudal") &
    !is.na(latitud) & !is.na(longitud) &
    between(latitud, -90, 90) & between(longitud, -180, 180)
]
DAY <- DAY[tipo_dato %chin% c("Precipitación", "Caudal") & !is.na(fecha)]
setkey(DAY, series_id, fecha)

SPAN <- DAY[n_obs_validas > 0,
            .(primera_fecha = min(fecha),
              ultima_fecha = max(fecha),
              n_dias_con_dato_total = uniqueN(fecha)),
            by = series_id]

SERIES <- merge(CAT, SPAN, by = "series_id", all.x = TRUE)


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

FMIN <- min(DAY$fecha, na.rm = TRUE)
FMAX <- max(DAY$fecha, na.rm = TRUE)
FDEF0 <- max(FMIN, as.IDate("1981-01-01"))
FDEF1 <- min(FMAX, as.IDate("2025-12-31"))


# Resumen anual por serie para búsquedas recursivas de ventanas N-años.
# Se calcula una sola vez al iniciar la app.
DAY[, anio := year(fecha)]

YEAR_OBS <- DAY[, .(
  n_obs_validas = sum(n_obs_validas, na.rm = TRUE),
  n_dias_alguna = sum(n_obs_validas > 0, na.rm = TRUE),
  n_dias_completos = sum(n_obs_validas >= expected_obs_day, na.rm = TRUE),
  n_dias_parciales = sum(n_obs_validas > 0 & n_obs_validas < expected_obs_day, na.rm = TRUE),
  expected_obs_day = {
    z <- expected_obs_day[!is.na(expected_obs_day) & expected_obs_day >= 1]
    if (length(z)) z[1] else 1L
  }
), by = .(series_id, station_id, tipo_dato, anio)]

# ----------------------------------------------------------------------------
# 3. REFERENCIAS OMM/WMO: km² por estación
# ----------------------------------------------------------------------------

WMO_DENSITY <- data.table(
  region = c("Costera", "Montañosa", "Llanura interior", "Ondulada / colinosa", "Islas pequeñas", "Polar / árida"),
  precip_no_reg = c(900, 250, 575, 575, 25, 10000),
  precip_reg = c(9000, 2500, 5750, 5750, 250, 100000),
  caudal = c(2750, 1000, 1875, 1875, 300, 20000)
)

# ----------------------------------------------------------------------------
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
    class = "border rounded p-3 bg-body-tertiary h-100",
    div(title, style = "font-size:.77rem;font-weight:700;text-transform:uppercase;color:#6c757d;"),
    div(value, style = "font-size:1.65rem;font-weight:700;line-height:1.2;margin-top:5px;"),
    if (!is.null(subtitle)) div(subtitle, style = "font-size:.78rem;color:#6c757d;margin-top:4px;")
  )
}

dt_opts <- function(n = 25) {
  list(
    dom = "Bfrtip", buttons = c("copy", "csv", "excel"), pageLength = n,
    scrollX = TRUE, autoWidth = TRUE,
    language = list(
      search = "Buscar:", lengthMenu = "Mostrar _MENU_ registros",
      info = "Mostrando _START_ a _END_ de _TOTAL_",
      zeroRecords = "Sin resultados",
      paginate = list(previous = "Anterior", `next` = "Siguiente")
    )
  )
}

read_vector_source <- function(path) {

  ext <- tolower(tools::file_ext(path))

  # Para SHP, st_read(path) es más directo y respeta los archivos auxiliares.
  if (ext == "shp") {

    x <- st_read(
      path,
      quiet = TRUE,
      stringsAsFactors = FALSE
    )

    if (!nrow(x)) {
      return(NULL)
    }

    return(x)
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

    return(x)
  }

  xs <- lapply(
    layers,
    function(lyr) {
      tryCatch(
        st_read(
          path,
          layer = lyr,
          quiet = TRUE,
          stringsAsFactors = FALSE
        ),
        error = function(e) NULL
      )
    }
  )

  xs <- Filter(
    function(x) {
      !is.null(x) && nrow(x) > 0
    },
    xs
  )

  if (!length(xs)) {
    return(NULL)
  }

  # Nos interesan las geometrías, no que todas las capas tengan
  # exactamente los mismos atributos.
  geoms <- do.call(
    c,
    lapply(
      xs,
      function(x) st_geometry(x)
    )
  )

  st_sf(
    id_geom = seq_along(geoms),
    geometry = geoms,
    crs = st_crs(xs[[1]])
  )
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

  # fileInput usa nombres temporales sin conservar siempre la extensión.
  # Copiamos cada componente usando el nombre original del usuario.
  copied <- character(nrow(file_df))

  for (i in seq_len(nrow(file_df))) {

    nm <- basename(
      as.character(file_df$name[i])
    )

    dst <- file.path(
      tmpdir,
      nm
    )

    ok <- file.copy(
      file_df$datapath[i],
      dst,
      overwrite = TRUE
    )

    if (!ok) {
      stop(
        "No se pudo copiar el archivo temporal: ",
        nm
      )
    }

    copied[i] <- dst
  }

  # ZIP: puede contener un shapefile completo, archivo espacial o GPKG.
  zip_files <- copied[
    tolower(
      tools::file_ext(copied)
    ) == "zip"
  ]

  extracted <- character()

  if (length(zip_files)) {

    unzip_dir <- file.path(
      tmpdir,
      "_unzipped"
    )

    dir.create(
      unzip_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )

    for (z in zip_files) {
      unzip(
        z,
        exdir = unzip_dir
      )
    }

    extracted <- list.files(
      unzip_dir,
      recursive = TRUE,
      full.names = TRUE
    )
  }

  all_files <- unique(
    c(
      copied,
      extracted
    )
  )

  exts <- tolower(
    tools::file_ext(
      all_files
    )
  )

  # -----------------------------------------------------------------
  # Validación específica de shapefile cuando se cargan partes sueltas.
  # -----------------------------------------------------------------

  shp_files <- all_files[
    exts == "shp"
  ]

  if (length(shp_files)) {

    for (shp in shp_files) {

      stem <- tools::file_path_sans_ext(
        shp
      )

      dbf <- paste0(
        stem,
        ".dbf"
      )

      shx <- paste0(
        stem,
        ".shx"
      )

      # .prj es necesario para no asumir CRS silenciosamente.
      prj <- paste0(
        stem,
        ".prj"
      )

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

  sources <- c(
    all_files[
      exts == "gpkg"
    ],
    all_files[
      exts == "kml"
    ],
    shp_files
  )

  sources <- unique(
    sources
  )

  if (!length(sources)) {
    stop(
      paste(
        "Formato no reconocido.",
        "Use KML, GPKG, ZIP con shapefile o seleccione juntos",
        ".shp + .shx + .dbf + .prj."
      )
    )
  }

  xs <- lapply(
    sources,
    function(path) {

      x <- tryCatch(
        read_vector_source(
          path
        ),
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

      x
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
      function(e) {
        paste0(
          basename(e$path),
          ": ",
          e$error
        )
      },
      character(1)
    )

    stop(
      "No se pudo leer ninguna geometría: ",
      paste(
        msgs,
        collapse = " | "
      )
    )
  }

  xs <- xs[
    !errores
  ]

  xs <- Filter(
    function(x) {
      !is.null(x) &&
        inherits(x, "sf") &&
        nrow(x) > 0
    },
    xs
  )

  if (!length(xs)) {
    stop(
      "No se encontraron geometrías vectoriales utilizables."
    )
  }

  # CRS: no asumir para SHP/GPKG sin definición.
  crs_missing <- vapply(
    xs,
    function(x) is.na(st_crs(x)),
    logical(1)
  )

  if (any(crs_missing)) {
    stop(
      paste(
        "Una o más capas no tienen CRS definido.",
        "Incluya el .prj correspondiente al shapefile",
        "o utilice un GPKG/archivo espacial con CRS válido."
      )
    )
  }

  # Pasar todo a WGS84 y reunir geometrías.
  geoms <- do.call(
    c,
    lapply(
      xs,
      function(x) {
        x <- suppressWarnings(
          st_make_valid(
            x
          )
        )

        x <- x[
          !st_is_empty(x),
        ]

        st_geometry(
          st_transform(
            x,
            4326
          )
        )
      }
    )
  )

  if (!length(geoms)) {
    stop(
      "Las capas no contienen geometrías no vacías."
    )
  }

  out <- st_sf(
    id_geom = seq_along(geoms),
    geometry = geoms,
    crs = 4326
  )

  out <- suppressWarnings(
    st_make_valid(
      out
    )
  )

  attr(
    out,
    "uploaded_names"
  ) <- paste(
    file_df$name,
    collapse = " + "
  )

  fmt <- unique(
    toupper(
      tools::file_ext(
        sources
      )
    )
  )

  attr(
    out,
    "source_format"
  ) <- paste(
    fmt,
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
    codigo_estacion = character(),
    nombre_estacion = character(),
    tipo_dato = character(),
    variable = character(),
    unidad = character(),
    expected_obs_day = integer(),
    primera_fecha = as.IDate(character()),
    ultima_fecha = as.IDate(character()),
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
      codigo_estacion,
      nombre_estacion,
      tipo_dato,
      variable,
      unidad,
      expected_obs_day,
      primera_fecha,
      ultima_fecha
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
# 4B. DIAGNÓSTICO INDIVIDUAL DE UNA SERIE
# ----------------------------------------------------------------------------

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

  d <- DAY[
    series_id == sid,
    .(
      fecha,
      n_obs_validas,
      expected_obs_day
    )
  ]

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
# 5. UI
# ----------------------------------------------------------------------------

ui <- page_sidebar(
  title = "ANA–SNIRH Spatial Station Finder",
  theme = bs_theme(bootswatch = "flatly", primary = "#176b9b"),

  sidebar = sidebar(
    width = 320,
    h5("Área de estudio", style = "font-weight:700;"),
    fileInput(
      "area_file",
      "Área espacial opcional",
      multiple = TRUE,
      accept = c(
        ".kml",
        ".gpkg",
        ".zip",
        ".shp",
        ".shx",
        ".dbf",
        ".prj",
        ".cpg",
        ".qpj",
        "application/vnd.google-earth.kml+xml",
        "application/zip"
      )
    ),
    tags$small(
      paste(
        "Formatos: KML, GPKG, ZIP de shapefile o",
        ".shp + .shx + .dbf + .prj seleccionados juntos."
      ),
      style = "color:#6c757d;"
    ),
    sliderInput("buffer_km", "Buffer de búsqueda (km)", 0, 200, BUFFER_DEFAULT_KM, step = 5),
    tags$small("50 km es un valor exploratorio por defecto cuando se carga un área; no es una distancia prescrita por la OMM.",
               style = "color:#6c757d;"),
    hr(),
    selectInput("tipo", "Variable", c("Precipitación", "Caudal"), "Precipitación"),
    dateRangeInput("periodo", "Periodo requerido",
                   start = as.Date(FDEF0), end = as.Date(FDEF1),
                   min = as.Date(FMIN), max = as.Date(FMAX),
                   format = "yyyy-mm-dd", separator = " a "),
    sliderInput("umbral", "Completitud mínima (%)", 50, 100, UMBRAL_DEFAULT, step = 1),
    checkboxInput("nominal", "Exigir cobertura nominal de todo el periodo", TRUE),
    hr(), uiOutput("kml_info")
  ),

  navset_tab(
    id = "main_nav",

    nav_panel(
      "Mapa y selección",
      value = "mapa",
      br(), uiOutput("spatial_cards"), br(),
      card(
        full_screen = TRUE,
        card_header(
          tags$span(
            "Área, buffer y estaciones candidatas",
            tags$small(
              " — clic en una estación para abrir su diagnóstico",
              style = "font-weight:400;color:#6c757d;"
            )
          )
        ),
        leafletOutput("map", height = "650px")
      )
    ),

    nav_panel(
      "Diagnóstico de estación",
      value = "diagnostico",
      br(),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header(
            tags$span(
              icon("magnifying-glass"),
              " Buscar estación"
            )
          ),

          selectizeInput(
            "diag_station",
            NULL,
            choices = NULL,
            options = list(
              placeholder = "Escriba nombre, código o unidad hidrográfica...",
              openOnFocus = FALSE,
              maxOptions = 80,
              selectOnTab = TRUE,
              closeAfterSelect = TRUE
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
            "diag_series",
            NULL,
            choices = character(),
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
          ),

          br(),

          uiOutput(
            "ana_download_ui"
          )
        )
      ),

      uiOutput("diag_selected_banner"),
      uiOutput("diag_no_series"),

      br(),

      uiOutput("diag_cards"),

      br(),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header("Completitud anual"),
          plotOutput("diag_annual_plot", height = "390px")
        ),

        card(
          card_header("Completitud mensual"),
          plotOutput("diag_monthly_heatmap", height = "390px")
        )
      ),

      br(),

      layout_columns(
        col_widths = c(6, 6),

        card(
          full_screen = TRUE,
          card_header("Mayores rachas de días completamente vacíos"),
          DTOutput("diag_gap_table")
        ),

        card(
          card_header("Continuidad y estructura del faltante"),
          uiOutput("diag_continuity")
        )
      ),

      br(),

      card(
        full_screen = TRUE,
        card_header("Resumen anual"),
        DTOutput("diag_annual_table")
      )
    ),

    nav_panel(
      "Estaciones candidatas", br(), uiOutput("temporal_msg"), br(), uiOutput("temporal_cards"), br(),
      card(full_screen = TRUE, card_header("Selección espacial + disponibilidad temporal"), DTOutput("candidate_table")),
      br(),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Completitud de observaciones"), plotOutput("comp_plot", height = "360px")),
        card(card_header("Estaciones por distancia"), plotOutput("dist_plot", height = "360px"))
      )
    ),

    nav_panel(
      "Ventana común por años", br(),

      layout_columns(
        col_widths = c(4, 4, 4),

        card(
          card_header("Longitud requerida"),
          numericInput(
            "rw_years",
            "Años consecutivos",
            value = 30,
            min = 1,
            max = 100,
            step = 1
          )
        ),

        card(
          card_header("Completitud"),
          sliderInput(
            "rw_threshold",
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
            "rw_nominal",
            "Exigir que la serie cubra toda la ventana",
            value = TRUE
          ),
          tags$small(
            paste(
              "Se aplica al universo actual:",
              "todo el Perú sin archivo espacial o estaciones dentro del buffer si hay archivo espacial."
            ),
            style = "color:#6c757d;"
          )
        )
      ),

      actionButton(
        "rw_run",
        "Buscar todas las ventanas",
        class = "btn-primary"
      ),

      br(), br(),

      uiOutput("rw_message"),
      uiOutput("rw_cards"),

      br(),

      layout_columns(
        col_widths = c(7, 5),

        card(
          full_screen = TRUE,
          card_header("Ranking de ventanas consecutivas"),
          DTOutput("rw_table")
        ),

        card(
          card_header("Estaciones aptas por año inicial"),
          plotOutput("rw_plot", height = "420px")
        )
      ),

      br(),

      card(
        full_screen = TRUE,
        card_header("Detalle de la ventana seleccionada"),
        tags$small(
          "Seleccione una fila del ranking; si no selecciona ninguna, se muestra la mejor ventana.",
          style = "color:#6c757d;"
        ),
        br(), br(),
        DTOutput("rw_detail")
      )
    ),

    nav_panel(
      "Diagnóstico OMM", br(),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Contexto fisiográfico"),
          selectInput("wmo_region", "Región de referencia", WMO_DENSITY$region, "Montañosa"),
          conditionalPanel(
            condition = "input.tipo == 'Precipitación'",
            radioButtons("wmo_p_type", "Referencia pluviométrica",
                         c("No registradora" = "no_reg", "Registradora" = "reg"), "no_reg")
          ),
          tags$small(
            "La densidad se calcula sobre el inventario físico y no usa filtros temporales.",
            style = "color:#6c757d;"
          )
        ),
        card(card_header("Referencia seleccionada"), uiOutput("wmo_ref"))
      ),
      br(), uiOutput("wmo_cards"), br(),
      card(card_header("Interpretación"), uiOutput("wmo_text")), br(),
      card(card_header("Tabla de referencias utilizada"), DTOutput("wmo_table"))
    ),

    nav_panel(
      "Metodología", br(),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Selección espacial"),
             tags$ul(
               tags$li("El archivo espacial es opcional. Sin archivo espacial se explora todo el inventario nacional; si se carga uno, se reproyecta a una zona UTM calculada desde su centroide."),
               tags$li("El buffer y las distancias se calculan en metros."),
               tags$li("La distancia es la mínima desde cada estación a la geometría original."),
               tags$li("Las estaciones dentro del polígono tienen distancia 0 km.")
             )),
        card(card_header("Selección temporal"),
             tags$ul(
               tags$li("Todas las estaciones se comparan contra el mismo periodo calendario."),
               tags$li("La completitud considera la frecuencia esperada de cada serie."),
               tags$li("Una precipitación de 12 h puede requerir dos observaciones por día."),
               tags$li("Si una estación tiene varias series, el diagnóstico individual permite inspeccionarlas por separado; en comparaciones masivas se usa la mejor del periodo sin fusionarlas."),
               tags$li("La aplicación no redistribuye los valores observados. Para descargar datos, dirige al usuario al visor oficial ANA/SNIRH e indica la cuenca, la capa hidrométrica/pluviométrica y la estación que debe localizar.")
             ))
      ),
      br(),
      card(
        card_header("Referencia OMM/WMO"),
        p(
          paste(
            "El módulo OMM es exclusivamente espacial.",
            "No utiliza periodo, completitud ni cobertura nominal.",
            "La densidad de red es una referencia de diseño y debe interpretarse",
            "junto con representatividad y fitness for purpose."
          )
        ),
        tags$a(href = "https://wmo.int/media/magazine-article/5-essential-elements-of-hydrological-monitoring-programme",
               target = "_blank", rel = "noopener noreferrer",
               "WMO — The 5 Essential Elements of a Hydrological Monitoring Programme")
      )
    )
  )
)

# ----------------------------------------------------------------------------
# 6. SERVER
# ----------------------------------------------------------------------------

server <- function(input, output, session) {

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


  spatial <- reactive({
    if (is.null(input$area_file) || !nrow(input$area_file)) {
      return(list(
        has_kml = FALSE,
        area = NULL,
        epsg = NA_integer_,
        target = NULL,
        target_m = NULL,
        buffer = NULL,
        area_km2 = NA_real_
      ))
    }

    x <- study_area()
    epsg <- utm_epsg(x)
    target <- st_sf(id = 1, geometry = st_union(st_geometry(x)), crs = 4326)
    target_m <- st_transform(target, epsg)
    buff_m <- st_buffer(target_m, input$buffer_km * 1000)

    list(
      has_kml = TRUE,
      area = x,
      epsg = epsg,
      target = target,
      target_m = target_m,
      buffer = st_transform(buff_m, 4326),
      area_km2 = polygon_area_km2(x, epsg)
    )
  })

  spatial_stations <- reactive({
    s <- spatial()
    pts <- STATIONS_SF[STATIONS_SF$tipo_dato == input$tipo, ]

    if (!nrow(pts)) {
      return(list(sf = pts, tab = data.table()))
    }

    # Modo nacional: el archivo espacial es opcional.
    if (!isTRUE(s$has_kml)) {
      pts$distancia_km <- NA_real_
      pts$dentro_kml <- NA
      pts$dentro_buffer <- TRUE
      pts$zona_distancia <- "Inventario nacional"
      return(list(
        sf = pts,
        tab = as.data.table(st_drop_geometry(pts))
      ))
    }

    # Modo espacial: área + buffer.
    pts_m <- st_transform(pts, s$epsg)
    dm <- as.numeric(st_distance(pts_m, s$target_m)[, 1])
    pts$distancia_km <- dm / 1000
    pts$dentro_kml <- dm <= 1e-6
    pts$dentro_buffer <- dm <= input$buffer_km * 1000 + 1e-6
    pts$zona_distancia <- zone_label(pts$distancia_km, input$buffer_km)
    pts <- pts[pts$dentro_buffer, ]

    list(
      sf = pts,
      tab = as.data.table(st_drop_geometry(pts))
    )
  })

  candidates <- reactive({

    sp <- spatial_stations()$tab

    if (!nrow(sp)) {
      return(
        data.table()
      )
    }

    # Solo las estaciones con series del catálogo tienen un station_id
    # que puede buscarse directamente en SERIES.
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

    # ev siempre contiene station_id aunque tenga 0 filas.
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
        out[
          is.na(get(z)),
          (z) := 0
        ]
      }
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
        out[
          is.na(get(z)),
          (z) := 0L
        ]
      }
    }

    if (!"cubre_nominalmente" %in% names(out)) {
      out[, cubre_nominalmente := FALSE]
    } else {
      out[
        is.na(cubre_nominalmente),
        cubre_nominalmente := FALSE
      ]
    }

    out[
      is.na(tiene_serie),
      tiene_serie := FALSE
    ]

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

    setorder(
      out,
      -apta,
      -tiene_serie,
      -completitud_obs_pct,
      distancia_km
    )

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
      tags$small(paste0("CRS métrico interno: EPSG:", s$epsg), style = "color:#6c757d;"), br(),
      tags$small(
        paste0("Área poligonal: ", if (is.na(s$area_km2)) "no aplicable" else paste0(fmt_num(s$area_km2, 1), " km²")),
        style = "color:#6c757d;"
      )
    )
  })

  output$spatial_cards <- renderUI({
    x <- spatial_stations()$tab
    s <- spatial()

    if (!isTRUE(s$has_kml)) {
      return(layout_columns(
        col_widths = c(4, 4, 4),
        metric_card(
          "Inventario nacional",
          fmt_num(nrow(x)),
          paste0(input$tipo, " | ", fmt_num(sum(x$tiene_serie, na.rm = TRUE)), " con serie")
        ),
        metric_card("Filtro espacial", "Desactivado", "Cargue un archivo espacial para usar buffer y distancias"),
        metric_card(
          "Rango global",
          paste0(format(as.Date(FMIN), "%Y"), "–", format(as.Date(FMAX), "%Y"))
        )
      ))
    }

    layout_columns(
      col_widths = c(3, 3, 3, 3),
      metric_card("Candidatas", fmt_num(nrow(x)), paste0(input$tipo, " dentro de ", input$buffer_km, " km")),
      metric_card("Dentro del área", fmt_num(sum(x$dentro_kml, na.rm = TRUE))),
      metric_card("Solo en buffer", fmt_num(sum(!x$dentro_kml, na.rm = TRUE))),
      metric_card("Área cargada", if (is.na(s$area_km2)) "—" else paste0(fmt_num(s$area_km2, 1), " km²"))
    )
  })

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -75, lat = -9.5, zoom = 5)
  })

  observe({
    s <- spatial()
    pts <- spatial_stations()$sf

    # Defensa adicional: jamás dibujar un tipo distinto al selector actual.
    if (nrow(pts)) {
      pts <- pts[
        pts$tipo_dato == input$tipo,
      ]
    }

    ev <- candidates()

    p <- leafletProxy("map") %>%
      clearGroup("Estaciones_filtradas") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls()

    # archivo espacial and buffer are only drawn when supplied.
    if (isTRUE(s$has_kml)) {
      p <- p %>% addPolygons(
        data = s$buffer,
        fillOpacity = 0.07,
        color = "#6c757d",
        weight = 2,
        dashArray = "6,6",
        group = "Buffer",
        label = paste0("Buffer ", input$buffer_km, " km")
      )

      gtypes <- as.character(st_geometry_type(s$area))

      if (any(grepl("POLYGON", gtypes))) {
        pol <- suppressWarnings(st_collection_extract(s$area, "POLYGON"))
        if (nrow(pol)) {
          p <- p %>% addPolygons(
            data = pol,
            fillOpacity = 0.15,
            color = "#1f2d3d",
            weight = 3,
            group = "Área cargada"
          )
        }
      }

      if (any(grepl("LINESTRING", gtypes))) {
        ln <- suppressWarnings(st_collection_extract(s$area, "LINESTRING"))
        if (nrow(ln)) {
          p <- p %>% addPolylines(
            data = ln,
            color = "#1f2d3d",
            weight = 4,
            group = "Área cargada"
          )
        }
      }
    }

    if (nrow(pts)) {
      idx <- match(pts$station_id, ev$station_id)
      pts$variable_sel <- ev$variable[idx]
      pts$comp <- ev$completitud_obs_pct[idx]
      pts$full_days <- ev$dias_completos_pct[idx]
      pts$nominal <- ev$cubre_nominalmente[idx]
      pts$apta <- ev$apta[idx]
      pts$serie_disp <- ev$tiene_serie[idx]

      pts$apta[is.na(pts$apta)] <- FALSE
      pts$serie_disp[is.na(pts$serie_disp)] <- FALSE
      pts$comp[is.na(pts$comp)] <- 0
      pts$full_days[is.na(pts$full_days)] <- 0
      pts$nominal[is.na(pts$nominal)] <- FALSE

      p <- p %>% addCircleMarkers(
        data = pts,
        lng = ~longitud,
        lat = ~latitud,
        layerId = ~station_id,
        group = "Estaciones_filtradas",
        radius = ~ifelse(apta, 7, 5),
        weight = ~ifelse(apta, 2, 1),
        fillOpacity = ~ifelse(apta, .90, .50),
        color = ~ifelse(apta, "#176b9b", "#7f8c8d"),
        fillColor = ~ifelse(apta, "#3498db", "#bdc3c7"),
        clusterOptions = markerClusterOptions(maxClusterRadius = 35),
        label = ~ifelse(
          is.na(distancia_km),
          nombre_estacion,
          paste0(nombre_estacion, " — ", round(distancia_km, 1), " km")
        ),
        popup = ~paste0(
          "<div style='min-width:230px'><b>", nombre_estacion, "</b><br>",
          "<b>Código:</b> ", ifelse(is.na(codigo_estacion), "—", codigo_estacion), "<br>",
          "<b>Variable:</b> ", ifelse(is.na(variable_sel), "—", variable_sel), "<br>",
          "<b>Serie disponible:</b> ", ifelse(serie_disp, "Sí", "No"), "<br>",
          ifelse(
            is.na(distancia_km),
            "",
            paste0("<b>Distancia al área:</b> ", round(distancia_km, 2), " km<br>")
          ),
          "<b>Completitud:</b> ", round(comp, 1), "%<br>",
          "<b>Días completos:</b> ", round(full_days, 1), "%<br>",
          "<b>Cubre periodo:</b> ", ifelse(nominal, "Sí", "No"), "<br>",
          "<b>Apta:</b> ", ifelse(apta, "Sí", "No"), "</div>"
        )
      )
    }

    if (isTRUE(s$has_kml)) {
      bb <- st_bbox(s$buffer)
      p <- p %>% fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    } else if (nrow(pts)) {
      bb <- st_bbox(pts)
      p <- p %>% fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    }

    p %>% addLegend(
      position = "bottomright",
      colors = c("#3498db", "#bdc3c7"),
      labels = c(paste0("Apta ≥ ", input$umbral, "%"), "No apta"),
      opacity = .9,
      title = paste0(input$tipo, " — selección temporal")
    )
  })




  # --------------------------------------------------------------------------
  # ESTADO ÚNICO DE ESTACIÓN SELECCIONADA
  #
  # El diagnóstico NO depende exclusivamente del valor del widget selectize.
  # Tanto la búsqueda manual como el clic en el mapa escriben aquí.
  # Esto evita que updateSelectizeInput(server = TRUE) se quede visualmente
  # actualizado pero no invalide correctamente el diagnóstico en clics sucesivos.
  # --------------------------------------------------------------------------

  diag_station_state <- reactiveVal(NULL)


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
        paste0(" — Cuenca ", unidad_hidrografica)
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
  # NAVEGACIÓN DIRECTA DESDE EL MAPA
  #
  # Un clic en una estación:
  #   1) selecciona station_id
  #   2) abre la pestaña Diagnóstico de estación
  #   3) el observer de diag_station actualiza automáticamente IDConfig
  # --------------------------------------------------------------------------

  observeEvent(
    input$map_marker_click,
    {
      click <- input$map_marker_click

      if (
        is.null(click$id) ||
        is.na(click$id) ||
        !nzchar(as.character(click$id))
      ) {
        return()
      }

      sid_click <- as.character(click$id)

      # Verificar que la estación pertenezca al universo actual.
      sc <- station_choices()

      if (
        !nrow(sc$table) ||
        !(sid_click %in% sc$table$station_id)
      ) {
        return()
      }

      # 1) Actualizar SIEMPRE el estado lógico primero.
      # Esto hace que el diagnóstico cambie aunque Selectize tarde.
      diag_station_state(
        sid_click
      )

      # 2) Abrir diagnóstico.
      bslib::nav_select(
        id = "main_nav",
        selected = "diagnostico",
        session = session
      )

      # 3) Sincronizar visualmente la lupa con TODAS sus opciones.
      # Reenviar choices + selected es más robusto con server=TRUE que
      # enviar únicamente selected.
      updateSelectizeInput(
        session,
        "diag_station",
        choices = sc$choices,
        selected = sid_click,
        server = TRUE
      )
    },
    ignoreInit = TRUE
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
          tipo_dato == input$tipo
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


  diag_series_state <- reactiveVal(NULL)


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
            tipo_dato == input$tipo
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
        tipo_dato == input$tipo,
      .N
    ]

    if (!valid) {
      return(NULL)
    }

    diagnosticar_serie(
      sid_series
    )
  })


  output$ana_download_ui <- renderUI({

    sid_series <- diag_series_state()

    if (
      is.null(
        sid_series
      )
    ) {
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
        input$tipo,
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

    div(
      class = "border rounded p-3 mt-2",
      style = "background:#F8FAFC;border-color:#DCE3EA !important;",

      tags$b(
        "Acceso a la serie"
      ),

      br(), br(),

      if (raw_available) {
        tagList(
          tags$a(
            href = raw_url,
            target = "_blank",
            rel = "noopener noreferrer",
            class = "btn btn-primary me-2 mb-2",
            icon("file-excel"),
            " Descargar reporte original ANA/SNIRH (.xlsx)"
          ),

          br(),

          tags$small(
            "Copia archivada sin modificaciones del reporte XLSX oficial obtenido de ANA/SNIRH. ",
            "El archivo conserva el formato y metadatos originales de ANA.",
            style = "color:#6c757d;"
          ),

          br(), br()
        )
      } else {
        tagList(
          div(
            class = "alert alert-warning py-2",
            "No se encontró una copia RAW archivada para este IDConfig. ",
            "Puede consultar la serie directamente en el visor oficial ANA/SNIRH."
          )
        )
      },

      tags$a(
        href = url_visor_ana,
        target = "_blank",
        rel = "noopener noreferrer",
        class = "btn btn-outline-primary mb-2",
        icon("arrow-up-right-from-square"),
        " Abrir visor oficial ANA/SNIRH"
      ),

      br(), br(),

      tags$span(
        "Para localizar la estación en ANA:"
      ),

      tags$ol(
        style = "margin-top:.4rem;margin-bottom:.2rem;",
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
  })


  output$diag_selected_banner <- renderUI({

    sid <- diag_station_state()

    if (is.null(sid)) {
      return(NULL)
    }

    x <- STATIONS[
      station_id == sid &
        tipo_dato == input$tipo
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
        tipo_dato == input$tipo
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
      col_widths = c(3, 3, 3, 3),

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
        paste0(
          "Meses vacíos máx.: ",
          x$max_racha_meses_vacios
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
  })


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

    div(
      tags$h5(
        paste0(
          x$nombre_estacion,
          " — ",
          x$variable
        )
      ),

      tags$ul(
        tags$li(
          tags$b("Frecuencia esperada: "),
          x$expected_obs_day,
          " observación(es)/día."
        ),

        tags$li(
          tags$b("Días: "),
          fmt_num(x$dias_completos),
          " completos; ",
          fmt_num(x$dias_parciales),
          " parciales; ",
          fmt_num(x$dias_vacios),
          " completamente vacíos."
        ),

        tags$li(
          tags$b("Cobertura de días con algún dato: "),
          fmt_pct(x$cobertura_dias_pct, 2),
          "; días completos: ",
          fmt_pct(x$dias_completos_pct, 2),
          "."
        ),

        tags$li(
          tags$b("Completitud anual min / mediana / max: "),
          fmt_pct(x$completitud_anual_min, 1),
          " / ",
          fmt_pct(x$completitud_anual_mediana, 1),
          " / ",
          fmt_pct(x$completitud_anual_max, 1),
          "."
        ),

        tags$li(
          tags$b("Completitud mensual min / mediana / max: "),
          fmt_pct(x$completitud_mensual_min, 1),
          " / ",
          fmt_pct(x$completitud_mensual_mediana, 1),
          " / ",
          fmt_pct(x$completitud_mensual_max, 1),
          "."
        ),

        tags$li(
          tags$b("Racha vacía mediana / P95 / máxima: "),
          round(x$mediana_racha_vacia_dias, 1),
          " / ",
          round(x$p95_racha_vacia_dias, 1),
          " / ",
          x$max_racha_vacia_dias,
          " días."
        ),

        tags$li(
          tags$b("Mayor racha de días parciales: "),
          x$max_racha_parcial_dias,
          " días."
        ),

        tags$li(
          tags$b("Mayor racha de meses totalmente vacíos: "),
          x$max_racha_meses_vacios,
          "; años totalmente vacíos: ",
          x$max_racha_anios_vacios,
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
  })


  output$temporal_msg <- renderUI({

    x <- candidates()

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
        completitud_obs_pct >= input$umbral,
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

    if (isTRUE(input$nominal)) {

      div(
        class = "alert alert-info",

        tags$b(
          paste0(
            input$tipo,
            " — ",
            input$periodo[1],
            " a ",
            input$periodo[2]
          )
        ),

        br(),

        paste0(
          n_comp,
          " de ",
          n_total,
          " alcanzan ≥",
          input$umbral,
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
            input$tipo,
            " — ",
            input$periodo[1],
            " a ",
            input$periodo[2]
          )
        ),

        br(),

        paste0(
          n_comp,
          " de ",
          n_total,
          " alcanzan ≥",
          input$umbral,
          "% de completitud."
        )
      )
    }
  })

  output$temporal_cards <- renderUI({

    x <- candidates()

    if (!nrow(x)) {
      return(NULL)
    }

    n_comp <- x[
      tiene_serie %in% TRUE &
        completitud_obs_pct >= input$umbral,
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

    layout_columns(
      col_widths = c(3, 3, 3, 3),

      metric_card(
        if (isTRUE(spatial()$has_kml)) {
          "Candidatas espaciales"
        } else {
          "Estaciones nacionales"
        },
        fmt_num(nrow(x)),
        paste0(
          fmt_num(
            sum(
              x$tiene_serie,
              na.rm = TRUE
            )
          ),
          " con serie disponible"
        )
      ),

      metric_card(
        paste0(
          "Completitud ≥ ",
          input$umbral,
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
          input$periodo[1],
          " y termina ≥ ",
          input$periodo[2]
        )
      ),

      metric_card(
        "Aptas finales",
        fmt_num(n_apta),
        if (isTRUE(input$nominal)) {
          paste0(
            "≥",
            input$umbral,
            "% + cobertura nominal"
          )
        } else {
          paste0(
            "≥",
            input$umbral,
            "% de completitud"
          )
        }
      )
    )
  })

  output$candidate_table <- renderDT({
    x <- copy(candidates())
    if (!nrow(x)) return(datatable(data.frame(Mensaje = "Sin estaciones candidatas."), rownames = FALSE))

    tab <- x[, .(
      Apta = apta,
      Zona = zona_distancia,
      `Distancia al archivo espacial (km)` = ifelse(is.na(distancia_km), NA_real_, round(distancia_km, 2)),
      Estacion = nombre_estacion,
      Codigo = codigo_estacion,
      `Serie disponible` = tiene_serie,
      `Variable seleccionada` = variable,
      Unidad = unidad,
      `Obs/día esperadas` = expected_obs_day,
      `Primera fecha serie` = as.Date(primera_fecha),
      `Última fecha serie` = as.Date(ultima_fecha),
      `Completitud periodo (%)` = round(completitud_obs_pct, 2),
      `Cobertura días (%)` = round(cobertura_dias_pct, 2),
      `Días completos (%)` = round(dias_completos_pct, 2),
      `Días parciales` = n_dias_parciales,
      `Cubre periodo nominal` = cubre_nominalmente,
      `Tipo estación` = tipo_estacion,
      `Unidad hidrográfica` = unidad_hidrografica,
      AAA = aaa, ALA = ala, Latitud = latitud, Longitud = longitud
    )]

    datatable(tab, filter = "top", extensions = "Buttons", options = dt_opts(30),
              rownames = FALSE, class = "stripe hover compact")
  })

  output$comp_plot <- renderPlot({

    x <- candidates()

    shiny::validate(
      shiny::need(
        nrow(x) > 0,
        "Sin estaciones."
      )
    )

    ggplot(
      x,
      aes(
        completitud_obs_pct
      )
    ) +
      geom_histogram(
        binwidth = 5,
        boundary = 0
      ) +
      geom_vline(
        xintercept = input$umbral,
        linetype = 2
      ) +
      scale_x_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 10)
      ) +
      labs(
        x = "Completitud de observaciones (%)",
        y = "Estaciones",
        subtitle = paste0(
          "Línea discontinua = ",
          input$umbral,
          "%. La cobertura nominal se evalúa por separado."
        )
      ) +
      theme_minimal(
        base_size = 11
      )
  })

  output$dist_plot <- renderPlot({
    x <- candidates(); shiny::validate(shiny::need(nrow(x) > 0, "Sin estaciones."))
    d <- x[, .N, by = zona_distancia]
    ggplot(d, aes(reorder(zona_distancia, -N), N)) +
      geom_col() + geom_text(aes(label = N), vjust = -.25) +
      labs(x = NULL, y = "Número de estaciones") + theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  })


  # --------------------------------------------------------------------------
  # BÚSQUEDA RECURSIVA DE VENTANAS N-AÑOS
  # --------------------------------------------------------------------------

  rw_result <- eventReactive(
    input$rw_run,
    {
      esp <- spatial_stations()$tab

      if (!nrow(esp)) {
        return(list(
          windows = data.table(),
          detail = data.table(),
          eligible_stations = character(),
          n_eligible = 0L
        ))
      }

      withProgress(
        message = "Evaluando ventanas consecutivas...",
        value = 0.15,
        {
          ans <- search_recursive_windows(
            station_ids = esp[
              tiene_serie %in% TRUE,
              station_id
            ],
            tipo = input$tipo,
            n_years = input$rw_years,
            threshold = input$rw_threshold,
            require_nominal = input$rw_nominal
          )
          incProgress(0.85)
          ans
        }
      )
    },
    ignoreNULL = FALSE
  )

  output$rw_message <- renderUI({
    r <- rw_result()

    if (!nrow(r$windows)) {
      return(
        div(
          class = "alert alert-warning",
          paste0(
            "No hay suficientes series nominalmente elegibles para construir ventanas de ",
            input$rw_years,
            " años en el universo actual."
          )
        )
      )
    }

    best <- r$windows[1]

    if (any(r$windows$universal)) {
      univ <- r$windows[universal == TRUE]

      div(
        class = "alert alert-success",
        tags$b("Sí existe una ventana universal. "),
        paste0(
          nrow(univ),
          " ventana(s) de ",
          input$rw_years,
          " años son cumplidas por las ",
          r$n_eligible,
          " estaciones elegibles con completitud ≥",
          input$rw_threshold,
          "%."
        )
      )
    } else {
      div(
        class = "alert alert-warning",
        tags$b("No existe una ventana que cumplan todas las estaciones elegibles. "),
        paste0(
          "La mejor es ",
          best$anio_inicio,
          "–",
          best$anio_fin,
          ": ",
          best$n_cumplen,
          " de ",
          best$n_elegibles,
          " estaciones (",
          fmt_pct(best$pct_cumplen, 1),
          ")."
        )
      )
    }
  })

  output$rw_cards <- renderUI({
    r <- rw_result()
    if (!nrow(r$windows)) return(NULL)

    best <- r$windows[1]
    scope <- if (isTRUE(spatial()$has_kml)) {
      paste0("Área + buffer ", input$buffer_km, " km")
    } else {
      "Todo el Perú"
    }

    layout_columns(
      col_widths = c(3, 3, 3, 3),
      metric_card(
        "Elegibles por longitud",
        fmt_num(r$n_eligible),
        paste0("≥ ", input$rw_years, " años nominales")
      ),
      metric_card(
        "Mejor ventana",
        paste0(best$anio_inicio, "–", best$anio_fin),
        scope
      ),
      metric_card(
        "Estaciones aptas",
        paste0(best$n_cumplen, " / ", best$n_elegibles),
        paste0("≥ ", input$rw_threshold, "%")
      ),
      metric_card(
        "Cobertura de elegibles",
        fmt_pct(best$pct_cumplen, 1)
      )
    )
  })

  output$rw_table <- renderDT({
    r <- rw_result()

    if (!nrow(r$windows)) {
      return(
        datatable(
          data.frame(Mensaje = "Sin ventanas disponibles."),
          rownames = FALSE
        )
      )
    }

    x <- r$windows[, .(
      Ranking = ranking,
      `Año inicio` = anio_inicio,
      `Año fin` = anio_fin,
      Elegibles = n_elegibles,
      `Cubren nominalmente` = n_cubren_nominal,
      Cumplen = n_cumplen,
      `Cumplen (%)` = round(pct_cumplen, 2),
      `Completitud mediana (%)` = round(completitud_mediana, 2),
      `Mediana aptas (%)` = round(completitud_mediana_aptas, 2),
      Universal = universal
    )]

    datatable(
      x,
      selection = "single",
      extensions = "Buttons",
      options = dt_opts(15),
      rownames = FALSE,
      class = "stripe hover compact"
    )
  })

  rw_active_rank <- reactive({
    r <- rw_result()
    req(nrow(r$windows) > 0)

    sel <- input$rw_table_rows_selected

    if (!length(sel)) {
      return(r$windows$ranking[1])
    }

    r$windows$ranking[sel[1]]
  })

  output$rw_plot <- renderPlot({
    r <- rw_result()

    shiny::validate(
      shiny::need(nrow(r$windows) > 0, "Sin ventanas.")
    )

    ggplot(
      r$windows,
      aes(
        x = anio_inicio,
        y = n_cumplen
      )
    ) +
      geom_line() +
      geom_point(size = 1.7) +
      geom_hline(
        yintercept = r$n_eligible,
        linetype = 2
      ) +
      labs(
        x = "Año inicial de la ventana",
        y = "Estaciones que cumplen",
        subtitle = paste0(
          input$rw_years,
          " años | completitud ≥",
          input$rw_threshold,
          "% | línea discontinua = todas las elegibles"
        )
      ) +
      theme_minimal(base_size = 11)
  })

  output$rw_detail <- renderDT({
    r <- rw_result()
    req(nrow(r$windows) > 0)

    rank <- rw_active_rank()

    x <- copy(
      r$detail[
        ranking == rank
      ]
    )

    setorder(
      x,
      -apta,
      -completitud_obs_pct,
      -cubre_nominalmente
    )

    tab <- x[, .(
      Apta = apta,
      Estacion = nombre_estacion,
      Codigo = codigo_estacion,
      `Variable/serie elegida` = variable,
      Unidad = unidad,
      `Primera fecha serie` = as.Date(primera_fecha),
      `Última fecha serie` = as.Date(ultima_fecha),
      `Completitud ventana (%)` = round(completitud_obs_pct, 2),
      `Cobertura días (%)` = round(cobertura_dias_pct, 2),
      `Días completos (%)` = round(dias_completos_pct, 2),
      `Cubre nominalmente` = cubre_nominalmente,
      `Tipo estación` = tipo_estacion,
      `Unidad hidrográfica` = unidad_hidrografica,
      AAA = aaa,
      ALA = ala,
      Latitud = latitud,
      Longitud = longitud
    )]

    datatable(
      tab,
      filter = "top",
      extensions = "Buttons",
      options = dt_opts(30),
      rownames = FALSE,
      class = "stripe hover compact"
    )
  })

  wmo <- reactive({

    s <- spatial()

    if (!isTRUE(s$has_kml)) {
      return(list(
        has_kml = FALSE,
        region = input$wmo_region,
        label = NA_character_,
        ref = NA_real_,
        area = NA_real_,
        n_all = NA_integer_,
        dens_all = NA_real_,
        n_ref = NA_integer_
      ))
    }

    row <- WMO_DENSITY[
      region == input$wmo_region
    ]

    if (input$tipo == "Caudal") {

      ref <- row$caudal
      label <- "Caudal"

    } else if (input$wmo_p_type == "reg") {

      ref <- row$precip_reg
      label <- "Precipitación registradora"

    } else {

      ref <- row$precip_no_reg
      label <- "Precipitación no registradora"
    }

    # --------------------------------------------------------------
    # OMM = contexto espacial exclusivamente.
    #
    # NO intervienen:
    #   - periodo seleccionado
    #   - completitud mínima
    #   - cobertura nominal
    #   - disponibilidad temporal de la serie
    #
    # Se cuentan estaciones físicas del inventario dentro del área.
    # --------------------------------------------------------------

    x <- spatial_stations()$tab

    if (nrow(x)) {

      x <- x[
        dentro_kml %in% TRUE
      ]

      n_all <- uniqueN(
        x$station_id
      )

    } else {

      n_all <- 0L
    }

    area <- s$area_km2

    dens_all <- if (
      !is.na(area) &&
      n_all > 0
    ) {
      area / n_all
    } else {
      NA_real_
    }

    n_ref <- if (
      !is.na(area) &&
      ref > 0
    ) {
      max(
        1L,
        ceiling(
          area / ref
        )
      )
    } else {
      NA_integer_
    }

    list(
      has_kml = TRUE,
      region = input$wmo_region,
      label = label,
      ref = ref,
      area = area,
      n_all = n_all,
      dens_all = dens_all,
      n_ref = n_ref
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

    if (!isTRUE(v$has_kml)) {
      return(NULL)
    }

    layout_columns(
      col_widths = c(3, 3, 3, 3),

      metric_card(
        "Área analizada",
        if (is.na(v$area)) {
          "—"
        } else {
          paste0(
            fmt_num(v$area, 1),
            " km²"
          )
        }
      ),

      metric_card(
        "Estaciones dentro",
        fmt_num(v$n_all),
        "Inventario físico"
      ),

      metric_card(
        "Densidad observada",
        if (is.na(v$dens_all)) {
          "—"
        } else {
          paste0(
            fmt_num(v$dens_all, 1),
            " km²/estación"
          )
        },
        "Área / estaciones observadas"
      ),

      metric_card(
        "Equivalente de referencia",
        if (is.na(v$n_ref)) {
          "—"
        } else {
          paste0(
            "≈ ",
            v$n_ref,
            " estación(es)"
          )
        },
        "Área / referencia OMM"
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

    if (is.na(v$area)) return(div(class = "alert alert-warning", "El archivo espacial no contiene área poligonal; no se puede calcular km²/estación."))
    if (!v$n_all) return(div(class = "alert alert-warning", "No hay estaciones de la variable seleccionada dentro del archivo espacial."))
    pass <- v$dens_all <= v$ref
    div(class = if (pass) "alert alert-success" else "alert alert-warning",
        tags$b(if (pass) "La densidad observada es igual o más densa que la referencia seleccionada."
               else "La densidad observada es menos densa que la referencia seleccionada."),
        br(),
        paste0(
          "Inventario físico: ",
          fmt_num(v$dens_all, 1),
          " km²/estación; referencia: ",
          fmt_num(v$ref),
          " km²/estación."
        ),
        br(), br(),
        tags$small(
          paste(
            "Este diagnóstico es exclusivamente espacial e independiente",
            "del periodo seleccionado, la completitud y la cobertura temporal.",
            "No sustituye representatividad hidrológica, topográfica, climática",
            "ni fitness for purpose."
          )
        )
      )
  })

  output$wmo_table <- renderDT({
    x <- copy(WMO_DENSITY)
    setnames(x,
             c("region", "precip_no_reg", "precip_reg", "caudal"),
             c("Región fisiográfica", "Precip. no registradora (km²/est.)",
               "Precip. registradora (km²/est.)", "Caudal (km²/est.)"))
    datatable(x, options = list(paging = FALSE, searching = FALSE, info = FALSE),
              rownames = FALSE, class = "stripe hover compact")
  })
}

shinyApp(ui, server)

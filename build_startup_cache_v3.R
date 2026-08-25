# ============================================================================
# BUILD STARTUP CACHE v3 — ANA–SNIRH Spatial Station Finder
#
# Colocar este archivo junto a app.R y ejecutarlo con Source.
#
# Genera:
#   data/startup_cache_v3/manifest.rds
#   data/startup_cache_v3/objects.rds
#   data/startup_cache_v3/day_*.rds
#
# v3:
#   - DAY reducido a 4 columnas:
#       series_id, fecha, n_obs_validas, expected_obs_day
#   - caché sin compresión y fragmentos menores de 50 MiB
#   - validación de fuentes mediante MD5
# ============================================================================

script_path <- tryCatch(
  normalizePath(
    sys.frame(1)$ofile,
    winslash = "/",
    mustWork = TRUE
  ),
  error = function(e) NA_character_
)

app_dir <- if (!is.na(script_path)) {
  dirname(script_path)
} else {
  normalizePath(
    getwd(),
    winslash = "/",
    mustWork = TRUE
  )
}

if (
  !file.exists(file.path(app_dir, "app.R")) ||
    !file.exists(file.path(app_dir, "R", "core_data.R"))
) {
  stop(
    "No se pudo identificar la carpeta de la aplicación desde: ",
    app_dir
  )
}

locale_profile <- file.path(app_dir, ".Rprofile")

if (file.exists(locale_profile)) {
  sys.source(locale_profile, envir = .GlobalEnv)
}
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(app_dir)

message("Aplicación: ", app_dir)

required <- c(
  "data.table",
  "sf",
  "lubridate"
)

missing <- required[
  !vapply(
    required,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing)) {
  stop(
    "Faltan paquetes: ",
    paste(missing, collapse = ", ")
  )
}

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(lubridate))

core_file <- file.path(app_dir, "R", "core_data.R")

if (!file.exists(core_file)) {
  stop("No se encontró: ", core_file)
}

code_env <- new.env(parent = .GlobalEnv)
sys.source(core_file, envir = code_env)

if (!exists("init_core_data", envir = code_env, inherits = FALSE)) {
  stop("R/core_data.R no define init_core_data().")
}

old_ignore <- getOption("ana.ignore.startup.cache")
old_profile <- getOption("ana.profile.startup")

on.exit(
  {
    options(ana.ignore.startup.cache = old_ignore)
    options(ana.profile.startup = old_profile)
  },
  add = TRUE
)

options(
  ana.ignore.startup.cache = TRUE,
  ana.profile.startup = TRUE
)

data_env <- new.env(parent = .GlobalEnv)

message("\nReconstruyendo objetos desde las fuentes...")

t_build <- system.time(
  code_env$init_core_data(data_env)
)[["elapsed"]]

object_names <- c(
  "DAY",
  "RAW_INDEX",
  "SERIES",
  "STATIONS",
  "STATIONS_SF",
  "YEAR_OBS",
  "WMO_DENSITY",
  "FMIN",
  "FMAX",
  "FDEF0",
  "FDEF1",
  "BUFFER_DEFAULT_KM",
  "UMBRAL_DEFAULT"
)

missing_objects <- object_names[
  !vapply(
    object_names,
    exists,
    logical(1),
    envir = data_env,
    inherits = FALSE
  )
]

if (length(missing_objects)) {
  stop(
    "No se generaron objetos requeridos: ",
    paste(missing_objects, collapse = ", ")
  )
}

objects <- mget(
  object_names,
  envir = data_env,
  inherits = FALSE
)

missing_series_types <- setdiff(
  c("Precipitación", "Caudal"),
  unique(objects$SERIES$tipo_dato)
)

if (length(missing_series_types)) {
  stop(
    "El caché perdió tipos de serie: ",
    paste(missing_series_types, collapse = ", ")
  )
}

required_day_cols <- c(
  "series_id",
  "fecha",
  "n_obs_validas",
  "expected_obs_day"
)

missing_day <- setdiff(
  required_day_cols,
  names(objects$DAY)
)

if (length(missing_day)) {
  stop(
    "DAY no contiene columnas requeridas: ",
    paste(missing_day, collapse = ", ")
  )
}

missing_day_series <- setdiff(
  objects$SERIES$series_id,
  unique(objects$DAY$series_id)
)

if (length(missing_day_series)) {
  stop(
    "El caché perdió ",
    length(missing_day_series),
    " series presentes en SERIES. Ejemplos: ",
    paste(head(missing_day_series, 5L), collapse = ", ")
  )
}

if (objects$DAY[n_obs_validas > expected_obs_day, .N]) {
  stop("El DAY contiene observaciones válidas por encima de lo esperado por día.")
}

objects$DAY <- objects$DAY[
  ,
  ..required_day_cols
]

setkey(
  objects$DAY,
  series_id,
  fecha
)

source_files <- c(
  file.path("data", "01_catalogo_series.csv"),
  file.path("data", "01_disponibilidad_diaria.rds"),
  file.path("data", "01_inventario_estaciones_validado.csv"),
  file.path("data", "01_estaciones_sin_serie.csv"),
  file.path("data", "02_indice_raw_xlsx.csv")
)

source_signature <- code_env$cache_source_signature(source_files)

day_mb <- as.numeric(object.size(objects$DAY)) / 1024^2
year_mb <- as.numeric(object.size(objects$YEAR_OBS)) / 1024^2
series_mb <- as.numeric(object.size(objects$SERIES)) / 1024^2

cache_dir <- file.path(
  "data",
  "startup_cache_v3"
)

dir.create(
  cache_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

old_parts <- list.files(
  cache_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

if (length(old_parts)) {
  unlink(old_parts)
}

objects_file <- "objects.rds"
day_rows_per_part <- 750000L
day_starts <- seq.int(
  1L,
  nrow(objects$DAY),
  by = day_rows_per_part
)
day_files <- sprintf(
  "day_%03d.rds",
  seq_along(day_starts)
)

cache_manifest <- list(
  version = "startup-cache-v3-sharded",
  created_at = Sys.time(),
  source_signature = source_signature,
  objects_file = objects_file,
  day_files = day_files,
  day_rows = nrow(objects$DAY),
  day_columns = names(objects$DAY)
)

message("\nGuardando FAST CACHE v3 fragmentado...")

t_save <- system.time(
  {
    saveRDS(
      objects[setdiff(names(objects), "DAY")],
      file.path(cache_dir, objects_file),
      compress = FALSE
    )

    for (i in seq_along(day_starts)) {
      first <- day_starts[i]
      last <- min(
        first + day_rows_per_part - 1L,
        nrow(objects$DAY)
      )

      saveRDS(
        objects$DAY[first:last],
        file.path(cache_dir, day_files[i]),
        compress = FALSE
      )
    }

    saveRDS(
      cache_manifest,
      file.path(cache_dir, "manifest.rds"),
      compress = FALSE
    )
  }
)[["elapsed"]]

cache_files <- file.path(
  cache_dir,
  c("manifest.rds", objects_file, day_files)
)
cache_sizes_mb <- file.info(cache_files)$size / 1024^2

if (any(cache_sizes_mb >= 50)) {
  stop(
    "Hay fragmentos del caché de 50 MiB o más: ",
    paste(
      basename(cache_files[cache_sizes_mb >= 50]),
      collapse = ", "
    )
  )
}

cache_size_mb <- sum(cache_sizes_mb)

rm(data_env)
invisible(gc())

message("\nProbando lectura del FAST CACHE v3 fragmentado...")

t_read <- system.time(
  {
    test_manifest <- readRDS(
      file.path(cache_dir, "manifest.rds")
    )
    test_objects <- readRDS(
      file.path(cache_dir, test_manifest$objects_file)
    )
    test_objects$DAY <- rbindlist(
      lapply(
        file.path(cache_dir, test_manifest$day_files),
        readRDS
      ),
      use.names = TRUE
    )
    setkey(test_objects$DAY, series_id, fecha)
  }
)[["elapsed"]]

if (
  !identical(
    names(test_objects$DAY),
    required_day_cols
  ) ||
    nrow(test_objects$DAY) != nrow(objects$DAY)
) {
  stop(
    "Verificación fallida del DAY fragmentado."
  )
}

legacy_cache_file <- file.path(
  "data",
  "00_startup_cache_v3.rds"
)

if (file.exists(legacy_cache_file)) {
  unlink(legacy_cache_file)
}

rm(test_manifest, test_objects, objects)
invisible(gc())

speedup <- if (
  is.finite(t_read) &&
  t_read > 0
) {
  t_build / t_read
} else {
  NA_real_
}

message("\n================ FAST CACHE v3 ================")
message(sprintf("Inicialización completa fuente : %.3f s", t_build))
message(sprintf("Leer FAST CACHE v3            : %.3f s", t_read))
message(sprintf("Guardar FAST CACHE v3         : %.3f s", t_save))
message(sprintf("Tamaño FAST CACHE v3          : %.1f MB", cache_size_mb))
message(sprintf("Fragmentos DAY                : %d", length(day_files)))
message(sprintf("Fragmento mayor               : %.1f MB", max(cache_sizes_mb)))
message(sprintf("DAY reducido en RAM           : %.1f MB", day_mb))
message(sprintf("YEAR_OBS en RAM               : %.1f MB", year_mb))
message(sprintf("SERIES en RAM                 : %.1f MB", series_mb))

if (is.finite(speedup)) {
  message(sprintf("Aceleración teórica           : %.1fx", speedup))
}

message(
  "Columnas DAY                   : ",
  paste(required_day_cols, collapse = ", ")
)

message("Firma de fuentes               : MD5")
message("================================================")
message("\nAhora ejecute run_profile.R para medir el arranque real.")

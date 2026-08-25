# ============================================================================
# CAPAS CARTOGRÁFICAS FIJAS DE REFERENCIA
# Solo visualización. No participan en filtros, distancias ni análisis.
# ============================================================================

init_spatial_reference <- function(target_env = parent.frame()) {

  F_CUENCAS_ANA <- file.path(
    "data",
    "spatial_reference",
    "cuencas_ana.gpkg"
  )

  CUENCAS_ANA_MAP <- NULL

  if (file.exists(F_CUENCAS_ANA)) {

    x <- tryCatch(
      sf::st_read(
        F_CUENCAS_ANA,
        layer = "uh",
        quiet = TRUE,
        stringsAsFactors = FALSE
      ),
      error = function(e) {
        warning(
          "No se pudo cargar la capa cartográfica de cuencas ANA: ",
          conditionMessage(e)
        )
        NULL
      }
    )

    if (!is.null(x) && nrow(x)) {
      if (is.na(sf::st_crs(x))) {
        warning(
          "cuencas_ana.gpkg no tiene CRS definido; la capa de referencia no se mostrará."
        )
        x <- NULL
      }
    }

    if (!is.null(x) && nrow(x)) {

      x <- suppressWarnings(sf::st_make_valid(x))
      x <- x[!sf::st_is_empty(x), ]

      gtype <- as.character(sf::st_geometry_type(x))
      x <- x[grepl("POLYGON", gtype), ]

      if (nrow(x)) {

        x <- sf::st_transform(x, 4326)

        # La fuente es muy detallada. Como esta capa es SOLO cartográfica,
        # se simplifica a 250 m para no penalizar el navegador.
        # Esta geometría simplificada NO se usa en ningún cálculo.
        x_m <- sf::st_transform(x, 3857)
        x_m <- suppressWarnings(
          sf::st_simplify(
            x_m,
            dTolerance = 250,
            preserveTopology = TRUE
          )
        )
        x <- sf::st_transform(x_m, 4326)

        # Solo necesitamos los bordes; descartamos atributos.
        CUENCAS_ANA_MAP <- sf::st_sf(
          geometry = sf::st_geometry(x),
          crs = 4326
        )
      }
    }
  }

  assign(
    "F_CUENCAS_ANA",
    F_CUENCAS_ANA,
    envir = target_env
  )

  assign(
    "CUENCAS_ANA_MAP",
    CUENCAS_ANA_MAP,
    envir = target_env
  )

  invisible(TRUE)
}

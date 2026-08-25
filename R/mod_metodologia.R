# ============================================================================
# MÓDULO — METODOLOGÍA
# v1.1.0-dev: metodología actualizada para multigeometría.
# ============================================================================

mod_metodologia_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
      card(
        card_header("Selección espacial"),
        tags$ul(
          tags$li("El archivo espacial es opcional. Sin archivo espacial se explora todo el inventario nacional; si se cargan una o varias geometrías, cada una conserva su identidad y se reproyecta independientemente a una zona UTM calculada desde su propio centroide."),
          tags$li("El buffer y las distancias se calculan en metros."),
          tags$li("La distancia se calcula por separado entre cada estación y cada geometría original; una estación puede pertenecer al buffer de más de una geometría."),
          tags$li("Las estaciones dentro de una geometría poligonal tienen distancia 0 km para esa geometría. Los análisis temporales y OMM conservan el identificador del área correspondiente.")
        )
      ),
      card(
        card_header("Selección temporal"),
        tags$ul(
          tags$li("Todas las estaciones se comparan contra el mismo periodo calendario."),
          tags$li("La completitud considera la frecuencia esperada de cada serie."),
          tags$li("Para precipitación de 12 h, el día D se calcula como 19:00 de D + 07:00 de D+1; si falta una componente, el día queda incompleto."),
          tags$li("Si una estación tiene varias series, el diagnóstico individual permite inspeccionarlas por separado; en comparaciones masivas se usa la mejor del periodo sin fusionarlas."),
          tags$li("La aplicación permite descargar series normalizadas para el periodo elegido a partir de la copia integrada de ANA/SNIRH. El reporte original archivado se conserva sin modificaciones y ANA/SNIRH permanece como fuente de las observaciones."),
          tags$li(
            paste0(
              "La base integrada en la aplicación está congelada al ",
              format_data_freeze_date_es(),
              " y se actualizará con frecuencia mensual. ",
              "Para consultar registros incorporados posteriormente debe utilizarse ",
              "el visor oficial ANA/SNIRH."
            )
          )
        )
      )
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
      tags$a(
        href = "https://wmo.int/media/magazine-article/5-essential-elements-of-hydrological-monitoring-programme",
        target = "_blank",
        rel = "noopener noreferrer",
        "WMO — The 5 Essential Elements of a Hydrological Monitoring Programme"
      )
    )
  )
}

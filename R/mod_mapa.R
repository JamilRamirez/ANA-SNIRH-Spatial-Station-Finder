# ============================================================================
# MÓDULO — MAPA Y SELECCIÓN
# v1.1.0-dev: soporte de múltiples geometrías independientes.
# ============================================================================

mod_mapa_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(), uiOutput(ns("spatial_cards")), br(),
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
      leafletOutput(ns("map"), height = "650px")
    ),
    br(),
    card(
      full_screen = TRUE,
      card_header("Identificación de geometrías cargadas"),
      tags$small(
        "El ID mostrado aquí es el mismo que aparece sobre cada geometría del mapa y en las tablas de resultados.",
        style = "color:#6c757d;"
      ),
      br(),
      DTOutput(ns("geometry_table"))
    )
  )
}

mod_mapa_server <- function(id, spatial, spatial_stations, candidates,
                            tipo, buffer_km, umbral, cuencas_ana = NULL) {
  moduleServer(id, function(input, output, session) {

    output$spatial_cards <- renderUI({
      x <- spatial_stations()$tab
      s <- spatial()

      if (!isTRUE(s$has_kml)) {
        return(layout_columns(
          col_widths = breakpoints(
            sm = c(12, 12, 12),
            md = c(6, 6, 12),
            lg = c(4, 4, 4)
          ),
          metric_card(
            "Inventario nacional",
            fmt_num(nrow(x)),
            paste0(tipo(), " | ", fmt_num(sum(x$tiene_serie, na.rm = TRUE)), " con serie")
          ),
          metric_card(
            "Filtro espacial",
            "Desactivado",
            "Cargue un archivo espacial para usar buffer y distancias"
          ),
          metric_card(
            "Rango global",
            paste0(format(as.Date(FMIN), "%Y"), "–", format(as.Date(FMAX), "%Y"))
          )
        ))
      }

      n_unique <- if (nrow(x)) uniqueN(x$station_id) else 0L
      n_inside_pairs <- if (nrow(x)) sum(x$dentro_kml %in% TRUE, na.rm = TRUE) else 0L

      layout_columns(
        col_widths = breakpoints(
          sm = c(12, 12, 12, 12),
          md = c(6, 6, 6, 6),
          lg = c(3, 3, 3, 3)
        ),
        metric_card(
          "Geometrías",
          fmt_num(s$n_geometries),
          "Procesadas de forma independiente"
        ),
        metric_card(
          "Estaciones únicas",
          fmt_num(n_unique),
          paste0(tipo(), " dentro de algún buffer")
        ),
        metric_card(
          "Relaciones área–estación",
          fmt_num(nrow(x)),
          paste0(fmt_num(n_inside_pairs), " dentro del área")
        ),
        metric_card(
          "Área poligonal total",
          if (is.na(s$area_km2)) "—" else paste0(fmt_num(s$area_km2, 1), " km²")
        )
      )
    })

    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
        addTiles() %>%
        setView(lng = -75, lat = -9.5, zoom = 5)
    })

    output$geometry_table <- renderDT({
      s <- spatial()

      if (!isTRUE(s$has_kml) || is.null(s$geometry_info) || !nrow(s$geometry_info)) {
        return(
          datatable(
            data.frame(Mensaje = "Cargue un archivo espacial para identificar sus geometrías."),
            rownames = FALSE,
            options = list(dom = "t", paging = FALSE)
          )
        )
      }

      x <- copy(s$geometry_info)
      tab <- x[, .(
        `ID geometría` = geometry_id,
        Área = geometry_name,
        `Campo usado como nombre/ID` = fifelse(
          is.na(geometry_label_field) | !nzchar(geometry_label_field),
          "Sin campo detectado — nombre automático",
          geometry_label_field
        ),
        `Archivo fuente` = source_file,
        Capa = source_layer,
        `Feature origen` = source_feature,
        `Tipo geometría` = geometry_type,
        `Área (km²)` = round(area_km2, 2)
      )]

      datatable(
        tab,
        filter = "top",
        extensions = "Buttons",
        options = dt_opts(15),
        rownames = FALSE,
        class = "stripe hover compact"
      )
    }, server = FALSE)

    observe({
      s <- spatial()
      pts <- spatial_stations()$sf

      if (nrow(pts)) {
        pts <- pts[pts$tipo_dato == tipo(), ]
      }

      ev <- candidates()

      p <- leafletProxy("map") %>%
        clearGroup("Estaciones_filtradas") %>%
        clearGroup("Mejor_candidata") %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls()

      # Capa fija ANA: referencia cartográfica solamente.
      # Se dibuja antes de las áreas del usuario, buffers y estaciones.
      # No tiene popup, etiqueta ni interacción y NO interviene en cálculos.
      if (!is.null(cuencas_ana) && inherits(cuencas_ana, "sf") && nrow(cuencas_ana)) {
        p <- p %>% addPolygons(
          data = cuencas_ana,
          fill = FALSE,
          color = "#7f8c8d",
          weight = 0.8,
          opacity = 0.45,
          group = "Cuencas ANA",
          options = pathOptions(interactive = FALSE)
        )
      }

      if (isTRUE(s$has_kml)) {

        if (!is.null(s$buffer) && nrow(s$buffer)) {
          p <- p %>% addPolygons(
            data = s$buffer,
            fillOpacity = 0.05,
            color = "#6c757d",
            weight = 2,
            dashArray = "6,6",
            group = "Buffer",
            label = ~paste0(geometry_id, " · ", geometry_name, " — buffer ", buffer_km(), " km")
          )
        }

        gtypes <- as.character(st_geometry_type(s$area))

        if (any(grepl("POLYGON", gtypes))) {
          # Mantener una feature por geometría cargada para que el ID visual
          # coincida exactamente con el ID usado en Candidatas/Ventana común.
          pol <- s$area[grepl("POLYGON", gtypes), ]
          if (nrow(pol)) {
            pol$map_label <- paste0(pol$geometry_id, " · ", pol$geometry_name)

            # Con pocas geometrías, el identificador queda siempre visible.
            # Con cargas grandes, permanece como tooltip para evitar saturación.
            show_labels <- nrow(pol) <= 25L

            p <- p %>% addPolygons(
              data = pol,
              layerId = ~geometry_id,
              fillOpacity = 0.15,
              color = "#1f2d3d",
              weight = 3,
              group = "Área cargada",
              label = ~map_label,
              labelOptions = labelOptions(
                noHide = show_labels,
                direction = "center",
                textOnly = TRUE,
                opacity = 0.95,
                style = list(
                  "font-weight" = "700",
                  "font-size" = "11px",
                  "background" = "rgba(255,255,255,0.80)",
                  "border" = "1px solid #6c757d",
                  "padding" = "2px 4px"
                )
              ),
              popup = ~paste0(
                "<b>", map_label, "</b><br>",
                "<b>Archivo:</b> ", source_file, "<br>",
                "<b>Capa:</b> ", source_layer, "<br>",
                "<b>Feature origen:</b> ", source_feature, "<br>",
                "<b>Campo de identificación:</b> ",
                ifelse(
                  is.na(geometry_label_field) | geometry_label_field == "",
                  "no detectado (nombre automático)",
                  geometry_label_field
                )
              )
            )
          }
        }

        if (any(grepl("LINESTRING", gtypes))) {
          ln <- s$area[grepl("LINESTRING", gtypes), ]
          if (nrow(ln)) {
            ln$map_label <- paste0(ln$geometry_id, " · ", ln$geometry_name)
            p <- p %>% addPolylines(
              data = ln,
              layerId = ~geometry_id,
              color = "#1f2d3d",
              weight = 4,
              group = "Área cargada",
              label = ~map_label,
              popup = ~paste0(
                "<b>", map_label, "</b><br>",
                "<b>Archivo:</b> ", source_file, "<br>",
                "<b>Capa:</b> ", source_layer, "<br>",
                "<b>Feature origen:</b> ", source_feature
              )
            )
          }
        }
      }

      if (nrow(pts)) {
        # La evaluación temporal es idéntica para una misma estación aunque
        # pertenezca a más de un buffer. Tomamos una fila para sus métricas
        # temporales y agregamos aparte las geometrías donde es mejor candidata.
        idx <- match(pts$station_id, ev$station_id)

        pts$variable_sel <- ev$variable[idx]
        pts$comp <- ev$completitud_obs_pct[idx]
        pts$full_days <- ev$dias_completos_pct[idx]
        pts$nominal <- ev$cubre_nominalmente[idx]
        pts$apta <- ev$apta[idx]
        pts$serie_disp <- ev$tiene_serie[idx]
        pts$gap_max <- ev$max_racha_vacia_periodo_dias[idx]
        pts$anios_nominales_sel <- ev$anios_nominales[idx]

        best_by_station <- ev[
          mejor_candidata %in% TRUE,
          .(
            mejor_candidata = TRUE,
            areas_prioritarias = paste(
              unique(
                paste0(
                  geometry_id,
                  " · ",
                  geometry_name
                )
              ),
              collapse = " | "
            )
          ),
          by = station_id
        ]

        pts$mejor_candidata <- FALSE
        pts$areas_prioritarias <- NA_character_

        if (nrow(best_by_station)) {
          idx_best <- match(
            pts$station_id,
            best_by_station$station_id
          )

          hit_best <- !is.na(idx_best)

          pts$mejor_candidata[hit_best] <- TRUE
          pts$areas_prioritarias[hit_best] <- best_by_station$areas_prioritarias[
            idx_best[hit_best]
          ]
        }

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
            "<div style='min-width:250px'><b>", nombre_estacion, "</b><br>",
            "<b>Código:</b> ", ifelse(is.na(codigo_estacion), "—", codigo_estacion), "<br>",
            ifelse(
              isTRUE(s$has_kml),
              paste0("<b>Área(s):</b> ", geometry_names, "<br>"),
              ""
            ),
            "<b>Variable:</b> ", ifelse(is.na(variable_sel), "—", variable_sel), "<br>",
            "<b>Serie disponible:</b> ", ifelse(serie_disp, "Sí", "No"), "<br>",
            ifelse(
              is.na(distancia_km),
              "",
              paste0("<b>Distancia mínima al área:</b> ", round(distancia_km, 2), " km<br>")
            ),
            ifelse(
              mejor_candidata,
              paste0(
                "<b style='color:#b9770e'>★ Mejor candidata disponible para:</b> ",
                areas_prioritarias,
                "<br>"
              ),
              ""
            ),
            "<b>Completitud:</b> ", round(comp, 1), "%<br>",
            "<b>Mayor racha vacía:</b> ",
            ifelse(is.na(gap_max), "—", paste0(gap_max, " días")),
            "<br>",
            "<b>Longitud nominal:</b> ",
            ifelse(
              is.na(anios_nominales_sel),
              "—",
              paste0(round(anios_nominales_sel, 1), " años")
            ),
            "<br>",
            "<b>Días completos:</b> ", round(full_days, 1), "%<br>",
            "<b>Cubre periodo:</b> ", ifelse(nominal, "Sí", "No"), "<br>",
            "<b>Apta:</b> ", ifelse(apta, "Sí", "No"),
            ifelse(
              mejor_candidata & !apta,
              paste0(
                "<br><b>Advertencia:</b> es la mejor opción disponible, ",
                "pero no cumple los criterios configurados."
              ),
              ""
            ),
            "</div>"
          )
        )

        # Overlay sin clustering para que la mejor candidata de cada área siga
        # siendo visible incluso cuando el resto de estaciones se agrupa.
        best_pts <- pts[
          pts$mejor_candidata %in% TRUE,
        ]

        if (nrow(best_pts)) {
          p <- p %>% addCircleMarkers(
            data = best_pts,
            lng = ~longitud,
            lat = ~latitud,
            layerId = ~station_id,
            group = "Mejor_candidata",
            radius = 10,
            weight = 4,
            fillOpacity = 0.95,
            color = "#b9770e",
            fillColor = "#f4d03f",
            label = ~paste0(
              "★ ",
              nombre_estacion,
              " — mejor candidata"
            ),
            popup = ~paste0(
              "<div style='min-width:270px'>",
              "<b>★ Mejor candidata disponible</b><br>",
              "<b>Estación:</b> ", nombre_estacion, "<br>",
              "<b>Área(s):</b> ", areas_prioritarias, "<br>",
              "<b>Completitud:</b> ", round(comp, 1), "%<br>",
              "<b>Mayor racha vacía:</b> ",
              ifelse(is.na(gap_max), "—", paste0(gap_max, " días")),
              "<br>",
              "<b>Longitud nominal:</b> ",
              ifelse(
                is.na(anios_nominales_sel),
                "—",
                paste0(round(anios_nominales_sel, 1), " años")
              ),
              "<br>",
              "<b>Distancia mínima al área:</b> ",
              ifelse(is.na(distancia_km), "—", paste0(round(distancia_km, 2), " km")),
              "<br>",
              "<b>Cumple criterios configurados:</b> ",
              ifelse(apta, "Sí", "No"),
              ifelse(
                !apta,
                "<br><b>Nota:</b> se destaca porque es la mejor opción relativa disponible en el área.",
                ""
              ),
              "</div>"
            )
          )
        }
      }

      if (isTRUE(s$has_kml) && !is.null(s$buffer) && nrow(s$buffer)) {
        bb <- st_bbox(s$buffer)
        p <- p %>% fitBounds(
          bb[["xmin"]], bb[["ymin"]],
          bb[["xmax"]], bb[["ymax"]]
        )
      } else if (nrow(pts)) {
        bb <- st_bbox(pts)
        p <- p %>% fitBounds(
          bb[["xmin"]], bb[["ymin"]],
          bb[["xmax"]], bb[["ymax"]]
        )
      }

      has_best <- nrow(ev) &&
        "mejor_candidata" %in% names(ev) &&
        any(ev$mejor_candidata %in% TRUE)

      if (has_best) {
        p %>% addLegend(
          position = "bottomright",
          colors = c("#f4d03f", "#3498db", "#bdc3c7"),
          labels = c(
            "Mejor candidata disponible",
            paste0("Apta ≥ ", umbral(), "%"),
            "No apta"
          ),
          opacity = .9,
          title = paste0(tipo(), " — selección temporal")
        )
      } else {
        p %>% addLegend(
          position = "bottomright",
          colors = c("#3498db", "#bdc3c7"),
          labels = c(paste0("Apta ≥ ", umbral(), "%"), "No apta"),
          opacity = .9,
          title = paste0(tipo(), " — selección temporal")
        )
      }
    })

    station_click <- reactive(input$map_marker_click)

    list(station_click = station_click)
  })
}

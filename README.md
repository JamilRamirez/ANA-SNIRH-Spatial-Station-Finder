# ANA–SNIRH Spatial Station Finder

Interactive Shiny application for exploring the spatial and temporal availability of hydrometeorological stations published through the **Sistema Nacional de Información de Recursos Hídricos (SNIRH)** of the **Autoridad Nacional del Agua (ANA), Peru**.

The application is designed as a station-finding and data-availability audit tool. It does not redistribute the original observed precipitation or streamflow values. When a downloadable series is available, the application requests the corresponding official XLSX report from ANA/SNIRH and opens the file hosted by ANA.

## Main features

- National exploration of hydrometric and pluviometric stations.
- Optional upload of KML, GPKG, ZIP shapefiles, or complete shapefile components.
- Configurable spatial buffer around an uploaded study area.
- Filtering of precipitation and streamflow stations.
- Station-level temporal diagnostics.
- Nominal period and completeness estimates.
- Annual and monthly completeness diagnostics.
- Longest missing-data runs and continuity metrics.
- Fixed-period station screening.
- Recursive search for consecutive common windows of N years.
- Selection of the best available series per station without merging IDConfig records.
- Exploratory WMO/OMM network-density context.
- Direct request of official XLSX reports from ANA/SNIRH.

## Data architecture

The public application uses only compact derived products required for station discovery and temporal availability assessment:

```text
data/
├── 01_catalogo_series.csv
├── 01_disponibilidad_diaria.rds
├── 01_inventario_estaciones_validado.csv
└── 01_estaciones_sin_serie.csv
```

The original observed precipitation and streamflow values are **not included in this repository**.

The application uses ANA/SNIRH `IDConfig` identifiers to request official reports directly from the ANA service when the user chooses to download a series.

## Station inventory validation

The station inventory was reconstructed from the detailed station tables returned by ANA/SNIRH for individual hydrographic units.

For each hydrographic unit and variable type, the extraction was checked against the number of stations declared by ANA in the corresponding detailed table. Hydrographic units without a station table were treated as units without stations for that variable.

Stations listed by ANA but without an exposed `IDConfig` remain in the inventory and are identified in the application as stations without a currently downloadable series.

## Completeness

Completeness is evaluated from the observation frequency expected for each series.

Examples:

- `1 DIA` / `24 HORAS`: 1 expected observation per day.
- `12 HORAS`: 2 expected observations per day.
- `8 HORAS`: 3 expected observations per day.

A distinction is maintained between:

- observation completeness;
- days with at least one observation;
- fully complete days;
- nominal coverage of a requested period.

A station may therefore have high observation completeness but fail a strict nominal-coverage requirement if its available series starts after or ends before the requested analysis period.

## Spatial files

The spatial file is optional.

Supported inputs include:

- KML;
- GPKG;
- zipped shapefile;
- `.shp + .shx + .dbf + .prj` uploaded together.

When an area is provided, distances and buffers are calculated in a projected metric CRS selected from the study-area location.

The default 50 km buffer is an exploratory search radius and **is not presented as a WMO/OMM requirement**.

## WMO/OMM module

The WMO/OMM module is intentionally separated from temporal completeness screening.

It provides an exploratory comparison between:

- study-area size;
- number of physical stations within the area;
- observed network density in km²/station;
- selected reference density.

It should not be interpreted as proof of hydrological, climatic, or topographic representativeness, nor as a substitute for fitness-for-purpose assessment.

## Official series downloads

The application does not serve original hydrometeorological observations.

For a selected `IDConfig`, it sends a request to the ANA/SNIRH reporting service. ANA generates the XLSX and the user's browser opens the report hosted under the ANA/SNIRH repository.

Therefore:

- original values remain distributed by ANA;
- the application acts as a discovery and audit interface;
- ANA remains the source of the downloaded report.

## Run locally

Install the required R packages:

```r
install.packages(c(
  "shiny",
  "bslib",
  "data.table",
  "DT",
  "sf",
  "leaflet",
  "lubridate",
  "ggplot2",
  "httr",
  "jsonlite"
))
```

Run from the project directory:

```r
shiny::runApp(".")
```

## Project structure

```text
ANA_SNIRH_WEB/
├── app.R
├── manifest.json
├── README.md
├── LICENSE
├── CITATION.cff
├── .gitignore
└── data/
    ├── 01_catalogo_series.csv
    ├── 01_disponibilidad_diaria.rds
    ├── 01_inventario_estaciones_validado.csv
    └── 01_estaciones_sin_serie.csv
```

## Data source and attribution

Station information and hydrometeorological-series metadata originate from the Autoridad Nacional del Agua (ANA), Sistema Nacional de Información de Recursos Hídricos (SNIRH), Peru.

SNIRH basin viewer:

`https://snirh.ana.gob.pe/VisorPorCuenca/`

This application performs independent processing, indexing, quality-control summaries, spatial screening, and temporal-availability diagnostics.

## License

The **software code and original project documentation** in this repository are released under the MIT License.

This license does **not** assert ownership of, nor relicense, third-party information originating from ANA/SNIRH. Original ANA/SNIRH data and reports remain subject to the terms and conditions of their source.

## Disclaimer

This is an independent research/software project and is not an official application of the Autoridad Nacional del Agua.

The application is intended to facilitate data discovery and exploratory assessment. Users remain responsible for verifying the suitability, quality, provenance, and current status of the information used in any analysis.

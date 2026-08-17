# ANA–SNIRH Spatial Station Finder

Interactive Shiny application for exploring the spatial and temporal availability of hydrometeorological stations published through the **Sistema Nacional de Información de Recursos Hídricos (SNIRH)** of the **Autoridad Nacional del Agua (ANA), Peru**.

The application is designed as a station-finding and data-availability audit tool. For series with an archived report, users can download an unmodified copy of the original ANA/SNIRH XLSX report directly from the companion public archive. The official ANA/SNIRH basin viewer remains linked for source consultation and verification.

## Live application

https://01a00cf4-6b8b-26f2-7fe6-e455d88a9978.share.connect.posit.cloud

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
- Direct download of archived, unmodified original ANA/SNIRH XLSX reports when available.
- Guided access to the official ANA/SNIRH basin viewer using the identified basin, **Hidrometría/Pluviometría** layer, and station name/code.

## Data architecture

The public application uses only compact derived products required for station discovery and temporal availability assessment:

```text
data/
├── 01_catalogo_series.csv
├── 01_disponibilidad_diaria.rds
├── 01_inventario_estaciones_validado.csv
├── 01_estaciones_sin_serie.csv
└── 02_indice_raw_xlsx.csv
```

The application repository itself remains lightweight. Original XLSX reports are stored separately in the companion archive:

`https://github.com/JamilRamirez/ANA-SNIRH-Official-Reports`

`02_indice_raw_xlsx.csv` maps each validated `IDConfig` to the preserved relative path of its archived report. The application uses this index only to construct the direct download URL; the report contents are not transformed by the Shiny application.

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

## Accessing original ANA/SNIRH reports

For validated series with an archived RAW report, the application provides a direct button:

**Download original ANA/SNIRH report (.xlsx)**

These files are preserved copies of the original XLSX reports obtained from ANA/SNIRH and are not converted to CSV, Parquet, or another normalized public-download format.

Companion RAW-report archive:

`https://github.com/JamilRamirez/ANA-SNIRH-Official-Reports`

The official ANA/SNIRH viewer remains available for source consultation and verification. To locate a station in the official interface:

1. open the ANA/SNIRH basin viewer;
2. search for the corresponding hydrographic basin;
3. activate the **Hidrometría** or **Pluviometría** layer;
4. locate the station by its name and, when available, its station code;
5. open the ANA station record.

Official viewer:

`https://snirh.ana.gob.pe/VisorPorCuenca/`

The archived XLSX copies are provided for convenient access and preserve their original ANA/SNIRH format. ANA remains the original source of the observations and report metadata.


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
  "ggplot2"
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
    ├── 01_estaciones_sin_serie.csv
    └── 02_indice_raw_xlsx.csv
```

## Data source and attribution

Station information and hydrometeorological-series metadata originate from the Autoridad Nacional del Agua (ANA), Sistema Nacional de Información de Recursos Hídricos (SNIRH), Peru.

SNIRH basin viewer:

`https://snirh.ana.gob.pe/VisorPorCuenca/`

This application performs independent processing, indexing, quality-control summaries, spatial screening, and temporal-availability diagnostics. A companion repository archives unmodified copies of the official XLSX reports used for direct user downloads.

## License

The **software code and original project documentation** in this repository are released under the MIT License.

This license does **not** assert ownership of, nor relicense, third-party information originating from ANA/SNIRH. The archived XLSX reports are preserved as source files with attribution to ANA/SNIRH and are outside the MIT license applied to this project's software code and original documentation.

## Disclaimer

This is an independent research/software project and is not an official application of the Autoridad Nacional del Agua.

The application is intended to facilitate station discovery, temporal-availability screening, and exploratory assessment. Users remain responsible for verifying the suitability, quality, provenance, current status, and official ANA/SNIRH record of any information used in an analysis.

# ANA–SNIRH Spatial Station Finder

**Version 1.1.0**

Interactive Shiny application for exploring, screening, comparing, and exporting the spatial and temporal availability of hydrometeorological stations published through the **Sistema Nacional de Información de Recursos Hídricos (SNIRH)** of the **Autoridad Nacional del Agua (ANA), Peru**.

The application is designed as a station-finding, temporal-availability audit, and reproducible data-selection tool. It supports national exploration or user-defined study areas, preserves the distinction between physical stations and individual `IDConfig` series, links back to official ANA/SNIRH sources, and can assemble selected normalized daily series into a wide-format Excel dataset.

## Live application

https://01a00cf4-6b8b-26f2-7fe6-e455d88a9978.share.connect.posit.cloud

## Version 1.1.0 highlights

Version 1.1.0 extends the original station finder from a discovery and screening interface into a more complete station-selection and export workflow.

### 1. Prioritized candidate station

For each uploaded geometry, the application identifies and highlights the best available candidate among stations with a usable series.

The prioritization considers:

1. completeness over the requested period;
2. maximum empty-data run over the requested period;
3. whether the series meets the current screening threshold, as a tie-break criterion;
4. nominal record length;
5. distance to the study geometry.

Stations are not excluded from the prioritization simply because they fall below the current completeness threshold. This is important in data-scarce areas where the best available record may still be incomplete.

### 2. Graphical temporal-availability calendar

The **Candidate stations** tab includes an on-demand annual availability calendar.

For the requested analysis period, each station-year is classified according to annual completeness:

- meets the current completeness threshold;
- contains observations but falls below the threshold;
- contains no observations;
- lies outside the nominal record.

Eligible and not-eligible stations are displayed in separate panels so that a large number of one group does not hide the other. Each panel preserves its own 10–50 station visualization limit and legend.

### 3. Optimized common-window solutions

The **Common window** module evaluates windows of at least the requested minimum length and returns three distinct compromise solutions:

- **Maximum coverage** — prioritizes the number of stations retained;
- **Balanced** — seeks a compromise between retained stations and window length;
- **Maximum length** — prioritizes the longest feasible common period.

Users can also define a **minimum start year** for the search (default: **1914**). Only windows beginning in that year or later are evaluated.

A start-year guidance curve shows how many stations can satisfy the current completeness and nominal-coverage criteria for each feasible initial year. This helps users decide whether shifting the start year produces a worthwhile gain in station coverage.

Each selected compromise solution reports its period, duration, number of retained stations, coverage, and median completeness. Selecting a solution updates an annual station-availability calendar restricted to that exact window.

Original archived ANA/SNIRH XLSX reports can also be downloaded in bulk for the active solution.

### 4. Station temporal-quality diagnostics

Station diagnostics report the longest consecutive blocks of calendar years with:

- **100% completeness**;
- **at least 90% completeness**;
- the start and end years of each block.

The diagnostic view also summarizes decision-oriented temporal quality, including:

- number of calendar years reaching ≥90%, ≥95%, and 100% completeness;
- intraday integrity among days with at least one observation;
- counts of interruptions lasting ≥30, ≥90, and ≥365 days;
- the proportion of all missing days concentrated in interruptions ≥90 days;
- annual completeness stability;
- the climatological month with the weakest median completeness.

Only complete calendar years are used for complete-year block metrics.

### 5. KML and KMZ support

Spatial uploads now support:

- KML;
- KMZ;
- GPKG;
- zipped shapefiles;
- complete shapefile components uploaded together.

KMZ files are extracted internally and processed through the same spatial workflow as KML.

### 6. Independent processing of multiple geometries

Files containing multiple geometries are no longer dissolved into one study area.

Each feature is processed independently and receives:

- a stable geometry ID (`G001`, `G002`, ...);
- a geometry name when an appropriate source attribute is available;
- its own metric CRS, area, buffer, station distances, candidate ranking, common-window solutions, and WMO/OMM diagnostics.

A station may legitimately appear in more than one geometry if it falls within multiple search buffers.

### 7. Normalized wide-format data export

Selections from both **Candidate stations** and **Common window** can be transferred to a dedicated **Normalized download** tab.

Before generating the dataset, users can:

- review the selected stations;
- change the export period without losing the station selection;
- include or exclude individual stations;
- inspect period completeness and nominal coverage;
- identify incompatible subdaily series.

The normalized exporter produces a **daily wide-format Excel workbook** from daily series and compatible 12-hour precipitation series.

The workbook contains:

- **Data** — one common daily date axis and one column per selected station;
- **Metadata** — station identity, `IDConfig`, variable, unit, elevation, coordinates, temporal coverage, completeness, geometry provenance, and source links;
- **Incidences** — only when source-record structure requires explicit reporting.

Missing dates are preserved explicitly on the common calendar and are not imputed.

For a nominally daily series containing multiple source records on the same date, the exporter does not sum or average values. It uses the last non-missing observation according to the preserved date/time order and records the affected date and source values in the **Incidences** sheet.

For compatible **12-hour precipitation**, day `D` combines the observation at 19:00 on `D` with the observation at 07:00 on `D+1`. Days without that complete valid pair remain missing and are documented in **Incidences**. Other incompatible subdaily frequencies are identified explicitly and excluded rather than silently transformed.

### 8. Direct station download and data snapshot

From **Station diagnostics**, a single selected series can be downloaded directly as a normalized XLSX file without first moving to the multi-station export workflow.

The export period defaults to the full nominal record and can be shortened by the user within the available series limits.

The application explicitly reports that its current ANA/SNIRH data snapshot is frozen at **15 August 2026** and is intended to be refreshed monthly. For records incorporated after that date, users are directed to the official ANA/SNIRH viewer to consult the most recent information available there.

## Additional v1.1.0 improvements

- ANA hydrographic-basin boundaries are shown as a lightweight reference layer on the map.
- Candidate and common-window tables were simplified to emphasize decision-relevant information.
- The station diagnostic tab includes an on-demand graph of the archived observed series when the original ANA/SNIRH report is available.
- WMO/OMM diagnostics are grouped with the methodology section in the application navigation.
- Spatial calculations reuse projected geometries where possible instead of repeating transformations.
- Expensive temporal calculations are separated from threshold-only changes where possible.
- A startup cache stores already prepared application objects and a reduced daily-availability table for faster initialization.
- Source-file cache validation uses MD5 signatures.
- Annual boolean diagnostic fields are displayed as **Yes/No** rather than raw logical values.
- Compact equal-width control cards reduce unused space in the Normalized download workflow.

## Main features

- National exploration of hydrometric and pluviometric stations.
- Optional spatial screening from user-uploaded study areas.
- KML, KMZ, GPKG, and shapefile support.
- Independent analysis of multiple geometries in the same file.
- Configurable metric buffer around each study geometry.
- ANA basin reference layer.
- Filtering of precipitation and streamflow stations.
- Station-level temporal diagnostics.
- Annual and monthly completeness diagnostics.
- Missing-data run and continuity diagnostics.
- Consecutive 100%-complete and ≥90%-complete year blocks.
- Best-candidate ranking and map highlighting.
- On-demand annual availability calendars.
- Three optimized common-window compromise solutions.
- Selection of the best available series per physical station without merging `IDConfig` records.
- Exploratory WMO/OMM network-density context.
- Direct access to archived, unmodified ANA/SNIRH XLSX reports when available.
- Bulk download of original reports for a selected common-window solution.
- Wide-format normalized daily export with metadata and incidence tracking.
- Guided access to the official ANA/SNIRH basin viewer.
- Direct single-station normalized XLSX export with selectable period.
- Minimum-start-year control and start-year guidance curve for common-window selection.
- Explicit data-snapshot date and monthly-refresh notice.

## Data architecture

The application uses compact derived products for station discovery and temporal availability assessment:

```text
data/
├── startup_cache_v3/
│   ├── manifest.rds
│   ├── objects.rds
│   └── day_*.rds
├── 01_catalogo_series.csv
├── 01_disponibilidad_diaria.rds
├── 01_inventario_estaciones_validado.csv
├── 01_estaciones_sin_serie.csv
├── 02_indice_raw_xlsx.csv
└── spatial_reference/
    └── cuencas_ana.gpkg
```

`startup_cache_v3/` contains the prepared application objects used to reduce startup time. The large daily-availability object is split into uncompressed fragments smaller than 50 MiB so the cache can remain versioned in GitHub. The cache is validated against the source products by file signatures and can be rebuilt when the underlying data change.

### Original ANA/SNIRH reports

Original XLSX reports are stored separately in the companion archive:

https://github.com/JamilRamirez/ANA-SNIRH-Official-Reports

`02_indice_raw_xlsx.csv` maps validated `IDConfig` values to the preserved relative path of their archived report.

These files remain preserved copies of the ANA/SNIRH reports. The application does not modify their contents.

### Normalized series repository

Normalized long-format series used by the v1.1.0 wide-format exporter are stored separately by `IDConfig`:

https://github.com/JamilRamirez/ANA-SNIRH-Normalized-Series

The application does **not** load this repository at startup. Only the RDS files corresponding to the stations explicitly selected for export are retrieved when the user prepares a normalized Excel dataset.

The normalized series preserve the source observation structure and do not imply infilling of missing observations. For 12-hour precipitation, daily assembly follows the ANA pluviometric day: `P_D = P(D, 19:00) + P(D + 1, 07:00)`. The same rule is used by normalized downloads and Station diagnostics; a missing or ambiguous component leaves the day incomplete. Other subdaily frequencies are excluded from daily export.

## Station inventory validation

The station inventory was reconstructed from the detailed station tables returned by ANA/SNIRH for individual hydrographic units.

For each hydrographic unit and variable type, the extraction was checked against the number of stations declared by ANA in the corresponding detailed table. Hydrographic units without a station table were treated as units without stations for that variable.

Stations listed by ANA but without an exposed `IDConfig` remain in the spatial inventory and are identified as stations without a currently downloadable series.

## Series identity

A physical station may expose more than one ANA/SNIRH series or `IDConfig`.

The application therefore keeps the distinction between:

- physical station identity;
- individual series identity;
- `IDConfig`.

When a single representative series is required for a station, the application ranks the available series rather than merging records belonging to different `IDConfig` values.

## Completeness

Completeness is evaluated from the expected observation frequency of each series.

Examples:

- `1 DIA` / `24 HORAS`: 1 expected observation per day;
- `12 HORAS`: 2 expected observations per day;
- `8 HORAS`: 3 expected observations per day.

The application distinguishes between:

- observation completeness;
- days with at least one observation;
- fully complete days;
- nominal coverage of a requested period;
- continuity and missing-data runs.

A station may therefore have high observation completeness but fail a strict nominal-coverage requirement if its available series starts after or ends before the requested period.

## Spatial processing

The spatial file is optional.

When no area is supplied, the application operates in national exploration mode.

When one or more geometries are supplied:

- each geometry is processed independently;
- geometries are transformed to an appropriate projected metric CRS for distances, buffers, and area;
- station-to-area relationships are preserved independently;
- a configurable search buffer is applied around each geometry.

The default 50 km buffer is an exploratory search radius and **is not presented as a WMO/OMM requirement**.

## WMO/OMM context

The WMO/OMM component is an exploratory network-density diagnostic and remains conceptually separate from temporal completeness screening.

It compares:

- study-area size;
- number of physical stations;
- observed density in km²/station;
- a selected reference density.

It should not be interpreted as proof of hydrological, climatic, or topographic representativeness, nor as a substitute for a fitness-for-purpose assessment.

## Accessing original ANA/SNIRH reports

For validated series with an archived RAW report, the application provides direct access to the preserved XLSX copy.

The application dataset is currently frozen at **15 August 2026** and is intended to be refreshed monthly. The official ANA/SNIRH viewer should be used to consult records incorporated after that snapshot or to check the most recent information available from ANA.

The official ANA/SNIRH viewer remains available for source consultation and verification:

https://snirh.ana.gob.pe/VisorPorCuenca/

To locate a record in the official interface:

1. open the basin viewer;
2. identify the corresponding hydrographic basin;
3. activate the **Hidrometría** or **Pluviometría** layer;
4. locate the station by name and, when available, station code;
5. open the ANA station record.

ANA remains the original source of the observations and report metadata.

## Run locally

Required R packages:

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
  "readxl",
  "zip",
  "openxlsx"
))
```

Run the application from the project directory:

```r
shiny::runApp(".")
```

## Project structure

```text
ANA_SNIRH_WEB/
├── app.R
├── manifest.json
├── README.md
├── CHANGELOG.md
├── CITATION.cff
├── LICENSE
├── .gitignore
├── .rscignore
├── R/
│   ├── core_data.R
│   ├── core_helpers.R
│   ├── core_spatial_reference.R
│   ├── mod_mapa.R
│   ├── mod_diagnostico.R
│   ├── mod_candidatas.R
│   ├── mod_ventana_comun.R
│   ├── mod_descarga_normalizada.R
│   ├── mod_omm.R
│   └── mod_metodologia.R
└── data/
    ├── startup_cache_v3/
    │   ├── manifest.rds
    │   ├── objects.rds
    │   └── day_*.rds
    ├── 01_catalogo_series.csv
    ├── 01_disponibilidad_diaria.rds
    ├── 01_inventario_estaciones_validado.csv
    ├── 01_estaciones_sin_serie.csv
    ├── 02_indice_raw_xlsx.csv
    └── spatial_reference/
        └── cuencas_ana.gpkg
```

## Data source and attribution

Station information, series metadata, and original hydrometeorological observations originate from the **Autoridad Nacional del Agua (ANA)** and its **Sistema Nacional de Información de Recursos Hídricos (SNIRH), Peru**.

This application performs independent processing, indexing, quality-control summaries, spatial screening, temporal-availability diagnostics, station ranking, common-window optimization, and user-directed dataset assembly.

The companion repositories provide convenient access to preserved reports and normalized processing products, but do not change the attribution of the underlying ANA/SNIRH observations.

## License

The **software code and original project documentation** in this repository are released under the MIT License.

This license does **not** assert ownership of, nor relicense, third-party information originating from ANA/SNIRH. Original and derived data products remain subject to the terms, provenance, and attribution requirements of their respective source material.

## Disclaimer

This is an independent research/software project and is not an official application of the Autoridad Nacional del Agua.

The application is intended to facilitate station discovery, temporal-availability screening, spatial comparison, and reproducible dataset assembly. Users remain responsible for verifying the suitability, quality, provenance, current status, and official ANA/SNIRH record of information used in analyses.

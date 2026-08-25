# Changelog

All notable changes to **ANA–SNIRH Spatial Station Finder** are documented here.

The project follows semantic versioning for public releases.

## [1.1.0] — 2026-08-25

### Added

- Best-candidate ranking for each uploaded geometry, with dedicated map highlighting.
- On-demand annual temporal-availability calendar in **Candidate stations**.
- Three optimized common-window compromise solutions:
  - Maximum coverage;
  - Balanced;
  - Maximum length.
- Window-specific annual availability calendar for the selected common-window solution.
- Bulk download of archived original ANA/SNIRH XLSX reports from a common-window solution.
- Longest consecutive block of 100%-complete calendar years.
- Longest consecutive block of calendar years with at least 90% completeness.
- Direct KMZ support in addition to KML, GPKG, and shapefiles.
- Independent processing of multiple geometries contained in one spatial file.
- Stable geometry identifiers (`G001`, `G002`, ...), names, per-geometry buffers, distances, areas, rankings, common windows, and WMO/OMM summaries.
- ANA hydrographic-basin boundaries as a lightweight map reference layer.
- Dedicated **Normalized download** workflow accessible from Candidate stations and Common window.
- Wide-format daily Excel export with:
  - common date axis;
  - one column per selected station;
  - Metadata sheet;
  - Incidences sheet when required.
- Connection to the companion normalized-series repository:
  - `JamilRamirez/ANA-SNIRH-Normalized-Series`.
- Startup cache v3 with reduced daily-availability table and MD5 source validation.
- Station-diagnostic **Temporal quality for analysis** summary with:
  - counts of calendar years reaching ≥90%, ≥95%, and 100% completeness;
  - intraday integrity among days with observations;
  - counts of long interruptions (≥30, ≥90, and ≥365 days);
  - concentration of missing days in interruptions ≥90 days;
  - annual completeness stability and weakest climatological month.
- Direct single-station normalized XLSX download from **Station diagnostics**, with a user-selectable period defaulting to the full nominal record.
- Explicit application data-snapshot notice: the current database is frozen at **2026-08-15**, with planned monthly refreshes and guidance to the official ANA/SNIRH viewer for newer records.
- User-defined **minimum start year** for optimized common-window searches (default: 1914).
- Common-window guidance curve showing the number of stations that satisfy the current criteria as a function of the candidate window start year.
- Candidate-station annual availability split into separate **Eligible** and **Not eligible** panels, each preserving its own visualization limit and legend.

### Changed

- Candidate-station prioritization no longer excludes stations merely because they fall below the current completeness threshold.
- Candidate tables were simplified to retain decision-relevant fields.
- Common-window tables were simplified and the former generic line plot was replaced by a window-specific annual availability calendar.
- Common-window search now treats the requested number of years as a minimum duration and compares longer feasible alternatives.
- WMO/OMM content is grouped with methodology in the application navigation.
- Spatial processing preserves feature identity instead of dissolving multi-feature files.
- Repeated spatial transformations are reduced by reusing projected objects where possible.
- Temporal screening calculations are separated from threshold-only ranking changes where possible.
- Normalized series are fetched only on demand when preparing a user export.
- Startup initialization was substantially reduced by caching prepared objects.
- Station-diagnostic boolean fields are displayed as **Yes/No** instead of raw logical values.
- Normalized-download controls were compacted into three equal-width columns to reduce unused vertical and horizontal space.
- Common-window solution and station-detail cards were compacted to avoid unnecessary empty vertical space.

### Data-export behavior

- Normalized export supports:
  - nominally daily series;
  - compatible **12-hour precipitation**, converted with the ANA pluviometric day: `P_D = P(D, 19:00) + P(D + 1, 07:00)`.
- For 12-hour precipitation, a day without the complete valid pair is exported as missing and documented in the `Incidences` sheet.
- Other incompatible subdaily frequencies remain explicitly identified and are excluded rather than silently transformed.
- Missing dates remain explicit and are not imputed.
- If a nominally daily series contains more than one record on a date, the exporter:
  - does not sum or average the records;
  - uses the last non-missing value according to preserved date/time order;
  - records the affected date, source values, and exported value in the `Incidences` sheet.
- The same normalization rules are used by the dedicated multi-station export workflow and the direct single-station download from Station diagnostics.

### Fixed

- The weakest climatological month now uses mean monthly completeness and reports ties instead of defaulting to January when several medians were 100%.
- Corrected 12-hour precipitation normalization and Station diagnostics so 07:00 is assigned to the preceding pluviometric day instead of being summed with 19:00 on the same calendar date.
- Normalized-download navigation from Candidate stations and Common window.
- Download workflow no longer keeps the browser request open while remote RDS files are assembled.
- Excel files are prepared and validated before the download button becomes available.
- Session-end cleanup of prepared temporary Excel files no longer accesses a reactive value outside an active reactive context.
- Daily series containing occasional multiple records on the same date no longer abort the entire normalized export.
- Common-window compromise card no longer expands vertically with unnecessary empty space.
- Candidate availability calendars now preserve a legend in both the eligible and not-eligible panels.
- Common-window plotting restored a full start-year guidance curve instead of showing only the three selected compromise points.
- Normalized-download header cards now use equal one-third widths without forced shared height.

### Performance

Local development benchmark for `init_core_data()`:

- original initialization: approximately **32.98 s**;
- startup cache v2: approximately **14.42 s**;
- startup cache v3: approximately **2.56 s**.

These values are development-machine benchmarks and are not runtime guarantees for hosted deployments.

## [1.0.0] — 2026-08-17

Initial public release.

### Included

- National exploration of ANA/SNIRH hydrometric and pluviometric stations.
- Optional spatial screening using KML, GPKG, and shapefiles.
- Configurable metric search buffer.
- Station-level temporal diagnostics.
- Completeness and nominal-coverage screening.
- Annual and monthly completeness summaries.
- Missing-data run diagnostics.
- Fixed-period candidate screening.
- Recursive search for consecutive common windows of a fixed number of years.
- Best available series selection per station without merging `IDConfig` records.
- Exploratory WMO/OMM density context.
- Access to archived unmodified ANA/SNIRH XLSX reports.
- Linkage to the official ANA/SNIRH basin viewer.

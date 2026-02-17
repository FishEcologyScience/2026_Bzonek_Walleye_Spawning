# Walleye Spawning Behavior Analysis

## Overview
Analysis of walleye spawning behavior and site selection in Hamilton Harbour using acoustic telemetry, electrofishing surveys, and habitat data (2015-2023).

## Data Sources
- Acoustic telemetry detections (2015-2023)
- Electrofishing surveys (2022-2024) 
- Nearshore habitat surveys (2021)
- Environmental variables (water levels, weather)

## Analysis Pipeline

All scripts are sourced via `Script0-0_UserInterface.R`. Higher-numbered scripts depend on lower-numbered ones.

### Data Processing
- `Script1-1_format_data_h.R` - Clean raw habitat and electrofishing data
- `Script1-1.2_format_data_efish_raw.R` - Combine walleye spawning survey data 2022-2024
- `Script1-2_format_data_t.R` - Import and format raw telemetry data
- `Script1-3_process_data_t.R` - Filter, calculate movements, residency, and spawning events

### Analysis Scripts
- `Script2-1_analysis_h_habfish_RF.R` - Habitat-fish relationships (Random Forest)
- `Script2-2_process_t_dataset.R` - Spawning event identification
- `Script2-4_analysis_c_repeatability.R` - Behavioral repeatability
- `Script2-5_analysis_t_station_consistency.R` - Spatial metrics for spawning behaviour
- `Script2-6_analysis_t_spawning_summary.R` - Spawning proportion per fish per year
- `Script2-7_analysis_t_spawning_proportion_by_hour.R` - Spawning proportion by hour (diel patterns)

### Visualization
- `Script3-1_plot_methods.R` - Spatial methods figures for manuscript
- `Script3-2_plot_publication.R` - Publication-ready results figures

## Usage
```r
source("02 - Scripts/Script0-0_UserInterface.R")
```

## Requirements
- R packages: tidyverse, randomForest, lme4, adehabitatHR, rptR, sf, corrplot, vegan, patchwork, ggridges, ggpmisc, paletteer
- telemetrytoolsFESL package (`devtools::install_github("pbzonek/telemetrytoolsFESL")`)
- Data files in `01 - Data/`
- Google Maps basemap: cached as `01 - Data/HHmap_basemap.rds` (included). If regenerating, requires a Google API key stored via `keyring::key_set("google_api", username = "Jake")`

## Author
Paul Bzonek
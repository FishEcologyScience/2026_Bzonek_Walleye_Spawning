# Walleye Spawning Behavior Analysis

## Overview
Analysis of walleye spawning behavior and site selection in Hamilton Harbour using acoustic telemetry, electrofishing surveys, and habitat data (2015-2023).

## Data Sources
- Acoustic telemetry detections (2015-2023)
- Electrofishing surveys (2022-2024) 
- Nearshore habitat surveys (2021)
- Environmental variables (water levels, weather)

## Analysis Pipeline

### Data Processing
- `Script0-0_UserInterface.R` - Main workflow
- `Script1-1_format_data_h.R` - Habitat data formatting
- `Script1-2_format_data_t.R` - Telemetry data formatting

### Analysis Scripts
- `Script2-1_analysis_h_habfish_RF.R` - Habitat-fish relationships (Random Forest)
- `Script2-2_process_t_dataset.R` - Spawning event identification  
- `Script2-3_analysis_t_homerange.R` - Home range analysis
- `Script2-4_analysis_c_repeatability.R` - Behavioral repeatability

### Visualization
- `Script3-1_methods_plot.R` - Spatial plots

## Usage
```r
source("02 - Scripts/Script0-0_UserInterface.R")
```

## Requirements
- R packages: tidyverse, randomForest, lme4, adehabitatHR, rptR, sf
- telemetrytoolsFESL package
- Data files in `01 - Data/`

## Author
Paul Bzonek
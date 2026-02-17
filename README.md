# 2026_Bzonek_Walleye_Spawning

## Overview
Code and data processing pipeline supporting the manuscript:

> **Habitat selection and behavioural repeatability during spawning in stocked Walleye (*Sander vitreus*) in Hamilton Harbour**
>
> Bzonek PA, Croft-White MV, Reddick DT, Turner NA, MacLeod MC, Cooke SJ, Brooks JL, Hasler C, Midwood JD

This study paired shoreline habitat surveys with spring electrofishing surveys (2022-2024) and multi-year acoustic telemetry (2016-2023) to evaluate habitat associations and the repeatability of stocked Walleye spawning behaviour in Hamilton Harbour, a degraded embayment of Lake Ontario. Random forest models identified cobble-gravel substrates, intermediate shoreline slopes, and high substrate diversity as key habitat predictors. Telemetry-derived spawning behaviours revealed significant individual consistency in spatial metrics (station count, station ratio, specialization index; R = 0.34-0.44), while total spawning duration varied widely among individuals and years.

## Data Sources
- Acoustic telemetry detections (2016-2023; n=45 tagged Walleye, 26 long-term receiver stations)
- Electrofishing spawning surveys (2022-2024; n=440 Walleye encountered)
- Nearshore habitat surveys (2021; substrate composition, slope, light index)

## Analysis Pipeline

All scripts are sourced via `Script0-0_UserInterface.R`. Higher-numbered scripts depend on lower-numbered ones.

### Data Processing
- `Script1-1_format_data_h.R` - Clean raw habitat and electrofishing data; remove correlated substrates; apply 25 m habitat buffer
- `Script1-1-1_format_data_efish.R` - Process raw walleye spawning survey data 2022-2024
- `Script1-2_format_data_t.R` - Import and format raw telemetry data
- `Script1-3_process_data_t.R` - Filter detections to April, dusk/night periods; identify spawning events via shallow-water (<2 m) residency criteria

### Analysis Scripts
- `Script2-1_analysis_h_habfish_RF.R` - Random forest models for Walleye occurrence and abundance against habitat predictors; partial dependence plots
- `Script2-2_process_t_dataset.R` - Spawning event identification and dataset construction
- `Script2-4_analysis_c_repeatability.R` - Intra-class correlation coefficients (rptR) for spawning behaviour repeatability
- `Script2-5_analysis_t_station_consistency.R` - Station specialization indices (Shannon diversity), spawning-site fidelity metrics
- `Script2-6_analysis_t_spawning_summary.R` - Spawning proportion per fish per year
- `Script2-7_analysis_t_spawning_proportion_by_hour.R` - Diel spawning patterns (spawning proportion by hour)

### Visualization
- `Script3-1_plot_methods.R` - Spatial methods figures (receiver map, habitat survey sites)
- `Script3-2_plot_publication.R` - Publication-ready results figures (variable importance, partial dependence, repeatability boxplots)

## Usage
```r
source("02 - Scripts/Script0-0_UserInterface.R")
```

## Requirements
- R (tested on v4.3.0)
- Core packages (loaded via `library()`): tidyverse, sf, randomForest, patchwork
- Additional packages (called via `::`): adehabitatHR, ggmap, corrplot, vegan, ggridges, rptR, ggExtra, caret, pdp, beepr, keyring
- Data files in `01 - Data/`
- Google Maps basemap: cached as `01 - Data/HHmap_basemap.rds` (included)

## Citation
If you use this code or data, please cite the associated publication:

> Bzonek PA, Croft-White MV, Reddick DT, Turner NA, MacLeod MC, Cooke SJ, Brooks JL, Hasler C, Midwood JD. Habitat selection and behavioural repeatability during spawning in stocked Walleye (*Sander vitreus*) in Hamilton Harbour. *In review*.

<!-- TODO: Update with journal name and DOI once published -->

## License
This project is licensed under the [MIT License](LICENSE). Copyright (c) 2026 His Majesty the King in Right of Canada, as represented by the Minister of Fisheries and Oceans.

## Author
Paul Bzonek

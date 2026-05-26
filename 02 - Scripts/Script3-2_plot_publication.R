## --------------------------------------------------------------#
## Script name: Script3-2_plot_publication
##
## Purpose of script: 
##    Generate method plots and visualizations for manuscript
##    
##    
## Dependencies: 
##    - All scripts
##    
##
## Author: Paul Bzonek 
##
## Date Created: 2025-10-31
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   
## --------------------------------------------------------------#


#####Setup Output Directory ######################################----
#-------------------------------------------------------------#

### Create date-stamped subfolder
#----------------------------#
param_output_date <- format(Sys.Date(), "%Y-%m-%d")
param_output_dir <- paste0("04 - Outputs/01 - Figures/", param_output_date)

# Create directory if it doesn't exist
if (!dir.exists(param_output_dir)) {
  dir.create(param_output_dir, recursive = TRUE)
  cat("Created output directory:", param_output_dir, "\n")
} else {
  cat("Using existing output directory:", param_output_dir, "\n")
}


#####Main Plots ##################################################----
#-------------------------------------------------------------#

### Figure 1: Location map 
#----------------------------#
plots$maps$LocationsMap

ggsave(paste0(param_output_dir, "/Fig1_LocationMap.tiff"),
       plot = plots$maps$LocationsMap, width = 7, height = 5.25, dpi = 400, units = "in")
ggsave(paste0(param_output_dir, "/Fig1_LocationMap.jpeg"),
       plot = plots$maps$LocationsMap, width = 7, height = 5.25, dpi = 400, units = "in")



### Figure 2: RF Predictor Importance 
#----------------------------#
plots$model_RF_plot$RF_RF2_R2

ggsave(paste0(param_output_dir, "/Fig2_RF_PredictorImportance.tiff"),
       plot = plots$model_RF_plot$RF_RF2_R2, width = 5, height = 4, dpi = 400, units = "in")
ggsave(paste0(param_output_dir, "/Fig2_RF_PredictorImportance.jpeg"),
       plot = plots$model_RF_plot$RF_RF2_R2, width = 5, height = 4, dpi = 400, units = "in")



### Figure 3: Univariate predictors 
#----------------------------#
plots$model_RF_plot$univiariate_RF2$combined

ggsave(paste0(param_output_dir, "/Fig3_UnivariatePredictors.tiff"),
       plot = plots$model_RF_plot$univiariate_RF2$combined, width = 7, height = 10, dpi = 400, units = "in")
ggsave(paste0(param_output_dir, "/Fig3_UnivariatePredictors.jpeg"),
       plot = plots$model_RF_plot$univiariate_RF2$combined, width = 7, height = 10, dpi = 400, units = "in")



### Figure 4: Bivariate predictors 
#----------------------------#
plots$model_RF_plot$biviariate_RF2$combined

ggsave(paste0(param_output_dir, "/Fig4_BivariatePredictors.tiff"),
       plot = plots$model_RF_plot$biviariate_RF2$combined, width = 7, height = 6, dpi = 400, units = "in")
ggsave(paste0(param_output_dir, "/Fig4_BivariatePredictors.jpeg"),
       plot = plots$model_RF_plot$biviariate_RF2$combined, width = 7, height = 6, dpi = 400, units = "in")



### Figure 5: Repeatability
#----------------------------#
  #  ## REVISION - REVIEWER COMMENT 23 ──────────────────────────
  # Add 25th / 50th / 75th percentile rank indicators to each panel.
  temp_n_fish_repeat <- length(unique(df_behaviour$animal_id))
  temp_p25r <- temp_n_fish_repeat * 0.25
  temp_p50r <- temp_n_fish_repeat * 0.50
  temp_p75r <- temp_n_fish_repeat * 0.75

  temp_pct_layers <- list(
    ggplot2::geom_vline(xintercept = temp_p25r, linetype = "dotted",  colour = "grey50", linewidth = 0.5),
    ggplot2::geom_vline(xintercept = temp_p50r, linetype = "dashed",  colour = "grey40", linewidth = 0.6),
    ggplot2::geom_vline(xintercept = temp_p75r, linetype = "longdash", colour = "grey30", linewidth = 0.5),
    ggplot2::annotate("text", x = temp_p25r, y = Inf, label = "25th",
                      vjust = 1.5, hjust = -0.1, size = 2.8, colour = "grey50"),
    ggplot2::annotate("text", x = temp_p50r, y = Inf, label = "50th",
                      vjust = 1.5, hjust = -0.1, size = 2.8, colour = "grey40"),
    ggplot2::annotate("text", x = temp_p75r, y = Inf, label = "75th",
                      vjust = 1.5, hjust = -0.1, size = 2.8, colour = "grey30")
  )

  # Add geom_rug on y-axis of each panel (shows value distribution)
  temp_rug <- geom_rug(sides = "r", outside=TRUE, position = "jitter",
                       alpha = 0.4, linewidth = 0.4)
  
  # geom_rug outside plot bounds needs to disable coord clip
  temp_coord <- coord_cartesian(clip = "off")
  
  plots$behaviour$station_count          <- plots$behaviour$station_count          + temp_pct_layers + temp_rug + temp_coord
  plots$behaviour$station_proportion     <- plots$behaviour$station_proportion     + temp_pct_layers + temp_rug + temp_coord
  plots$behaviour$station_specialization <- plots$behaviour$station_specialization + temp_pct_layers + temp_rug + temp_coord
  plots$behaviour$residence_duration     <- plots$behaviour$residence_duration     + temp_pct_layers + temp_rug + temp_coord
  plots$behaviour$spawning_depth         <- plots$behaviour$spawning_depth         + temp_pct_layers + temp_rug + temp_coord
 
  #  ## END REVISION ──────────────────────────────────────────────

### Independent rankings
plots$behaviour$FinalRepeatability <-
 with(plots$behaviour,

      (station_count/
       station_proportion/
       station_specialization/
       residence_duration/
       spawning_depth)
      )

plots$behaviour$FinalRepeatability

ggsave(paste0(param_output_dir, "/Fig5_Repeatability.tiff"),
       plot = plots$behaviour$FinalRepeatability, width = 7, height = 10, dpi = 400, units = "in")
ggsave(paste0(param_output_dir, "/Fig5_Repeatability.jpeg"),
       plot = plots$behaviour$FinalRepeatability, width = 7, height = 10, dpi = 400, units = "in")


### Combined rankings
plots$behaviour$ranked_repeatability

ggsave(paste0(param_output_dir, "/Fig5_Repeatability2.tiff"),
       plot = plots$behaviour$ranked_repeatability, width = 7, height = 10, dpi = 400, units = "in")
ggsave(paste0(param_output_dir, "/Fig5_Repeatability2.jpeg"),
       plot = plots$behaviour$ranked_repeatability, width = 7, height = 10, dpi = 400, units = "in")

#####Supplementary Plots #########################################----
#-------------------------------------------------------------#

### S1: Light Index
#----------------------------#
plots$maps$LightIndex

ggsave(paste0(param_output_dir, "/FigS1_LightIndex.tiff"),
       plot = plots$maps$LightIndex, width = 7, height = 5.25, dpi = 400, units = "in")
ggsave(paste0(param_output_dir, "/FigS1_LightIndex.jpeg"),
       plot = plots$maps$LightIndex, width = 7, height = 5.25, dpi = 400, units = "in")


### S3: Variable Correlations 
#----------------------------#
temp_cor_matrix <- data_habfish_raw %>%
  select(Silt:Boulder, SubstrateDiversity, Fetch, Slope, Light) %>%
  cor()

# Display
plots$methods$Correlations <- corrplot::corrplot.mixed(temp_cor_matrix, order='AOE', upper='shade', tl.pos = c("lt"))

# Save (corrplot creates base R graphics, needs different save approach)
tiff(paste0(param_output_dir, "/FigS2_VariableCorrelations.tiff"),
     width = 6, height = 6, units = "in", res = 400)
corrplot::corrplot.mixed(temp_cor_matrix, order='AOE', upper='shade', tl.pos = c("lt"))
dev.off()

jpeg(paste0(param_output_dir, "/FigS2_VariableCorrelations.jpeg"),
     width = 6, height = 6, units = "in", res = 400)
corrplot::corrplot.mixed(temp_cor_matrix, order='AOE', upper='shade', tl.pos = c("lt"))
dev.off()

rm(temp_cor_matrix)


### S4: Walleye Size
#----------------------------#
plots$methods$Walleye

ggsave(paste0(param_output_dir, "/FigS3_WalleyeSize.tiff"),
       plot = plots$methods$Walleye, width = 7, height = 7, dpi = 400, units = "in")
ggsave(paste0(param_output_dir, "/FigS3_WalleyeSize.jpeg"),
       plot = plots$methods$Walleye, width = 7, height = 7, dpi = 400, units = "in")



### S5: Spawn by Hour
#----------------------------#
plots$advanced$spawning_hourly$proportion_by_hour
ggsave(paste0(param_output_dir, "/FigSX_SpawningByHour.tiff"),
       plot = plots$advanced$spawning_hourly$proportion_by_hour, width = 7, height = 7, dpi = 400, units = "in")
ggsave(paste0(param_output_dir, "/FigSX_SpawningByHour.jpeg"),
       plot = plots$advanced$spawning_hourly$proportion_by_hour, width = 7, height = 7, dpi = 400, units = "in")


### S6: Depth histogram
#----------------------------#
plots$methods$depth_hist

ggsave(paste0(param_output_dir, "/FigSZ_DepthHistogram.tiff"),
       plot = plots$methods$depth_hist, width = 6, height = 5, dpi = 400, units = "in")
ggsave(paste0(param_output_dir, "/FigSZ_DepthHistogram.jpeg"),
       plot = plots$methods$depth_hist, width = 6, height = 5, dpi = 400, units = "in")


### S7: Spawn by Water Level
#----------------------------#
plots$advanced$spawning_summary$proportion_by_year_waterlevel
ggsave(paste0(param_output_dir, "/FigSY_Prop_WaterLevel.tiff"),
       plot = plots$advanced$spawning_summary$proportion_by_year_waterlevel, width = 7, height = 6, dpi = 400, units = "in")
ggsave(paste0(param_output_dir, "/FigSY_Prop_WaterLevel.jpeg"),
       plot = plots$advanced$spawning_summary$proportion_by_year_waterlevel, width = 7, height = 6, dpi = 400, units = "in")

 
### SX: Full efishing dataset
#----------------------------#
plots$maps$Efish
ggsave(paste0(param_output_dir, "/FigSX_EFish_Full.tiff"),
       plot = plots$maps$Efish, width = 7, height = 6, dpi = 400, units = "in")
ggsave(paste0(param_output_dir, "/FigSX_EFish_Full.jpeg"),
       plot = plots$maps$Efish, width = 7, height = 6, dpi = 400, units = "in")
 

### SY: Walleye growth curve
#----------------------------#
plots$behaviour$growth_trajectory_combined
ggsave(paste0(param_output_dir, "/FigSY_Growth_Curve.tiff"),
       plot = plots$behaviour$growth_trajectory_combined, width = 7, height = 8, dpi = 400, units = "in")
ggsave(paste0(param_output_dir, "/FigSY_Growth_Curve.jpeg"),
       plot = plots$behaviour$growth_trajectory_combined, width = 7, height = 8, dpi = 400, units = "in")


#####Cleanup #####################################################----
#-------------------------------------------------------------#

rm(list = ls(pattern = "^temp_"))
# Defensive removal of orphaned for-loop variables from a prior session
for (obj in c("d", "i", "p", "tr", "yvar")) if (exists(obj)) rm(list = obj)
rm(obj)
cat("Cleanup complete.\n")

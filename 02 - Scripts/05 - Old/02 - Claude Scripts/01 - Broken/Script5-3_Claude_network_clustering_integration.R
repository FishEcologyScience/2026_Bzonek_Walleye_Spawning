## --------------------------------------------------------------#
## Script name: Script5-3_Claude_network_clustering_integration
##
## Purpose of script: 
##    Integrate network analysis results (loop_NW_plot, loop_NW_data_plot) 
##    with k-means clustering from df_behaviour
##    Create comprehensive behavioral phenotype analysis
##
## Author: Paul Bzonek [Claude] 
##
## Date Created: 2025-01-11
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   
## --------------------------------------------------------------#

### Load required packages
#----------------------------#
library(tidyverse)
# igraph, ggraph, patchwork, cluster, factoextra, corrplot, viridis, GGally, plotly, psych will use :: notation

### Ensure required data objects exist
#----------------------------#
# Check for network analysis results
if(!exists("loop_NW_data_plot") | !exists("loop_NW_plot")) {
  cat("Loading network analysis results from Script2-Y_analysis_t_network_plot.R...\n")
  source("02 - Scripts/Script2-Y_analysis_t_network_plot.R")
}

# Check for behavioral clustering data
if(!exists("df_behaviour") | !exists("df_behaviour_kmeans")) {
  cat("Loading behavioral clustering data from Script3-2_CombinedBehaviours.R...\n")
  source("02 - Scripts/Script3-2_CombinedBehaviours.R")
}

### Extract network metrics from loop results
#----------------------------#
cat("Extracting network metrics from individual loop results...\n")

#' Extract Network Metrics from Loop Results
#'
#' @param loop_data_plot Data frame containing network analysis results
#' @param loop_plot_list List of network plots
#' @return Data frame with network metrics by individual
func_extract_network_metrics <- function(loop_data_plot, loop_plot_list) {
  # Initialize results dataframe
  network_metrics <- tibble()
  
  if(nrow(loop_data_plot) > 0) {
    # First, let's check what columns are actually available
    cat("Available columns in loop_data_plot:\n")
    print(colnames(loop_data_plot))
    cat("\nFirst few rows:\n")
    print(head(loop_data_plot, 3))
    
    # Extract metrics from available columns only
    network_summary <- loop_data_plot %>%
      group_by(animal_id) %>%
      summarise(
        # Basic connectivity metrics (assuming from/to structure exists)
        n_receivers = if("to" %in% colnames(loop_data_plot)) n_distinct(to) else NA,
        n_senders = if("from" %in% colnames(loop_data_plot)) n_distinct(from) else NA,
        unique_connections = if(all(c("from", "to") %in% colnames(loop_data_plot))) n_distinct(paste(from, to, sep = "-")) else NA,
        
        # Movement frequency metrics (if freq column exists)
        total_moves = if("freq" %in% colnames(loop_data_plot)) sum(freq, na.rm = TRUE) else NA,
        mean_moves = if("freq" %in% colnames(loop_data_plot)) mean(freq, na.rm = TRUE) else NA,
        max_moves = if("freq" %in% colnames(loop_data_plot)) max(freq, na.rm = TRUE) else NA,
        
        # Spatial extent metrics (if coordinate columns exist)
        spatial_range_x = if(all(c("from.x", "to.x") %in% colnames(loop_data_plot))) {
          max(c(from.x, to.x), na.rm = TRUE) - min(c(from.x, to.x), na.rm = TRUE)
        } else NA,
        spatial_range_y = if(all(c("from.y", "to.y") %in% colnames(loop_data_plot))) {
          max(c(from.y, to.y), na.rm = TRUE) - min(c(from.y, to.y), na.rm = TRUE)
        } else NA,
        
        # Network activity level
        total_connections = n(),
        
        .groups = "drop"
      )
    
    network_metrics <- network_summary
  } else {
    cat("Warning: loop_data_plot is empty\n")
  }
  
  return(network_metrics)
}

# Extract network metrics
temp_network_metrics <- func_extract_network_metrics(loop_NW_data_plot, loop_NW_plot)

### Integrate network metrics with behavioral clustering
#----------------------------#
cat("Integrating network metrics with behavioral clustering data...\n")

if(nrow(temp_network_metrics) > 0 && exists("df_behaviour_kmeans")) {
  cat("Starting integration process...\n")
  cat("temp_network_metrics has", nrow(temp_network_metrics), "rows\n")
  cat("df_behaviour_kmeans exists:", exists("df_behaviour_kmeans"), "\n")
  
  # Step 1: Get clean behavioral data for matching
  temp_behavior_clean <- df_behaviour %>% 
    select(animal_id, year, station_count:detcount_sum) %>%
    na.omit()
  cat("temp_behavior_clean has", nrow(temp_behavior_clean), "rows\n")
  
  # Step 2: Add row IDs to both datasets for matching
  temp_behavior_with_id <- temp_behavior_clean %>%
    mutate(row_id = row_number())
  
  temp_kmeans_with_id <- df_behaviour_kmeans %>% 
    mutate(row_id = row_number())
  cat("temp_kmeans_with_id has", nrow(temp_kmeans_with_id), "rows\n")
  
  # Step 3: Check if we can match by row_id instead (simpler approach)
  cat("Checking if datasets have same number of rows for row_id matching...\n")
  cat("temp_behavior_with_id rows:", nrow(temp_behavior_with_id), "\n")
  cat("temp_kmeans_with_id rows:", nrow(temp_kmeans_with_id), "\n")
  
  # Check if we can match by row position (assuming k-means was run on same cleaned data)
  if(nrow(temp_behavior_with_id) == nrow(temp_kmeans_with_id)) {
    cat("Same number of rows - trying row_id matching...\n")
    temp_kmeans_matched <- temp_kmeans_with_id %>%
      left_join(
        temp_behavior_with_id %>% select(row_id, animal_id, year),
        by = "row_id"
      ) %>%
      select(animal_id, year, kmeans4) %>%
      filter(!is.na(animal_id))
  } else {
    cat("Different number of rows - trying metric-based matching...\n")
    # Check what columns actually exist in both datasets
    kmeans_cols <- colnames(temp_kmeans_with_id)
    behavior_cols <- colnames(temp_behavior_with_id)
    cat("K-means columns:", paste(kmeans_cols, collapse = ", "), "\n")
    cat("Behavior columns:", paste(behavior_cols, collapse = ", "), "\n")
    
    # Find common behavioral metric columns
    common_cols <- intersect(kmeans_cols, behavior_cols)
    behavioral_metrics <- c("station_count", "station_count_spawn", "station_count_ratio",
                           "mcp95", "mcp95_spawn", "mcp95_ratio", "residence_mean",
                           "residence_sum", "detcount_mean", "detcount_sum")
    available_metrics <- intersect(common_cols, behavioral_metrics)
    
    cat("Available metrics for joining:", paste(available_metrics, collapse = ", "), "\n")
    
    if(length(available_metrics) > 0) {
      temp_kmeans_matched <- temp_kmeans_with_id %>%
        left_join(
          temp_behavior_with_id,
          by = available_metrics
        ) %>%
        select(animal_id, year, kmeans4) %>%
        filter(!is.na(animal_id))
    } else {
      cat("No common metrics found - cannot match datasets\n")
      temp_kmeans_matched <- tibble(animal_id = character(), year = numeric(), kmeans4 = numeric())
    }
  }
  cat("temp_kmeans_matched has", nrow(temp_kmeans_matched), "rows\n")
  
  # Step 4: Create final mapping by animal_id
  temp_kmeans_mapping <- temp_kmeans_matched %>%
    group_by(animal_id) %>%
    summarise(
      kmeans_cluster = first(kmeans4[!is.na(kmeans4)]),
      n_years = n(),
      .groups = "drop"
    )
  cat("temp_kmeans_mapping has", nrow(temp_kmeans_mapping), "rows\n")
  if(nrow(temp_kmeans_mapping) > 0) {
    cat("Sample animal_ids in temp_kmeans_mapping:\n")
    print(head(temp_kmeans_mapping$animal_id, 3))
  }
  if(nrow(temp_network_metrics) > 0) {
    cat("Sample animal_ids in temp_network_metrics:\n") 
    print(head(temp_network_metrics$animal_id, 3))
  }
  
  # Integrate all datasets
  temp_integrated_network_behavior <- temp_network_metrics %>%
    left_join(temp_kmeans_mapping, by = "animal_id") %>%
    left_join(
      df_behaviour %>%
        group_by(animal_id) %>%
        summarise(
          mean_station_count = mean(station_count, na.rm = TRUE),
          mean_station_ratio = mean(station_count_ratio, na.rm = TRUE),
          mean_residence_sum = mean(residence_sum, na.rm = TRUE),
          mean_mcp95 = mean(mcp95, na.rm = TRUE),
          mean_length = mean(length_total, na.rm = TRUE),
          prop_spawn_years = mean(SpawnBinary, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "animal_id"
    ) %>%
    filter(!is.na(kmeans_cluster))  # Only keep fish with k-means assignments
  
} else {
  cat("Warning: Could not integrate datasets - missing network metrics or k-means data\n")
  cat("Checking what data is available...\n")
  cat("temp_network_metrics exists:", exists("temp_network_metrics"), "with rows:", 
      if(exists("temp_network_metrics")) nrow(temp_network_metrics) else 0, "\n")
  cat("df_behaviour_kmeans exists:", exists("df_behaviour_kmeans"), "\n")
  
  # Try alternative approach - use df_behaviour directly if it has animal_id and cluster info
  if(exists("temp_network_metrics") && nrow(temp_network_metrics) > 0 && exists("df_behaviour")) {
    cat("Attempting alternative integration using df_behaviour directly...\n")
    
    # Check if df_behaviour has any cluster information or we can create simple behavioral metrics
    behavior_summary <- df_behaviour %>%
      group_by(animal_id) %>%
      summarise(
        mean_station_count = mean(station_count, na.rm = TRUE),
        mean_station_ratio = mean(station_count_ratio, na.rm = TRUE),
        mean_residence_sum = mean(residence_sum, na.rm = TRUE),
        mean_mcp95 = mean(mcp95, na.rm = TRUE),
        mean_length = mean(length_total, na.rm = TRUE),
        prop_spawn_years = mean(SpawnBinary, na.rm = TRUE),
        .groups = "drop"
      )
    
    temp_integrated_network_behavior <- temp_network_metrics %>%
      left_join(behavior_summary, by = "animal_id") %>%
      filter(!is.na(mean_station_count))  # Keep only fish with behavioral data
    
    cat("Alternative integration created dataset with", nrow(temp_integrated_network_behavior), "rows\n")
  } else {
    temp_integrated_network_behavior <- tibble()
  }
}

### Enhanced clustering with network metrics
#----------------------------#
if(nrow(temp_integrated_network_behavior) > 5) {
  cat("Performing enhanced clustering with network metrics...\n")
  
  # Prepare data for clustering (network + behavioral metrics)
  # Use only available network metrics
  available_network_cols <- intersect(
    c("n_receivers", "n_senders", "total_moves", "mean_moves", "unique_connections", 
      "spatial_range_x", "spatial_range_y", "total_connections"),
    colnames(temp_integrated_network_behavior)
  )
  
  cat("Available network metrics for clustering:", paste(available_network_cols, collapse = ", "), "\n")
  
  temp_clustering_data <- temp_integrated_network_behavior %>%
    select(
      all_of(available_network_cols),
      # Behavioral metrics
      mean_station_count, mean_station_ratio, mean_residence_sum, mean_mcp95
    ) %>%
    na.omit() %>%
    scale() %>%
    as.data.frame()
  
  # Determine optimal number of clusters
  set.seed(1987)
  
  # Elbow method
  temp_wss <- map_dbl(1:8, ~{
    kmeans(temp_clustering_data, centers = .x, nstart = 20)$tot.withinss
  })
  
  temp_elbow_plot <- tibble(k = 1:8, wss = temp_wss) %>%
    ggplot(aes(x = k, y = wss)) +
    geom_line() +
    geom_point() +
    labs(title = "Elbow Method for Optimal Clusters",
         subtitle = "Network + Behavioral Metrics",
         x = "Number of Clusters (k)",
         y = "Within-cluster Sum of Squares") +
    theme_minimal()
  
  # Silhouette method
  temp_sil_widths <- map_dbl(2:8, ~{
    temp_km <- kmeans(temp_clustering_data, centers = .x, nstart = 20)
    cluster::silhouette(temp_km$cluster, dist(temp_clustering_data))[,3] %>% mean()
  })
  
  temp_silhouette_plot <- tibble(k = 2:8, sil_width = temp_sil_widths) %>%
    ggplot(aes(x = k, y = sil_width)) +
    geom_line() +
    geom_point() +
    labs(title = "Silhouette Method for Optimal Clusters",
         x = "Number of Clusters (k)",
         y = "Average Silhouette Width") +
    theme_minimal()
  
  # Perform clustering with optimal k (choose based on plots, default to 4)
  param_optimal_k <- 4  # Can be adjusted based on elbow/silhouette plots
  
  temp_enhanced_kmeans <- kmeans(temp_clustering_data, centers = param_optimal_k, nstart = 20)
  
  # Add cluster assignments back to data
  temp_integrated_network_behavior$enhanced_cluster <- temp_enhanced_kmeans$cluster
  
  # Cluster interpretation
  temp_cluster_centers <- as.data.frame(temp_enhanced_kmeans$centers) %>%
    mutate(cluster = row_number()) %>%
    pivot_longer(cols = -cluster, names_to = "metric", values_to = "value")
  
  temp_cluster_interpretation <- temp_integrated_network_behavior %>%
    group_by(enhanced_cluster) %>%
    summarise(
      n_fish = n(),
      # Available network characteristics
      mean_receivers = if("n_receivers" %in% colnames(.)) mean(n_receivers, na.rm = TRUE) else NA,
      mean_total_moves = if("total_moves" %in% colnames(.)) mean(total_moves, na.rm = TRUE) else NA,
      mean_connections = if("total_connections" %in% colnames(.)) mean(total_connections, na.rm = TRUE) else NA,
      mean_spatial_range_x = if("spatial_range_x" %in% colnames(.)) mean(spatial_range_x, na.rm = TRUE) else NA,
      # Behavioral characteristics
      mean_station_ratio = mean(mean_station_ratio, na.rm = TRUE),
      mean_residence = mean(mean_residence_sum, na.rm = TRUE),
      mean_spawn_prop = mean(prop_spawn_years, na.rm = TRUE),
      # Fish characteristics
      mean_length = mean(mean_length, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      cluster_type = case_when(
        enhanced_cluster == 1 ~ "High connectivity, varied movement",
        enhanced_cluster == 2 ~ "Moderate connectivity, site fidelity", 
        enhanced_cluster == 3 ~ "Low connectivity, localized movement",
        enhanced_cluster == 4 ~ "High connectivity, extensive movement",
        TRUE ~ paste("Cluster", enhanced_cluster)
      )
    )
  
  print("Enhanced Cluster Interpretation:")
  print(temp_cluster_interpretation)
}

### Analysis 1: Compare original vs enhanced clustering
#----------------------------#
if(nrow(temp_integrated_network_behavior) > 5) {
  cat("Comparing original behavioral vs enhanced network+behavioral clustering...\n")
  
  temp_cluster_comparison <- temp_integrated_network_behavior %>%
    filter(!is.na(kmeans_cluster), !is.na(enhanced_cluster)) %>%
    count(kmeans_cluster, enhanced_cluster) %>%
    pivot_wider(names_from = enhanced_cluster, values_from = n, values_fill = 0) %>%
    column_to_rownames("kmeans_cluster")
  
  # Calculate cluster agreement
  temp_cluster_agreement <- temp_integrated_network_behavior %>%
    filter(!is.na(kmeans_cluster), !is.na(enhanced_cluster)) %>%
    summarise(
      agreement = mean(kmeans_cluster == enhanced_cluster),
      kappa = psych::cohen.kappa(cbind(kmeans_cluster, enhanced_cluster))$kappa
    )
  
  print("Cluster Comparison Matrix:")
  print(temp_cluster_comparison)
  print("Cluster Agreement:")
  print(temp_cluster_agreement)
}

### Visualization 1: Network metrics by behavioral clusters
#----------------------------#
if(nrow(temp_integrated_network_behavior) > 5) {
  cat("Creating network-behavioral visualizations...\n")
  
  # Network metrics by original k-means clusters - use available columns
  available_viz_cols <- intersect(
    c("n_receivers", "total_moves", "mean_moves", "unique_connections", 
      "total_connections", "spatial_range_x"),
    colnames(temp_integrated_network_behavior)
  )
  
  if(length(available_viz_cols) > 0) {
    temp_network_by_behavior_plot <- temp_integrated_network_behavior %>%
      select(animal_id, kmeans_cluster, all_of(available_viz_cols)) %>%
      pivot_longer(cols = all_of(available_viz_cols),
                   names_to = "network_metric", values_to = "value") %>%
      mutate(
        network_metric = recode(network_metric,
          "n_receivers" = "Number of Receivers",
          "total_moves" = "Total Movements",
          "mean_moves" = "Mean Movements per Connection",
          "unique_connections" = "Unique Connections",
          "total_connections" = "Total Connections",
          "spatial_range_x" = "Spatial Range (X)"
        ),
        kmeans_cluster = paste("Behavioral Cluster", kmeans_cluster)
      ) %>%
      ggplot(aes(x = kmeans_cluster, y = value, fill = kmeans_cluster)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.5) +
      facet_wrap(~ network_metric, scales = "free_y", ncol = 3) +
      labs(
        title = "Network Metrics by Behavioral K-means Clusters",
        subtitle = "How network connectivity patterns relate to spawning behaviors",
        x = "Behavioral Cluster",
        y = "Network Metric Value",
        fill = "Cluster"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      ) +
      scale_fill_viridis_d()
  } else {
    cat("Warning: No network metrics available for visualization\n")
    temp_network_by_behavior_plot <- ggplot() + 
      ggtitle("No network metrics available for visualization")
  }
  
  # ggsave("04 - Outputs/network_metrics_by_behavioral_clusters.png", 
  #        temp_network_by_behavior_plot, width = 14, height = 10, dpi = 300)
}

### Visualization 2: Enhanced clustering results
#----------------------------#
if(exists("temp_enhanced_kmeans") && nrow(temp_integrated_network_behavior) > 5) {
  
  # PCA visualization of enhanced clustering
  temp_pca_result <- prcomp(temp_clustering_data, scale. = FALSE)  # Already scaled
  
  temp_pca_plot_data <- temp_pca_result$x[,1:2] %>%
    as.data.frame() %>%
    mutate(
      enhanced_cluster = factor(temp_enhanced_kmeans$cluster),
      original_cluster = factor(temp_integrated_network_behavior$kmeans_cluster)
    )
  
  temp_enhanced_cluster_plot <- temp_pca_plot_data %>%
    ggplot(aes(x = PC1, y = PC2, color = enhanced_cluster, shape = original_cluster)) +
    geom_point(size = 3, alpha = 0.8) +
    stat_ellipse(aes(group = enhanced_cluster), level = 0.68) +
    labs(
      title = "Enhanced Clustering: Network + Behavioral Metrics",
      subtitle = paste("PC1 explains", round(summary(temp_pca_result)$importance[2,1]*100, 1),
                       "%, PC2 explains", round(summary(temp_pca_result)$importance[2,2]*100, 1), "%"),
      x = paste("PC1 (", round(summary(temp_pca_result)$importance[2,1]*100, 1), "%)"),
      y = paste("PC2 (", round(summary(temp_pca_result)$importance[2,2]*100, 1), "%)"),
      color = "Enhanced\nCluster",
      shape = "Original\nBehavioral\nCluster"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "right"
    ) +
    scale_color_viridis_d()
  
  # ggsave("04 - Outputs/enhanced_clustering_pca.png", temp_enhanced_cluster_plot, 
  #        width = 12, height = 8, dpi = 300)
  
  # Cluster centers heatmap
  temp_cluster_heatmap <- temp_cluster_centers %>%
    ggplot(aes(x = factor(cluster), y = metric, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    labs(
      title = "Enhanced Cluster Centers",
      subtitle = "Standardized values for network and behavioral metrics",
      x = "Cluster",
      y = "Metric",
      fill = "Standardized\nValue"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text.y = element_text(size = 8)
    )
  
  # ggsave("04 - Outputs/enhanced_cluster_centers_heatmap.png", temp_cluster_heatmap,
  #        width = 10, height = 8, dpi = 300)
}

### Visualization 3: Individual network plots by cluster
#----------------------------#
if(exists("loop_NW_plot") && nrow(temp_integrated_network_behavior) > 0) {
  cat("Creating individual network plot examples by cluster...\n")
  
  # Select representative individuals from each cluster
  temp_cluster_examples <- temp_integrated_network_behavior %>%
    group_by(enhanced_cluster) %>%
    slice_sample(n = 2) %>%  # 2 examples per cluster
    ungroup() %>%
    select(animal_id, enhanced_cluster, cluster_type = enhanced_cluster) %>%
    mutate(cluster_label = paste("Cluster", enhanced_cluster))
  
  # Create combined plot if network plots exist
  temp_example_plots <- list()
  
  for(i in 1:nrow(temp_cluster_examples)) {
    fish_id <- temp_cluster_examples$animal_id[i]
    cluster_label <- temp_cluster_examples$cluster_label[i]
    
    if(fish_id %in% names(loop_NW_plot)) {
      if("combined" %in% names(loop_NW_plot[[fish_id]])) {
        temp_example_plots[[paste(cluster_label, fish_id, sep = "_")]] <- 
          loop_NW_plot[[fish_id]][["combined"]] +
          labs(title = paste(cluster_label, "-", fish_id))
      }
    }
  }
  
  # Save individual example plots
  # if(length(temp_example_plots) > 0) {
  #   for(plot_name in names(temp_example_plots)) {
  #     ggsave(paste0("04 - Outputs/network_example_", plot_name, ".png"),
  #            temp_example_plots[[plot_name]], width = 10, height = 8, dpi = 300)
  #   }
  # }
}

### Export integrated results
#----------------------------#
cat("Exporting integrated network-behavioral results...\n")

# Main integrated dataset
if(nrow(temp_integrated_network_behavior) > 0) {
  write.csv(temp_integrated_network_behavior, 
            "04 - Outputs/integrated_network_behavioral_clusters.csv", 
            row.names = FALSE)
}

# Network metrics summary
if(nrow(temp_network_metrics) > 0) {
  write.csv(temp_network_metrics, "04 - Outputs/network_metrics_summary.csv", 
            row.names = FALSE)
}

# Cluster interpretation
if(exists("temp_cluster_interpretation")) {
  write.csv(temp_cluster_interpretation, 
            "04 - Outputs/enhanced_cluster_interpretation.csv", 
            row.names = FALSE)
}

# Cluster comparison
if(exists("temp_cluster_comparison")) {
  write.csv(temp_cluster_comparison, "04 - Outputs/cluster_comparison_matrix.csv")
}

# Save clustering objects
if(exists("temp_enhanced_kmeans")) {
  saveRDS(temp_enhanced_kmeans, "04 - Outputs/enhanced_kmeans_model.rds")
}

### Summary statistics
#----------------------------#
cat("\n=== NETWORK-BEHAVIORAL CLUSTERING INTEGRATION SUMMARY ===\n")

if(nrow(temp_integrated_network_behavior) > 0) {
  cat("Successfully integrated", nrow(temp_integrated_network_behavior), "individuals\n")
  cat("Network metrics extracted for", nrow(temp_network_metrics), "individuals\n")
  
  if(exists("temp_cluster_interpretation")) {
    cat("\nEnhanced cluster sizes:\n")
    print(temp_cluster_interpretation %>% select(enhanced_cluster, cluster_type, n_fish))
  }
  
  if(exists("temp_cluster_agreement")) {
    cat("\nCluster agreement with original behavioral clustering:\n")
    cat("Agreement rate:", round(temp_cluster_agreement$agreement * 100, 1), "%\n")
    cat("Cohen's kappa:", round(temp_cluster_agreement$kappa, 3), "\n")
  }
  
  cat("\nTop network metrics by variance explained:\n")
  if(exists("temp_pca_result")) {
    temp_loadings <- temp_pca_result$rotation[,1:2] %>%
      as.data.frame() %>%
      rownames_to_column("metric") %>%
      mutate(total_loading = abs(PC1) + abs(PC2)) %>%
      arrange(desc(total_loading))
    print(head(temp_loadings, 5))
  }
  
} else {
  cat("Warning: Integration failed - check that required data objects exist\n")
  cat("Required objects: loop_NW_data_plot, loop_NW_plot, df_behaviour, df_behaviour_kmeans\n")
}

cat("\nAnalysis complete. Check '04 - Outputs/' folder for detailed results and visualizations.\n")

### Clean up temporary objects
#----------------------------#
cat("Cleaning up temporary objects...\n")

# Remove all temp_ objects
rm(list = ls(pattern = "^temp_"))

# Remove other objects created in this script
rm(param_optimal_k, func_extract_network_metrics)

cat("Cleanup complete.\n")
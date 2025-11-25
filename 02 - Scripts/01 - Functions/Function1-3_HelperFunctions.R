## --------------------------------------------------------------#
## Script name: Script0-2_helper_functions
##
## Purpose of script:
##    Build simple helper functions to be used in workflow
##
##
##
##
## Author: Paul Bzonek
##
## Date Created: 2025-10-03
##
## --------------------------------------------------------------#
## Modification Notes:
##   
## --------------------------------------------------------------#

 

#-------------------------------------------------------------#
#####func_calculate_exceeding ####################################----
#-------------------------------------------------------------#
#' Calculate the percentage of values exceeding specified thresholds
#'
#' @description
#' This function evaluates how many values in a numeric vector exceed 
#' a series of user-defined thresholds. It reports, for each threshold, 
#' the percentage of values greater than or equal to that cutoff.
#'
#' @param x A numeric vector containing the values to evaluate. 
#'   Non-numeric inputs will trigger an error. Missing values (`NA`) 
#'   are automatically ignored.
#' @param thresholds A numeric vector of threshold values to test. 
#'   Defaults to integers 1 through 12.
#' @param verbose Logical; if TRUE (default), prints progress messages 
#'   to the console showing each threshold and the percentage of values exceeding it.
#'
#' @return
#' A named numeric vector where:
#' \itemize{
#'   \item Names correspond to threshold values (e.g., "Threshold_1", "Threshold_2", ...)
#'   \item Values represent the percentage of input values greater than or equal to each threshold
#' }
#'
#' @examples
#' # Example data
#' test_values <- c(0.5, 1.2, 3.8, 5.0, 9.7, 10.5, 12.3, NA, 15.0)
#'
#' # Compute percentages exceeding thresholds from 1 to 12
#' calculate_exceeding(x = test_values, thresholds = 1:12)
#'
#' @export
func_calculate_exceeding <- function(x, thresholds = 1:12, verbose = TRUE) {
  #----------------------------
  # 1. Validate and prepare input
  #----------------------------
  # Ensure input is numeric
  if (!is.numeric(x)) {stop("`x` must be a numeric vector.")}
  
  # Convert to numeric explicitly and remove missing values
  x <- as.numeric(x)
  valid_values <- x[!is.na(x)]
  
  # Handle the case where all values are missing
  if (length(valid_values) == 0) {
    warning("No valid numeric values found in `x`. Returning NA vector.")
    return(setNames(rep(NA_real_, length(thresholds)), thresholds))
  }
  
  #----------------------------
  # 2. Compute percent exceeding each threshold
  #----------------------------
  
  # Iterate over each threshold and compute % exceeding
  results <- sapply(thresholds, function(th) {
    # Proportion of valid values ≥ threshold
    proportion_exceeding <- sum(valid_values >= th) / length(valid_values)
    
    # Convert to percentage and round to two decimal places
    percent_exceeding <- round(proportion_exceeding * 100, 2)
    
    # Optionally print progress for transparency
    if (verbose) {
      cat("Threshold:", th, "| Percent Exceeding:", percent_exceeding, "%\n")
    }
    
    return(percent_exceeding)
  })
  
  #----------------------------
  # 3. Format and return output
  #----------------------------
  
  # Assign readable names for clarity
  names(results) <- paste0("Threshold_", thresholds)
  
  return(results)
}




#-------------------------------------------------------------#
#####func_source_clean ####################################----
#-------------------------------------------------------------#
#' Source R scripts with verbosity control
#'
#' @description
#' A wrapper around `source()` that provides flexible control over console output
#' verbosity. Useful for cleaning up console output when sourcing multiple scripts
#' in a workflow, while maintaining the ability to debug individual scripts when needed.
#'
#' @param file Character string specifying the path to the R script file to source.
#'   Accepts both relative and absolute paths.
#' @param level Character string controlling verbosity level. Must be one of:
#'   \itemize{
#'     \item \code{"debug"} - Shows code expressions, output, and progress messages
#'       (most verbose, for troubleshooting)
#'     \item \code{"full"} - Shows output and progress messages but not code expressions
#'       (default interactive mode)
#'     \item \code{"minimal"} - Shows cat() output and errors, suppresses warnings,
#'       messages like "Joining with...", and plots (recommended for production runs)
#'     \item \code{"silent"} - Completely silent execution with no console output
#'       (for automated pipelines)
#'     \item \code{"code_only"} - Shows code expressions but suppresses output
#'       (rare debugging use case)
#'   }
#'   Defaults to \code{"minimal"}.
#' @param beep_on_complete Logical; if TRUE, plays a sound after script completes (requires beepr package).
#'   Useful for marking the end of a selected code block. Defaults to FALSE.
#' @param beep_on_error Logical; if TRUE, plays an alarm sound when an error occurs (requires beepr package).
#'   Useful for audio notification of script failures. Defaults to FALSE.
#'
#' @return
#' Invisibly returns the result of `source()`. Side effects include loading
#' objects, functions, and variables from the sourced script into the current
#' environment, and optionally printing progress/output to console based on
#' verbosity level.
#'
#' @details
#' The function uses `switch()` to control both the `echo` argument of `source()`
#' (which controls code visibility) and `capture.output()` (which suppresses
#' output like `cat()`, `print()`, and plot displays).
#'
#' Progress messages show only the basename of the file for cleaner output.
#'
#' @examples
#' # Minimal output (just progress)
#' func_source_clean("02 - Scripts/Script1-1_format_data_h.R", level = "minimal")
#'
#' # Full debugging with code and output
#' func_source_clean("02 - Scripts/Script1-1_format_data_h.R", level = "debug")
#'
#' # Completely silent
#' func_source_clean("02 - Scripts/Script1-1_format_data_h.R", level = "silent")
#'
#' # Using with a global parameter
#' param_verbose <- "minimal"
#' func_source_clean("02 - Scripts/Script1-1_format_data_h.R", level = param_verbose)
#'
#' @export
func_source_clean <- function(file, level = "minimal", beep_on_complete = FALSE, beep_on_error = FALSE) {

  #----------------------------
  # 1. Validate inputs
  #----------------------------
  valid_levels <- c("debug", "full", "minimal", "silent", "code_only")
  if (!level %in% valid_levels) {
    stop("`level` must be one of: ", paste(valid_levels, collapse = ", "))
  }

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  if ((beep_on_complete || beep_on_error) && !requireNamespace("beepr", quietly = TRUE)) {
    warning("beepr package not installed. Beep will not sound.")
    beep_on_complete <- FALSE
    beep_on_error <- TRUE
  }

  #----------------------------
  # 2. Execute source with verbosity control and error handling
  #----------------------------
  tryCatch(
    {
       switch(level,
 
              "debug" = {
                # Shows code + output + progress message
                cat("\n=== SOURCING:", basename(file), "===\n")
                source(file, echo = TRUE)
                cat("=== COMPLETE ===\n\n")
              },
      
              "full" = {
                # Shows output but NOT code, with progress message
                cat("Loading:", basename(file), "...")
                source(file, echo = FALSE)
                cat(" Done.\n")
              },
      
              "minimal" = {
                # Shows cat output and errors, suppresses warnings and messages
                cat("\n---", basename(file), "---\n")
                suppressMessages(suppressWarnings(source(file, echo = FALSE)))
                cat("--- COMPLETE ---\n")
              },
      
              "silent" = {
                # Completely silent - no progress, no output, no code
                cat("\n---", basename(file), "---\n")
                pdf(NULL)  # Redirect plots to null device
                suppressMessages(suppressWarnings(
                  invisible(capture.output(source(file, echo = FALSE)))
                ))
                dev.off()  # Close null device
              },
      
              "code_only" = {
                # Shows code but suppresses output (rare use case)
                cat("\n>>> Code from:", basename(file), "\n")
                invisible(capture.output(source(file, echo = TRUE)))
              }
            ) #End switch
    }, #End main try catch
    error = function(e) {
      # Play error beep if requested
      if (beep_on_error) {
        beepr::beep(sound = 7)  # sound = 9 is "wilhelm" scream (alarm)
     }
      # Re-throw the error to stop execution
      stop(e)
    }
  )

  #----------------------------
  # 3. Play completion sound if requested
  #----------------------------
  if (beep_on_complete) {
    beepr::beep(sound = 1)  # Add alarm
  }
}



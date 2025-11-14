#' euissGeo Package Configuration Functions
#' 
#' @name config
#' @keywords internal
NULL

#' Configure data paths for EUISS geospatial functions
#'
#' This function sets up and validates data paths for various geospatial data sources
#' used by euissGeo functions. It provides intelligent fallbacks and cross-platform
#' compatibility.
#'
#' @param data_path Optional custom base data path. If NULL, uses environment variables or defaults
#' @param gisco_path Optional specific path for GISCO data
#' @param ne_path Optional specific path for Natural Earth data  
#' @param raster_path Optional specific path for raster data
#' @param create_missing Logical. Should missing directories be created? Default TRUE
#' @param verbose Logical. Print path information? Default FALSE
#'
#' @return Named list with configured paths:
#' \describe{
#'   \item{base}{Base data directory}
#'   \item{gisco}{GISCO data directory}
#'   \item{natural_earth}{Natural Earth data directory}
#'   \item{raster}{Raster data directory}
#' }
#'
#' @details
#' The function tries to locate data in the following order:
#' 1. User-provided paths
#' 2. Environment variables (EUISS_DATA_PATH, EUISS_GISCO_PATH, etc.)
#' 3. Common default locations
#' 4. Temporary directory as fallback
#'
#' Environment variables can be set in .Renviron:
#' \code{EUISS_DATA_PATH="/path/to/your/data"}
#'
#' @examples
#' # Use defaults
#' paths <- euiss_configure_paths()
#' 
#' # Custom base path
#' paths <- euiss_configure_paths("/my/data/directory")
#' 
#' # Specific paths for each data type
#' paths <- euiss_configure_paths(
#'   gisco_path = "/data/gisco",
#'   ne_path = "/data/natural_earth"
#' )
#'
#' @export
euiss_configure_paths <- function(data_path = NULL,
                                 gisco_path = NULL,
                                 ne_path = NULL,
                                 raster_path = NULL,
                                 create_missing = TRUE,
                                 verbose = FALSE) {
  
  # Configure base data path
  if (is.null(data_path)) {
    # Try environment variable first
    env_path <- Sys.getenv("EUISS_DATA_PATH")
    if (env_path != "") {
      data_path <- env_path
    } else {
      # Try common locations
      possible_paths <- c(
        "D:/data",           # Original Windows path
        "~/data",            # User home data
        "./data",            # Relative to working directory
        "/data",             # Unix system data
        file.path(getwd(), "data")  # Working directory data
      )
      
      data_path <- Find(dir.exists, possible_paths)
      
      if (is.null(data_path)) {
        # Use temporary directory as last resort
        data_path <- file.path(tempdir(), "euiss_data")
        if (verbose) {
          message("No existing data directory found. Using temporary: ", data_path)
        }
      }
    }
  }
  
  # Validate base path
  if (!dir.exists(data_path) && create_missing) {
    dir.create(data_path, recursive = TRUE, showWarnings = FALSE)
    if (verbose) message("Created data directory: ", data_path)
  }
  
  # Configure GISCO path
  if (is.null(gisco_path)) {
    env_gisco <- Sys.getenv("EUISS_GISCO_PATH")
    if (env_gisco != "") {
      gisco_path <- env_gisco
    } else {
      gisco_path <- file.path(data_path, "giscoR")
    }
  }
  
  # Configure Natural Earth path
  if (is.null(ne_path)) {
    env_ne <- Sys.getenv("EUISS_NE_PATH")
    if (env_ne != "") {
      ne_path <- env_ne
    } else {
      ne_path <- file.path(data_path, "natural_earth")
    }
  }
  
  # Configure raster path
  if (is.null(raster_path)) {
    env_raster <- Sys.getenv("EUISS_RASTER_PATH")
    if (env_raster != "") {
      raster_path <- env_raster
    } else {
      raster_path <- file.path(data_path, "raster")
    }
  }
  
  # Create missing directories if requested
  if (create_missing) {
    for (path in c(gisco_path, ne_path, raster_path)) {
      if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE, showWarnings = FALSE)
        if (verbose) message("Created directory: ", path)
      }
    }
  }
  
  paths <- list(
    base = normalizePath(data_path, mustWork = FALSE),
    gisco = normalizePath(gisco_path, mustWork = FALSE),
    natural_earth = normalizePath(ne_path, mustWork = FALSE),
    raster = normalizePath(raster_path, mustWork = FALSE)
  )
  
  if (verbose) {
    message("Configured paths:")
    for (name in names(paths)) {
      status <- if (dir.exists(paths[[name]])) "EXISTS" else "MISSING"
      message("  ", name, ": ", paths[[name]], " [", status, "]")
    }
  }
  
  return(paths)
}

#' Get EUISS package configuration
#' 
#' Returns current configuration including paths, available data sources,
#' and package status.
#' 
#' @return List with package configuration information
#' @export
euiss_get_config <- function() {
  
  paths <- euiss_configure_paths()
  
  # Check data availability
  data_status <- list(
    gisco_available = length(list.files(paths$gisco, pattern = "\\.zip$")) > 0,
    ne_available = length(list.files(paths$natural_earth, recursive = TRUE)) > 0,
    raster_available = length(list.files(paths$raster, pattern = "\\.tif$", recursive = TRUE)) > 0
  )
  
  # Check package dependencies
  deps <- c("sf", "terra", "dplyr", "ggplot2", "rnaturalearth", "giscoR", 
           "tmaptools", "countrycode", "cartogram", "progress")
  
  deps_status <- sapply(deps, function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  })
  
  # Check euissR integration
  euissR_available <- requireNamespace("euissR", quietly = TRUE)
  
  config <- list(
    paths = paths,
    data_status = data_status,
    dependencies = list(
      available = names(deps_status)[deps_status],
      missing = names(deps_status)[!deps_status]
    ),
    euissR_integration = euissR_available,
    version = utils::packageVersion("euissGeo")
  )
  
  return(config)
}

#' Print package configuration summary
#' 
#' @export
euiss_check_setup <- function() {
  
  cat("=== euissGeo Package Configuration ===\n\n")
  
  config <- euiss_get_config()
  
  # Print paths
  cat("Data Paths:\n")
  for (name in names(config$paths)) {
    status <- if (dir.exists(config$paths[[name]])) "✓" else "✗"
    cat("  ", status, " ", name, ": ", config$paths[[name]], "\n")
  }
  
  # Print data availability
  cat("\nData Availability:\n")
  for (name in names(config$data_status)) {
    status <- if (config$data_status[[name]]) "✓" else "✗"
    cat("  ", status, " ", gsub("_", " ", name), "\n")
  }
  
  # Print dependencies
  cat("\nDependencies:\n")
  if (length(config$dependencies$available) > 0) {
    cat("  ✓ Available:", paste(config$dependencies$available, collapse = ", "), "\n")
  }
  if (length(config$dependencies$missing) > 0) {
    cat("  ✗ Missing:", paste(config$dependencies$missing, collapse = ", "), "\n")
    cat("    Install with: install.packages(c(", 
        paste0('"', config$dependencies$missing, '"', collapse = ", "), "))\n")
  }
  
  # Print euissR status
  euiss_status <- if (config$euissR_integration) "✓ Available" else "✗ Not available"
  cat("  euissR Integration:", euiss_status, "\n")
  
  cat("\nFor help configuring paths, see ?euiss_configure_paths\n")
}

#' Set environment variable for data paths
#' 
#' Helper function to set environment variables for persistent path configuration.
#' 
#' @param data_path Base data path
#' @param permanent Logical. Should this be saved permanently? Requires .Renviron edit
#' @export
euiss_set_data_path <- function(data_path, permanent = FALSE) {
  
  # Validate path
  if (!dir.exists(data_path)) {
    stop("Directory does not exist: ", data_path)
  }
  
  # Set environment variable for current session
  Sys.setenv(EUISS_DATA_PATH = normalizePath(data_path))
  
  message("Set EUISS_DATA_PATH to: ", normalizePath(data_path))
  
  if (permanent) {
    # Check if .Renviron exists
    renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")
    
    if (file.exists(renviron_path)) {
      # Read existing .Renviron
      renviron_lines <- readLines(renviron_path)
      
      # Check if EUISS_DATA_PATH already exists
      existing_line <- grep("^EUISS_DATA_PATH=", renviron_lines)
      
      if (length(existing_line) > 0) {
        # Update existing line
        renviron_lines[existing_line[1]] <- paste0("EUISS_DATA_PATH=", normalizePath(data_path))
        message("Updated existing EUISS_DATA_PATH in .Renviron")
      } else {
        # Add new line
        renviron_lines <- c(renviron_lines, paste0("EUISS_DATA_PATH=", normalizePath(data_path)))
        message("Added EUISS_DATA_PATH to .Renviron")
      }
      
      # Write back to .Renviron
      writeLines(renviron_lines, renviron_path)
      
    } else {
      # Create new .Renviron
      writeLines(paste0("EUISS_DATA_PATH=", normalizePath(data_path)), renviron_path)
      message("Created .Renviron with EUISS_DATA_PATH")
    }
    
    message("Restart R session for permanent changes to take effect.")
  }
}

#' euissGeo Package Configuration Functions
#' 
#' @name config
NULL

#' Configure data paths for EUISS geospatial functions
#'
#' Sets up and validates data paths for various geospatial data sources.
#' Provides intelligent fallbacks and cross-platform compatibility.
#'
#' @param data_path Optional custom base data path. If NULL, uses environment variables or defaults.
#' @param gisco_path Optional specific path for GISCO data.
#' @param ne_path Optional specific path for Natural Earth data.
#' @param raster_path Optional specific path for raster data.
#' @param create_missing Create missing directories? (default: TRUE)
#' @param verbose Print path information? (default: FALSE)
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
#' Path resolution order:
#' 1. User-provided paths
#' 2. Environment variables (EUISS_DATA_PATH, etc.)
#' 3. Common default locations
#' 4. Temporary directory as fallback
#'
#' Set environment variables in .Renviron:
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
  
  # Resolve base data path
  data_path <- .resolve_base_path(data_path, verbose)
  
  # Resolve component paths
  gisco_path <- .resolve_component_path(
    gisco_path, 
    env_var = "EUISS_GISCO_PATH",
    default_subdir = "giscoR",
    base_path = data_path
  )
  
  ne_path <- .resolve_component_path(
    ne_path,
    env_var = "EUISS_NE_PATH", 
    default_subdir = "natural_earth",
    base_path = data_path
  )
  
  raster_path <- .resolve_component_path(
    raster_path,
    env_var = "EUISS_RASTER_PATH",
    default_subdir = "raster", 
    base_path = data_path
  )
  
  # Create directories if needed
  all_paths <- c(data_path, gisco_path, ne_path, raster_path)
  
  if (create_missing) {
    for (path in all_paths) {
      if (!dir.exists(path)) {
        created <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
        if (created && verbose) {
          message("Created directory: ", path)
        }
      }
    }
  }
  
  # Build result list
  paths <- list(
    base = normalizePath(data_path, mustWork = FALSE),
    gisco = normalizePath(gisco_path, mustWork = FALSE),
    natural_earth = normalizePath(ne_path, mustWork = FALSE),
    raster = normalizePath(raster_path, mustWork = FALSE)
  )
  
  # Print summary if requested
  if (verbose) {
    message("\nConfigured paths:")
    for (name in names(paths)) {
      exists_flag <- if (dir.exists(paths[[name]])) "EXISTS" else "MISSING"
      message("  ", name, ": ", paths[[name]], " [", exists_flag, "]")
    }
  }
  
  paths
}

#' Resolve base data path with fallbacks
#' @keywords internal
#' @noRd
.resolve_base_path <- function(data_path, verbose = FALSE) {
  
  if (!is.null(data_path)) {
    return(data_path)
  }
  
  # Try environment variable
  env_path <- Sys.getenv("EUISS_DATA_PATH", unset = "")
  if (env_path != "") {
    if (verbose) message("Using EUISS_DATA_PATH: ", env_path)
    return(env_path)
  }
  
  # Try common locations
  possible_paths <- c(
    "D:/data",                    # Original Windows path
    "~/data",                     # User home data
    file.path(getwd(), "data"),   # Working directory data
    "./data",                     # Relative path
    "/data"                       # Unix system data
  )
  
  for (path in possible_paths) {
    expanded <- path.expand(path)
    if (dir.exists(expanded)) {
      if (verbose) message("Found data directory: ", expanded)
      return(expanded)
    }
  }
  
  # Fallback to temporary directory
  temp_data <- file.path(tempdir(), "euiss_data")
  if (verbose) {
    message("No data directory found. Using temporary: ", temp_data)
  }
  
  temp_data
}

#' Resolve component path (gisco, natural_earth, raster)
#' @keywords internal
#' @noRd
.resolve_component_path <- function(component_path, 
                                    env_var, 
                                    default_subdir,
                                    base_path) {
  
  # User-provided path takes precedence
  if (!is.null(component_path)) {
    return(component_path)
  }
  
  # Try environment variable
  env_value <- Sys.getenv(env_var, unset = "")
  if (env_value != "") {
    return(env_value)
  }
  
  # Default to subdirectory of base path
  file.path(base_path, default_subdir)
}

#' Get EUISS package configuration
#' 
#' Returns current configuration including paths, available data sources,
#' and package status.
#' 
#' @return List with package configuration information
#' @export
euiss_get_config <- function() {
  
  paths <- euiss_configure_paths(verbose = FALSE)
  
  # Check data availability
  data_status <- list(
    gisco_available = .check_gisco_data(paths$gisco),
    ne_available = .check_ne_data(paths$natural_earth),
    raster_available = .check_raster_data(paths$raster)
  )
  
  # Check package dependencies
  required_deps <- c("sf", "terra", "dplyr", "ggplot2")
  suggested_deps <- c("rnaturalearth", "giscoR", "tmaptools", 
                      "countrycode", "cartogram", "progress")
  
  deps_status <- list(
    required_available = vapply(required_deps, requireNamespace, logical(1), quietly = TRUE),
    suggested_available = vapply(suggested_deps, requireNamespace, logical(1), quietly = TRUE)
  )
  
  # Check euissR integration
  euissR_available <- requireNamespace("euissR", quietly = TRUE)
  
  list(
    version = utils::packageVersion("euissGeo"),
    paths = paths,
    data_status = data_status,
    dependencies = list(
      required = list(
        available = names(deps_status$required_available)[deps_status$required_available],
        missing = names(deps_status$required_available)[!deps_status$required_available]
      ),
      suggested = list(
        available = names(deps_status$suggested_available)[deps_status$suggested_available],
        missing = names(deps_status$suggested_available)[!deps_status$suggested_available]
      )
    ),
    euissR_integration = euissR_available
  )
}

#' Check for GISCO data files
#' @keywords internal
#' @noRd
.check_gisco_data <- function(path) {
  if (!dir.exists(path)) return(FALSE)
  length(list.files(path, pattern = "\\.zip$")) > 0
}

#' Check for Natural Earth data files
#' @keywords internal
#' @noRd
.check_ne_data <- function(path) {
  if (!dir.exists(path)) return(FALSE)
  length(list.files(path, recursive = TRUE)) > 0
}

#' Check for raster data files
#' @keywords internal
#' @noRd
.check_raster_data <- function(path) {
  if (!dir.exists(path)) return(FALSE)
  length(list.files(path, pattern = "\\.tif$", recursive = TRUE)) > 0
}

#' Print package configuration summary
#' 
#' @export
euiss_check_setup <- function() {
  
  cat("=== euissGeo Package Configuration ===\n\n")
  
  config <- euiss_get_config()
  
  # Print version
  cat("Version:", as.character(config$version), "\n\n")
  
  # Print paths
  cat("Data Paths:\n")
  for (name in names(config$paths)) {
    exists <- dir.exists(config$paths[[name]])
    symbol <- .get_check_symbol(exists)
    cat("  ", symbol, " ", name, ": ", config$paths[[name]], "\n", sep = "")
  }
  
  # Print data availability
  cat("\nData Availability:\n")
  data_labels <- c(
    gisco_available = "GISCO data",
    ne_available = "Natural Earth data",
    raster_available = "Raster data"
  )
  
  for (key in names(config$data_status)) {
    symbol <- .get_check_symbol(config$data_status[[key]])
    label <- data_labels[key]
    cat("  ", symbol, " ", label, "\n", sep = "")
  }
  
  # Print dependencies
  cat("\nRequired Dependencies:\n")
  if (length(config$dependencies$required$available) > 0) {
    cat("  ", .get_check_symbol(TRUE), " Available: ", 
        paste(config$dependencies$required$available, collapse = ", "), "\n", sep = "")
  }
  if (length(config$dependencies$required$missing) > 0) {
    cat("  ", .get_check_symbol(FALSE), " Missing: ", 
        paste(config$dependencies$required$missing, collapse = ", "), "\n", sep = "")
    cat("    Install with: install.packages(c('", 
        paste(config$dependencies$required$missing, collapse = "', '"), "'))\n", sep = "")
  }
  
  # Print suggested dependencies
  if (length(config$dependencies$suggested$missing) > 0) {
    cat("\nSuggested Dependencies (optional):\n")
    cat("  Missing: ", paste(config$dependencies$suggested$missing, collapse = ", "), "\n", sep = "")
  }
  
  # Print euissR status
  cat("\neuissR Integration: ")
  if (config$euissR_integration) {
    cat(.get_check_symbol(TRUE), " Available\n", sep = "")
  } else {
    cat(.get_check_symbol(FALSE), " Not available\n", sep = "")
    cat("  Install with: devtools::install_github('cdtrich/euissR')\n")
  }
  
  cat("\nFor help: ?euiss_configure_paths\n")
  
  invisible(config)
}

#' Set environment variable for data paths
#' 
#' Helper function to set environment variables for persistent path configuration.
#' 
#' @param data_path Base data path
#' @param permanent Save to .Renviron file? (default: FALSE)
#' 
#' @export
euiss_set_data_path <- function(data_path, permanent = FALSE) {
  
  # Validate path
  if (!dir.exists(data_path)) {
    stop("Directory does not exist: ", data_path, call. = FALSE)
  }
  
  # Normalize path
  data_path_normalized <- normalizePath(data_path)
  
  # Set for current session
  Sys.setenv(EUISS_DATA_PATH = data_path_normalized)
  message("Set EUISS_DATA_PATH to: ", data_path_normalized)
  
  # Make permanent if requested
  if (permanent) {
    .write_to_renviron("EUISS_DATA_PATH", data_path_normalized)
    message("Restart R for permanent changes to take effect")
  }
  
  invisible(data_path_normalized)
}

#' Write environment variable to .Renviron
#' @keywords internal
#' @noRd
.write_to_renviron <- function(var_name, var_value) {
  
  renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")
  
  # Read existing or create empty
  if (file.exists(renviron_path)) {
    lines <- readLines(renviron_path, warn = FALSE)
  } else {
    lines <- character(0)
  }
  
  # Check if variable exists
  pattern <- paste0("^", var_name, "=")
  existing_idx <- grep(pattern, lines)
  
  new_line <- paste0(var_name, "=", var_value)
  
  if (length(existing_idx) > 0) {
    # Update existing
    lines[existing_idx[1]] <- new_line
    message("Updated ", var_name, " in .Renviron")
  } else {
    # Add new
    lines <- c(lines, new_line)
    message("Added ", var_name, " to .Renviron")
  }
  
  # Write back
  writeLines(lines, renviron_path)
  
  invisible(TRUE)
}
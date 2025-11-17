#' Shared Utility Functions
#' 
#' Internal utility functions used across multiple euissGeo modules.
#' 
#' @name utilities
#' @keywords internal
NULL

# Symbol handling ----

#' Get check mark symbol (UTF-8 safe)
#' @param success Logical indicating success/failure
#' @param unicode Use Unicode symbols if available?
#' @return Character string with appropriate symbol
#' @keywords internal
#' @noRd
.get_check_symbol <- function(success, unicode = TRUE) {
  
  use_unicode <- unicode && l10n_info()$`UTF-8`
  
  if (success) {
    if (use_unicode) "\u2713" else "[OK]"  # ✓
  } else {
    if (use_unicode) "\u2717" else "[X]"   # ✗
  }
}

# Package checking ----

#' Check if required packages are available
#' 
#' @param packages Character vector of package names to check
#' @param stop_on_missing Should function stop if packages are missing?
#' @param quiet Suppress messages?
#' @return Named logical vector indicating package availability
#' @keywords internal
#' @noRd
.check_packages <- function(packages, stop_on_missing = TRUE, quiet = FALSE) {
  
  available <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  missing <- packages[!available]
  
  if (length(missing) > 0) {
    msg <- paste0(
      "Required packages missing: ", paste(missing, collapse = ", "), "\n",
      "Install with: install.packages(c('", 
      paste(missing, collapse = "', '"), "'))"
    )
    
    if (stop_on_missing) {
      stop(msg, call. = FALSE)
    } else if (!quiet) {
      warning(msg, call. = FALSE)
    }
  }
  
  available
}

#' Check package version meets minimum requirement
#' @param package_name Package name
#' @param minimum_version Minimum required version string
#' @return Logical indicating if version requirement is met
#' @keywords internal
#' @noRd
.check_package_version <- function(package_name, minimum_version) {
  
  if (!requireNamespace(package_name, quietly = TRUE)) {
    return(FALSE)
  }
  
  tryCatch({
    installed <- utils::packageVersion(package_name)
    required <- as.package_version(minimum_version)
    installed >= required
  }, error = function(e) FALSE)
}

# Input validation ----

#' Validate positive numeric input
#' @param x Value to validate
#' @param param_name Parameter name for error messages
#' @param allow_zero Allow zero values?
#' @return Invisible TRUE if valid, stops otherwise
#' @keywords internal
#' @noRd
.validate_positive_number <- function(x, param_name, allow_zero = FALSE) {
  
  if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
    stop("`", param_name, "` must be a single numeric value", call. = FALSE)
  }
  
  min_val <- if (allow_zero) 0 else .Machine$double.eps
  
  if (x < min_val) {
    stop(
      "`", param_name, "` must be ", 
      if (allow_zero) "non-negative" else "positive",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

# Country standardization ----

#' Standardize country names to ISO codes
#' 
#' Unified function for country code conversion with Kosovo handling.
#' 
#' @param countries Character vector of country names or codes
#' @param origin Origin format (default: "country.name")
#' @param destination Target format (default: "iso3c")
#' @param handle_kosovo Handle Kosovo specially? (default: TRUE)
#' @param warn Show countrycode warnings? (default: FALSE)
#' @return Character vector of standardized country codes
#' @keywords internal
#' @noRd
.standardize_country_codes <- function(countries, 
                                       origin = "country.name",
                                       destination = "iso3c",
                                       handle_kosovo = TRUE,
                                       warn = FALSE) {
  
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    warning("countrycode package not available for country standardization", 
            call. = FALSE)
    return(countries)
  }
  
  # Convert to target format
  codes <- tryCatch({
    countrycode::countrycode(
      countries,
      origin = origin,
      destination = destination,
      warn = warn
    )
  }, error = function(e) {
    warning("Country code conversion failed: ", e$message, call. = FALSE)
    rep(NA_character_, length(countries))
  })
  
  # Handle Kosovo specially
  if (handle_kosovo) {
    kosovo_idx <- grepl("kosovo", countries, ignore.case = TRUE)
    if (any(kosovo_idx, na.rm = TRUE)) {
      kosovo_code <- switch(destination,
                            "iso3c" = "KOS",
                            "iso2c" = "XK",
                            "country.name" = "Kosovo",
                            "KOS"  # default fallback
      )
      codes[kosovo_idx] <- kosovo_code
    }
  }
  
  codes
}

# File/path utilities ----

#' Resolve file path with fallbacks
#' 
#' @param paths Character vector of paths to try
#' @param type Type of path for error messages
#' @param create_if_missing Create directory if it doesn't exist?
#' @return First existing path, or NULL if none exist
#' @keywords internal
#' @noRd
.resolve_path <- function(paths, type = "data", create_if_missing = FALSE) {
  
  if (length(paths) == 0) {
    return(NULL)
  }
  
  # Expand all paths
  expanded_paths <- vapply(paths, path.expand, character(1))
  
  # Find first existing path
  existing_paths <- expanded_paths[file.exists(expanded_paths)]
  
  if (length(existing_paths) > 0) {
    return(existing_paths[1])
  }
  
  # Create first path if requested
  if (create_if_missing && length(expanded_paths) > 0) {
    first_path <- expanded_paths[1]
    
    created <- tryCatch({
      dir.create(first_path, recursive = TRUE, showWarnings = FALSE)
      TRUE
    }, error = function(e) FALSE)
    
    if (created && dir.exists(first_path)) {
      return(first_path)
    }
  }
  
  NULL
}

#' Format file size for human reading
#' 
#' @param bytes Numeric file size in bytes
#' @return Character string with formatted file size
#' @keywords internal
#' @noRd
.format_file_size <- function(bytes) {
  
  if (is.na(bytes) || bytes == 0) {
    return("0 B")
  }
  
  units <- c("B", "KB", "MB", "GB", "TB")
  unit_index <- min(floor(log(bytes) / log(1024)) + 1, length(units))
  
  size <- bytes / (1024^(unit_index - 1))
  
  # Adaptive precision
  precision <- if (size >= 100) 0 else if (size >= 10) 1 else 2
  
  sprintf(paste0("%.", precision, "f %s"), size, units[unit_index])
}

# SF/geometry utilities ----

#' Validate and optionally repair sf object
#' 
#' @param sf_object sf object to validate
#' @param repair Attempt to repair invalid geometries?
#' @return Validated and optionally repaired sf object
#' @keywords internal
#' @noRd
.validate_sf <- function(sf_object, repair = TRUE) {
  
  if (!inherits(sf_object, "sf")) {
    stop("Object is not an sf object", call. = FALSE)
  }
  
  if (nrow(sf_object) == 0) {
    warning("sf object has no rows", call. = FALSE)
    return(sf_object)
  }
  
  # Check for valid geometries
  valid_geoms <- sf::st_is_valid(sf_object)
  
  if (any(!valid_geoms, na.rm = TRUE)) {
    n_invalid <- sum(!valid_geoms, na.rm = TRUE)
    warning("Found ", n_invalid, " invalid geometries", call. = FALSE)
    
    if (repair) {
      sf_object <- tryCatch({
        sf::st_make_valid(sf_object)
      }, error = function(e) {
        warning("Could not repair geometries: ", e$message, call. = FALSE)
        sf_object
      })
    }
  }
  
  # Remove empty geometries
  empty_geoms <- sf::st_is_empty(sf_object)
  if (any(empty_geoms, na.rm = TRUE)) {
    n_empty <- sum(empty_geoms, na.rm = TRUE)
    warning("Removing ", n_empty, " empty geometries", call. = FALSE)
    sf_object <- sf_object[!empty_geoms, ]
  }
  
  sf_object
}

#' Get appropriate CRS for a geographic region
#' 
#' @param sf_object sf object representing the region
#' @param preference CRS preference: "equal_area", "conformal", or "geographic"
#' @return CRS object or EPSG code
#' @keywords internal
#' @noRd
.suggest_crs <- function(sf_object, preference = "equal_area") {
  
  if (!inherits(sf_object, "sf")) {
    return(4326)  # Default WGS84
  }
  
  # Calculate centroid
  bbox <- sf::st_bbox(sf_object)
  centroid_lon <- mean(c(bbox["xmin"], bbox["xmax"]))
  centroid_lat <- mean(c(bbox["ymin"], bbox["ymax"]))
  
  # Suggest CRS based on location and preference
  if (preference == "equal_area") {
    
    if (abs(centroid_lat) > 60) {
      # Polar regions - polar stereographic
      return(if (centroid_lat > 0) 3413L else 3031L)
    } else {
      # Lambert Azimuthal Equal Area centered on region
      proj_string <- sprintf(
        "+proj=laea +lat_0=%.1f +lon_0=%.1f +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
        centroid_lat, centroid_lon
      )
      return(sf::st_crs(proj_string))
    }
    
  } else if (preference == "conformal") {
    
    # UTM zone
    utm_zone <- floor((centroid_lon + 180) / 6) + 1
    epsg_code <- if (centroid_lat >= 0) {
      32600L + utm_zone
    } else {
      32700L + utm_zone
    }
    return(epsg_code)
    
  } else {
    # Geographic
    return(4326L)
  }
}

# Progress bar utilities ----

#' Create progress bar if package available
#' 
#' @param show Show progress bar?
#' @param total Total number of steps
#' @param format Progress bar format string
#' @return Progress bar object or NULL
#' @keywords internal
#' @noRd
.create_progress_bar <- function(show = TRUE, 
                                 total = 10, 
                                 format = "[:bar] :percent :etas") {
  
  if (!show || !requireNamespace("progress", quietly = TRUE)) {
    return(NULL)
  }
  
  tryCatch({
    progress::progress_bar$new(
      format = format,
      total = total,
      clear = FALSE,
      width = 60
    )
  }, error = function(e) NULL)
}

#' Safely tick progress bar
#' 
#' @param pb Progress bar object (can be NULL)
#' @keywords internal
#' @noRd
.tick_progress <- function(pb) {
  if (!is.null(pb) && inherits(pb, "progress_bar")) {
    tryCatch(pb$tick(), error = function(e) invisible())
  }
  invisible()
}
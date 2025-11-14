#' Shared Utility Functions
#' 
#' Internal utility functions used across multiple euissGeo modules.
#' 
#' @name utilities
#' @keywords internal
NULL

#' Check if required packages are available
#' 
#' @param packages Character vector of package names to check
#' @param stop_on_missing Logical. Should function stop if packages are missing?
#' @param quiet Logical. Suppress messages?
#' @return Named logical vector indicating package availability
#' @keywords internal
.check_packages <- function(packages, stop_on_missing = TRUE, quiet = FALSE) {
  
  available <- sapply(packages, function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  })
  
  missing <- packages[!available]
  
  if (length(missing) > 0) {
    message_text <- paste0(
      "Required packages missing: ", paste(missing, collapse = ", "), 
      "\nInstall with: install.packages(c(", 
      paste0('"', missing, '"', collapse = ", "), "))"
    )
    
    if (stop_on_missing) {
      stop(message_text)
    } else if (!quiet) {
      warning(message_text)
    }
  }
  
  return(available)
}

#' Standardize country names using multiple strategies
#' 
#' @param countries Character vector of country names
#' @param target_format Target format for standardization
#' @param handle_kosovo Logical. Handle Kosovo specially?
#' @return Character vector of standardized country names
#' @keywords internal
.standardize_countries <- function(countries, 
                                  target_format = "iso3c", 
                                  handle_kosovo = TRUE) {
  
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    warning("countrycode package not available for country standardization")
    return(countries)
  }
  
  # Convert to target format
  standardized <- tryCatch({
    countrycode::countrycode(
      countries,
      origin = "country.name",
      destination = target_format,
      warn = FALSE
    )
  }, error = function(e) {
    warning("Country standardization failed: ", e$message)
    return(rep(NA, length(countries)))
  })
  
  # Handle Kosovo specially
  if (handle_kosovo) {
    kosovo_matches <- grepl("kosovo", countries, ignore.case = TRUE)
    if (any(kosovo_matches)) {
      kosovo_code <- switch(target_format,
        "iso3c" = "KOS",
        "iso2c" = "XK",
        "country.name" = "Kosovo"
      )
      standardized[kosovo_matches] <- kosovo_code
    }
  }
  
  return(standardized)
}

#' Safe file path resolution with fallbacks
#' 
#' @param paths Character vector of paths to try
#' @param type Type of path for error messages
#' @param create_if_missing Logical. Create directory if it doesn't exist?
#' @return First existing path, or NULL if none exist
#' @keywords internal
.resolve_path <- function(paths, type = "data", create_if_missing = FALSE) {
  
  if (length(paths) == 0) {
    return(NULL)
  }
  
  # Expand all paths
  expanded_paths <- sapply(paths, path.expand)
  
  # Find first existing path
  existing_paths <- expanded_paths[file.exists(expanded_paths)]
  
  if (length(existing_paths) > 0) {
    return(existing_paths[1])
  }
  
  # If no paths exist and creation is requested, try to create first path
  if (create_if_missing && length(expanded_paths) > 0) {
    first_path <- expanded_paths[1]
    
    created <- tryCatch({
      dir.create(first_path, recursive = TRUE, showWarnings = FALSE)
    }, error = function(e) {
      FALSE
    })
    
    if (created && dir.exists(first_path)) {
      return(first_path)
    }
  }
  
  return(NULL)
}

#' Format file size for human reading
#' 
#' @param bytes Numeric file size in bytes
#' @return Character string with formatted file size
#' @keywords internal
.format_file_size <- function(bytes) {
  
  if (is.na(bytes) || bytes == 0) {
    return("0 B")
  }
  
  units <- c("B", "KB", "MB", "GB", "TB")
  unit_index <- floor(log(bytes) / log(1024)) + 1
  unit_index <- min(unit_index, length(units))
  
  size <- bytes / (1024^(unit_index - 1))
  
  if (size >= 100) {
    format_str <- "%.0f %s"
  } else if (size >= 10) {
    format_str <- "%.1f %s"
  } else {
    format_str <- "%.2f %s"
  }
  
  sprintf(format_str, size, units[unit_index])
}

#' Safe geometry operations with error handling
#' 
#' @param sf_object sf object to process
#' @param operation Function to apply
#' @param ... Additional arguments to operation
#' @return Processed sf object or original if operation fails
#' @keywords internal
.safe_geom_operation <- function(sf_object, operation, ...) {
  
  if (!inherits(sf_object, "sf")) {
    return(sf_object)
  }
  
  tryCatch({
    result <- operation(sf_object, ...)
    return(result)
  }, error = function(e) {
    warning("Geometry operation failed: ", e$message, ". Returning original data.")
    return(sf_object)
  })
}

#' Create progress bar if requested and package available
#' 
#' @param show Logical. Should progress bar be created?
#' @param total Total number of steps
#' @param format Progress bar format string
#' @return Progress bar object or NULL
#' @keywords internal
.create_progress_bar <- function(show = TRUE, total = 10, format = "[:bar] :percent :etas") {
  
  if (!show) {
    return(NULL)
  }
  
  if (!requireNamespace("progress", quietly = TRUE)) {
    return(NULL)
  }
  
  tryCatch({
    progress::progress_bar$new(
      format = format,
      total = total,
      clear = FALSE,
      width = 60
    )
  }, error = function(e) {
    NULL
  })
}

#' Tick progress bar safely
#' 
#' @param pb Progress bar object (can be NULL)
#' @keywords internal
.tick_progress <- function(pb) {
  if (!is.null(pb) && inherits(pb, "progress_bar")) {
    tryCatch({
      pb$tick()
    }, error = function(e) {
      # Fail silently
    })
  }
}

#' Get appropriate CRS for a geographic region
#' 
#' @param sf_object sf object representing the region
#' @param preference CRS preference: "equal_area", "conformal", or "geographic"
#' @return CRS string or EPSG code
#' @keywords internal
.suggest_crs <- function(sf_object, preference = "equal_area") {
  
  if (!inherits(sf_object, "sf")) {
    return(4326)  # Default to WGS84
  }
  
  # Calculate bbox and centroid
  bbox <- sf::st_bbox(sf_object)
  centroid_lon <- mean(c(bbox["xmin"], bbox["xmax"]))
  centroid_lat <- mean(c(bbox["ymin"], bbox["ymax"]))
  
  # Suggest appropriate CRS based on location and preference
  if (preference == "equal_area") {
    
    if (abs(centroid_lat) > 60) {
      # Polar regions - use polar stereographic
      if (centroid_lat > 0) {
        return(3413)  # NSIDC Sea Ice Polar Stereographic North
      } else {
        return(3031)  # Antarctic Polar Stereographic
      }
    } else {
      # Use Lambert Azimuthal Equal Area centered on the region
      return(paste0(
        "+proj=laea +lat_0=", round(centroid_lat, 1),
        " +lon_0=", round(centroid_lon, 1),
        " +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
      ))
    }
    
  } else if (preference == "conformal") {
    
    # Use UTM zone for small areas
    utm_zone <- floor((centroid_lon + 180) / 6) + 1
    hemisphere <- ifelse(centroid_lat >= 0, "north", "south")
    
    if (hemisphere == "north") {
      epsg_code <- 32600 + utm_zone
    } else {
      epsg_code <- 32700 + utm_zone
    }
    
    return(epsg_code)
    
  } else {
    # Geographic (lat/lon)
    return(4326)
  }
}

#' Validate and clean sf object
#' 
#' @param sf_object sf object to validate
#' @param repair Logical. Attempt to repair invalid geometries?
#' @return Validated and optionally repaired sf object
#' @keywords internal
.validate_sf <- function(sf_object, repair = TRUE) {
  
  if (!inherits(sf_object, "sf")) {
    stop("Object is not an sf object")
  }
  
  if (nrow(sf_object) == 0) {
    warning("sf object has no rows")
    return(sf_object)
  }
  
  # Check for valid geometries
  valid_geoms <- sf::st_is_valid(sf_object)
  
  if (any(!valid_geoms)) {
    warning("Found ", sum(!valid_geoms), " invalid geometries")
    
    if (repair) {
      sf_object <- .safe_geom_operation(sf_object, sf::st_make_valid)
    }
  }
  
  # Remove empty geometries
  empty_geoms <- sf::st_is_empty(sf_object)
  if (any(empty_geoms)) {
    warning("Removing ", sum(empty_geoms), " empty geometries")
    sf_object <- sf_object[!empty_geoms, ]
  }
  
  return(sf_object)
}

#' Extract numeric version from package name for comparison
#' 
#' @param package_name Name of package to check
#' @param minimum_version Minimum required version
#' @return Logical indicating if version requirement is met
#' @keywords internal
.check_package_version <- function(package_name, minimum_version) {
  
  if (!requireNamespace(package_name, quietly = TRUE)) {
    return(FALSE)
  }
  
  tryCatch({
    installed_version <- utils::packageVersion(package_name)
    minimum_version <- as.package_version(minimum_version)
    
    return(installed_version >= minimum_version)
  }, error = function(e) {
    return(FALSE)
  })
}

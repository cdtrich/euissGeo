#' Spatial Data Conversion Functions
#' 
#' Functions for converting between different spatial data formats and coordinate systems.
#' 
#' @name spatial-convert
NULL

#' Convert coordinates in a data frame to Simple Features (sf) object
#'
#' This function takes a data frame containing coordinates as a single column and converts it
#' into a Simple Features (sf) object. It assumes that the coordinates column contains 
#' two values separated by a specified separator.
#'
#' @param df A data frame containing coordinates as a single column.
#' @param coords The name of the coordinates column in the data frame.
#' @param sep The separator used in the coordinates column to separate x and y values.
#' @param x The name for the x coordinate variable after separation.
#' @param y The name for the y coordinate variable after separation.
#' @param remove Logical; should the original coordinates column be removed?
#' @param crs The coordinate reference system (CRS) code to be used. Default is EPSG 4326 (WGS 84).
#'
#' @return A Simple Features (sf) object with point geometries based on the coordinates.
#'
#' @details
#' The function expects coordinates in the format "y, x" (latitude, longitude) by default.
#' This follows the common convention where latitude is listed first. If your data
#' has coordinates in "x, y" format, adjust the x and y parameter names accordingly.
#'
#' @examples
#' # Example data frame with coordinates
#' data <- data.frame(
#'   location = c("Paris", "London", "Berlin"),
#'   coords = c("48.8566, 2.3522", "51.5074, -0.1278", "52.5200, 13.4050"),
#'   population = c(2161000, 8982000, 3669000)
#' )
#'
#' # Convert coordinates to sf object
#' sf_data <- euiss_coords_to_sf(data, coords = "coords")
#' 
#' \dontrun{
#' # Custom separator and column names
#' data2 <- data.frame(
#'   name = "Rome",
#'   position = "41.9028|12.4964"
#' )
#' sf_data2 <- euiss_coords_to_sf(data2, coords = "position", sep = "|")
#' }
#'
#' @import dplyr
#' @import sf
#' @import tidyr
#'
#' @export
euiss_coords_to_sf <- function(df,
                               coords = "coords",
                               sep = ", ",
                               x = "x",
                               y = "y",
                               remove = TRUE,
                               crs = 4326) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame")
  }
  
  if (nrow(df) == 0) {
    stop("`df` must have at least one row")
  }
  
  if (!coords %in% names(df)) {
    stop("Column '", coords, "' not found in data frame. Available columns: ", 
         paste(names(df), collapse = ", "))
  }
  
  # Check if coordinates column contains the separator
  coords_sample <- df[[coords]][!is.na(df[[coords]])]
  if (length(coords_sample) == 0) {
    stop("No non-missing values found in coordinates column")
  }
  
  has_sep <- any(grepl(sep, coords_sample, fixed = TRUE))
  if (!has_sep) {
    # Show sample values to help diagnose the issue
    sample_vals <- head(coords_sample, 3)
    stop("Separator '", sep, "' not found in coordinates column.\n",
         "Sample values: ", paste(sample_vals, collapse = ", "), "\n",
         "Check your separator or coordinate format.")
  }
  
  # Validate CRS
  if (!is.numeric(crs) && !is.character(crs)) {
    stop("`crs` must be numeric EPSG code or character CRS string")
  }
  
  # Separate coordinates with error handling
  result <- tryCatch({
    df %>%
      tidyr::separate(.data[[coords]], 
                     into = c(y, x),  # Note: y first (lat, lon convention)
                     sep = sep, 
                     convert = TRUE,
                     remove = remove,
                     extra = "drop",    # Handle extra separators gracefully
                     fill = "right")    # Handle missing values
  }, error = function(e) {
    stop("Failed to separate coordinates. Error: ", e$message, "\n",
         "Check that your data uses the separator '", sep, "' between coordinates.")
  })
  
  # Check for successful coordinate conversion
  if (!x %in% names(result) || !y %in% names(result)) {
    stop("Failed to create coordinate columns. Check your separator and data format.")
  }
  
  # Validate coordinate values
  x_vals <- result[[x]]
  y_vals <- result[[y]]
  
  if (all(is.na(x_vals)) || all(is.na(y_vals))) {
    stop("All coordinate values are missing after conversion. Check your data format.")
  }
  
  # Check for reasonable coordinate ranges (assuming geographic coordinates)
  if (is.numeric(crs) && crs == 4326) {
    if (any(abs(y_vals) > 90, na.rm = TRUE)) {
      warning("Some latitude values are outside valid range [-90, 90]. ",
              "Check coordinate order - should be 'latitude, longitude'.")
    }
    if (any(abs(x_vals) > 180, na.rm = TRUE)) {
      warning("Some longitude values are outside valid range [-180, 180]. ",
              "Check coordinate format and values.")
    }
  }
  
  # Convert to sf object
  sf_result <- tryCatch({
    sf::st_as_sf(result, coords = c(x, y), crs = crs)
  }, error = function(e) {
    stop("Failed to create sf object. Error: ", e$message, "\n",
         "Check coordinate values and CRS specification.")
  })
  
  return(sf_result)
}

#' Extract coordinates from Simple Features (sf) object
#'
#' This function extracts coordinate values from an sf object and returns them
#' as separate columns or as a combined coordinates column.
#'
#' @param sf_data A Simple Features (sf) object with point geometries.
#' @param format Output format. Either "separate" (x and y columns) or "combined" (single coords column).
#' @param x_name Name for the x coordinate column (longitude).
#' @param y_name Name for the y coordinate column (latitude).
#' @param coords_name Name for combined coordinates column.
#' @param sep Separator for combined coordinates (when format = "combined").
#' @param digits Number of decimal places for coordinates.
#' @param drop_geometry Logical; should the geometry column be dropped?
#'
#' @return Data frame with extracted coordinates.
#'
#' @examples
#' # Create sample sf data
#' data <- data.frame(
#'   name = c("Paris", "London"),
#'   coords = c("48.8566, 2.3522", "51.5074, -0.1278")
#' )
#' sf_data <- euiss_coords_to_sf(data, coords = "coords")
#' 
#' # Extract as separate columns
#' coords_separate <- euiss_sf_to_coords(sf_data, format = "separate")
#' 
#' # Extract as combined column
#' coords_combined <- euiss_sf_to_coords(sf_data, format = "combined", sep = " | ")
#'
#' @import sf
#' @import dplyr
#'
#' @export
euiss_sf_to_coords <- function(sf_data,
                              format = "separate",
                              x_name = "x",
                              y_name = "y", 
                              coords_name = "coords",
                              sep = ", ",
                              digits = 6,
                              drop_geometry = TRUE) {
  
  # Input validation
  if (!inherits(sf_data, "sf")) {
    stop("`sf_data` must be a Simple Features (sf) object")
  }
  
  if (nrow(sf_data) == 0) {
    stop("`sf_data` must have at least one row")
  }
  
  if (!format %in% c("separate", "combined")) {
    stop("`format` must be either 'separate' or 'combined'")
  }
  
  # Check geometry type
  geom_type <- unique(sf::st_geometry_type(sf_data))
  if (length(geom_type) > 1 || !geom_type %in% c("POINT", "MULTIPOINT")) {
    warning("Function designed for POINT geometries. Results may be unexpected for other geometry types.")
  }
  
  # Extract coordinates
  coords_matrix <- tryCatch({
    sf::st_coordinates(sf_data)
  }, error = function(e) {
    stop("Failed to extract coordinates from sf object: ", e$message)
  })
  
  if (ncol(coords_matrix) < 2) {
    stop("Could not extract X,Y coordinates from sf object")
  }
  
  # Round coordinates
  x_coords <- round(coords_matrix[, 1], digits)
  y_coords <- round(coords_matrix[, 2], digits)
  
  # Start with the original data (without geometry if requested)
  if (drop_geometry) {
    result <- sf::st_drop_geometry(sf_data)
  } else {
    result <- sf_data
  }
  
  # Add coordinates in requested format
  if (format == "separate") {
    result[[x_name]] <- x_coords
    result[[y_name]] <- y_coords
    
  } else if (format == "combined") {
    # Combine as "y, x" (latitude, longitude convention)
    combined_coords <- paste(y_coords, x_coords, sep = sep)
    result[[coords_name]] <- combined_coords
  }
  
  return(result)
}

#' Convert character coordinates to decimal degrees
#'
#' This function converts coordinates from various character formats (DMS, DM, etc.)
#' to decimal degrees. Supports multiple input formats commonly found in geographic data.
#'
#' @param coord_string Character string containing coordinate information.
#' @param format Expected input format. Options: "auto", "dms", "dm", "decimal".
#' @param hemisphere_column Optional column name containing hemisphere information (N/S/E/W).
#' 
#' @return Numeric value in decimal degrees.
#'
#' @details
#' Supported input formats:
#' \itemize{
#'   \item DMS: "48°51'29.5\"N" or "48 51 29.5 N"
#'   \item DM: "48°51.492'N" or "48 51.492 N" 
#'   \item Decimal: "48.8582" or "48.8582°N"
#' }
#'
#' @examples
#' # DMS format
#' euiss_coords_char2dec("48°51'29.5\"N")
#' euiss_coords_char2dec("2°21'7.9\"E")
#' 
#' # DM format  
#' euiss_coords_char2dec("48°51.492'N")
#' 
#' # Decimal format
#' euiss_coords_char2dec("48.8582°N")
#' euiss_coords_char2dec("-2.3522")
#'
#' @import stringr
#'
#' @export
euiss_coords_char2dec <- function(coord_string, format = "auto", hemisphere_column = NULL) {
  
  # Input validation
  if (!is.character(coord_string)) {
    stop("`coord_string` must be a character vector")
  }
  
  if (any(is.na(coord_string))) {
    warning("Missing values detected in coordinate strings")
  }
  
  # Process each coordinate string
  results <- sapply(coord_string, function(coord) {
    
    if (is.na(coord) || coord == "" || is.null(coord)) {
      return(NA_real_)
    }
    
    # Clean the string
    coord_clean <- stringr::str_trim(coord)
    
    # Determine hemisphere (N/S/E/W)
    hemisphere <- ""
    if (stringr::str_detect(coord_clean, "[NSEW]")) {
      hemisphere <- stringr::str_extract(coord_clean, "[NSEW]")
      # Remove hemisphere from string for parsing
      coord_clean <- stringr::str_remove(coord_clean, "[NSEW]")
    }
    
    # Auto-detect format if needed
    if (format == "auto") {
      if (stringr::str_detect(coord_clean, "°.*'.*\"")) {
        detected_format <- "dms"
      } else if (stringr::str_detect(coord_clean, "°.*'")) {
        detected_format <- "dm"
      } else {
        detected_format <- "decimal"
      }
    } else {
      detected_format <- format
    }
    
    # Parse based on detected format
    decimal_value <- tryCatch({
      
      if (detected_format == "dms") {
        # Degrees, Minutes, Seconds: 48°51'29.5"
        parts <- stringr::str_extract_all(coord_clean, "[0-9]+\\.?[0-9]*")[[1]]
        if (length(parts) >= 3) {
          degrees <- as.numeric(parts[1])
          minutes <- as.numeric(parts[2])
          seconds <- as.numeric(parts[3])
          degrees + minutes/60 + seconds/3600
        } else {
          NA_real_
        }
        
      } else if (detected_format == "dm") {
        # Degrees, Decimal Minutes: 48°51.492'
        parts <- stringr::str_extract_all(coord_clean, "[0-9]+\\.?[0-9]*")[[1]]
        if (length(parts) >= 2) {
          degrees <- as.numeric(parts[1])
          minutes <- as.numeric(parts[2])
          degrees + minutes/60
        } else {
          NA_real_
        }
        
      } else {
        # Decimal degrees: 48.8582
        # Extract numeric value, removing any symbols
        numeric_part <- stringr::str_extract(coord_clean, "-?[0-9]+\\.?[0-9]*")
        as.numeric(numeric_part)
      }
      
    }, error = function(e) {
      warning("Failed to parse coordinate: ", coord, " - ", e$message)
      NA_real_
    })
    
    # Apply hemisphere correction
    if (!is.na(decimal_value) && hemisphere %in% c("S", "W")) {
      decimal_value <- -abs(decimal_value)
    }
    
    return(decimal_value)
  })
  
  # Return as numeric vector
  as.numeric(results)
}

#' Batch convert coordinates from data frame
#'
#' Convenience function to convert multiple coordinate columns from character to decimal.
#'
#' @param df Data frame containing coordinate columns.
#' @param lat_col Name of latitude column.
#' @param lon_col Name of longitude column.
#' @param format Expected coordinate format.
#' @param new_lat_col Name for new decimal latitude column.
#' @param new_lon_col Name for new decimal longitude column.
#' @param remove_original Should original columns be removed?
#'
#' @return Data frame with decimal coordinate columns.
#'
#' @examples
#' # Sample data with character coordinates
#' coord_data <- data.frame(
#'   place = c("Paris", "London"),
#'   lat_dms = c("48°51'29.5\"N", "51°30'26.5\"N"),
#'   lon_dms = c("2°21'7.9\"E", "0°7'39.9\"W")
#' )
#' 
#' # Convert to decimal
#' decimal_coords <- euiss_coords_convert_df(
#'   coord_data, 
#'   lat_col = "lat_dms", 
#'   lon_col = "lon_dms"
#' )
#'
#' @export
euiss_coords_convert_df <- function(df,
                                   lat_col,
                                   lon_col,
                                   format = "auto",
                                   new_lat_col = "latitude",
                                   new_lon_col = "longitude",
                                   remove_original = FALSE) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame")
  }
  
  if (!lat_col %in% names(df)) {
    stop("Latitude column '", lat_col, "' not found in data frame")
  }
  
  if (!lon_col %in% names(df)) {
    stop("Longitude column '", lon_col, "' not found in data frame")
  }
  
  # Convert coordinates
  df[[new_lat_col]] <- euiss_coords_char2dec(df[[lat_col]], format = format)
  df[[new_lon_col]] <- euiss_coords_char2dec(df[[lon_col]], format = format)
  
  # Remove original columns if requested
  if (remove_original) {
    df[[lat_col]] <- NULL
    df[[lon_col]] <- NULL
  }
  
  # Report conversion results
  successful_lat <- sum(!is.na(df[[new_lat_col]]))
  successful_lon <- sum(!is.na(df[[new_lon_col]]))
  total_rows <- nrow(df)
  
  message("Coordinate conversion completed:")
  message("  Latitude: ", successful_lat, "/", total_rows, " successful")
  message("  Longitude: ", successful_lon, "/", total_rows, " successful")
  
  if (successful_lat < total_rows || successful_lon < total_rows) {
    warning("Some coordinates could not be converted. Check input format and data quality.")
  }
  
  return(df)
}

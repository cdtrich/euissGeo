#' Spatial Data Conversion Functions
#' 
#' Functions for converting between different spatial data formats and coordinate systems.
#' 
#' @name spatial-convert
NULL

#' Convert coordinates in a data frame to Simple Features (sf) object
#'
#' Takes a data frame with coordinates as a single column (e.g., "48.8566, 2.3522")
#' and converts it into a Simple Features (sf) object with point geometries.
#'
#' @param df A data frame containing coordinates as a single column.
#' @param coords Name of the coordinates column in the data frame.
#' @param sep Separator used between coordinates (default: ", ").
#' @param x Name for x coordinate (longitude) after separation (default: "x").
#' @param y Name for y coordinate (latitude) after separation (default: "y").
#' @param remove Remove original coordinates column? (default: TRUE)
#' @param crs Coordinate reference system. Default is EPSG:4326 (WGS84).
#'
#' @return Simple Features (sf) object with point geometries.
#'
#' @details
#' The function expects coordinates in "latitude, longitude" format by default.
#' This follows the common convention where latitude (y) is listed first.
#' 
#' Coordinate validation includes:
#' \itemize{
#'   \item Checking for valid numeric values
#'   \item Warning if lat/lon appear outside valid ranges
#'   \item Handling missing values gracefully
#' }
#'
#' @examples
#' # Basic usage
#' cities <- data.frame(
#'   name = c("Paris", "London", "Berlin"),
#'   coords = c("48.8566, 2.3522", "51.5074, -0.1278", "52.5200, 13.4050"),
#'   population = c(2161000, 8982000, 3669000)
#' )
#' 
#' sf_cities <- euiss_coords_to_sf(cities, coords = "coords")
#' 
#' # Custom separator
#' cities2 <- data.frame(
#'   name = "Rome",
#'   position = "41.9028|12.4964"
#' )
#' sf_cities2 <- euiss_coords_to_sf(cities2, coords = "position", sep = "|")
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
    stop("`df` must be a data frame", call. = FALSE)
  }
  
  if (nrow(df) == 0) {
    stop("`df` must have at least one row", call. = FALSE)
  }
  
  if (!coords %in% names(df)) {
    stop(
      "Column '", coords, "' not found in data frame.\n",
      "Available columns: ", paste(names(df), collapse = ", "),
      call. = FALSE
    )
  }
  
  # Validate CRS
  if (!is.numeric(crs) && !is.character(crs)) {
    stop("`crs` must be numeric EPSG code or character CRS string", call. = FALSE)
  }
  
  # Check for separator in data
  coords_sample <- df[[coords]][!is.na(df[[coords]])]
  
  if (length(coords_sample) == 0) {
    stop("No non-missing values found in coordinates column", call. = FALSE)
  }
  
  has_sep <- any(grepl(sep, coords_sample, fixed = TRUE))
  if (!has_sep) {
    stop(
      "Separator '", sep, "' not found in coordinates column.\n",
      "Sample values: ", paste(head(coords_sample, 3), collapse = " | "), "\n",
      "Check your separator or coordinate format.",
      call. = FALSE
    )
  }
  
  # Split coordinates using fixed string matching (avoids regex issues)
  result <- tryCatch({
    coords_split <- strsplit(df[[coords]], sep, fixed = TRUE)
    
    # Convert to coordinate columns
    coords_matrix <- do.call(rbind, lapply(coords_split, function(x) {
      if (length(x) >= 2) {
        c(as.numeric(trimws(x[1])), as.numeric(trimws(x[2])))
      } else {
        c(NA_real_, NA_real_)
      }
    }))
    
    # Create result data frame
    result_df <- df
    result_df[[y]] <- coords_matrix[, 1]
    result_df[[x]] <- coords_matrix[, 2]
    
    # Remove original coords column if requested
    if (remove) {
      result_df[[coords]] <- NULL
    }
    
    result_df
    
  }, error = function(e) {
    stop(
      "Failed to separate coordinates.\n",
      "Error: ", e$message, "\n",
      "Check that your data uses separator '", sep, "' correctly.",
      call. = FALSE
    )
  })
  
  # Validate that coordinate columns were created
  if (!x %in% names(result) || !y %in% names(result)) {
    stop(
      "Failed to create coordinate columns '", x, "' and '", y, "'.\n",
      "Check your separator and data format.",
      call. = FALSE
    )
  }
  
  # Validate coordinate values
  x_vals <- result[[x]]
  y_vals <- result[[y]]
  
  if (all(is.na(x_vals)) || all(is.na(y_vals))) {
    stop(
      "All coordinate values are missing after conversion.\n",
      "Check your data format and separator.",
      call. = FALSE
    )
  }
  
  # Warn about out-of-range coordinates (for geographic CRS)
  if ((is.numeric(crs) && crs == 4326) || 
      (is.character(crs) && grepl("4326", crs))) {
    
    if (any(abs(y_vals) > 90, na.rm = TRUE)) {
      warning(
        "Some latitude values outside valid range [-90, 90].\n",
        "Check coordinate order - expecting 'latitude, longitude'.",
        call. = FALSE
      )
    }
    
    if (any(abs(x_vals) > 180, na.rm = TRUE)) {
      warning(
        "Some longitude values outside valid range [-180, 180].\n",
        "Check coordinate format and values.",
        call. = FALSE
      )
    }
  }
  
  # Convert to sf object
  sf_result <- tryCatch({
    result %>%
      dplyr::filter(!is.na(!!rlang::sym(x)), !is.na(!!rlang::sym(y))) %>%
      sf::st_as_sf(coords = c(x, y), crs = crs, remove = TRUE)
  }, error = function(e) {
    stop(
      "Failed to create sf object.\n",
      "Error: ", e$message,
      call. = FALSE
    )
  })
  
  # Report if rows were dropped
  n_dropped <- nrow(result) - nrow(sf_result)
  if (n_dropped > 0) {
    warning(
      n_dropped, " row(s) with missing coordinates were excluded",
      call. = FALSE
    )
  }
  
  sf_result
}

#' Extract coordinates from Simple Features (sf) object
#'
#' Extracts coordinate values from sf object and returns them as separate
#' columns or as a combined coordinates column.
#'
#' @param sf_data Simple Features (sf) object with point geometries.
#' @param format Output format: "separate" (x and y columns) or "combined" (single column).
#' @param x_name Name for x coordinate column (longitude) (default: "lon").
#' @param y_name Name for y coordinate column (latitude) (default: "lat").
#' @param coords_name Name for combined coordinates column (default: "coords").
#' @param sep Separator for combined coordinates (default: ", ").
#' @param digits Number of decimal places for coordinates (default: 6).
#' @param drop_geometry Drop geometry column from output? (default: TRUE)
#'
#' @return Data frame with extracted coordinates.
#'
#' @examples
#' # Create sample sf data
#' cities <- data.frame(
#'   name = c("Paris", "London"),
#'   coords = c("48.8566, 2.3522", "51.5074, -0.1278")
#' )
#' sf_cities <- euiss_coords_to_sf(cities, coords = "coords")
#' 
#' # Extract as separate columns
#' coords_separate <- euiss_sf_to_coords(sf_cities, format = "separate")
#' 
#' # Extract as combined column
#' coords_combined <- euiss_sf_to_coords(sf_cities, format = "combined")
#'
#' @import sf
#' @import dplyr
#'
#' @export
euiss_sf_to_coords <- function(sf_data,
                               format = "separate",
                               x_name = "lon",
                               y_name = "lat",
                               coords_name = "coords",
                               sep = ", ",
                               digits = 6,
                               drop_geometry = TRUE) {
  
  # Input validation
  if (!inherits(sf_data, "sf")) {
    stop("`sf_data` must be a Simple Features (sf) object", call. = FALSE)
  }
  
  if (nrow(sf_data) == 0) {
    stop("`sf_data` must have at least one row", call. = FALSE)
  }
  
  if (!format %in% c("separate", "combined")) {
    stop("`format` must be either 'separate' or 'combined'", call. = FALSE)
  }
  
  # Check geometry type
  geom_types <- unique(sf::st_geometry_type(sf_data))
  if (!all(geom_types %in% c("POINT", "MULTIPOINT"))) {
    warning(
      "Function designed for POINT geometries. ",
      "Found: ", paste(geom_types, collapse = ", "), ". ",
      "Results may be unexpected.",
      call. = FALSE
    )
  }
  
  # Extract coordinates
  coords_matrix <- tryCatch({
    sf::st_coordinates(sf_data)
  }, error = function(e) {
    stop("Failed to extract coordinates: ", e$message, call. = FALSE)
  })
  
  if (ncol(coords_matrix) < 2) {
    stop("Could not extract X,Y coordinates from sf object", call. = FALSE)
  }
  
  # Round coordinates
  x_coords <- round(coords_matrix[, 1], digits)
  y_coords <- round(coords_matrix[, 2], digits)
  
  # Start with original data
  if (drop_geometry) {
    result <- sf::st_drop_geometry(sf_data)
  } else {
    result <- as.data.frame(sf_data)
  }
  
  # Add coordinates in requested format
  if (format == "separate") {
    result[[x_name]] <- x_coords
    result[[y_name]] <- y_coords
    
  } else if (format == "combined") {
    # Combine as "y, x" (latitude, longitude convention)
    result[[coords_name]] <- paste(y_coords, x_coords, sep = sep)
  }
  
  result
}

#' Convert character coordinates to decimal degrees
#'
#' Converts coordinates from various character formats (DMS, DM, decimal)
#' to decimal degrees. Supports multiple common geographic coordinate formats.
#'
#' @param coord_string Character vector of coordinate strings.
#' @param format Expected input format: "auto", "dms", "dm", or "decimal".
#' 
#' @return Numeric vector in decimal degrees.
#'
#' @details
#' Supported input formats:
#' \itemize{
#'   \item DMS (Degrees Minutes Seconds): "48°51'29.5\"N" or "48 51 29.5 N"
#'   \item DM (Degrees Decimal Minutes): "48°51.492'N" or "48 51.492 N"
#'   \item Decimal: "48.8582" or "48.8582°N"
#' }
#' 
#' Hemisphere indicators (N/S/E/W) automatically determine sign.
#' South and West are negative.
#'
#' @examples
#' # DMS format
#' euiss_coords_char2dec("48°51'29.5\"N")  # Returns 48.85819
#' euiss_coords_char2dec("2°21'7.9\"E")    # Returns 2.35219
#' 
#' # DM format
#' euiss_coords_char2dec("48°51.492'N")    # Returns 48.8582
#' 
#' # Decimal format
#' euiss_coords_char2dec("48.8582°N")      # Returns 48.8582
#' euiss_coords_char2dec("-2.3522")        # Returns -2.3522
#'  
#' # Auto-detect format
#' coords <- c("48°51'29.5\"N", "48°51.492'N", "48.8582")
#' euiss_coords_char2dec(coords, format = "auto")
#'
#' @import stringr
#'
#' @export
euiss_coords_char2dec <- function(coord_string, format = "auto") {
  
  # Input validation
  if (!is.character(coord_string)) {
    stop("`coord_string` must be a character vector", call. = FALSE)
  }
  
  # Process each coordinate
  vapply(coord_string, .parse_single_coord, numeric(1), format = format)
}

#' Parse single coordinate string
#' @keywords internal
#' @noRd
.parse_single_coord <- function(coord, format) {
  
  if (is.na(coord) || coord == "" || is.null(coord)) {
    return(NA_real_)
  }
  
  # Clean string
  coord_clean <- stringr::str_trim(coord)
  
  # Extract hemisphere
  hemisphere <- ""
  if (stringr::str_detect(coord_clean, "[NSEW]")) {
    hemisphere <- stringr::str_extract(coord_clean, "[NSEW]")
    coord_clean <- stringr::str_remove(coord_clean, "[NSEW]")
    coord_clean <- stringr::str_trim(coord_clean)
  }
  
  # Auto-detect format
  if (format == "auto") {
    format <- .detect_coord_format(coord_clean)
  }
  
  # Parse based on format
  decimal_value <- tryCatch({
    switch(format,
           "dms" = .parse_dms(coord_clean),
           "dm" = .parse_dm(coord_clean),
           "decimal" = .parse_decimal(coord_clean),
           NA_real_
    )
  }, error = function(e) {
    warning("Failed to parse coordinate: ", coord, call. = FALSE)
    NA_real_
  })
  
  # Apply hemisphere correction
  if (!is.na(decimal_value) && hemisphere %in% c("S", "W")) {
    decimal_value <- -abs(decimal_value)
  }
  
  decimal_value
}

#' Detect coordinate format
#' @keywords internal
#' @noRd
.detect_coord_format <- function(coord_clean) {
  
  # Check for degree/minute/second symbols
  has_deg <- stringr::str_detect(coord_clean, "\u00B0|\u00B0")  # degree symbol
  has_min <- stringr::str_detect(coord_clean, "'")         # minute symbol
  has_sec <- stringr::str_detect(coord_clean, '"')         # second symbol
  
  if (has_deg && has_min && has_sec) {
    return("dms")
  } else if (has_deg && has_min) {
    return("dm")
  } else {
    return("decimal")
  }
}

#' Parse DMS (Degrees Minutes Seconds) format
#' @keywords internal
#' @noRd
.parse_dms <- function(coord_clean) {
  
  # Extract all numeric parts
  parts <- stringr::str_extract_all(coord_clean, "[0-9]+\\.?[0-9]*")[[1]]
  
  if (length(parts) < 3) {
    return(NA_real_)
  }
  
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- as.numeric(parts[3])
  
  degrees + minutes/60 + seconds/3600
}

#' Parse DM (Degrees Decimal Minutes) format
#' @keywords internal
#' @noRd
.parse_dm <- function(coord_clean) {
  
  # Extract all numeric parts
  parts <- stringr::str_extract_all(coord_clean, "[0-9]+\\.?[0-9]*")[[1]]
  
  if (length(parts) < 2) {
    return(NA_real_)
  }
  
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  
  degrees + minutes/60
}

#' Parse decimal format
#' @keywords internal
#' @noRd
.parse_decimal <- function(coord_clean) {
  
  # Extract numeric value (including negative sign)
  numeric_part <- stringr::str_extract(coord_clean, "-?[0-9]+\\.?[0-9]*")
  
  if (is.na(numeric_part)) {
    return(NA_real_)
  }
  
  as.numeric(numeric_part)
}

#' Batch convert coordinates from data frame
#'
#' Convenience function to convert multiple coordinate columns from
#' character format to decimal degrees.
#'
#' @param df Data frame containing coordinate columns.
#' @param lat_col Name of latitude column.
#' @param lon_col Name of longitude column.
#' @param format Expected coordinate format (default: "auto").
#' @param new_lat_col Name for new decimal latitude column (default: "latitude").
#' @param new_lon_col Name for new decimal longitude column (default: "longitude").
#' @param remove_original Remove original columns? (default: FALSE)
#'
#' @return Data frame with decimal coordinate columns added.
#'
#' @examples
#' # Sample data with DMS coordinates
#' cities <- data.frame(
#'   place = c("Paris", "London"),
#'   lat_dms = c("48°51'29.5\"N", "51°30'26.5\"N"),
#'   lon_dms = c("2°21'7.9\"E", "0°7'39.9\"W")
#' )
#' 
#' # Convert to decimal
#' cities_decimal <- euiss_coords_convert_df(
#'   cities,
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
    stop("`df` must be a data frame", call. = FALSE)
  }
  
  if (!lat_col %in% names(df)) {
    stop("Latitude column '", lat_col, "' not found in data frame", call. = FALSE)
  }
  
  if (!lon_col %in% names(df)) {
    stop("Longitude column '", lon_col, "' not found in data frame", call. = FALSE)
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
  n_total <- nrow(df)
  n_lat_success <- sum(!is.na(df[[new_lat_col]]))
  n_lon_success <- sum(!is.na(df[[new_lon_col]]))
  
  message("Coordinate conversion completed:")
  message("  Latitude:  ", n_lat_success, "/", n_total, " successful")
  message("  Longitude: ", n_lon_success, "/", n_total, " successful")
  
  if (n_lat_success < n_total || n_lon_success < n_total) {
    warning(
      "Some coordinates could not be converted. ",
      "Check input format and data quality.",
      call. = FALSE
    )
  }
  
  df
}
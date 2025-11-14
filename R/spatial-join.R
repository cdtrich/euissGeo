#' Spatial Joining Functions
#' 
#' Functions for joining spatial and non-spatial data with enhanced country code handling.
#' 
#' @name spatial-join
NULL

#' Left join a data frame with a Simple Features (sf) object using country codes
#'
#' This function performs a left join between a data frame and a Simple Features (sf) object
#' based on country codes. It uses the 'countrycode' package to convert country names
#' to ISO3 codes before joining, with special handling for Kosovo and other edge cases.
#'
#' @param df A data frame containing country information.
#' @param sf A Simple Features (sf) object with country codes as one of the columns.
#' @param country The name of the column in the data frame containing country names.
#' @param by A character vector specifying the columns to join on, with format c("df_column" = "sf_column").
#' @param country_format Format for country matching. Options: "auto", "iso3c", "iso2c", "country.name".
#' @param handle_kosovo Logical. Should Kosovo be handled specially? Default TRUE.
#' @param kosovo_code ISO3 code to use for Kosovo. Default "KOS".
#' @param verbose Logical. Print information about country matching? Default FALSE.
#' @param ... Additional arguments passed to 'left_join' function from 'dplyr'.
#'
#' @return A data frame with the result of the left join operation.
#'
#' @details
#' This function enhances standard spatial joins by automatically handling country name
#' standardization. It converts various country name formats to ISO codes for reliable
#' matching with spatial data.
#' 
#' Special cases handled:
#' \itemize{
#'   \item Kosovo (assigns custom ISO3 code)
#'   \item Country name variations (uses countrycode package)
#'   \item Missing country matches (reports unmatched countries)
#' }
#'
#' @examples
#' \dontrun{
#' # Example data frame with country information
#' df <- data.frame(
#'   country = c("United States", "Canada", "Germany", "Kosovo"),
#'   value = c(100, 200, 150, 75)
#' )
#' 
#' # Load GISCO countries data
#' countries_sf <- euiss_gisco(res = "20")
#' 
#' # Perform spatial join
#' result <- euiss_left_join(df, countries_sf, country = "country")
#' 
#' # Custom join column names
#' result <- euiss_left_join(
#'   df, countries_sf, 
#'   country = "country",
#'   by = c("iso3c" = "ISO3_CODE")
#' )
#' }
#'
#' @import countrycode
#' @import dplyr
#' @import sf
#'
#' @export
euiss_left_join <- function(df,
                           sf,
                           country = "country",
                           by = c("iso3c" = "ISO3_CODE"),
                           country_format = "auto",
                           handle_kosovo = TRUE,
                           kosovo_code = "KOS",
                           verbose = FALSE,
                           ...) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame")
  }
  
  if (!inherits(sf, "sf")) {
    stop("`sf` must be a Simple Features (sf) object")
  }
  
  if (!country %in% names(df)) {
    stop("Column '", country, "' not found in df. Available columns: ", 
         paste(names(df), collapse = ", "))
  }
  
  if (nrow(df) == 0) {
    warning("Input data frame is empty")
    return(df)
  }
  
  # Check if countrycode package is available
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop("countrycode package is required. Install with: install.packages('countrycode')")
  }
  
  # Extract country names
  country_names <- df[[country]]
  unique_countries <- unique(country_names[!is.na(country_names)])
  
  if (verbose) {
    message("Processing ", length(unique_countries), " unique countries")
  }
  
  # Auto-detect country format if needed
  if (country_format == "auto") {
    # Sample some country names to detect format
    sample_countries <- head(unique_countries, 5)
    
    # Check if they look like ISO codes
    if (all(nchar(sample_countries) == 3) && all(grepl("^[A-Z]{3}$", sample_countries))) {
      country_format <- "iso3c"
    } else if (all(nchar(sample_countries) == 2) && all(grepl("^[A-Z]{2}$", sample_countries))) {
      country_format <- "iso2c"
    } else {
      country_format <- "country.name"
    }
    
    if (verbose) {
      message("Auto-detected country format: ", country_format)
    }
  }
  
  # Convert countries to ISO3 codes
  if (country_format == "iso3c") {
    # Already in ISO3 format
    iso3_codes <- country_names
  } else {
    # Convert to ISO3
    iso3_codes <- tryCatch({
      countrycode::countrycode(
        country_names,
        origin = country_format,
        destination = "iso3c",
        warn = verbose
      )
    }, error = function(e) {
      stop("Country code conversion failed: ", e$message, "\n",
           "Check country_format parameter and country names in your data.")
    })
  }
  
  # Handle Kosovo specifically
  if (handle_kosovo) {
    kosovo_matches <- grepl("kosovo", country_names, ignore.case = TRUE)
    if (any(kosovo_matches)) {
      iso3_codes[kosovo_matches] <- kosovo_code
      if (verbose) {
        message("Assigned code '", kosovo_code, "' to ", sum(kosovo_matches), " Kosovo entries")
      }
    }
  }
  
  # Create working data frame with ISO codes
  df_with_iso <- df %>%
    dplyr::mutate(iso3c = iso3_codes)
  
  # Check for unmatched countries
  unmatched <- is.na(iso3_codes)
  if (any(unmatched)) {
    unmatched_countries <- unique(country_names[unmatched])
    if (verbose) {
      message("Warning: Could not match ", length(unmatched_countries), " countries:")
      message("  ", paste(unmatched_countries, collapse = ", "))
    } else {
      warning("Could not match ", length(unmatched_countries), " countries. Use verbose=TRUE for details.")
    }
  }
  
  # Check join key exists in sf data
  sf_join_col <- names(by)[1]  # Right side of join (sf column)
  df_join_col <- by[1]         # Left side of join (df column)
  
  if (!sf_join_col %in% names(sf)) {
    stop("Join column '", sf_join_col, "' not found in sf object. Available columns: ",
         paste(names(sf), collapse = ", "))
  }
  
  # Perform the join
  result <- tryCatch({
    df_with_iso %>%
      dplyr::left_join(sf, by = by, ...)
  }, error = function(e) {
    stop("Join operation failed: ", e$message)
  })
  
  # Report join results
  if (verbose) {
    successful_joins <- sum(!is.na(result$geometry))
    total_rows <- nrow(result)
    message("Join completed: ", successful_joins, "/", total_rows, " rows matched with spatial data")
    
    if (successful_joins < total_rows) {
      # Identify which countries didn't match
      unjoined_countries <- result %>%
        dplyr::filter(is.na(.data$geometry)) %>%
        dplyr::pull(!!rlang::sym(country)) %>%
        unique()
      message("Countries without spatial match: ", paste(unjoined_countries, collapse = ", "))
    }
  }
  
  return(result)
}

#' Enhanced spatial join with multiple fallback strategies
#'
#' This function performs spatial joins with multiple fallback strategies for
#' better matching success rates. It tries exact matches first, then fuzzy matching,
#' and finally spatial intersection for point data.
#'
#' @param df Data frame with country/location information.
#' @param sf Spatial features object for joining.
#' @param location_col Column name in df containing location information.
#' @param sf_name_col Column name in sf containing names for matching.
#' @param sf_code_col Column name in sf containing codes for matching.
#' @param strategies Vector of strategies to try: "exact", "code", "fuzzy", "spatial".
#' @param fuzzy_threshold Threshold for fuzzy string matching (0-1).
#' @param coords_cols For spatial strategy: c("longitude", "latitude") column names in df.
#' @param verbose Logical. Print detailed matching information?
#'
#' @return Data frame with joined spatial information and matching metadata.
#'
#' @examples
#' \dontrun{
#' # Data with various country name formats
#' messy_data <- data.frame(
#'   location = c("USA", "United Kingdom", "Deutschland", "Espana"),
#'   value = c(100, 200, 150, 175)
#' )
#' 
#' countries_sf <- euiss_gisco(res = "20")
#' 
#' # Try multiple matching strategies
#' result <- euiss_enhanced_join(
#'   messy_data, countries_sf,
#'   location_col = "location",
#'   strategies = c("exact", "code", "fuzzy")
#' )
#' }
#'
#' @import dplyr
#' @import sf
#' @importFrom utils adist
#'
#' @export
euiss_enhanced_join <- function(df,
                               sf,
                               location_col = "location",
                               sf_name_col = "NAME_ENGL",
                               sf_code_col = "ISO3_CODE",
                               strategies = c("exact", "code", "fuzzy", "spatial"),
                               fuzzy_threshold = 0.8,
                               coords_cols = c("longitude", "latitude"),
                               verbose = FALSE) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame")
  }
  
  if (!inherits(sf, "sf")) {
    stop("`sf` must be a Simple Features (sf) object")
  }
  
  if (!location_col %in% names(df)) {
    stop("Column '", location_col, "' not found in df")
  }
  
  # Initialize result with matching metadata
  result <- df %>%
    dplyr::mutate(
      .match_strategy = NA_character_,
      .match_confidence = NA_real_,
      .original_location = .data[[location_col]]
    )
  
  # Track unmatched rows
  unmatched_rows <- seq_len(nrow(result))
  
  for (strategy in strategies) {
    
    if (length(unmatched_rows) == 0) break  # All matched
    
    if (verbose) {
      message("Trying strategy: ", strategy, " for ", length(unmatched_rows), " unmatched rows")
    }
    
    strategy_result <- NULL
    
    if (strategy == "exact") {
      # Exact name matching
      strategy_result <- euiss_exact_match(
        result[unmatched_rows, ], sf, location_col, sf_name_col
      )
      
    } else if (strategy == "code") {
      # ISO code matching via countrycode
      strategy_result <- euiss_code_match(
        result[unmatched_rows, ], sf, location_col, sf_code_col, verbose
      )
      
    } else if (strategy == "fuzzy") {
      # Fuzzy string matching
      strategy_result <- euiss_fuzzy_match(
        result[unmatched_rows, ], sf, location_col, sf_name_col, fuzzy_threshold
      )
      
    } else if (strategy == "spatial") {
      # Spatial intersection (for point data)
      if (all(coords_cols %in% names(result))) {
        strategy_result <- euiss_spatial_match(
          result[unmatched_rows, ], sf, coords_cols
        )
      } else if (verbose) {
        message("Skipping spatial strategy: coordinate columns not found")
      }
    }
    
    # Update result with successful matches
    if (!is.null(strategy_result) && nrow(strategy_result) > 0) {
      matched_indices <- which(!is.na(strategy_result$geometry))
      
      if (length(matched_indices) > 0) {
        # Update the main result
        result_indices <- unmatched_rows[matched_indices]
        result[result_indices, ] <- strategy_result[matched_indices, ]
        
        # Remove successfully matched rows from unmatched list
        unmatched_rows <- unmatched_rows[-matched_indices]
        
        if (verbose) {
          message("  Successfully matched ", length(matched_indices), " rows")
        }
      }
    }
  }
  
  # Final summary
  if (verbose) {
    total_matched <- sum(!is.na(result$geometry))
    total_rows <- nrow(result)
    message("\nFinal results: ", total_matched, "/", total_rows, " rows matched")
    
    # Strategy breakdown
    strategy_summary <- result %>%
      dplyr::filter(!is.na(.data$.match_strategy)) %>%
      dplyr::count(.data$.match_strategy, name = "count")
    
    if (nrow(strategy_summary) > 0) {
      message("Strategy breakdown:")
      for (i in seq_len(nrow(strategy_summary))) {
        message("  ", strategy_summary$.match_strategy[i], ": ", strategy_summary$count[i])
      }
    }
  }
  
  return(result)
}

#' Exact name matching helper
#' @keywords internal
#' @export
euiss_exact_match <- function(df, sf, location_col, sf_name_col) {
  
  join_by <- setNames(sf_name_col, location_col)
  
  result <- df %>%
    dplyr::left_join(sf, by = join_by, suffix = c("", ".sf")) %>%
    dplyr::mutate(
      .match_strategy = ifelse(!is.na(.data$geometry), "exact", .data$.match_strategy),
      .match_confidence = ifelse(!is.na(.data$geometry), 1.0, .data$.match_confidence)
    )
  
  return(result)
}

#' Country code matching helper  
#' @keywords internal
#' @export
euiss_code_match <- function(df, sf, location_col, sf_code_col, verbose) {
  
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    if (verbose) message("  countrycode package not available, skipping code matching")
    return(df)
  }
  
  # Convert location names to ISO3 codes
  iso3_codes <- tryCatch({
    countrycode::countrycode(
      df[[location_col]],
      origin = "country.name",
      destination = "iso3c",
      warn = FALSE
    )
  }, error = function(e) {
    if (verbose) message("  Country code conversion failed: ", e$message)
    return(rep(NA, nrow(df)))
  })
  
  # Handle Kosovo
  kosovo_matches <- grepl("kosovo", df[[location_col]], ignore.case = TRUE)
  iso3_codes[kosovo_matches] <- "KOS"
  
  df_with_codes <- df %>%
    dplyr::mutate(.temp_iso3 = iso3_codes)
  
  join_by <- setNames(sf_code_col, ".temp_iso3")
  
  result <- df_with_codes %>%
    dplyr::left_join(sf, by = join_by, suffix = c("", ".sf")) %>%
    dplyr::select(-.data$.temp_iso3) %>%
    dplyr::mutate(
      .match_strategy = ifelse(!is.na(.data$geometry) & is.na(.data$.match_strategy), 
                              "code", .data$.match_strategy),
      .match_confidence = ifelse(!is.na(.data$geometry) & is.na(.data$.match_confidence), 
                                0.9, .data$.match_confidence)
    )
  
  return(result)
}

#' Fuzzy string matching helper
#' @keywords internal  
#' @export
euiss_fuzzy_match <- function(df, sf, location_col, sf_name_col, threshold) {
  
  # For unmatched rows, try fuzzy matching
  unmatched_df <- df %>%
    dplyr::filter(is.na(.data$geometry) | is.na(.data$.match_strategy))
  
  if (nrow(unmatched_df) == 0) return(df)
  
  sf_names <- sf[[sf_name_col]]
  
  for (i in seq_len(nrow(unmatched_df))) {
    location <- unmatched_df[[location_col]][i]
    
    if (is.na(location) || location == "") next
    
    # Calculate string distances
    distances <- utils::adist(location, sf_names, ignore.case = TRUE)[1, ]
    max_len <- pmax(nchar(location), nchar(sf_names))
    similarities <- 1 - (distances / max_len)
    
    best_match_idx <- which.max(similarities)
    best_similarity <- similarities[best_match_idx]
    
    if (best_similarity >= threshold) {
      # Found a good fuzzy match
      matched_row <- sf[best_match_idx, ]
      
      # Update the result
      row_idx <- which(df[[location_col]] == location & 
                      (is.na(df$.match_strategy) | is.na(df$geometry)))[1]
      
      if (!is.na(row_idx)) {
        # Copy geometry and other sf columns
        for (col in names(matched_row)) {
          if (col != "geometry") {
            df[row_idx, col] <- matched_row[[col]]
          }
        }
        df[row_idx, "geometry"] <- matched_row$geometry
        df[row_idx, ".match_strategy"] <- "fuzzy"
        df[row_idx, ".match_confidence"] <- round(best_similarity, 3)
      }
    }
  }
  
  return(df)
}

#' Spatial intersection matching helper
#' @keywords internal
#' @export
euiss_spatial_match <- function(df, sf, coords_cols) {
  
  # Convert coordinates to sf points
  coord_data <- df %>%
    dplyr::filter(!is.na(.data[[coords_cols[1]]]), !is.na(.data[[coords_cols[2]]])) %>%
    sf::st_as_sf(coords = coords_cols, crs = 4326)
  
  if (nrow(coord_data) == 0) return(df)
  
  # Transform to same CRS as sf object
  sf_crs <- sf::st_crs(sf)
  coord_data_transformed <- sf::st_transform(coord_data, sf_crs)
  
  # Spatial intersection
  intersections <- sf::st_intersects(coord_data_transformed, sf)
  
  for (i in seq_along(intersections)) {
    if (length(intersections[[i]]) > 0) {
      # Take first intersection
      matched_idx <- intersections[[i]][1]
      matched_row <- sf[matched_idx, ]
      
      # Find corresponding row in original data
      orig_row_idx <- which(df[[coords_cols[1]]] == coord_data[[coords_cols[1]]][i] &
                           df[[coords_cols[2]]] == coord_data[[coords_cols[2]]][i] &
                           (is.na(df$.match_strategy) | is.na(df$geometry)))[1]
      
      if (!is.na(orig_row_idx)) {
        # Update with spatial match
        for (col in names(matched_row)) {
          if (col != "geometry") {
            df[orig_row_idx, col] <- matched_row[[col]]
          }
        }
        df[orig_row_idx, "geometry"] <- matched_row$geometry  
        df[orig_row_idx, ".match_strategy"] <- "spatial"
        df[orig_row_idx, ".match_confidence"] <- 1.0
      }
    }
  }
  
  return(df)
}

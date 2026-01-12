#' Left join data frame with sf object using country codes
#'
#' Performs a left join between a data frame and an sf object based on country codes.
#' Uses countrycode package to convert country names to ISO3 codes before joining,
#' with special handling for Kosovo and other edge cases.
#'
#' @param df Data frame containing country information.
#' @param sf Simple Features (sf) object with country codes.
#' @param country Name of the column in df containing country names.
#' @param by Join specification: c("df_column" = "sf_column").
#' @param country_format Format for country matching: "auto", "iso3c", "iso2c", "country.name".
#' @param handle_kosovo Handle Kosovo specially? (default: TRUE)
#' @param kosovo_code ISO3 code for Kosovo (default: "KOS").
#' @param verbose Print information about country matching? (default: FALSE)
#' @param ... Additional arguments passed to dplyr::left_join().
#'
#' @return Data frame with result of left join operation.
#'
#' @details
#' Enhances standard spatial joins by automatically handling country name standardization.
#' 
#' Special cases:
#' \itemize{
#'   \item Kosovo (assigns custom ISO3 code)
#'   \item Country name variations (uses countrycode package)
#'   \item Missing matches (reports unmatched countries)
#'   \item Factor columns (automatically converted to character)
#' }
#'
#' @examples
#' \dontrun{
#' # Example data
#' df <- data.frame(
#'   country = c("United States", "Canada", "Germany", "Kosovo"),
#'   value = c(100, 200, 150, 75)
#' )
#' 
#' # Load GISCO data
#' countries_sf <- euiss_gisco(res = "20")
#' 
#' # Perform spatial join
#' result <- euiss_left_join(df, countries_sf, country = "country")
#' 
#' # Custom join columns
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
    stop("`df` must be a data frame", call. = FALSE)
  }
  
  if (!inherits(sf, "sf")) {
    stop("`sf` must be a Simple Features (sf) object", call. = FALSE)
  }
  
  if (!country %in% names(df)) {
    stop(
      "Column '", country, "' not found in df.\n",
      "Available columns: ", paste(names(df), collapse = ", "),
      call. = FALSE
    )
  }
  
  if (nrow(df) == 0) {
    warning("Input data frame is empty", call. = FALSE)
    return(df)
  }
  
  # Check countrycode package
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop(
      "countrycode package required.\n",
      "Install with: install.packages('countrycode')",
      call. = FALSE
    )
  }
  
  # Extract country names and convert to character (handles factors)
  country_names <- as.character(df[[country]])
  unique_countries <- unique(country_names[!is.na(country_names) & country_names != ""])
  
  if (length(unique_countries) == 0) {
    stop("No valid country names found in column '", country, "'", call. = FALSE)
  }
  
  if (verbose) {
    message("Processing ", length(unique_countries), " unique countries")
  }
  
  # Auto-detect country format
  if (country_format == "auto") {
    country_format <- .detect_country_format(unique_countries)
    
    if (verbose) {
      message("Auto-detected country format: ", country_format)
    }
  }
  
  # Convert to ISO3 codes
  if (country_format == "iso3c") {
    # Already in ISO3 format
    iso3_codes <- country_names
  } else {
    # Use consolidated standardization function
    iso3_codes <- .standardize_country_codes(
      country_names,
      origin = country_format,
      destination = "iso3c",
      handle_kosovo = handle_kosovo,
      warn = verbose
    )
  }
  
  # Create working data frame with ISO codes
  df_with_iso <- df %>%
    dplyr::mutate(.iso3c_temp = iso3_codes)
  
  # Check for unmatched countries
  unmatched <- is.na(iso3_codes) & !is.na(country_names) & country_names != ""
  if (any(unmatched)) {
    unmatched_countries <- unique(country_names[unmatched])
    
    if (verbose) {
      message(
        "Could not match ", length(unmatched_countries), " countries:\n",
        "  ", paste(unmatched_countries, collapse = ", ")
      )
    } else {
      warning(
        "Could not match ", length(unmatched_countries), " countries. ",
        "Use verbose=TRUE for details.",
        call. = FALSE
      )
    }
  }
  
  # Validate join key exists in sf
  sf_join_col <- names(by)
  if (length(sf_join_col) == 0) {
    sf_join_col <- by[1]
  }
  
  if (!sf_join_col %in% names(sf)) {
    stop(
      "Join column '", sf_join_col, "' not found in sf object.\n",
      "Available columns: ", paste(names(sf), collapse = ", "),
      call. = FALSE
    )
  }
  
  # Perform the join
  join_by <- setNames(".iso3c_temp", sf_join_col)
  
  result <- tryCatch({
    df_with_iso %>%
      dplyr::left_join(sf, by = join_by, ...) %>%
      dplyr::select(-.data$.iso3c_temp)  # Clean up temp column
  }, error = function(e) {
    stop("Join operation failed: ", e$message, call. = FALSE)
  })
  
  # Report join results
  if (verbose) {
    n_matched <- sum(!is.na(result$geometry))
    n_total <- nrow(result)
    message("Join completed: ", n_matched, "/", n_total, " rows matched")
    
    if (n_matched < n_total) {
      unjoined_countries <- result %>%
        dplyr::filter(is.na(.data$geometry)) %>%
        dplyr::pull(!!rlang::sym(country)) %>%
        unique()
      
      message("Countries without spatial match:\n  ", 
              paste(unjoined_countries, collapse = ", "))
    }
  }
  
  result
}

#' Detect country format from sample
#' @keywords internal
#' @noRd
.detect_country_format <- function(countries) {
  
  # Sample some countries
  sample_countries <- head(countries, 5)
  
  # Check if they look like ISO3 codes
  if (all(nchar(sample_countries) == 3) && 
      all(grepl("^[A-Z]{3}$", sample_countries))) {
    return("iso3c")
  }
  
  # Check if they look like ISO2 codes
  if (all(nchar(sample_countries) == 2) && 
      all(grepl("^[A-Z]{2}$", sample_countries))) {
    return("iso2c")
  }
  
  # Default to country names
  "country.name"
}

#' Enhanced spatial join with multiple fallback strategies
#'
#' Performs spatial joins with multiple fallback strategies for better
#' matching success rates. Tries exact matches, then code matching, then
#' fuzzy matching, and optionally spatial intersection.
#'
#' @param df Data frame with country/location information.
#' @param sf Spatial features object for joining.
#' @param location_col Column name in df containing location information.
#' @param sf_name_col Column name in sf containing names for matching.
#' @param sf_code_col Column name in sf containing codes for matching.
#' @param strategies Vector of strategies: "exact", "code", "fuzzy", "spatial".
#' @param fuzzy_threshold Threshold for fuzzy string matching (0-1, default: 0.8).
#' @param coords_cols For spatial strategy: c("longitude", "latitude") in df.
#' @param verbose Print detailed matching information? (default: FALSE)
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
    stop("`df` must be a data frame", call. = FALSE)
  }
  
  if (!inherits(sf, "sf")) {
    stop("`sf` must be a Simple Features (sf) object", call. = FALSE)
  }
  
  if (!location_col %in% names(df)) {
    stop("Column '", location_col, "' not found in df", call. = FALSE)
  }
  
  # Initialize result with matching metadata
  result <- df %>%
    dplyr::mutate(
      .match_strategy = NA_character_,
      .match_confidence = NA_real_,
      .original_location = .data[[location_col]],
      .row_id = dplyr::row_number()
    )
  
  # Track unmatched row IDs
  unmatched_ids <- result$.row_id
  
  # Try each strategy
  for (strategy in strategies) {
    
    if (length(unmatched_ids) == 0) break
    
    if (verbose) {
      message("Strategy '", strategy, "': ", length(unmatched_ids), " unmatched rows")
    }
    
    # Get unmatched subset
    unmatched_df <- result %>%
      dplyr::filter(.data$.row_id %in% unmatched_ids)
    
    # Apply strategy
    strategy_result <- .apply_matching_strategy(
      unmatched_df, sf, strategy, location_col, sf_name_col, 
      sf_code_col, fuzzy_threshold, coords_cols, verbose
    )
    
    # Update matched rows
    if (!is.null(strategy_result)) {
      matched_ids <- strategy_result %>%
        dplyr::filter(!is.na(.data$geometry)) %>%
        dplyr::pull(.data$.row_id)
      
      if (length(matched_ids) > 0) {
        # Update result with matched rows
        result <- result %>%
          dplyr::rows_update(
            strategy_result %>% dplyr::filter(.data$.row_id %in% matched_ids),
            by = ".row_id"
          )
        
        # Remove from unmatched
        unmatched_ids <- setdiff(unmatched_ids, matched_ids)
        
        if (verbose) {
          message("  Matched ", length(matched_ids), " rows")
        }
      }
    }
  }
  
  # Final summary
  if (verbose) {
    n_matched <- sum(!is.na(result$geometry))
    n_total <- nrow(result)
    message("\nFinal: ", n_matched, "/", n_total, " rows matched")
    
    # Strategy breakdown
    if (n_matched > 0) {
      strategy_summary <- result %>%
        dplyr::filter(!is.na(.data$.match_strategy)) %>%
        dplyr::count(.data$.match_strategy, name = "count")
      
      message("By strategy:")
      for (i in seq_len(nrow(strategy_summary))) {
        message("  ", strategy_summary$.match_strategy[i], ": ", 
                strategy_summary$count[i])
      }
    }
  }
  
  # Remove internal row_id
  result %>% dplyr::select(-.data$.row_id)
}

#' Apply a single matching strategy
#' @keywords internal
#' @noRd
.apply_matching_strategy <- function(df, sf, strategy, location_col, sf_name_col,
                                     sf_code_col, fuzzy_threshold, coords_cols, verbose) {
  
  tryCatch({
    switch(strategy,
           "exact" = .match_exact(df, sf, location_col, sf_name_col),
           "code" = .match_code(df, sf, location_col, sf_code_col, verbose),
           "fuzzy" = .match_fuzzy_vectorized(df, sf, location_col, sf_name_col, fuzzy_threshold),
           "spatial" = .match_spatial(df, sf, coords_cols, verbose),
           NULL
    )
  }, error = function(e) {
    if (verbose) {
      message("  Strategy failed: ", e$message)
    }
    NULL
  })
}

#' Exact name matching
#' @keywords internal
#' @noRd
.match_exact <- function(df, sf, location_col, sf_name_col) {
  
  join_by <- setNames(sf_name_col, location_col)
  
  df %>%
    dplyr::left_join(sf, by = join_by, suffix = c("", ".sf")) %>%
    dplyr::mutate(
      .match_strategy = dplyr::if_else(
        !is.na(.data$geometry) & is.na(.data$.match_strategy),
        "exact",
        .data$.match_strategy
      ),
      .match_confidence = dplyr::if_else(
        !is.na(.data$geometry) & is.na(.data$.match_confidence),
        1.0,
        .data$.match_confidence
      )
    )
}

#' Country code matching
#' @keywords internal
#' @noRd
.match_code <- function(df, sf, location_col, sf_code_col, verbose) {
  
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    if (verbose) message("  countrycode package not available")
    return(df)
  }
  
  # Convert location names to ISO3 codes using utility function
  iso3_codes <- .standardize_country_codes(
    df[[location_col]],
    origin = "country.name",
    destination = "iso3c",
    handle_kosovo = TRUE,
    warn = FALSE
  )
  
  df_with_codes <- df %>%
    dplyr::mutate(.temp_iso3 = iso3_codes)
  
  join_by <- setNames(sf_code_col, ".temp_iso3")
  
  df_with_codes %>%
    dplyr::left_join(sf, by = join_by, suffix = c("", ".sf")) %>%
    dplyr::select(-.data$.temp_iso3) %>%
    dplyr::mutate(
      .match_strategy = dplyr::if_else(
        !is.na(.data$geometry) & is.na(.data$.match_strategy),
        "code",
        .data$.match_strategy
      ),
      .match_confidence = dplyr::if_else(
        !is.na(.data$geometry) & is.na(.data$.match_confidence),
        0.9,
        .data$.match_confidence
      )
    )
}

#' Vectorized fuzzy string matching
#' @keywords internal
#' @noRd
.match_fuzzy_vectorized <- function(df, sf, location_col, sf_name_col, threshold) {
  
  # Get unmatched locations
  unmatched <- df %>%
    dplyr::filter(is.na(.data$geometry) | is.na(.data$.match_strategy))
  
  if (nrow(unmatched) == 0) return(df)
  
  # Get unique locations to match
  locations_to_match <- unique(unmatched[[location_col]])
  locations_to_match <- locations_to_match[!is.na(locations_to_match) & locations_to_match != ""]
  
  if (length(locations_to_match) == 0) return(df)
  
  sf_names <- sf[[sf_name_col]]
  
  # Calculate similarity matrix (vectorized)
  dist_matrix <- utils::adist(locations_to_match, sf_names, ignore.case = TRUE)
  
  # Calculate max lengths for normalization
  len_matrix <- outer(
    nchar(locations_to_match),
    nchar(sf_names),
    FUN = pmax
  )
  
  # Similarity scores
  similarity_matrix <- 1 - (dist_matrix / len_matrix)
  
  # Find best matches for each location
  best_match_idx <- apply(similarity_matrix, 1, which.max)
  best_similarity <- apply(similarity_matrix, 1, max)
  
  # Create lookup table for good matches
  match_lookup <- data.frame(
    location = locations_to_match,
    best_match_idx = best_match_idx,
    similarity = best_similarity,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::filter(.data$similarity >= threshold)
  
  if (nrow(match_lookup) == 0) return(df)
  
  # Join matched sf features
  for (i in seq_len(nrow(match_lookup))) {
    loc <- match_lookup$location[i]
    sf_idx <- match_lookup$best_match_idx[i]
    conf <- match_lookup$similarity[i]
    
    matched_sf <- sf[sf_idx, ]
    
    # Update df for this location
    df <- df %>%
      dplyr::rows_update(
        df %>%
          dplyr::filter(.data[[location_col]] == loc, 
                        is.na(.data$.match_strategy)) %>%
          dplyr::select(-.data$geometry) %>%
          dplyr::bind_cols(
            sf::st_drop_geometry(matched_sf)
          ) %>%
          dplyr::mutate(
            geometry = matched_sf$geometry,
            .match_strategy = "fuzzy",
            .match_confidence = round(conf, 3)
          ),
        by = ".row_id",
        unmatched = "ignore"
      )
  }
  
  df
}

#' Spatial intersection matching
#' @keywords internal
#' @noRd
.match_spatial <- function(df, sf, coords_cols, verbose) {
  
  # Check if coordinate columns exist
  if (!all(coords_cols %in% names(df))) {
    if (verbose) {
      message("  Coordinate columns not found: ", paste(coords_cols, collapse = ", "))
    }
    return(df)
  }
  
  # Filter rows with valid coordinates
  coord_data <- df %>%
    dplyr::filter(
      !is.na(.data[[coords_cols[1]]]),
      !is.na(.data[[coords_cols[2]]]),
      is.na(.data$.match_strategy)
    )
  
  if (nrow(coord_data) == 0) return(df)
  
  # Convert to sf points
  coord_sf <- tryCatch({
    coord_data %>%
      sf::st_as_sf(coords = coords_cols, crs = 4326) %>%
      sf::st_transform(sf::st_crs(sf))
  }, error = function(e) {
    if (verbose) message("  Failed to create point geometries: ", e$message)
    return(NULL)
  })
  
  if (is.null(coord_sf)) return(df)
  
  # Spatial intersection
  intersections <- sf::st_intersects(coord_sf, sf)
  
  # Update df with matches
  for (i in seq_along(intersections)) {
    if (length(intersections[[i]]) > 0) {
      sf_idx <- intersections[[i]][1]  # Take first match
      row_id <- coord_sf$.row_id[i]
      
      matched_sf <- sf[sf_idx, ]
      
      df <- df %>%
        dplyr::rows_update(
          df %>%
            dplyr::filter(.data$.row_id == row_id) %>%
            dplyr::select(-.data$geometry) %>%
            dplyr::bind_cols(
              sf::st_drop_geometry(matched_sf)
            ) %>%
            dplyr::mutate(
              geometry = matched_sf$geometry,
              .match_strategy = "spatial",
              .match_confidence = 1.0
            ),
          by = ".row_id",
          unmatched = "ignore"
        )
    }
  }
  
  df
}
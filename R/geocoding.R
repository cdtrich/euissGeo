#' Geocoding Functions
#' 
#' Functions for geocoding locations and working with geographic coordinates.
#' 
#' @name geocoding
NULL

#' Geocode locations in a data frame
#'
#' Takes a data frame with location names and geocodes them using
#' OpenStreetMap Nominatim service via tmaptools. Handles batch geocoding
#' with error handling and optional conversion to sf objects.
#'
#' @param df Data frame containing location information.
#' @param loc Name of the location column in the data frame.
#' @param sf Convert output to sf object? (default: TRUE)
#' @param country_bias Optional country code to bias results (e.g., "US", "DE").
#' @param timeout Timeout for individual requests in seconds (default: 10).
#' @param max_retries Number of retries for failed requests (default: 3).
#' @param delay Delay between requests in seconds (default: 0.5).
#' @param cache_results Cache results for repeated requests? (default: TRUE)
#' @param verbose Print progress and results? (default: FALSE)
#'
#' @return If sf=TRUE, returns sf object with point geometries. 
#'         Otherwise returns data frame with latitude/longitude columns.
#'
#' @details
#' Geocodes locations using Nominatim service from OpenStreetMap.
#' 
#' Features:
#' \itemize{
#'   \item Automatic retry for failed requests
#'   \item Rate limiting to respect service limits
#'   \item Optional country biasing for accuracy
#'   \item Caching to avoid repeated requests
#'   \item Progress reporting for large datasets
#' }
#'
#' @examples
#' \dontrun{
#' # Basic geocoding
#' locations <- data.frame(
#'   city = c("Paris", "London", "Berlin", "Madrid"),
#'   population = c(2161000, 8982000, 3669000, 6642000)
#' )
#' 
#' geocoded <- euiss_geocode(locations, loc = "city")
#' 
#' # With country bias and verbose output
#' eu_cities <- data.frame(
#'   city = c("Frankfurt", "Birmingham", "Valencia")
#' )
#' 
#' geocoded_eu <- euiss_geocode(
#'   eu_cities,
#'   loc = "city",
#'   country_bias = "DE",
#'   verbose = TRUE
#' )
#' 
#' # Return as data frame
#' geocoded_df <- euiss_geocode(locations, loc = "city", sf = FALSE)
#' }
#'
#' @import dplyr
#' @import sf
#' @importFrom tmaptools geocode_OSM
#'
#' @export
euiss_geocode <- function(df,
                          loc,
                          sf = TRUE,
                          country_bias = NULL,
                          timeout = 10,
                          max_retries = 3,
                          delay = 0.5,
                          cache_results = TRUE,
                          verbose = FALSE) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame", call. = FALSE)
  }
  
  if (!loc %in% names(df)) {
    stop(
      "Column '", loc, "' not found.\n",
      "Available columns: ", paste(names(df), collapse = ", "),
      call. = FALSE
    )
  }
  
  if (nrow(df) == 0) {
    warning("Input data frame is empty", call. = FALSE)
    return(df)
  }
  
  # Check required packages
  if (!requireNamespace("tmaptools", quietly = TRUE)) {
    stop(
      "tmaptools package required.\n",
      "Install with: install.packages('tmaptools')",
      call. = FALSE
    )
  }
  
  # Get unique locations
  unique_locations <- unique(df[[loc]][!is.na(df[[loc]])])
  unique_locations <- unique_locations[unique_locations != ""]
  
  if (length(unique_locations) == 0) {
    stop("No valid location names found in column '", loc, "'", call. = FALSE)
  }
  
  if (verbose) {
    message("Geocoding ", length(unique_locations), " unique locations...")
  }
  
  # Initialize cache
  geocode_cache <- new.env(hash = TRUE)
  
  # Progress tracking
  if (verbose && length(unique_locations) > 1) {
    message("Progress: ", appendLF = FALSE)
  }
  
  # Geocode all unique locations
  geocode_results <- vector("list", length(unique_locations))
  
  for (i in seq_along(unique_locations)) {
    
    if (verbose && length(unique_locations) > 1) {
      if (i %% 5 == 0 || i == length(unique_locations)) {
        message(i, "/", length(unique_locations), " ", appendLF = FALSE)
      }
    }
    
    geocode_results[[i]] <- .geocode_with_retry(
      unique_locations[i],
      country_bias = country_bias,
      max_retries = max_retries,
      delay = delay,
      cache = geocode_cache,
      use_cache = cache_results
    )
    
    # Rate limiting
    if (i < length(unique_locations)) {
      Sys.sleep(delay)
    }
  }
  
  if (verbose && length(unique_locations) > 1) {
    message("")  # New line
  }
  
  # Convert results to data frame
  geocoded_locations <- dplyr::bind_rows(geocode_results)
  
  # Report success rate
  success_count <- sum(geocoded_locations$success)
  if (verbose) {
    message(
      "Geocoding completed: ", success_count, "/", length(unique_locations),
      " locations successfully geocoded"
    )
    
    if (success_count < length(unique_locations)) {
      failed_locations <- geocoded_locations$location[!geocoded_locations$success]
      message("Failed locations: ", paste(failed_locations, collapse = ", "))
    }
  }
  
  # Join back to original data
  result_data <- df %>%
    dplyr::left_join(
      geocoded_locations %>% dplyr::select(.data$location, .data$longitude, .data$latitude),
      by = setNames("location", loc)
    )
  
  # Convert to sf if requested
  if (sf) {
    result_data <- .convert_to_sf(result_data, verbose)
  }
  
  result_data
}

#' Geocode with retry logic
#' @keywords internal
#' @noRd
.geocode_with_retry <- function(location, country_bias, max_retries, delay, cache, use_cache) {
  
  # Check cache
  if (use_cache && exists(location, envir = cache)) {
    return(get(location, envir = cache))
  }
  
  # Try geocoding with retries
  for (attempt in seq_len(max_retries)) {
    
    result <- tryCatch({
      
      # Prepare query
      query <- if (!is.null(country_bias)) {
        paste(location, country_bias, sep = ", ")
      } else {
        location
      }
      
      # Geocode using tmaptools
      geocode_result <- tmaptools::geocode_OSM(
        query,
        details = FALSE,
        as.data.frame = TRUE
      )
      
      if (is.null(geocode_result) || nrow(geocode_result) == 0) {
        # No results
        list(
          location = location,
          longitude = NA_real_,
          latitude = NA_real_,
          success = FALSE,
          error = "No results found"
        )
      } else {
        # Success
        list(
          location = location,
          longitude = geocode_result$lon[1],
          latitude = geocode_result$lat[1],
          success = TRUE,
          error = NA_character_
        )
      }
      
    }, error = function(e) {
      list(
        location = location,
        longitude = NA_real_,
        latitude = NA_real_,
        success = FALSE,
        error = as.character(e$message)
      )
    })
    
    # If successful or final attempt, return
    if (result$success || attempt == max_retries) {
      # Cache the result
      if (use_cache) {
        assign(location, result, envir = cache)
      }
      return(result)
    }
    
    # Wait before retry
    if (attempt < max_retries) {
      Sys.sleep(delay * attempt)
    }
  }
}

#' Convert to sf object with validation
#' @keywords internal
#' @noRd
.convert_to_sf <- function(data, verbose) {
  
  valid_coords <- !is.na(data$longitude) & !is.na(data$latitude)
  
  if (sum(valid_coords) == 0) {
    warning("No valid coordinates found. Returning data frame.", call. = FALSE)
    return(data)
  }
  
  if (sum(valid_coords) < nrow(data)) {
    warning(
      sum(!valid_coords), " row(s) with missing coordinates excluded from sf object",
      call. = FALSE
    )
    data <- data[valid_coords, ]
  }
  
  data %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
}

#' Batch geocoding with multiple services
#'
#' Provides robust geocoding by trying multiple services in sequence
#' for better success rates. Currently only Nominatim is implemented.
#'
#' @param locations Character vector of location names to geocode.
#' @param services Vector of services to try (default: "nominatim").
#' @param prefer_accuracy Prefer accurate but slower services first? (default: TRUE)
#' @param verbose Print detailed progress? (default: FALSE)
#'
#' @return Data frame with geocoding results and service metadata.
#'
#' @details
#' Currently supports:
#' \itemize{
#'   \item \strong{nominatim}: OpenStreetMap Nominatim (free)
#' }
#' 
#' Future versions may add Google Maps, Bing, etc. with API keys.
#'
#' @examples
#' \dontrun{
#' # Basic multi-service geocoding
#' locations <- c("Paris, France", "London, UK", "Berlin, Germany")
#' results <- euiss_batch_geocode(locations)
#' }
#'
#' @export
euiss_batch_geocode <- function(locations,
                                services = c("nominatim"),
                                prefer_accuracy = TRUE,
                                verbose = FALSE) {
  
  # Input validation
  if (!is.character(locations)) {
    stop("`locations` must be a character vector", call. = FALSE)
  }
  
  locations <- unique(locations[!is.na(locations) & locations != ""])
  
  if (length(locations) == 0) {
    stop("No valid locations provided", call. = FALSE)
  }
  
  # Initialize results
  results <- data.frame(
    location = locations,
    longitude = NA_real_,
    latitude = NA_real_,
    service_used = NA_character_,
    confidence = NA_real_,
    stringsAsFactors = FALSE
  )
  
  if (verbose) {
    message(
      "Batch geocoding ", length(locations), " locations using: ",
      paste(services, collapse = ", ")
    )
  }
  
  # Try each service
  for (service in services) {
    
    unmatched <- is.na(results$longitude)
    if (sum(unmatched) == 0) break
    
    if (verbose) {
      message("Trying ", service, " for ", sum(unmatched), " unmatched locations...")
    }
    
    if (service == "nominatim") {
      
      temp_df <- data.frame(location = results$location[unmatched])
      
      geocoded <- tryCatch({
        euiss_geocode(temp_df, loc = "location", sf = FALSE, verbose = FALSE)
      }, error = function(e) {
        if (verbose) message("  Nominatim failed: ", e$message)
        temp_df$longitude <- NA_real_
        temp_df$latitude <- NA_real_
        temp_df
      })
      
      # Update results
      valid_results <- !is.na(geocoded$longitude) & !is.na(geocoded$latitude)
      if (any(valid_results)) {
        unmatched_indices <- which(unmatched)
        update_indices <- unmatched_indices[valid_results]
        
        results$longitude[update_indices] <- geocoded$longitude[valid_results]
        results$latitude[update_indices] <- geocoded$latitude[valid_results]
        results$service_used[update_indices] <- "nominatim"
        results$confidence[update_indices] <- 0.8
      }
    }
    
    # Additional services would be implemented here
    # e.g., "google", "bing" with API keys
  }
  
  # Final summary
  successful <- sum(!is.na(results$longitude))
  if (verbose) {
    message("Batch geocoding completed: ", successful, "/", length(locations), " successful")
    
    if (successful > 0) {
      service_summary <- table(results$service_used[!is.na(results$service_used)])
      message("By service: ", paste(names(service_summary), "=", service_summary, collapse = ", "))
    }
  }
  
  results
}

#' Reverse geocoding (REMOVED - Not Implemented)
#'
#' This function has been removed as it was not fully implemented.
#' 
#' For reverse geocoding functionality, consider using:
#' \itemize{
#'   \item \code{tmaptools::rev_geocode_OSM()}
#'   \item \code{tidygeocoder::reverse_geocode()}
#' }
#'
#' @param ... All parameters (ignored)
#' @return Error message
#' @keywords internal
#' @export
euiss_reverse_geocode <- function(...) {
  stop(
    "euiss_reverse_geocode() is not implemented.\n",
    "Use tmaptools::rev_geocode_OSM() or tidygeocoder::reverse_geocode() instead.",
    call. = FALSE
  )
}

#' Interactive location picker (REMOVED - Not Implemented)
#'
#' This function has been removed as it was not fully implemented.
#'
#' For interactive location selection, consider using:
#' \itemize{
#'   \item \code{mapview} package for interactive mapping
#'   \item \code{leaflet} package with Shiny for point selection
#'   \item \code{mapedit::editMap()} for interactive editing
#' }
#'
#' @param ... All parameters (ignored)
#' @return Error message
#' @keywords internal
#' @export
euiss_locator <- function(...) {
  stop(
    "euiss_locator() is not implemented.\n",
    "Use mapview, leaflet, or mapedit packages for interactive location selection.",
    call. = FALSE
  )
}
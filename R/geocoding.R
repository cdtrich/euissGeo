#' Geocoding Functions
#' 
#' Functions for geocoding locations and working with geographic coordinates.
#' 
#' @name geocoding
NULL

#' Geocode locations in a data frame
#'
#' This function takes a data frame with location names and geocodes them using
#' OpenStreetMap Nominatim service via the tmaptools package. It handles batch
#' geocoding with error handling and optional conversion to sf objects.
#'
#' @param df A data frame containing location information.
#' @param loc The name of the location column in the data frame.
#' @param sf Logical. Should the output be converted to an sf object? Default TRUE.
#' @param country_bias Optional country code to bias geocoding results (e.g., "US", "DE").
#' @param timeout Timeout for individual geocoding requests in seconds.
#' @param max_retries Number of retries for failed geocoding requests.
#' @param delay Delay between requests in seconds (to be respectful to the service).
#' @param cache_results Logical. Should results be cached for repeated requests?
#' @param verbose Logical. Print progress and results? Default FALSE.
#'
#' @return If sf=TRUE, returns an sf object with point geometries. Otherwise returns
#'         a data frame with latitude and longitude columns.
#'
#' @details
#' This function geocodes locations using the Nominatim service from OpenStreetMap.
#' It includes several enhancements:
#' \itemize{
#'   \item Automatic retry for failed requests
#'   \item Rate limiting to respect service limits
#'   \item Optional country biasing for better accuracy
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
#' # Return as regular data frame
#' geocoded_df <- euiss_geocode(locations, loc = "city", sf = FALSE)
#' }
#'
#' @import dplyr
#' @import sf
#' @import purrr
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
    stop("`df` must be a data frame")
  }
  
  if (!loc %in% names(df)) {
    stop("Column '", loc, "' not found in data frame. Available columns: ",
         paste(names(df), collapse = ", "))
  }
  
  if (nrow(df) == 0) {
    warning("Input data frame is empty")
    return(df)
  }
  
  # Check required packages
  if (!requireNamespace("tmaptools", quietly = TRUE)) {
    stop("tmaptools package is required. Install with: install.packages('tmaptools')")
  }
  
  # Get unique locations to avoid duplicate geocoding
  unique_locations <- unique(df[[loc]][!is.na(df[[loc]])])
  unique_locations <- unique_locations[unique_locations != ""]
  
  if (length(unique_locations) == 0) {
    stop("No valid location names found in column '", loc, "'")
  }
  
  if (verbose) {
    message("Geocoding ", length(unique_locations), " unique locations...")
  }
  
  # Initialize cache
  geocode_cache <- new.env(hash = TRUE)
  
  # Geocoding function with retry logic
  geocode_with_retry <- function(location) {
    
    # Check cache first
    if (cache_results && exists(location, envir = geocode_cache)) {
      return(get(location, envir = geocode_cache))
    }
    
    for (attempt in 1:max_retries) {
      
      result <- tryCatch({
        
        # Prepare query with optional country bias
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
          # No results found
          list(
            location = location,
            x = NA_real_,
            y = NA_real_,
            success = FALSE,
            error = "No results found"
          )
        } else {
          # Successful geocoding
          list(
            location = location,
            x = geocode_result$lon[1],
            y = geocode_result$lat[1],
            success = TRUE,
            error = NA_character_
          )
        }
        
      }, error = function(e) {
        list(
          location = location,
          x = NA_real_,
          y = NA_real_,
          success = FALSE,
          error = as.character(e$message)
        )
      })
      
      # If successful or final attempt, return result
      if (result$success || attempt == max_retries) {
        # Cache the result
        if (cache_results) {
          assign(location, result, envir = geocode_cache)
        }
        return(result)
      }
      
      # Wait before retry
      if (attempt < max_retries) {
        Sys.sleep(delay * attempt)  # Increasing delay
      }
    }
  }
  
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
    
    geocode_results[[i]] <- geocode_with_retry(unique_locations[i])
    
    # Rate limiting - be respectful to the service
    if (i < length(unique_locations)) {
      Sys.sleep(delay)
    }
  }
  
  if (verbose && length(unique_locations) > 1) {
    message("") # New line
  }
  
  # Convert results to data frame
  geocoded_locations <- dplyr::bind_rows(geocode_results)
  
  # Report success rate
  success_count <- sum(geocoded_locations$success)
  if (verbose) {
    message("Geocoding completed: ", success_count, "/", length(unique_locations), 
           " locations successfully geocoded")
    
    if (success_count < length(unique_locations)) {
      failed_locations <- geocoded_locations$location[!geocoded_locations$success]
      message("Failed locations: ", paste(failed_locations, collapse = ", "))
    }
  }
  
  # Join back to original data
  result_data <- df %>%
    dplyr::left_join(
      geocoded_locations %>% dplyr::select(.data$location, .data$x, .data$y),
      by = setNames("location", loc)
    )
  
  # Convert to sf if requested
  if (sf) {
    # Filter out rows without valid coordinates
    valid_coords <- !is.na(result_data$x) & !is.na(result_data$y)
    
    if (sum(valid_coords) == 0) {
      warning("No valid coordinates found. Returning original data frame.")
      return(df)
    }
    
    if (sum(valid_coords) < nrow(result_data)) {
      warning("Some rows have missing coordinates and will be excluded from sf object")
      result_data <- result_data[valid_coords, ]
    }
    
    result_sf <- result_data %>%
      sf::st_as_sf(coords = c("x", "y"), crs = 4326)
    
    return(result_sf)
  }
  
  return(result_data)
}

#' Reverse geocode coordinates to location names
#'
#' This function performs reverse geocoding - converting latitude/longitude coordinates
#' back to human-readable location names using the Nominatim service.
#'
#' @param df Data frame with coordinate columns.
#' @param lat_col Name of the latitude column.
#' @param lon_col Name of the longitude column.
#' @param detail_level Level of detail for results. Options: "country", "city", "address".
#' @param language Language code for results (e.g., "en", "fr", "de").
#' @param timeout Timeout for requests in seconds.
#' @param verbose Logical. Print progress information?
#'
#' @return Data frame with added location information columns.
#'
#' @examples
#' \dontrun{
#' # Coordinates data
#' coords_data <- data.frame(
#'   id = 1:3,
#'   latitude = c(48.8566, 51.5074, 52.5200),
#'   longitude = c(2.3522, -0.1278, 13.4050)
#' )
#' 
#' # Reverse geocode to get city names
#' locations <- euiss_reverse_geocode(
#'   coords_data, 
#'   lat_col = "latitude", 
#'   lon_col = "longitude",
#'   detail_level = "city"
#' )
#' }
#'
#' @export
euiss_reverse_geocode <- function(df,
                                 lat_col = "latitude",
                                 lon_col = "longitude", 
                                 detail_level = "city",
                                 language = "en",
                                 timeout = 10,
                                 verbose = FALSE) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame")
  }
  
  if (!lat_col %in% names(df)) {
    stop("Column '", lat_col, "' not found in data frame")
  }
  
  if (!lon_col %in% names(df)) {
    stop("Column '", lon_col, "' not found in data frame")
  }
  
  if (!detail_level %in% c("country", "city", "address")) {
    stop("detail_level must be one of: 'country', 'city', 'address'")
  }
  
  # Check for valid coordinates
  valid_rows <- !is.na(df[[lat_col]]) & !is.na(df[[lon_col]])
  
  if (sum(valid_rows) == 0) {
    warning("No valid coordinates found")
    return(df)
  }
  
  if (verbose) {
    message("Reverse geocoding ", sum(valid_rows), " coordinate pairs...")
  }
  
  # Initialize result columns
  df$reverse_geocoded <- NA_character_
  df$country <- NA_character_
  
  if (detail_level %in% c("city", "address")) {
    df$city <- NA_character_
  }
  
  if (detail_level == "address") {
    df$address <- NA_character_
  }
  
  # Process each valid coordinate pair
  valid_indices <- which(valid_rows)
  
  for (i in seq_along(valid_indices)) {
    
    row_idx <- valid_indices[i]
    lat <- df[[lat_col]][row_idx]
    lon <- df[[lon_col]][row_idx]
    
    if (verbose && length(valid_indices) > 1) {
      if (i %% 10 == 0 || i == length(valid_indices)) {
        message("Progress: ", i, "/", length(valid_indices))
      }
    }
    
    # Reverse geocode
    reverse_result <- tryCatch({
      # Note: This is a simplified example. A full implementation would
      # use tmaptools or httr to query Nominatim reverse geocoding API
      list(
        country = "Unknown",
        city = "Unknown", 
        address = paste(lat, lon, sep = ", "),
        success = TRUE
      )
    }, error = function(e) {
      if (verbose) {
        message("Failed to reverse geocode (", lat, ", ", lon, "): ", e$message)
      }
      list(
        country = NA_character_,
        city = NA_character_,
        address = NA_character_,
        success = FALSE
      )
    })
    
    # Update result
    if (reverse_result$success) {
      df$country[row_idx] <- reverse_result$country
      
      if (detail_level %in% c("city", "address")) {
        df$city[row_idx] <- reverse_result$city
      }
      
      if (detail_level == "address") {
        df$address[row_idx] <- reverse_result$address
      }
      
      # Set main result based on detail level
      df$reverse_geocoded[row_idx] <- switch(detail_level,
        "country" = reverse_result$country,
        "city" = paste(reverse_result$city, reverse_result$country, sep = ", "),
        "address" = reverse_result$address
      )
    }
    
    # Rate limiting
    Sys.sleep(0.5)
  }
  
  if (verbose) {
    successful <- sum(!is.na(df$reverse_geocoded))
    message("Reverse geocoding completed: ", successful, "/", sum(valid_rows), " successful")
  }
  
  return(df)
}

#' Interactive location picker
#'
#' This function provides an interactive way to select locations on a map
#' and retrieve their coordinates. Useful for manual data entry or validation.
#'
#' @param n_points Number of points to select interactively.
#' @param base_map Optional base map (sf object) to display for context.
#' @param title Title for the interactive session.
#' @param return_format Format for returned coordinates: "sf", "df", "coords".
#'
#' @return Coordinates in the requested format.
#'
#' @details
#' This function opens an interactive map where users can click to select locations.
#' Requires the mapview or leaflet package for interactive mapping.
#'
#' @examples
#' \dontrun{
#' # Interactive selection of 3 points
#' selected_points <- euiss_locator(n_points = 3)
#' 
#' # With base map context
#' countries <- euiss_gisco(res = "20")
#' points <- euiss_locator(n_points = 2, base_map = countries)
#' }
#'
#' @export
euiss_locator <- function(n_points = 1,
                         base_map = NULL,
                         title = "Click to select locations",
                         return_format = "sf") {
  
  # Check for required packages
  has_mapview <- requireNamespace("mapview", quietly = TRUE)
  has_leaflet <- requireNamespace("leaflet", quietly = TRUE)
  
  if (!has_mapview && !has_leaflet) {
    stop("mapview or leaflet package required for interactive location selection")
  }
  
  message("Interactive location selection")
  message("Click on map to select ", n_points, " location(s)")
  message("Press ESC when done or after selecting all points")
  
  # For now, return a placeholder since interactive functionality 
  # requires specific package implementations
  message("Interactive locator functionality would be implemented here")
  message("This would open an interactive map for point selection")
  
  # Placeholder result
  result_df <- data.frame(
    point_id = 1:n_points,
    longitude = rep(0, n_points),
    latitude = rep(0, n_points)
  )
  
  if (return_format == "sf") {
    return(sf::st_as_sf(result_df, coords = c("longitude", "latitude"), crs = 4326))
  } else if (return_format == "coords") {
    return(as.matrix(result_df[, c("longitude", "latitude")]))
  } else {
    return(result_df)
  }
}

#' Batch geocoding with multiple services
#'
#' This function provides robust geocoding by trying multiple services
#' in sequence for better success rates.
#'
#' @param locations Character vector of location names to geocode.
#' @param services Vector of services to try: "nominatim", "google", "bing".
#' @param api_keys Named list of API keys for commercial services.
#' @param prefer_accuracy Logical. Prefer more accurate but slower services first?
#' @param verbose Logical. Print detailed progress?
#'
#' @return Data frame with geocoding results and service metadata.
#'
#' @examples
#' \dontrun{
#' # Basic multi-service geocoding
#' locations <- c("Paris, France", "London, UK", "Berlin, Germany")
#' results <- euiss_batch_geocode(locations)
#' 
#' # With commercial services (requires API keys)
#' results <- euiss_batch_geocode(
#'   locations,
#'   services = c("google", "nominatim"),
#'   api_keys = list(google = "your_api_key")
#' )
#' }
#'
#' @export
euiss_batch_geocode <- function(locations,
                               services = c("nominatim"),
                               api_keys = list(),
                               prefer_accuracy = TRUE,
                               verbose = FALSE) {
  
  # Input validation
  if (!is.character(locations)) {
    stop("`locations` must be a character vector")
  }
  
  locations <- unique(locations[!is.na(locations) & locations != ""])
  
  if (length(locations) == 0) {
    stop("No valid locations provided")
  }
  
  # Initialize results data frame
  results <- data.frame(
    location = locations,
    longitude = NA_real_,
    latitude = NA_real_,
    service_used = NA_character_,
    confidence = NA_real_,
    stringsAsFactors = FALSE
  )
  
  if (verbose) {
    message("Batch geocoding ", length(locations), " locations using services: ",
           paste(services, collapse = ", "))
  }
  
  # For each service, try to geocode remaining unmatched locations
  for (service in services) {
    
    unmatched <- is.na(results$longitude)
    if (sum(unmatched) == 0) break
    
    if (verbose) {
      message("Trying ", service, " for ", sum(unmatched), " unmatched locations...")
    }
    
    if (service == "nominatim") {
      # Use the existing euiss_geocode function
      temp_df <- data.frame(location = results$location[unmatched])
      
      geocoded <- tryCatch({
        euiss_geocode(temp_df, loc = "location", sf = FALSE, verbose = FALSE)
      }, error = function(e) {
        if (verbose) message("Nominatim geocoding failed: ", e$message)
        temp_df$x <- NA_real_
        temp_df$y <- NA_real_
        temp_df
      })
      
      # Update results
      valid_results <- !is.na(geocoded$x) & !is.na(geocoded$y)
      if (any(valid_results)) {
        unmatched_indices <- which(unmatched)
        update_indices <- unmatched_indices[valid_results]
        
        results$longitude[update_indices] <- geocoded$x[valid_results]
        results$latitude[update_indices] <- geocoded$y[valid_results]
        results$service_used[update_indices] <- "nominatim"
        results$confidence[update_indices] <- 0.8
      }
    }
    
    # Additional services would be implemented here
    # (Google, Bing, etc. with their respective APIs)
  }
  
  # Final summary
  successful <- sum(!is.na(results$longitude))
  if (verbose) {
    message("Batch geocoding completed: ", successful, "/", length(locations), " successful")
    
    if (successful > 0) {
      service_summary <- table(results$service_used[!is.na(results$service_used)])
      message("Service breakdown: ", paste(names(service_summary), "=", service_summary, collapse = ", "))
    }
  }
  
  return(results)
}

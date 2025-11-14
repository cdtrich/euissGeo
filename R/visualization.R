#' Spatial Visualization Functions
#' 
#' Functions for creating specialized spatial visualizations like spike maps and cartograms.
#' 
#' @name visualization
NULL

#' Transform Simple Features object into spike coordinates for spike maps
#'
#' This function transforms a Simple Features (sf) object into coordinate data for
#' creating spike visualizations. Spikes are triangular representations where 
#' height corresponds to data values, useful for showing quantitative data on maps.
#'
#' @param data A Simple Features (sf) object with polygon or point geometries.
#' @param val The variable name representing the height/value of spikes (unquoted).
#' @param fact Scaling factor to control the height of spikes relative to the map.
#' @param spread The width factor for spike base. If NULL, defaults to fact/50.
#' @param of_largest_polygon Logical. For multipolygons, use centroid of largest polygon? Default TRUE.
#' @param drop_geom Logical. Should the geometry column be dropped from output? Default TRUE.
#' @param polygon_format Logical. Return data formatted for geom_polygon()? Default TRUE.
#' @param min_value Minimum value threshold - spikes below this won't be drawn.
#' @param max_spikes Maximum number of spikes to draw (for performance).
#'
#' @return A data frame with coordinates for creating spike visualizations.
#'         If polygon_format=TRUE, returns data suitable for geom_polygon().
#'         If FALSE, returns line format for backward compatibility.
#'
#' @details
#' The function creates triangular spikes where:
#' \itemize{
#'   \item Base width is controlled by \code{spread}
#'   \item Height is proportional to \code{val * fact}
#'   \item Base is positioned at polygon centroid
#'   \item Peak extends upward from centroid
#' }
#'
#' For best results, experiment with the \code{fact} parameter to get appropriate
#' spike heights relative to your map extent. Typical values range from 0.0001 to 100000
#' depending on your coordinate system and data range.
#'
#' @examples
#' \dontrun{
#' # Load country data
#' countries <- euiss_gisco(res = "20")
#' 
#' # Add some sample data
#' countries$population <- runif(nrow(countries), 1e6, 1e8)
#' 
#' # Create spike data
#' spike_data <- euiss_spikemap(
#'   countries, 
#'   val = population, 
#'   fact = 100000
#' )
#' 
#' # Plot with ggplot2
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = countries, fill = "lightgray") +
#'   geom_polygon(data = spike_data, aes(x = x, y = y, group = spike_id, fill = population)) +
#'   theme_void()
#' 
#' # Custom spike parameters
#' spike_data_custom <- euiss_spikemap(
#'   countries,
#'   val = population,
#'   fact = 50000,
#'   spread = 20000,
#'   min_value = 5e6
#' )
#' }
#'
#' @import dplyr
#' @import sf
#' @import tidyr
#' @import stringr
#'
#' @export
euiss_spikemap <- function(data,
                          val,
                          fact = 100000,
                          spread = NULL,
                          of_largest_polygon = TRUE,
                          drop_geom = TRUE,
                          polygon_format = TRUE,
                          min_value = NULL,
                          max_spikes = NULL) {
  
  # Input validation
  if (!inherits(data, "sf")) {
    stop("`data` must be a Simple Features (sf) object")
  }
  
  if (nrow(data) == 0) {
    stop("`data` must have at least one row")
  }
  
  val_name <- deparse(substitute(val))
  if (!val_name %in% names(data)) {
    stop("Column '", val_name, "' not found in data. Available columns: ",
         paste(names(data), collapse = ", "))
  }
  
  if (!is.numeric(data[[val_name]])) {
    stop("Column '", val_name, "' must be numeric")
  }
  
  if (!is.numeric(fact) || fact <= 0) {
    stop("`fact` must be a positive number")
  }
  
  # Set default spread if not provided
  if (is.null(spread)) {
    spread <- fact / 50
  }
  
  # Filter by minimum value if specified
  if (!is.null(min_value)) {
    data <- data %>%
      dplyr::filter(.data[[val_name]] >= min_value)
    
    if (nrow(data) == 0) {
      warning("No data remains after applying min_value filter")
      return(data.frame(x = numeric(0), y = numeric(0)))
    }
  }
  
  # Limit number of spikes if specified (keep largest values)
  if (!is.null(max_spikes) && nrow(data) > max_spikes) {
    data <- data %>%
      dplyr::arrange(dplyr::desc(.data[[val_name]])) %>%
      dplyr::slice_head(n = max_spikes)
    
    message("Limited to ", max_spikes, " largest spikes")
  }
  
  # Print value range for user information
  val_range <- range(data[[val_name]], na.rm = TRUE)
  message("Value range for ", val_name, ": Min = ", round(val_range[1], 2), 
         ", Max = ", round(val_range[2], 2))
  
  # Get centroids and convert to coordinates
  centroids <- data %>%
    sf::st_make_valid() %>%
    sf::st_centroid(of_largest_polygon = of_largest_polygon)
  
  # Extract coordinates
  coords_matrix <- sf::st_coordinates(centroids)
  
  # Create data frame with coordinates and values
  spike_base_data <- centroids %>%
    {if (drop_geom) sf::st_drop_geometry(.) else .} %>%
    dplyr::mutate(
      x = coords_matrix[, 1],
      y = coords_matrix[, 2]
    ) %>%
    dplyr::rename(val = !!rlang::sym(val_name)) %>%
    dplyr::mutate(val = as.numeric(.data$val)) %>%
    dplyr::filter(!is.na(.data$val))
  
  if (nrow(spike_base_data) == 0) {
    warning("No valid data points for spike creation")
    return(data.frame(x = numeric(0), y = numeric(0)))
  }
  
  # Calculate spike coordinates
  spike_coords <- spike_base_data %>%
    dplyr::mutate(
      xmin = .data$x - spread,
      xmax = .data$x + spread,
      ymin = .data$y,
      ymax = .data$y + .data$val * fact
    )
  
  # Return polygon format if requested
  if (polygon_format) {
    polygon_data <- spike_coords %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        coords = list(data.frame(
          x_poly = c(.data$xmin, .data$x, .data$xmax, .data$xmin),  # Triangle: left, peak, right, close
          y_poly = c(.data$ymin, .data$ymax, .data$ymin, .data$ymin)   # Triangle: base, top, base, close
        ))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(spike_id = dplyr::row_number()) %>%
      tidyr::unnest(.data$coords) %>%
      # Keep other variables for each coordinate point
      dplyr::select(-c(.data$xmin, .data$xmax, .data$ymin, .data$ymax, .data$x, .data$y)) %>%
      dplyr::rename(
        x = .data$x_poly,
        y = .data$y_poly
      )
    
    return(polygon_data)
  }
  
  # Original line format for backward compatibility
  line_data <- spike_coords %>%
    dplyr::mutate(
      x_left = .data$xmin,
      y_left = .data$ymin,
      x_mid = .data$x,
      y_mid = .data$ymax,
      x_right = .data$xmax,
      y_right = .data$ymin
    ) %>%
    tidyr::pivot_longer(
      cols = c(.data$x_left, .data$y_left, .data$x_mid, .data$y_mid, .data$x_right, .data$y_right),
      names_to = "coord_type",
      values_to = "coord"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      side = stringr::str_sub(
        .data$coord_type,
        stringr::str_locate(.data$coord_type, "_")[1] + 1,
        stringr::str_length(.data$coord_type)
      ),
      coord_type = stringr::str_c(
        stringr::str_sub(.data$coord_type, 1, 1),
        "_coord"
      )
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      values_from = .data$coord,
      names_from = .data$coord_type
    )
  
  return(line_data)
}

#' Generate cartogram from sf object with data
#'
#' This function creates cartograms where polygon sizes are distorted based on
#' a weight variable. Uses the cartogram package to create area or distance cartograms.
#'
#' @param sf_data A Simple Features (sf) object with polygon geometries and associated data.
#' @param weight The variable name for cartogram weights (unquoted).
#' @param type Type of cartogram. Options: "cont" (continuous), "ncont" (non-contiguous), "dorling".
#' @param k Smoothing parameter for continuous cartograms. Higher values = smoother.
#' @param crs Target coordinate reference system. If NULL, uses data's current CRS.
#' @param itermax Maximum iterations for cartogram algorithm.
#' @param prepare_data Logical. Should data be prepared/cleaned before cartogram creation?
#' @param min_weight Minimum weight threshold - polygons below this are excluded.
#' @param verbose Logical. Print progress and diagnostic information?
#'
#' @return A Simple Features (sf) object representing the cartogram.
#'
#' @details
#' Cartogram types:
#' \itemize{
#'   \item \strong{cont}: Continuous cartogram preserving topology
#'   \item \strong{ncont}: Non-contiguous cartogram (shapes don't touch)
#'   \item \strong{dorling}: Dorling cartogram (circles proportional to weight)
#' }
#'
#' The weight variable should be positive numeric values. Zero or negative values
#' may cause issues with cartogram algorithms.
#'
#' @examples
#' \dontrun{
#' # Load country data
#' countries <- euiss_gisco(res = "20") %>%
#'   slice_head(n = 10)  # Subset for faster processing
#' 
#' # Add sample population data
#' countries$population <- runif(nrow(countries), 1e6, 1e8)
#' 
#' # Create continuous cartogram
#' cartogram_cont <- euiss_cartogram(
#'   countries, 
#'   weight = population, 
#'   type = "cont"
#' )
#' 
#' # Create Dorling cartogram (circles)
#' cartogram_dorling <- euiss_cartogram(
#'   countries,
#'   weight = population,
#'   type = "dorling"
#' )
#' 
#' # Plot comparison
#' library(ggplot2)
#' original_plot <- ggplot(countries) + 
#'   geom_sf(aes(fill = population)) + 
#'   ggtitle("Original")
#' 
#' cartogram_plot <- ggplot(cartogram_cont) + 
#'   geom_sf(aes(fill = population)) +
#'   ggtitle("Cartogram")
#' }
#'
#' @import sf
#' @import dplyr
#' @importFrom cartogram cartogram_cont cartogram_ncont cartogram_dorling
#'
#' @export
euiss_cartogram <- function(sf_data,
                           weight,
                           type = "cont",
                           k = 0.05,
                           crs = NULL,
                           itermax = 15,
                           prepare_data = TRUE,
                           min_weight = NULL,
                           verbose = FALSE) {
  
  # Input validation
  if (!inherits(sf_data, "sf")) {
    stop("`sf_data` must be a Simple Features (sf) object")
  }
  
  if (nrow(sf_data) == 0) {
    stop("`sf_data` must have at least one row")
  }
  
  weight_name <- deparse(substitute(weight))
  if (!weight_name %in% names(sf_data)) {
    stop("Column '", weight_name, "' not found in data. Available columns: ",
         paste(names(sf_data), collapse = ", "))
  }
  
  if (!type %in% c("cont", "ncont", "dorling")) {
    stop("type must be one of: 'cont', 'ncont', 'dorling'")
  }
  
  # Check required packages
  if (!requireNamespace("cartogram", quietly = TRUE)) {
    stop("cartogram package is required. Install with: install.packages('cartogram')")
  }
  
  # Prepare data
  if (prepare_data) {
    if (verbose) message("Preparing data for cartogram creation...")
    
    # Remove rows with missing weights
    original_nrow <- nrow(sf_data)
    sf_data <- sf_data %>%
      dplyr::filter(!is.na(.data[[weight_name]]))
    
    if (nrow(sf_data) < original_nrow && verbose) {
      message("Removed ", original_nrow - nrow(sf_data), " rows with missing weights")
    }
    
    # Apply minimum weight filter
    if (!is.null(min_weight)) {
      sf_data <- sf_data %>%
        dplyr::filter(.data[[weight_name]] >= min_weight)
      
      if (verbose) {
        message("Filtered to ", nrow(sf_data), " rows with weight >= ", min_weight)
      }
    }
    
    # Ensure positive weights
    if (any(sf_data[[weight_name]] <= 0, na.rm = TRUE)) {
      if (verbose) message("Converting non-positive weights to small positive values...")
      sf_data <- sf_data %>%
        dplyr::mutate(
          !!weight_name := pmax(.data[[weight_name]], 0.001)
        )
    }
    
    # Make geometries valid
    sf_data <- sf_data %>%
      sf::st_make_valid()
    
    # Remove empty geometries
    empty_geoms <- sf::st_is_empty(sf_data)
    if (any(empty_geoms)) {
      sf_data <- sf_data[!empty_geoms, ]
      if (verbose) message("Removed ", sum(empty_geoms), " empty geometries")
    }
  }
  
  if (nrow(sf_data) == 0) {
    stop("No valid data remaining after preparation")
  }
  
  # Transform to appropriate CRS if needed
  if (!is.null(crs)) {
    if (verbose) message("Transforming to CRS: ", crs)
    sf_data <- sf::st_transform(sf_data, crs)
  } else {
    # For cartograms, equal-area projection is often better
    current_crs <- sf::st_crs(sf_data)
    if (is.na(current_crs) || current_crs$epsg == 4326) {
      if (verbose) message("Converting to equal-area projection for better cartogram results")
      sf_data <- sf::st_transform(sf_data, 3857)  # Web Mercator as fallback
    }
  }
  
  # Print weight statistics
  if (verbose) {
    weight_stats <- summary(sf_data[[weight_name]])
    message("Weight statistics:")
    print(weight_stats)
  }
  
  # Create cartogram based on type
  cartogram_result <- tryCatch({
    
    if (verbose) {
      message("Creating ", type, " cartogram with ", nrow(sf_data), " features...")
    }
    
    if (type == "cont") {
      cartogram::cartogram_cont(sf_data, weight = weight_name, itermax = itermax)
      
    } else if (type == "ncont") {
      cartogram::cartogram_ncont(sf_data, weight = weight_name, k = k, inplace = FALSE)
      
    } else if (type == "dorling") {
      cartogram::cartogram_dorling(sf_data, weight = weight_name, k = k)
    }
    
  }, error = function(e) {
    stop("Cartogram creation failed: ", e$message, "\n",
         "Try adjusting parameters or using prepare_data=TRUE")
  })
  
  if (verbose) {
    message("Cartogram creation successful!")
    
    # Compare areas if possible
    if (type %in% c("cont", "ncont")) {
      original_area <- sum(sf::st_area(sf_data))
      cartogram_area <- sum(sf::st_area(cartogram_result))
      area_ratio <- as.numeric(cartogram_area / original_area)
      message("Area preservation ratio: ", round(area_ratio, 3))
    }
  }
  
  return(cartogram_result)
}

#' Create multiple cartogram variants for comparison
#'
#' This function creates several cartogram variants using different methods
#' and parameters, useful for exploring which approach works best for your data.
#'
#' @param sf_data A Simple Features (sf) object.
#' @param weight The weight variable name (unquoted).
#' @param types Vector of cartogram types to create.
#' @param k_values Vector of k values to test (for applicable methods).
#' @param save_results Logical. Should results be saved to files?
#' @param output_dir Directory to save results if save_results=TRUE.
#' @param verbose Logical. Print progress information?
#'
#' @return List of cartogram sf objects with descriptive names.
#'
#' @examples
#' \dontrun{
#' countries <- euiss_gisco(res = "20") %>% slice_head(n = 5)
#' countries$pop <- runif(nrow(countries), 1e6, 1e8)
#' 
#' cartograms <- euiss_cartogram_compare(
#'   countries, 
#'   weight = pop,
#'   types = c("cont", "dorling")
#' )
#' 
#' # Access individual cartograms
#' continuous_cartogram <- cartograms$cont_k005
#' dorling_cartogram <- cartograms$dorling_k005
#' }
#'
#' @export
euiss_cartogram_compare <- function(sf_data,
                                   weight,
                                   types = c("cont", "dorling"),
                                   k_values = c(0.05, 0.1, 0.2),
                                   save_results = FALSE,
                                   output_dir = "cartograms",
                                   verbose = TRUE) {
  
  weight_name <- deparse(substitute(weight))
  
  if (verbose) {
    message("Creating cartogram comparison with:")
    message("  Types: ", paste(types, collapse = ", "))
    message("  K values: ", paste(k_values, collapse = ", "))
  }
  
  results <- list()
  
  for (type in types) {
    for (k in k_values) {
      
      result_name <- paste0(type, "_k", sprintf("%03d", k * 100))
      
      if (verbose) {
        message("Creating: ", result_name)
      }
      
      cartogram_result <- tryCatch({
        euiss_cartogram(
          sf_data,
          weight = !!rlang::sym(weight_name),
          type = type,
          k = k,
          verbose = FALSE
        )
      }, error = function(e) {
        if (verbose) {
          message("Failed to create ", result_name, ": ", e$message)
        }
        NULL
      })
      
      if (!is.null(cartogram_result)) {
        results[[result_name]] <- cartogram_result
        
        # Save if requested
        if (save_results) {
          if (!dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE)
          }
          
          output_file <- file.path(output_dir, paste0(result_name, ".rds"))
          saveRDS(cartogram_result, output_file)
          
          if (verbose) {
            message("  Saved: ", output_file)
          }
        }
      }
    }
  }
  
  if (verbose) {
    message("Comparison complete. Created ", length(results), " cartograms.")
  }
  
  return(results)
}

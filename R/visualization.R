#' Spatial Visualization Functions
#' 
#' Functions for creating specialized spatial visualizations like spike maps and cartograms.
#' 
#' @name visualization
NULL

#' Create spike map coordinates from spatial data
#'
#' Transforms sf object into coordinates for spike visualizations where height
#' represents data values. Spikes are triangular shapes extending from polygon centroids.
#'
#' @param data Simple Features (sf) object with polygon or point geometries.
#' @param val Variable name for spike height/value (unquoted).
#' @param fact Scaling factor for spike height relative to map extent.
#' @param spread Width factor for spike base. If NULL, defaults to fact/50.
#' @param of_largest_polygon For multipolygons, use largest polygon centroid? (default: TRUE)
#' @param drop_geom Drop geometry column from output? (default: TRUE)
#' @param polygon_format Return data for geom_polygon()? (default: TRUE)
#' @param min_value Minimum value threshold - spikes below this won't be drawn.
#' @param max_spikes Maximum number of spikes to draw (keeps largest values).
#'
#' @return Data frame with coordinates for spike visualizations.
#'
#' @details
#' Creates triangular spikes where:
#' \itemize{
#'   \item Base width controlled by \code{spread}
#'   \item Height proportional to \code{val * fact}
#'   \item Base positioned at polygon centroid
#'   \item Peak extends upward from centroid
#' }
#'
#' Experiment with \code{fact} parameter for appropriate spike heights.
#' Typical values: 0.0001 to 100000 depending on coordinate system and data range.
#'
#' @examples
#' \dontrun{
#' # Load country data
#' countries <- euiss_gisco(res = "20")
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
#'   geom_polygon(
#'     data = spike_data,
#'     aes(x = x, y = y, group = spike_id, fill = val),
#'     color = "darkred"
#'   ) +
#'   theme_void()
#' }
#'
#' @import dplyr
#' @import sf
#' @import tidyr
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
    stop("`data` must be a Simple Features (sf) object", call. = FALSE)
  }
  
  if (nrow(data) == 0) {
    stop("`data` must have at least one row", call. = FALSE)
  }
  
  # Get value column name
  val_name <- deparse(substitute(val))
  if (!val_name %in% names(data)) {
    stop(
      "Column '", val_name, "' not found.\n",
      "Available columns: ", paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!is.numeric(data[[val_name]])) {
    stop("Column '", val_name, "' must be numeric", call. = FALSE)
  }
  
  .validate_positive_number(fact, "fact", allow_zero = FALSE)
  
  # Set default spread
  if (is.null(spread)) {
    spread <- fact / 50
  }
  
  # Filter by minimum value
  if (!is.null(min_value)) {
    data <- data %>%
      dplyr::filter(.data[[val_name]] >= min_value)
    
    if (nrow(data) == 0) {
      warning("No data remains after applying min_value filter", call. = FALSE)
      return(data.frame(x = numeric(0), y = numeric(0)))
    }
  }
  
  # Limit number of spikes (keep largest)
  if (!is.null(max_spikes) && nrow(data) > max_spikes) {
    data <- data %>%
      dplyr::arrange(dplyr::desc(.data[[val_name]])) %>%
      dplyr::slice_head(n = max_spikes)
    
    message("Limited to ", max_spikes, " largest spikes")
  }
  
  # Report value range
  val_range <- range(data[[val_name]], na.rm = TRUE)
  message(
    "Value range: ", 
    round(val_range[1], 2), " to ", round(val_range[2], 2)
  )
  
  # Calculate centroids
  centroids <- data %>%
    sf::st_make_valid() %>%
    sf::st_centroid(of_largest_polygon = of_largest_polygon)
  
  # Extract coordinates
  coords <- sf::st_coordinates(centroids)
  
  # Build base data
  spike_base <- centroids %>%
    {if (drop_geom) sf::st_drop_geometry(.) else .} %>%
    dplyr::mutate(
      x = coords[, 1],
      y = coords[, 2],
      val = as.numeric(.data[[val_name]])
    ) %>%
    dplyr::filter(!is.na(.data$val))
  
  if (nrow(spike_base) == 0) {
    warning("No valid data points for spike creation", call. = FALSE)
    return(data.frame(x = numeric(0), y = numeric(0)))
  }
  
  # Generate spike coordinates
  if (polygon_format) {
    result <- .create_spike_polygons(spike_base, fact, spread)
  } else {
    result <- .create_spike_lines(spike_base, fact, spread)
  }
  
  result
}

#' Create spike polygon coordinates (vectorized)
#' @keywords internal
#' @noRd
.create_spike_polygons <- function(spike_base, fact, spread) {
  
  # Pre-calculate all coordinates
  spike_base <- spike_base %>%
    dplyr::mutate(
      spike_id = dplyr::row_number(),
      x_left = .data$x - spread,
      x_right = .data$x + spread,
      y_base = .data$y,
      y_peak = .data$y + .data$val * fact
    )
  
  # Create triangle vertices for each spike
  # Each triangle: left-base, peak, right-base, close
  left_points <- spike_base %>%
    dplyr::select(.data$spike_id, x = .data$x_left, y = .data$y_base, 
                  .data$val, dplyr::everything()) %>%
    dplyr::select(-.data$x_left, -.data$x_right, -.data$y_base, -.data$y_peak)
  
  peak_points <- spike_base %>%
    dplyr::select(.data$spike_id, x = .data$x, y = .data$y_peak, 
                  .data$val, dplyr::everything()) %>%
    dplyr::select(-.data$x_left, -.data$x_right, -.data$y_base, -.data$y_peak)
  
  right_points <- spike_base %>%
    dplyr::select(.data$spike_id, x = .data$x_right, y = .data$y_base, 
                  .data$val, dplyr::everything()) %>%
    dplyr::select(-.data$x_left, -.data$x_right, -.data$y_base, -.data$y_peak)
  
  # Combine in triangle order (with closing point)
  result <- dplyr::bind_rows(
    left_points,
    peak_points,
    right_points,
    left_points  # Close the polygon
  ) %>%
    dplyr::arrange(.data$spike_id)
  
  result
}

#' Create spike line coordinates (legacy format)
#' @keywords internal
#' @noRd
.create_spike_lines <- function(spike_base, fact, spread) {
  
  spike_base %>%
    dplyr::mutate(
      x_left = .data$x - spread,
      x_mid = .data$x,
      x_right = .data$x + spread,
      y_left = .data$y,
      y_mid = .data$y + .data$val * fact,
      y_right = .data$y
    ) %>%
    tidyr::pivot_longer(
      cols = c(.data$x_left, .data$y_left, .data$x_mid, .data$y_mid, 
               .data$x_right, .data$y_right),
      names_to = "coord_type",
      values_to = "coord"
    ) %>%
    dplyr::mutate(
      axis = stringr::str_sub(.data$coord_type, 1, 1),
      side = stringr::str_sub(.data$coord_type, 3, -1)
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$axis,
      values_from = .data$coord
    ) %>%
    dplyr::select(-.data$coord_type)
}

#' Generate cartogram from sf object
#'
#' Creates cartograms where polygon sizes are distorted based on a weight variable.
#' Uses the cartogram package to create area or distance cartograms.
#'
#' @param sf_data Simple Features (sf) object with polygon geometries.
#' @param weight Variable name for cartogram weights (unquoted).
#' @param type Cartogram type: "cont" (continuous), "ncont" (non-contiguous), 
#'   or "dorling" (circles).
#' @param k Smoothing parameter for continuous cartograms (default: 0.05).
#' @param crs Target coordinate reference system. If NULL, uses data's CRS.
#' @param itermax Maximum iterations for cartogram algorithm (default: 15).
#' @param prepare_data Clean/prepare data before cartogram creation? (default: TRUE)
#' @param min_weight Minimum weight threshold - polygons below excluded.
#' @param verbose Print progress and diagnostic information? (default: FALSE)
#'
#' @return Simple Features (sf) object representing the cartogram.
#'
#' @details
#' Cartogram types:
#' \itemize{
#'   \item \strong{cont}: Continuous cartogram preserving topology
#'   \item \strong{ncont}: Non-contiguous (shapes don't touch)
#'   \item \strong{dorling}: Dorling cartogram (circles proportional to weight)
#' }
#'
#' Weight variable should be positive. Zero/negative values may cause errors.
#'
#' @examples
#' \dontrun{
#' # Load country data
#' countries <- euiss_gisco(res = "20") %>%
#'   slice_head(n = 10)
#' 
#' countries$population <- runif(nrow(countries), 1e6, 1e8)
#' 
#' # Continuous cartogram
#' cartogram_cont <- euiss_cartogram(
#'   countries,
#'   weight = population,
#'   type = "cont"
#' )
#' 
#' # Dorling cartogram (circles)
#' cartogram_dorling <- euiss_cartogram(
#'   countries,
#'   weight = population,
#'   type = "dorling"
#' )
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
    stop("`sf_data` must be a Simple Features (sf) object", call. = FALSE)
  }
  
  if (nrow(sf_data) == 0) {
    stop("`sf_data` must have at least one row", call. = FALSE)
  }
  
  weight_name <- deparse(substitute(weight))
  if (!weight_name %in% names(sf_data)) {
    stop(
      "Column '", weight_name, "' not found.\n",
      "Available columns: ", paste(names(sf_data), collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!type %in% c("cont", "ncont", "dorling")) {
    stop("`type` must be one of: 'cont', 'ncont', 'dorling'", call. = FALSE)
  }
  
  .validate_positive_number(k, "k", allow_zero = FALSE)
  .validate_positive_number(itermax, "itermax", allow_zero = FALSE)
  
  # Check cartogram package
  if (!requireNamespace("cartogram", quietly = TRUE)) {
    stop(
      "cartogram package required.\n",
      "Install with: install.packages('cartogram')",
      call. = FALSE
    )
  }
  
  # Prepare data
  if (prepare_data) {
    sf_data <- .prepare_cartogram_data(
      sf_data, 
      weight_name, 
      min_weight,
      verbose
    )
    
    if (nrow(sf_data) == 0) {
      stop("No valid data remaining after preparation", call. = FALSE)
    }
  }
  
  # Transform CRS if needed
  if (!is.null(crs)) {
    if (verbose) message("Transforming to CRS: ", crs)
    sf_data <- sf::st_transform(sf_data, crs)
  } else {
    # Use equal-area projection for better cartogram results
    current_crs <- sf::st_crs(sf_data)
    if (is.na(current_crs) || current_crs$epsg == 4326) {
      if (verbose) message("Converting to equal-area projection")
      sf_data <- sf::st_transform(sf_data, 3857)  # Web Mercator
    }
  }
  
  # Print weight statistics
  if (verbose) {
    weight_stats <- summary(sf_data[[weight_name]])
    message("Weight statistics:")
    print(weight_stats)
  }
  
  # Create cartogram
  cartogram_result <- .create_cartogram(
    sf_data, 
    weight_name, 
    type, 
    k, 
    itermax,
    verbose
  )
  
  # Report results
  if (verbose) {
    message("Cartogram creation successful!")
    
    if (type %in% c("cont", "ncont")) {
      original_area <- sum(sf::st_area(sf_data))
      cartogram_area <- sum(sf::st_area(cartogram_result))
      area_ratio <- as.numeric(cartogram_area / original_area)
      message("Area preservation ratio: ", round(area_ratio, 3))
    }
  }
  
  cartogram_result
}

#' Prepare data for cartogram creation
#' @keywords internal
#' @noRd
.prepare_cartogram_data <- function(sf_data, weight_name, min_weight, verbose) {
  
  if (verbose) message("Preparing data for cartogram...")
  
  original_nrow <- nrow(sf_data)
  
  # Remove NA weights
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
    if (verbose) message("Converting non-positive weights to 0.001")
    sf_data <- sf_data %>%
      dplyr::mutate(
        !!rlang::sym(weight_name) := pmax(.data[[weight_name]], 0.001)
      )
  }
  
  # Clean geometries
  sf_data <- sf_data %>%
    sf::st_make_valid()
  
  # Remove empty geometries
  empty_geoms <- sf::st_is_empty(sf_data)
  if (any(empty_geoms)) {
    sf_data <- sf_data[!empty_geoms, ]
    if (verbose) {
      message("Removed ", sum(empty_geoms), " empty geometries")
    }
  }
  
  sf_data
}

#' Create cartogram with error handling
#' @keywords internal
#' @noRd
.create_cartogram <- function(sf_data, weight_name, type, k, itermax, verbose) {
  
  if (verbose) {
    message("Creating ", type, " cartogram with ", nrow(sf_data), " features...")
  }
  
  tryCatch({
    switch(type,
           "cont" = cartogram::cartogram_cont(
             sf_data, 
             weight = weight_name, 
             itermax = itermax
           ),
           "ncont" = cartogram::cartogram_ncont(
             sf_data, 
             weight = weight_name, 
             k = k, 
             inplace = FALSE
           ),
           "dorling" = cartogram::cartogram_dorling(
             sf_data, 
             weight = weight_name, 
             k = k
           )
    )
  }, error = function(e) {
    stop(
      "Cartogram creation failed: ", e$message, "\n",
      "Try adjusting parameters or using prepare_data=TRUE",
      call. = FALSE
    )
  })
}

#' Create multiple cartogram variants for comparison
#'
#' Creates several cartogram variants using different methods and parameters.
#' Useful for exploring which approach works best for your data.
#'
#' @param sf_data Simple Features (sf) object.
#' @param weight Weight variable name (unquoted).
#' @param types Vector of cartogram types to create (default: c("cont", "dorling")).
#' @param k_values Vector of k values to test (default: c(0.05, 0.1, 0.2)).
#' @param save_results Save results to files? (default: FALSE)
#' @param output_dir Directory for saved results (default: "cartograms").
#' @param verbose Print progress? (default: TRUE)
#'
#' @return Named list of cartogram sf objects.
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
    message("Creating cartogram comparison:")
    message("  Types: ", paste(types, collapse = ", "))
    message("  K values: ", paste(k_values, collapse = ", "))
  }
  
  results <- list()
  
  for (type in types) {
    for (k in k_values) {
      
      result_name <- sprintf("%s_k%03d", type, as.integer(k * 100))
      
      if (verbose) message("\nCreating: ", result_name)
      
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
          message("  Failed: ", e$message)
        }
        NULL
      })
      
      if (!is.null(cartogram_result)) {
        results[[result_name]] <- cartogram_result
        
        # Save if requested
        if (save_results) {
          .save_cartogram_result(
            cartogram_result, 
            result_name, 
            output_dir,
            verbose
          )
        }
      }
    }
  }
  
  if (verbose) {
    message("\nComparison complete. Created ", length(results), " cartograms.")
  }
  
  results
}

#' Save cartogram result to file
#' @keywords internal
#' @noRd
.save_cartogram_result <- function(cartogram, name, output_dir, verbose) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    if (verbose) message("Created output directory: ", output_dir)
  }
  
  output_file <- file.path(output_dir, paste0(name, ".rds"))
  saveRDS(cartogram, output_file)
  
  if (verbose) message("  Saved: ", output_file)
  
  invisible(output_file)
}
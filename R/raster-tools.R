#' Raster Processing Tools
#'
#' Functions for loading, processing, and manipulating raster data for spatial analysis.
#'
#' @name raster-tools
NULL

#' Load raster file with enhanced error handling
#'
#' Loads raster files using terra package with comprehensive error handling,
#' validation, and optional processing steps.
#'
#' @param path Path to raster file (absolute or relative).
#' @param validate Validate raster after loading? (default: TRUE)
#' @param info Print raster information? (default: FALSE)
#' @param fallback_paths Alternative paths to try if primary fails.
#' @param ... Additional arguments passed to terra::rast().
#'
#' @return SpatRaster object from terra package.
#'
#' @details
#' Enhancements over terra::rast():
#' \itemize{
#'   \item Intelligent path resolution with fallbacks
#'   \item File existence validation before loading
#'   \item Optional raster validation
#'   \item Informative error messages
#' }
#'
#' @examples
#' \dontrun{
#' # Basic loading
#' raster <- euiss_load_raster("elevation.tif")
#'
#' # With fallback paths
#' raster <- euiss_load_raster(
#'   "elevation.tif",
#'   fallback_paths = c("./data/elevation.tif", "~/data/elevation.tif")
#' )
#'
#' # With information display
#' raster <- euiss_load_raster("elevation.tif", info = TRUE)
#' }
#'
#' @importFrom terra rast
#'
#' @export
euiss_load_raster <- function(path,
                              validate = TRUE,
                              info = FALSE,
                              fallback_paths = NULL,
                              ...) {
  # Input validation
  if (!is.character(path) || length(path) != 1) {
    stop("`path` must be a single character string", call. = FALSE)
  }
  
  # Check terra package
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(
      "terra package required for raster operations.\n",
      "Install with: install.packages('terra')",
      call. = FALSE
    )
  }
  
  # Try all paths
  paths_to_try <- c(path, fallback_paths)
  raster <- NULL
  successful_path <- NULL
  
  for (try_path in paths_to_try) {
    expanded_path <- path.expand(try_path)
    
    if (!file.exists(expanded_path)) {
      if (info)
        message("Path not found: ", expanded_path)
      next
    }
    
    # Try loading
    raster <- tryCatch({
      terra::rast(expanded_path, ...)
    }, error = function(e) {
      if (info)
        message("Failed to load ", expanded_path, ": ", e$message)
      NULL
    })
    
    if (!is.null(raster)) {
      successful_path <- expanded_path
      break
    }
  }
  
  # Check success
  if (is.null(raster)) {
    stop(
      "Failed to load raster from any provided path:\n",
      paste("  -", paths_to_try, collapse = "\n"),
      call. = FALSE
    )
  }
  
  if (info) {
    message("Loaded raster from: ", successful_path)
  }
  
  # Validate if requested
  if (validate) {
    validation <- euiss_validate_raster(raster, verbose = info)
    if (!validation$valid) {
      warning("Raster validation failed: ", validation$message, call. = FALSE)
    }
  }
  
  # Print info if requested
  if (info) {
    message("\nRaster Information:")
    print(raster)
  }
  
  raster
}

#' Crop and project raster to match spatial extent and CRS
#'
#' Crops raster to specified extent, projects to new CRS, and optionally
#' converts to data frame.
#'
#' @param raster SpatRaster object to crop and project.
#' @param crop_extent Extent to crop to (SpatExtent, sf object, or bbox).
#' @param crs Target coordinate reference system.
#' @param buffer Buffer distance around crop extent (in extent's CRS units).
#' @param aggregate_fact Aggregation factor for downsampling (default: 1).
#' @param to_dataframe Convert result to data frame? (default: FALSE)
#' @param normalize Normalize values to 0-1 range? (default: FALSE)
#' @param column_name Name for raster values column in df output (default: "value").
#' @param ... Additional arguments passed to terra functions.
#'
#' @return SpatRaster object or data frame with raster data.
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Apply buffer to crop extent (if specified)
#'   \item Crop raster to (buffered) extent
#'   \item Aggregate if factor > 1
#'   \item Project to target CRS
#'   \item Optionally normalize values
#'   \item Optionally convert to data frame
#' }
#'
#' @examples
#' \dontrun{
#' # Load data
#' raster <- euiss_load_raster("elevation.tif")
#' countries <- euiss_gisco(res = "20")
#' albania <- countries[countries$NAME_ENGL == "Albania", ]
#'
#' # Crop and project
#' albania_elev <- euiss_crop_raster(
#'   raster,
#'   crop_extent = albania,
#'   crs = "+proj=laea +lat_0=41 +lon_0=20",
#'   buffer = 10000
#' )
#'
#' # Convert to data frame for ggplot
#' albania_df <- euiss_crop_raster(
#'   raster,
#'   crop_extent = albania,
#'   crs = 3857,
#'   to_dataframe = TRUE,
#'   normalize = TRUE
#' )
#' }
#'
#' @import dplyr
#' @import sf
#' @importFrom terra crop project aggregate as.data.frame ext
#' @importFrom tibble as_tibble
#'
#' @export
euiss_crop_raster <- function(raster,
                              crop_extent,
                              crs,
                              buffer = 0,
                              aggregate_fact = 1,
                              to_dataframe = FALSE,
                              normalize = FALSE,
                              column_name = "value",
                              ...) {
  # Input validation
  if (!inherits(raster, "SpatRaster")) {
    stop("`raster` must be a SpatRaster object from terra", call. = FALSE)
  }
  
  if (missing(crop_extent)) {
    stop("`crop_extent` is required", call. = FALSE)
  }
  
  if (missing(crs)) {
    stop("`crs` is required", call. = FALSE)
  }
  
  .validate_positive_number(buffer, "buffer", allow_zero = TRUE)
  .validate_positive_number(aggregate_fact, "aggregate_fact", allow_zero = FALSE)
  
  if (aggregate_fact != round(aggregate_fact)) {
    stop("`aggregate_fact` must be an integer", call. = FALSE)
  }
  
  # Check terra package
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("terra package required", call. = FALSE)
  }
  
  # Process crop extent
  crop_extent <- .process_crop_extent(crop_extent, raster, buffer)
  
  # Crop raster
  raster_cropped <- tryCatch({
    terra::crop(raster, crop_extent, ...)
  }, error = function(e) {
    stop("Failed to crop raster: ", e$message, call. = FALSE)
  })
  
  # Aggregate if requested
  if (aggregate_fact > 1) {
    raster_cropped <- tryCatch({
      terra::aggregate(raster_cropped, fact = aggregate_fact, ...)
    }, error = function(e) {
      warning("Failed to aggregate raster: ", e$message, call. = FALSE)
      raster_cropped
    })
  }
  
  # Project to target CRS
  raster_projected <- tryCatch({
    terra::project(raster_cropped, crs, ...)
  }, error = function(e) {
    stop("Failed to project raster: ", e$message, call. = FALSE)
  })
  
  # Convert to data frame if requested
  if (to_dataframe) {
    return(.raster_to_dataframe(raster_projected, column_name, normalize))
  }
  
  raster_projected
}

#' Process crop extent to SpatExtent
#' @keywords internal
#' @noRd
.process_crop_extent <- function(crop_extent, raster, buffer) {
  # Convert sf to extent
  if (inherits(crop_extent, "sf")) {
    raster_crs <- terra::crs(raster)
    if (!is.na(raster_crs) &&
        sf::st_crs(crop_extent) != raster_crs) {
      crop_extent <- sf::st_transform(crop_extent, raster_crs)
    }
    
    bbox <- sf::st_bbox(crop_extent)
    crop_extent <- terra::ext(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"])
    
  } else if (!inherits(crop_extent, "SpatExtent")) {
    # Try to convert
    crop_extent <- tryCatch({
      terra::ext(crop_extent)
    }, error = function(e) {
      stop("Could not convert crop_extent to SpatExtent: ",
           e$message,
           call. = FALSE)
    })
  }
  
  # Apply buffer
  if (buffer > 0) {
    crop_extent <- terra::buffer(crop_extent, buffer)
  }
  
  crop_extent
}

#' Convert raster to data frame
#' @keywords internal
#' @noRd
.raster_to_dataframe <- function(raster_projected, column_name, normalize) {
  raster_df <- tryCatch({
    terra::as.data.frame(raster_projected, xy = TRUE)
  }, error = function(e) {
    stop("Failed to convert raster to data frame: ", e$message, call. = FALSE)
  })
  
  # Convert to tibble
  raster_df <- tibble::as_tibble(raster_df)
  
  # Rename value column
  value_cols <- setdiff(names(raster_df), c("x", "y"))
  
  if (length(value_cols) == 1) {
    names(raster_df)[names(raster_df) == value_cols[1]] <- column_name
  } else if (length(value_cols) > 1) {
    warning("Multiple raster bands found. Using first band as '",
            column_name,
            "'",
            call. = FALSE)
    names(raster_df)[names(raster_df) == value_cols[1]] <- column_name
  }
  
  # Normalize if requested
  if (normalize && column_name %in% names(raster_df)) {
    val_min <- min(raster_df[[column_name]], na.rm = TRUE)
    val_max <- max(raster_df[[column_name]], na.rm = TRUE)
    val_range <- val_max - val_min
    
    if (val_range > 0) {
      raster_df <- raster_df %>%
        dplyr::mutate(!!rlang::sym(column_name) := (.data[[column_name]] - val_min) / val_range)
    }
  }
  
  raster_df
}

#' Validate raster data quality and properties
#'
#' Performs comprehensive validation of raster data to identify potential issues.
#'
#' @param raster SpatRaster object to validate.
#' @param check_crs Check if CRS is defined? (default: TRUE)
#' @param check_values Check for valid data values? (default: TRUE)
#' @param check_extent Check if extent is reasonable? (default: TRUE)
#' @param verbose Print detailed validation results? (default: FALSE)
#'
#' @return List with validation results:
#'   \item{valid}{Logical indicating if raster passed validation}
#'   \item{message}{Summary message}
#'   \item{issues}{Character vector of critical issues}
#'   \item{warnings}{Character vector of warnings}
#'
#' @examples
#' \dontrun{
#' raster <- euiss_load_raster("elevation.tif")
#' validation <- euiss_validate_raster(raster, verbose = TRUE)
#'
#' if (!validation$valid) {
#'   warning("Raster validation failed: ", validation$message)
#' }
#' }
#'
#' @export
euiss_validate_raster <- function(raster,
                                  check_crs = TRUE,
                                  check_values = TRUE,
                                  check_extent = TRUE,
                                  verbose = FALSE) {
  if (!inherits(raster, "SpatRaster")) {
    return(
      list(
        valid = FALSE,
        message = "Object is not a SpatRaster",
        issues = "Not a SpatRaster",
        warnings = character(0)
      )
    )
  }
  
  issues <- character(0)
  warnings <- character(0)
  
  # Check CRS
  if (check_crs) {
    raster_crs <- terra::crs(raster)
    if (is.na(raster_crs) || raster_crs == "") {
      issues <- c(issues, "No coordinate reference system (CRS) defined")
    } else if (verbose) {
      message("CRS: ", raster_crs)
    }
  }
  
  # Check extent
  if (check_extent) {
    ext <- terra::ext(raster)
    if (ext[1] >= ext[2] || ext[3] >= ext[4]) {
      issues <- c(issues, "Invalid extent (xmin >= xmax or ymin >= ymax)")
    } else if (verbose) {
      message("Extent: xmin=",
              ext[1],
              " xmax=",
              ext[2],
              " ymin=",
              ext[3],
              " ymax=",
              ext[4])
    }
  }
  
  # Check values
  if (check_values) {
    n_cells <- terra::ncell(raster)
    
    if (n_cells == 0) {
      issues <- c(issues, "Raster has no cells")
    } else {
      # Sample values (efficient for large rasters)
      sample_size <- min(1000, n_cells)
      sample_values <- terra::spatSample(raster,
                                         sample_size,
                                         na.rm = FALSE,
                                         method = "random")
      
      if (all(is.na(sample_values))) {
        issues <- c(issues, "All sampled values are NA")
      } else {
        # Check NA proportion
        na_prop <- sum(is.na(sample_values)) / length(sample_values)
        if (na_prop > 0.9) {
          warnings <- c(warnings,
                        sprintf("High proportion of NA values (%.1f%%)", na_prop * 100))
        }
        
        # Check valid values
        valid_values <- sample_values[!is.na(sample_values)]
        if (length(valid_values) > 0) {
          if (verbose) {
            message("Value range: ",
                    min(valid_values),
                    " to ",
                    max(valid_values))
            message("NA proportion: ", round(na_prop * 100, 1), "%")
          }
          
          # Check for infinite values
          if (any(is.infinite(valid_values))) {
            issues <- c(issues, "Infinite values detected")
          }
        }
      }
    }
  }
  
  # Print basic properties
  if (verbose) {
    dims <- dim(raster)
    message("Dimensions: ", paste(dims, collapse = " x "))
    message("Resolution: ", paste(round(terra::res(raster), 6), collapse = " x "))
    message("Number of layers: ", terra::nlyr(raster))
  }
  
  # Compile results
  valid <- length(issues) == 0
  message_text <- if (valid) {
    "Raster validation passed"
  } else {
    paste("Validation issues:", paste(issues, collapse = "; "))
  }
  
  if (length(warnings) > 0 && verbose) {
    message("Warnings: ", paste(warnings, collapse = "; "))
  }
  
  list(
    valid = valid,
    message = message_text,
    issues = issues,
    warnings = warnings
  )
}

#' Create styled geom_raster layer for ggplot2
#'
#' Creates a styled geom_raster layer optimized for elevation and terrain
#' visualization with EUISS color schemes.
#'
#' @param data Raster data in data frame format (with x, y, value columns).
#' @param mapping Aesthetic mappings for the raster.
#' @param value_col Name of column containing raster values.
#' @param alpha Transparency level (default: 0.7).
#' @param colors Color palette. If NULL, uses EUISS terrain colors.
#' @param guide Guide type for legend (default: "colourbar").
#' @param ... Additional arguments passed to geom_raster.
#'
#' @return List of ggplot2 layers (geom_raster + scale_fill).
#'
#' @examples
#' \dontrun{
#' # Prepare raster data
#' raster_df <- euiss_crop_raster(
#'   my_raster,
#'   crop_extent = my_extent,
#'   crs = 3857,
#'   to_dataframe = TRUE
#' )
#'
#' # Create map
#' library(ggplot2)
#' ggplot() +
#'   euiss_geom_raster(raster_df, aes(x = x, y = y, fill = value)) +
#'   theme_void()
#' }
#'
#' @export
euiss_geom_raster <- function(data = NULL,
                              mapping = NULL,
                              value_col = "value",
                              alpha = 0.7,
                              colors = NULL,
                              guide = "colourbar",
                              ...) {
  # Get EUISS colors if not provided
  if (is.null(colors)) {
    colors <- .get_euiss_terrain_colors()
  }
  
  list(
    ggplot2::geom_raster(
      data = data,
      mapping = mapping,
      alpha = alpha,
      ...
    ),
    ggplot2::scale_fill_gradientn(
      colors = colors,
      guide = guide,
      name = "Elevation"
    )
  )
}

#' Get EUISS terrain colors
#' @keywords internal
#' @noRd
.get_euiss_terrain_colors <- function() {
  # Try euissR colors
  if (requireNamespace("euissR", quietly = TRUE)) {
    colors <- tryCatch({
      col_grid <- euissR::pal_seq_v_euiss(1)
      col_axis <- "#1D1D1B"
      c("white", "white", "white", col_grid, col_axis)
    }, error = function(e)
      NULL)
    
    if (!is.null(colors))
      return(colors)
  }
  
  # Fallback colors
  c("white", "#f0f0f0", "#d0d0d0", "#C6C6C6", "#1D1D1B")
}

#' Batch process multiple raster files
#'
#' Processes multiple raster files using same parameters. Useful for preparing
#' datasets with consistent processing.
#'
#' @param file_paths Vector of paths to raster files.
#' @param output_dir Directory to save processed rasters.
#' @param crop_extent Optional extent to crop all rasters to.
#' @param target_crs Target CRS for all rasters.
#' @param aggregate_fact Aggregation factor for downsampling.
#' @param file_suffix Suffix to add to output filenames (default: "_processed").
#' @param overwrite Overwrite existing output files? (default: FALSE)
#' @param verbose Print progress information? (default: TRUE)
#'
#' @return Character vector of output file paths (successful only).
#'
#' @examples
#' \dontrun{
#' # Process multiple elevation files
#' dem_files <- c("dem1.tif", "dem2.tif", "dem3.tif")
#'
#' processed_files <- euiss_batch_raster_process(
#'   dem_files,
#'   output_dir = "processed_dems",
#'   target_crs = 3857,
#'   aggregate_fact = 2,
#'   file_suffix = "_processed"
#' )
#' }
#'
#' @export
euiss_batch_raster_process <- function(file_paths,
                                       output_dir,
                                       crop_extent = NULL,
                                       target_crs = NULL,
                                       aggregate_fact = 1,
                                       file_suffix = "_processed",
                                       overwrite = FALSE,
                                       verbose = TRUE) {
  # Input validation
  if (length(file_paths) == 0) {
    stop("No file paths provided", call. = FALSE)
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    if (verbose)
      message("Created output directory: ", output_dir)
  }
  
  # Process each file
  output_paths <- character(length(file_paths))
  successful <- logical(length(file_paths))
  
  for (i in seq_along(file_paths)) {
    input_path <- file_paths[i]
    base_name <- tools::file_path_sans_ext(basename(input_path))
    output_filename <- paste0(base_name, file_suffix, ".tif")
    output_path <- file.path(output_dir, output_filename)
    
    output_paths[i] <- output_path
    
    if (verbose) {
      message("Processing ",
              i,
              "/",
              length(file_paths),
              ": ",
              basename(input_path))
    }
    
    # Skip if exists and not overwriting
    if (file.exists(output_path) && !overwrite) {
      if (verbose)
        message("  Skipping (exists): ", output_filename)
      successful[i] <- TRUE
      next
    }
    
    # Process raster
    success <- tryCatch({
      # Load
      raster <- euiss_load_raster(input_path, info = FALSE)
      
      # Crop if extent provided
      if (!is.null(crop_extent)) {
        raster <- terra::crop(raster, crop_extent)
      }
      
      # Aggregate if requested
      if (aggregate_fact > 1) {
        raster <- terra::aggregate(raster, fact = aggregate_fact)
      }
      
      # Project if CRS provided
      if (!is.null(target_crs)) {
        raster <- terra::project(raster, target_crs)
      }
      
      # Save
      terra::writeRaster(raster, output_path, overwrite = overwrite)
      
      if (verbose)
        message("  Success: ", output_filename)
      TRUE
      
    }, error = function(e) {
      if (verbose)
        message("  Failed: ", e$message)
      FALSE
    })
    
    successful[i] <- success
  }
  
  # Summary
  if (verbose) {
    success_count <- sum(successful)
    message("\nBatch complete: ",
            success_count,
            "/",
            length(file_paths),
            " successful")
  }
  
  output_paths[successful]
}
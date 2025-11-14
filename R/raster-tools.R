#' Raster Processing Tools
#' 
#' Functions for loading, processing, and manipulating raster data for spatial analysis.
#' 
#' @name raster-tools
NULL

#' Load a raster file with enhanced error handling
#'
#' This function loads raster files using the terra package with comprehensive
#' error handling, validation, and optional processing steps.
#'
#' @param path Path to the raster file. Can be absolute or relative.
#' @param validate Logical. Should the raster be validated after loading?
#' @param info Logical. Print raster information after loading?
#' @param fallback_paths Vector of alternative paths to try if primary path fails.
#' @param ... Additional arguments passed to terra::rast().
#'
#' @return A SpatRaster object from the terra package.
#'
#' @details
#' This function enhances the basic terra::rast() functionality by:
#' \itemize{
#'   \item Providing intelligent path resolution with fallbacks
#'   \item Validating file existence before attempting to load
#'   \item Optional validation of the loaded raster
#'   \item Informative error messages
#'   \item Optional raster information display
#' }
#'
#' @examples
#' \dontrun{
#' # Basic raster loading
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
    stop("`path` must be a single character string")
  }
  
  # Check if terra package is available
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("terra package is required for raster operations. Install with: install.packages('terra')")
  }
  
  # Build list of paths to try
  paths_to_try <- c(path)
  if (!is.null(fallback_paths)) {
    paths_to_try <- c(paths_to_try, fallback_paths)
  }
  
  # Try each path until one works
  raster <- NULL
  successful_path <- NULL
  
  for (try_path in paths_to_try) {
    
    # Expand path (handle ~, relative paths, etc.)
    expanded_path <- path.expand(try_path)
    
    # Check if file exists
    if (!file.exists(expanded_path)) {
      if (info) message("Path not found: ", expanded_path)
      next
    }
    
    # Try to load raster
    raster <- tryCatch({
      terra::rast(expanded_path, ...)
    }, error = function(e) {
      if (info) message("Failed to load ", expanded_path, ": ", e$message)
      NULL
    })
    
    if (!is.null(raster)) {
      successful_path <- expanded_path
      break
    }
  }
  
  # Check if loading was successful
  if (is.null(raster)) {
    stop("Failed to load raster from any of the provided paths:\n",
         paste("  -", paths_to_try, collapse = "\n"))
  }
  
  if (info) {
    message("Successfully loaded raster from: ", successful_path)
  }
  
  # Validate raster if requested
  if (validate) {
    validation_result <- euiss_validate_raster(raster, verbose = info)
    if (!validation_result$valid) {
      warning("Raster validation failed: ", validation_result$message)
    }
  }
  
  # Print raster information if requested
  if (info) {
    message("\nRaster Information:")
    print(raster)
  }
  
  return(raster)
}

#' Crop and project a raster to match spatial extent and CRS
#'
#' This function crops a raster to a specified extent, projects it to a new
#' coordinate reference system, and optionally converts it to a data frame.
#'
#' @param raster A SpatRaster object to be cropped and projected.
#' @param crop_extent Extent to crop to. Can be SpatExtent, sf object, or bbox.
#' @param crs Target coordinate reference system for projection.
#' @param buffer Buffer distance around crop extent (in units of crop_extent's CRS).
#' @param aggregate_fact Aggregation factor for downsampling (for performance).
#' @param to_dataframe Logical. Convert result to data frame?
#' @param normalize Logical. Normalize values to 0-1 range?
#' @param column_name Name for raster values column in data frame output.
#' @param ... Additional arguments passed to terra functions.
#'
#' @return Either a SpatRaster object or a tibble/data frame with raster data.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Applies buffer to crop extent if specified
#'   \item Crops raster to the (buffered) extent
#'   \item Aggregates raster if aggregation factor > 1
#'   \item Projects to target CRS
#'   \item Optionally normalizes values
#'   \item Optionally converts to data frame format
#' }
#'
#' @examples
#' \dontrun{
#' # Load sample data
#' raster <- euiss_load_raster("elevation.tif")
#' countries <- euiss_gisco(res = "20")
#' albania <- countries[countries$NAME_ENGL == "Albania", ]
#' 
#' # Crop and project
#' albania_elevation <- euiss_crop_raster(
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
#' @importFrom terra crop project aggregate as.data.frame ext buffer
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
    stop("`raster` must be a SpatRaster object from terra package")
  }
  
  if (missing(crop_extent)) {
    stop("`crop_extent` is required")
  }
  
  if (missing(crs)) {
    stop("`crs` is required")
  }
  
  # Check terra package
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("terra package required for raster operations")
  }
  
  # Process crop extent
  if (inherits(crop_extent, "sf")) {
    # If sf object, ensure it's in same CRS as raster for cropping
    raster_crs <- terra::crs(raster)
    if (!is.na(raster_crs) && sf::st_crs(crop_extent) != raster_crs) {
      crop_extent <- sf::st_transform(crop_extent, raster_crs)
    }
    
    # Convert sf to terra extent
    bbox <- sf::st_bbox(crop_extent)
    crop_extent <- terra::ext(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"])
    
  } else if (!inherits(crop_extent, "SpatExtent")) {
    # Try to convert to SpatExtent
    crop_extent <- tryCatch({
      terra::ext(crop_extent)
    }, error = function(e) {
      stop("Could not convert crop_extent to SpatExtent: ", e$message)
    })
  }
  
  # Apply buffer if specified
  if (buffer > 0) {
    crop_extent <- terra::buffer(crop_extent, buffer)
  }
  
  # Crop raster
  raster_cropped <- tryCatch({
    terra::crop(raster, crop_extent, ...)
  }, error = function(e) {
    stop("Failed to crop raster: ", e$message)
  })
  
  # Aggregate if requested (for performance)
  if (aggregate_fact > 1) {
    raster_cropped <- tryCatch({
      terra::aggregate(raster_cropped, fact = aggregate_fact, ...)
    }, error = function(e) {
      warning("Failed to aggregate raster: ", e$message)
      raster_cropped  # Continue without aggregation
    })
  }
  
  # Project to target CRS
  raster_projected <- tryCatch({
    terra::project(raster_cropped, crs, ...)
  }, error = function(e) {
    stop("Failed to project raster: ", e$message)
  })
  
  # Convert to data frame if requested
  if (to_dataframe) {
    
    raster_df <- tryCatch({
      terra::as.data.frame(raster_projected, xy = TRUE)
    }, error = function(e) {
      stop("Failed to convert raster to data frame: ", e$message)
    })
    
    # Convert to tibble
    raster_df <- tibble::as_tibble(raster_df)
    
    # Rename value column if it exists
    value_cols <- names(raster_df)[!names(raster_df) %in% c("x", "y")]
    if (length(value_cols) == 1) {
      names(raster_df)[names(raster_df) == value_cols[1]] <- column_name
    } else if (length(value_cols) > 1) {
      warning("Multiple raster bands found. Using first band as '", column_name, "'")
      names(raster_df)[names(raster_df) == value_cols[1]] <- column_name
      # Keep other bands with original names
    }
    
    # Normalize if requested
    if (normalize && column_name %in% names(raster_df)) {
      raster_df <- raster_df %>%
        dplyr::mutate(
          !!column_name := (.data[[column_name]] - min(.data[[column_name]], na.rm = TRUE)) /
                          (max(.data[[column_name]], na.rm = TRUE) - min(.data[[column_name]], na.rm = TRUE))
        )
    }
    
    return(raster_df)
  }
  
  return(raster_projected)
}

#' Validate raster data quality and properties
#'
#' This function performs comprehensive validation of raster data to identify
#' potential issues before processing.
#'
#' @param raster A SpatRaster object to validate.
#' @param check_crs Logical. Check if CRS is defined?
#' @param check_values Logical. Check for valid data values?
#' @param check_extent Logical. Check if extent is reasonable?
#' @param verbose Logical. Print detailed validation results?
#'
#' @return List with validation results and diagnostic information.
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
    return(list(valid = FALSE, message = "Object is not a SpatRaster"))
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
      message("Extent: ", paste(as.vector(ext), collapse = ", "))
    }
  }
  
  # Check values
  if (check_values) {
    if (terra::ncell(raster) == 0) {
      issues <- c(issues, "Raster has no cells")
    } else {
      # Sample values for efficiency on large rasters
      sample_size <- min(1000, terra::ncell(raster))
      sample_values <- terra::spatSample(raster, sample_size, na.rm = FALSE)
      
      if (all(is.na(sample_values))) {
        issues <- c(issues, "All sampled values are NA")
      } else {
        na_prop <- sum(is.na(sample_values)) / length(sample_values)
        if (na_prop > 0.9) {
          warnings <- c(warnings, paste0("High proportion of NA values (", round(na_prop*100, 1), "%)"))
        }
        
        valid_values <- sample_values[!is.na(sample_values)]
        if (length(valid_values) > 0) {
          if (verbose) {
            message("Value range: ", min(valid_values), " to ", max(valid_values))
            message("NA proportion: ", round(na_prop*100, 1), "%")
          }
          
          # Check for infinite values
          if (any(is.infinite(valid_values))) {
            issues <- c(issues, "Infinite values detected")
          }
        }
      }
    }
  }
  
  # Basic properties
  if (verbose) {
    message("Dimensions: ", paste(dim(raster), collapse = " x "))
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
  
  return(list(
    valid = valid,
    message = message_text,
    issues = issues,
    warnings = warnings
  ))
}

#' Create a geom_raster layer with EUISS styling
#'
#' This function creates a styled geom_raster layer optimized for elevation
#' and terrain visualization with EUISS color schemes.
#'
#' @param data Raster data in data frame format (with x, y, and value columns).
#' @param mapping Aesthetic mappings for the raster.
#' @param value_col Name of the column containing raster values.
#' @param alpha Transparency level for the raster layer.
#' @param colors Color palette for raster values. If NULL, uses EUISS terrain colors.
#' @param guide Guide type for the legend.
#' @param ... Additional arguments passed to geom_raster.
#'
#' @return A ggplot2 geom_raster layer with styling.
#'
#' @examples
#' \dontrun{
#' # Load and prepare data
#' raster_df <- euiss_crop_raster(
#'   my_raster, 
#'   crop_extent = my_extent,
#'   crs = 3857,
#'   to_dataframe = TRUE
#' )
#' 
#' # Create map
#' ggplot() +
#'   euiss_geom_raster(data = raster_df, aes(x = x, y = y, fill = value)) +
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
    if (requireNamespace("euissR", quietly = TRUE)) {
      try({
        col_grid <- euissR::pal_seq_v_euiss(1)
        col_axis <- "#1D1D1B"
        colors <- c("white", "white", "white", col_grid, col_axis)
      }, silent = TRUE)
    }
    
    # Fallback colors
    if (is.null(colors)) {
      colors <- c("white", "#f0f0f0", "#d0d0d0", "#C6C6C6", "#1D1D1B")
    }
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

#' Batch process multiple raster files
#'
#' This function processes multiple raster files using the same parameters,
#' useful for preparing datasets with consistent processing.
#'
#' @param file_paths Vector of paths to raster files.
#' @param output_dir Directory to save processed rasters.
#' @param crop_extent Optional extent to crop all rasters to.
#' @param target_crs Target CRS for all rasters.
#' @param aggregate_fact Aggregation factor for downsampling.
#' @param file_suffix Suffix to add to output filenames.
#' @param overwrite Logical. Overwrite existing output files?
#' @param verbose Logical. Print progress information?
#'
#' @return Character vector of output file paths.
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
    stop("No file paths provided")
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    if (verbose) message("Created output directory: ", output_dir)
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
      message("Processing ", i, "/", length(file_paths), ": ", basename(input_path))
    }
    
    # Check if output exists and skip if not overwriting
    if (file.exists(output_path) && !overwrite) {
      if (verbose) message("  Skipping (output exists): ", output_filename)
      successful[i] <- TRUE
      next
    }
    
    # Process raster
    tryCatch({
      
      # Load raster
      raster <- euiss_load_raster(input_path, info = FALSE)
      
      # Apply processing steps
      if (!is.null(crop_extent)) {
        raster <- terra::crop(raster, crop_extent)
      }
      
      if (aggregate_fact > 1) {
        raster <- terra::aggregate(raster, fact = aggregate_fact)
      }
      
      if (!is.null(target_crs)) {
        raster <- terra::project(raster, target_crs)
      }
      
      # Save processed raster
      terra::writeRaster(raster, output_path, overwrite = overwrite)
      
      successful[i] <- TRUE
      if (verbose) message("  Success: ", output_filename)
      
    }, error = function(e) {
      if (verbose) message("  Failed: ", e$message)
      successful[i] <- FALSE
    })
  }
  
  # Summary
  if (verbose) {
    success_count <- sum(successful)
    message("\nBatch processing complete: ", success_count, "/", length(file_paths), " successful")
  }
  
  return(output_paths[successful])
}

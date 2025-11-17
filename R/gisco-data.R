#' GISCO Data Functions
#' 
#' Functions for retrieving and managing Eurostat GISCO geographic data.
#' 
#' @name gisco-data
NULL

#' Retrieve geographic data from Eurostat GISCO service
#'
#' Fetches geographic data from Eurostat GISCO service. Data is retrieved from
#' local shapefile archives with support for different spatial types, resolutions,
#' years, projections, boundary types, and country subsets.
#'
#' @param res Resolution of map data: "60", "20", "10", "03", "01" (million scale).
#' @param verbose Print additional information during execution? (default: TRUE)
#' @param type Type of geographic data (default: "countries").
#' @param shp Shape of data: "RG" (regions), "BN" (boundaries), "LB" (labels).
#' @param bordertype Boundary type for "BN" shape: "INLAND" or "COASTL".
#' @param year NUTS regulation year: "2024", "2020", "2016", "2013", "2010", "2006", "2003".
#' @param crs Coordinate reference system: "4326" (WGS84), "3035" (ETRS89), "3857" (Web Mercator).
#' @param data_path Base path for GISCO data. If NULL, uses configured paths.
#'
#' @return Simple Features (sf) object with requested geographic data.
#'
#' @details
#' Expects GISCO data to be downloaded and available locally. Data structure
#' should follow GISCO naming conventions:
#' \code{ref-countries-YYYY-SSm.shp.zip}
#' 
#' Use \code{euiss_check_gisco_data()} to verify available data files.
#'
#' @examples
#' \dontrun{
#' # Retrieve countries at 20M resolution
#' countries <- euiss_gisco(res = "20", verbose = TRUE)
#' 
#' # Get country boundaries only
#' boundaries <- euiss_gisco(res = "10", shp = "BN", bordertype = "COASTL")
#' 
#' # Get country labels/centroids
#' labels <- euiss_gisco(res = "20", shp = "LB")
#' }
#'
#' @import dplyr
#' @import sf
#' @importFrom utils unzip
#'
#' @export
euiss_gisco <- function(res = "60",
                        verbose = TRUE,
                        type = "countries",
                        shp = "RG",
                        bordertype = "INLAND",
                        year = "2024",
                        crs = "4326",
                        data_path = NULL) {
  
  # Input validation
  valid_res <- c("01", "03", "10", "20", "60")
  if (!res %in% valid_res) {
    stop("`res` must be one of: ", paste(valid_res, collapse = ", "), call. = FALSE)
  }
  
  valid_shp <- c("RG", "BN", "LB")
  if (!shp %in% valid_shp) {
    stop("`shp` must be one of: ", paste(valid_shp, collapse = ", "), call. = FALSE)
  }
  
  valid_crs <- c("3035", "3857", "4326")
  if (!crs %in% valid_crs) {
    stop("`crs` must be one of: ", paste(valid_crs, collapse = ", "), call. = FALSE)
  }
  
  valid_years <- c("2024", "2020", "2016", "2013", "2010", "2006", "2003")
  if (!year %in% valid_years) {
    stop("`year` must be one of: ", paste(valid_years, collapse = ", "), call. = FALSE)
  }
  
  if (shp == "BN") {
    valid_bordertype <- c("INLAND", "COASTL")
    if (!bordertype %in% valid_bordertype) {
      stop("`bordertype` must be one of: ", paste(valid_bordertype, collapse = ", "), call. = FALSE)
    }
  }
  
  # Get data path
  if (is.null(data_path)) {
    paths <- euiss_configure_paths(verbose = FALSE)
    data_path <- paths$gisco
  }
  
  if (!dir.exists(data_path)) {
    stop(
      "GISCO data directory not found: ", data_path, "\n",
      "Use euiss_configure_paths() to set up data directories.",
      call. = FALSE
    )
  }
  
  # Build file paths
  path_zip <- if (type == "communes") "COMM" else "CNTR"
  year_adjusted <- if (type == "communes") "2016" else year
  res_adjusted <- if (type == "communes") "01" else res
  
  # Construct outer zip filename
  zip_filename <- sprintf("ref-%s-%s-%sm.shp.zip", type, year_adjusted, res_adjusted)
  zip_path <- file.path(data_path, zip_filename)
  
  # Check if file exists
  if (!file.exists(zip_path)) {
    available_files <- list.files(data_path, pattern = "\\.zip$")
    
    if (length(available_files) == 0) {
      stop(
        "No GISCO data files found in: ", data_path, "\n",
        "Download GISCO data first or check path configuration.",
        call. = FALSE
      )
    } else {
      stop(
        "GISCO data file not found: ", zip_filename, "\n",
        "Available files: ", paste(available_files, collapse = ", "), "\n",
        "Check parameters or download required data.",
        call. = FALSE
      )
    }
  }
  
  if (verbose) {
    message("Loading GISCO ", type, " data: ", zip_filename)
  }
  
  # Load data with nested zip handling
  data <- .load_gisco_nested_zip(
    zip_path, 
    path_zip, 
    shp, 
    res_adjusted, 
    year_adjusted, 
    crs, 
    bordertype,
    verbose
  )
  
  if (verbose) {
    message(
      "Successfully loaded: ", nrow(data), " features at ",
      res, "M resolution"
    )
  }
  
  data
}

#' Load GISCO data from nested zip structure
#' @keywords internal
#' @noRd
.load_gisco_nested_zip <- function(outer_zip, path_zip, shp, res, year, crs, 
                                   bordertype, verbose) {
  
  # Create temporary directories
  temp_outer <- tempfile()
  temp_inner <- tempfile()
  
  on.exit({
    unlink(temp_outer, recursive = TRUE)
    unlink(temp_inner, recursive = TRUE)
  }, add = TRUE)
  
  # Extract outer zip
  tryCatch({
    utils::unzip(outer_zip, exdir = temp_outer)
  }, error = function(e) {
    stop(
      "Failed to extract outer zip: ", basename(outer_zip), "\n",
      "Error: ", e$message,
      call. = FALSE
    )
  })
  
  # Build inner zip filename
  inner_filename <- .build_inner_filename(path_zip, shp, res, year, crs, bordertype)
  inner_zip_path <- file.path(temp_outer, inner_filename)
  
  # Check inner zip exists
  if (!file.exists(inner_zip_path)) {
    available_inner <- list.files(temp_outer, pattern = "\\.zip$")
    stop(
      "Inner zip not found: ", inner_filename, "\n",
      "Available in archive: ", paste(available_inner, collapse = ", "),
      call. = FALSE
    )
  }
  
  # Extract inner zip
  tryCatch({
    utils::unzip(inner_zip_path, exdir = temp_inner)
  }, error = function(e) {
    stop(
      "Failed to extract inner zip: ", inner_filename, "\n",
      "Error: ", e$message,
      call. = FALSE
    )
  })
  
  # Read shapefile
  tryCatch({
    sf::st_read(temp_inner, quiet = !verbose)
  }, error = function(e) {
    stop(
      "Failed to read shapefile from: ", temp_inner, "\n",
      "Error: ", e$message,
      call. = FALSE
    )
  })
}

#' Build inner zip filename based on shape type
#' @keywords internal
#' @noRd
.build_inner_filename <- function(path_zip, shp, res, year, crs, bordertype) {
  
  if (shp == "BN") {
    sprintf("%s_%s_%sM_%s_%s_%s.shp.zip", 
            path_zip, shp, res, year, crs, bordertype)
  } else if (shp == "LB") {
    sprintf("%s_%s_%s_%s.shp.zip", 
            path_zip, shp, year, crs)
  } else if (shp == "RG") {
    sprintf("%s_%s_%sM_%s_%s.shp.zip", 
            path_zip, shp, res, year, crs)
  } else {
    stop("Unknown shape type: ", shp, call. = FALSE)
  }
}

#' Check available GISCO data files
#' 
#' Scans GISCO data directory and reports available data files and specifications.
#' 
#' @param data_path GISCO data path. If NULL, uses configured paths.
#' @param detailed Return detailed file information? (default: FALSE)
#' 
#' @return If detailed=FALSE, character vector of filenames.
#'         If detailed=TRUE, data frame with parsed file information.
#' 
#' @examples
#' \dontrun{
#' # Check available data
#' euiss_check_gisco_data()
#' 
#' # Get detailed information
#' available_data <- euiss_check_gisco_data(detailed = TRUE)
#' }
#' 
#' @export
euiss_check_gisco_data <- function(data_path = NULL, detailed = FALSE) {
  
  if (is.null(data_path)) {
    paths <- euiss_configure_paths(verbose = FALSE)
    data_path <- paths$gisco
  }
  
  if (!dir.exists(data_path)) {
    stop("GISCO data directory not found: ", data_path, call. = FALSE)
  }
  
  # Find GISCO zip files
  gisco_files <- list.files(
    data_path, 
    pattern = "^ref-.+\\.shp\\.zip$", 
    full.names = FALSE
  )
  
  if (length(gisco_files) == 0) {
    message("No GISCO data files found in: ", data_path)
    return(character(0))
  }
  
  if (!detailed) {
    return(gisco_files)
  }
  
  # Parse filenames: ref-TYPE-YEAR-RESOLUTIONm.shp.zip
  file_info <- data.frame(
    filename = gisco_files,
    stringsAsFactors = FALSE
  )
  
  parsed <- stringr::str_match(
    gisco_files, 
    "^ref-([^-]+)-([0-9]{4})-([0-9]{2})m\\.shp\\.zip$"
  )
  
  file_info$type <- parsed[, 2]
  file_info$year <- parsed[, 3]
  file_info$resolution <- parsed[, 4]
  
  # Add file sizes
  full_paths <- file.path(data_path, gisco_files)
  file_info$file_size <- vapply(full_paths, function(f) {
    .format_file_size(file.info(f)$size)
  }, character(1))
  
  file_info
}

#' Download GISCO data from Eurostat (REMOVED - Not Implemented)
#'
#' This function has been removed as automated downloading was not implemented.
#'
#' To download GISCO data:
#' \enumerate{
#'   \item Visit: \url{https://gisco-services.ec.europa.eu/distribution/v2/countries/}
#'   \item Download desired resolution files (e.g., ref-countries-2024-20m.shp.zip)
#'   \item Save to your GISCO data directory
#' }
#'
#' Check your data directory with: \code{euiss_get_config()$paths$gisco}
#'
#' @param ... All parameters (ignored)
#' @return Error with download instructions
#' @export
euiss_download_gisco <- function(...) {
  
  paths <- euiss_configure_paths(verbose = FALSE)
  
  stop(
    "Automated download not implemented.\n\n",
    "Manual download instructions:\n",
    "1. Visit: https://gisco-services.ec.europa.eu/distribution/v2/countries/\n",
    "2. Download desired files (e.g., ref-countries-2024-20m.shp.zip)\n",
    "3. Save to: ", paths$gisco, "\n\n",
    "Check available data with: euiss_check_gisco_data()",
    call. = FALSE
  )
}

#' Create Kosovo-compatible GISCO data
#' 
#' Creates a version of GISCO countries data that includes Kosovo as a separate
#' entity, extracted from Serbia's boundaries using Natural Earth data.
#' 
#' @param res GISCO resolution to use (default: "10").
#' @param year GISCO year to use (default: "2024").
#' @param data_path GISCO data path. If NULL, uses configured paths.
#' @param save_file Optional filename to save result (as .rds).
#' 
#' @return sf object with Kosovo-compatible countries data.
#' 
#' @examples
#' \dontrun{
#' # Create Kosovo-compatible data
#' countries_kosovo <- euiss_gisco_kosovo(res = "10")
#' 
#' # Save for future use
#' countries_kosovo <- euiss_gisco_kosovo(
#'   res = "10",
#'   save_file = "countries_2024_10m_kosovo.rds"
#' )
#' }
#' 
#' @export
euiss_gisco_kosovo <- function(res = "10", 
                               year = "2024", 
                               data_path = NULL, 
                               save_file = NULL) {
  
  # Check dependencies
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    stop(
      "rnaturalearth package required for Kosovo processing.\n",
      "Install with: install.packages('rnaturalearth')",
      call. = FALSE
    )
  }
  
  # Get Kosovo from Natural Earth
  kosovo_ne <- tryCatch({
    rnaturalearth::ne_countries(scale = 10, country = "Kosovo", returnclass = "sf") %>%
      dplyr::select(
        NAME_ENGL = .data$name,
        ISO3_CODE = .data$adm0_a3,
        .data$geometry
      )
  }, error = function(e) {
    stop("Failed to load Kosovo from Natural Earth: ", e$message, call. = FALSE)
  })
  
  # Load GISCO countries
  gisco_countries <- euiss_gisco(
    res = res, 
    year = year, 
    data_path = data_path, 
    verbose = FALSE
  )
  
  # Check if Serbia exists
  if (!"NAME_ENGL" %in% names(gisco_countries)) {
    stop("Expected column 'NAME_ENGL' not found in GISCO data", call. = FALSE)
  }
  
  if (!any(gisco_countries$NAME_ENGL == "Serbia")) {
    warning("Serbia not found in GISCO data. Returning original data with Kosovo added.", call. = FALSE)
    
    # Just add Kosovo without subtracting from Serbia
    countries_with_kosovo <- dplyr::bind_rows(
      gisco_countries %>% dplyr::select(.data$NAME_ENGL, .data$ISO3_CODE, .data$geometry),
      kosovo_ne
    )
    
  } else {
    
    # Extract Serbia
    serbia <- gisco_countries %>%
      dplyr::filter(.data$NAME_ENGL == "Serbia") %>%
      dplyr::select(.data$NAME_ENGL, .data$ISO3_CODE, .data$geometry)
    
    other_countries <- gisco_countries %>%
      dplyr::filter(.data$NAME_ENGL != "Serbia") %>%
      dplyr::select(.data$NAME_ENGL, .data$ISO3_CODE, .data$geometry)
    
    # Subtract Kosovo from Serbia
    serbia_minus_kosovo <- tryCatch({
      sf::st_difference(serbia, kosovo_ne)
    }, error = function(e) {
      warning(
        "Failed to subtract Kosovo from Serbia: ", e$message, "\n",
        "Returning Serbia unchanged.",
        call. = FALSE
      )
      serbia
    })
    
    # Combine all countries
    countries_with_kosovo <- dplyr::bind_rows(
      other_countries,
      serbia_minus_kosovo,
      kosovo_ne
    )
  }
  
  # Save if requested
  if (!is.null(save_file)) {
    if (is.null(data_path)) {
      paths <- euiss_configure_paths(verbose = FALSE)
      data_path <- paths$gisco
    }
    
    save_path <- file.path(data_path, save_file)
    saveRDS(countries_with_kosovo, save_path)
    message("Saved Kosovo-compatible data to: ", save_path)
  }
  
  message(
    "Created dataset with ", nrow(countries_with_kosovo),
    " countries including Kosovo as separate entity"
  )
  
  countries_with_kosovo
}
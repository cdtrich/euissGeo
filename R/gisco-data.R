#' GISCO Data Functions
#' 
#' Functions for retrieving and managing Eurostat GISCO geographic data.
#' 
#' @name gisco-data
NULL

#' Retrieve geographic data from Eurostat GISCO service
#'
#' This function fetches geographic data from the Eurostat GISCO service based on specified parameters.
#' The data is retrieved from local shapefile archives with support for different spatial types, 
#' resolutions, years, projections, boundary types, and country subsets.
#'
#' @param res Resolution of the map data. Options: "60", "20", "10", "03", "01" (in million scale).
#' @param verbose Logical indicating whether to print additional information during execution.
#' @param type Type of geographic data. Currently, only "countries" is supported.
#' @param shp Shape of the data. Options: "RG" (regions), "BN" (boundaries), "LB" (labels).
#' @param bordertype Type of boundary for "BN" shape. Options: "INLAND" or "COASTL".
#' @param year Year of the NUTS regulation. Options: "2024", "2020", "2016", "2013", "2010", "2006", "2003".
#' @param crs Coordinate reference system. Options: "4326" (WGS84), "3035" (ETRS 1989), "3857" (Web Mercator).
#' @param data_path Base path for GISCO data. If NULL, uses configured paths from euiss_configure_paths().
#'
#' @return A Simple Features (sf) object containing the requested geographic data.
#'
#' @details
#' This function expects GISCO data to be downloaded and available locally. The data structure
#' should follow GISCO's naming conventions:
#' \code{ref-countries-YYYY-SSm.shp.zip} where YYYY is year and SS is resolution.
#' 
#' Use \code{euiss_check_gisco_data()} to verify available data files.
#'
#' @examples
#' \dontrun{
#' # Retrieve countries at 20M resolution for 2024 in WGS84
#' countries <- euiss_gisco(res = "20", verbose = TRUE)
#' 
#' # Get country boundaries only
#' boundaries <- euiss_gisco(res = "10", shp = "BN", bordertype = "COASTL")
#' 
#' # Get country labels/centroids
#' labels <- euiss_gisco(res = "20", shp = "LB")
#' }
#'
#' @import stringr
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
    stop("res must be one of: ", paste(valid_res, collapse = ", "))
  }
  
  valid_shp <- c("RG", "BN", "LB")
  if (!shp %in% valid_shp) {
    stop("shp must be one of: ", paste(valid_shp, collapse = ", "))
  }
  
  valid_crs <- c("3035", "3857", "4326")
  if (!crs %in% valid_crs) {
    stop("crs must be one of: ", paste(valid_crs, collapse = ", "))
  }
  
  valid_years <- c("2024", "2020", "2016", "2013", "2010", "2006", "2003")
  if (!year %in% valid_years) {
    stop("year must be one of: ", paste(valid_years, collapse = ", "))
  }
  
  valid_bordertype <- c("INLAND", "COASTL")
  if (shp == "BN" && !bordertype %in% valid_bordertype) {
    stop("bordertype must be one of: ", paste(valid_bordertype, collapse = ", "))
  }
  
  # Get data path
  if (is.null(data_path)) {
    paths <- euiss_configure_paths(verbose = FALSE)
    data_path <- paths$gisco
  }
  
  if (!dir.exists(data_path)) {
    stop("GISCO data directory not found: ", data_path, "\n",
         "Use euiss_configure_paths() to set up data directories.")
  }
  
  # Build file paths
  path_zip <- ifelse(type == "communes", "COMM", "CNTR")
  year_adjusted <- ifelse(type == "communes", "2016", year)
  res_adjusted <- ifelse(type == "communes", "01", res)
  
  # Construct outer zip filename
  zip_filename <- paste0("ref-", type, "-", year_adjusted, "-", res_adjusted, "m.shp.zip")
  zip_path <- file.path(data_path, zip_filename)
  
  # Check if file exists
  if (!file.exists(zip_path)) {
    available_files <- list.files(data_path, pattern = "\\.zip$")
    if (length(available_files) == 0) {
      stop("No GISCO data files found in: ", data_path, "\n",
           "Download GISCO data first using euiss_download_gisco() or manually.")
    } else {
      stop("GISCO data file not found: ", zip_filename, "\n",
           "Available files: ", paste(available_files, collapse = ", "), "\n",
           "Check parameters or download required data.")
    }
  }
  
  if (verbose) {
    message("Loading GISCO ", type, " data: ", zip_filename)
  }
  
  # Create temporary directories
  temp_outer <- tempfile()
  temp_inner <- tempfile()
  
  # Ensure cleanup on exit
  on.exit({
    unlink(temp_outer, recursive = TRUE)
    unlink(temp_inner, recursive = TRUE)
  })
  
  # Extract outer zip
  tryCatch({
    utils::unzip(zip_path, exdir = temp_outer)
  }, error = function(e) {
    stop("Failed to extract outer zip file: ", zip_filename, "\nError: ", e$message)
  })
  
  # Build inner zip filename based on shape type
  if (shp == "BN") {
    inner_filename <- paste0(path_zip, "_", shp, "_", res_adjusted, "M_", 
                            year_adjusted, "_", crs, "_", bordertype, ".shp.zip")
  } else if (shp == "LB") {
    inner_filename <- paste0(path_zip, "_", shp, "_", year_adjusted, "_", crs, ".shp.zip")
  } else if (shp == "RG") {
    inner_filename <- paste0(path_zip, "_", shp, "_", res_adjusted, "M_", 
                            year_adjusted, "_", crs, ".shp.zip")
  }
  
  inner_zip_path <- file.path(temp_outer, inner_filename)
  
  # Check if inner zip exists
  if (!file.exists(inner_zip_path)) {
    available_inner <- list.files(temp_outer, pattern = "\\.zip$")
    stop("Inner zip file not found: ", inner_filename, "\n",
         "Available files in archive: ", paste(available_inner, collapse = ", "))
  }
  
  # Extract inner zip
  tryCatch({
    utils::unzip(inner_zip_path, exdir = temp_inner)
  }, error = function(e) {
    stop("Failed to extract inner zip file: ", inner_filename, "\nError: ", e$message)
  })
  
  # Read shapefile
  tryCatch({
    data <- sf::st_read(temp_inner, quiet = !verbose)
  }, error = function(e) {
    stop("Failed to read shapefile from: ", temp_inner, "\nError: ", e$message)
  })
  
  if (verbose) {
    message("Successfully loaded GISCO ", type, " data: ", 
           nrow(data), " features at ", res, "M resolution")
  }
  
  return(data)
}

#' Check available GISCO data files
#' 
#' This function scans the GISCO data directory and reports available data files
#' and their specifications.
#' 
#' @param data_path GISCO data path. If NULL, uses configured paths.
#' @param detailed Logical. Return detailed file information?
#' 
#' @return If detailed=FALSE, returns character vector of filenames. 
#'         If detailed=TRUE, returns data frame with parsed file information.
#' 
#' @examples
#' \dontrun{
#' # Check what data is available
#' euiss_check_gisco_data()
#' 
#' # Get detailed information
#' available_data <- euiss_check_gisco_data(detailed = TRUE)
#' print(available_data)
#' }
#' 
#' @export
euiss_check_gisco_data <- function(data_path = NULL, detailed = FALSE) {
  
  if (is.null(data_path)) {
    paths <- euiss_configure_paths(verbose = FALSE)
    data_path <- paths$gisco
  }
  
  if (!dir.exists(data_path)) {
    stop("GISCO data directory not found: ", data_path)
  }
  
  # Find GISCO zip files
  gisco_files <- list.files(data_path, pattern = "^ref-.+\\.shp\\.zip$", full.names = FALSE)
  
  if (length(gisco_files) == 0) {
    message("No GISCO data files found in: ", data_path)
    return(character(0))
  }
  
  if (!detailed) {
    return(gisco_files)
  }
  
  # Parse filenames for detailed information
  file_info <- data.frame(
    filename = gisco_files,
    stringsAsFactors = FALSE
  )
  
  # Extract information from filename pattern: ref-TYPE-YEAR-RESOLUTIONm.shp.zip
  parsed <- stringr::str_match(gisco_files, "^ref-([^-]+)-([0-9]{4})-([0-9]{2})m\\.shp\\.zip$")
  
  if (any(!is.na(parsed[,1]))) {
    file_info$type <- parsed[,2]
    file_info$year <- parsed[,3]
    file_info$resolution <- parsed[,4]
    file_info$file_size_mb <- round(file.info(file.path(data_path, gisco_files))$size / 1024^2, 1)
  }
  
  return(file_info)
}

#' Download GISCO data from Eurostat
#' 
#' This function downloads GISCO geographic data directly from Eurostat's servers.
#' Note: This is a placeholder for future implementation of automated downloads.
#' 
#' @param res Resolution to download
#' @param year Year to download
#' @param type Data type to download
#' @param dest_path Destination path for downloads
#' @param force Re-download even if file exists
#' @param timeout Download timeout in seconds
#' 
#' @details
#' Currently, this function provides download URLs and instructions for manual download.
#' Future versions will implement automated downloading.
#' 
#' @examples
#' \dontrun{
#' # Get download information
#' euiss_download_gisco(res = "20", year = "2024")
#' }
#' 
#' @export
euiss_download_gisco <- function(res = "60", 
                                year = "2024",
                                type = "countries",
                                dest_path = NULL,
                                force = FALSE,
                                timeout = 300) {
  
  if (is.null(dest_path)) {
    paths <- euiss_configure_paths()
    dest_path <- paths$gisco
  }
  
  # Validate inputs
  valid_res <- c("01", "03", "10", "20", "60")
  if (!res %in% valid_res) {
    stop("res must be one of: ", paste(valid_res, collapse = ", "))
  }
  
  filename <- paste0("ref-", type, "-", year, "-", res, "m.shp.zip")
  dest_file <- file.path(dest_path, filename)
  
  # Check if file already exists
  if (file.exists(dest_file) && !force) {
    message("File already exists: ", filename)
    message("Use force=TRUE to re-download")
    return(invisible(dest_file))
  }
  
  # GISCO download URL (this is the actual pattern)
  base_url <- "https://gisco-services.ec.europa.eu/distribution/v2/countries/download/"
  download_url <- paste0(base_url, filename)
  
  message("GISCO Data Download")
  message("===================")
  message("Download URL: ", download_url)
  message("Destination: ", dest_file)
  message("")
  message("Manual download instructions:")
  message("1. Visit: ", download_url)
  message("2. Save file to: ", dest_path)
  message("3. Ensure filename is: ", filename)
  message("")
  message("Automated download will be implemented in future version.")
  
  # Future implementation would use:
  # download.file(download_url, dest_file, timeout = timeout)
  
  invisible(download_url)
}

#' Create Kosovo-compatible GISCO data
#' 
#' This function creates a version of GISCO countries data that includes Kosovo
#' as a separate entity, extracted from Serbia's boundaries using Natural Earth data.
#' 
#' @param res GISCO resolution to use
#' @param year GISCO year to use
#' @param data_path GISCO data path
#' @param save_file Optional filename to save the result
#' 
#' @return sf object with Kosovo-compatible countries data
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
euiss_gisco_kosovo <- function(res = "10", year = "2024", data_path = NULL, save_file = NULL) {
  
  # Check dependencies
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    stop("rnaturalearth package required for Kosovo data processing")
  }
  
  # Get Kosovo from Natural Earth
  kosovo_ne <- tryCatch({
    rnaturalearth::ne_countries(scale = 10, country = "Kosovo", returnclass = "sf") %>%
      dplyr::select(NAME_ENGL = .data$name,
                   ISO3_CODE = .data$adm0_a3,
                   geometry = .data$geometry)
  }, error = function(e) {
    stop("Failed to load Kosovo from Natural Earth: ", e$message)
  })
  
  # Load GISCO countries
  gisco_countries <- euiss_gisco(res = res, year = year, data_path = data_path, verbose = FALSE)
  
  # Extract Serbia and remove it from the main dataset
  serbia <- gisco_countries %>%
    dplyr::filter(.data$NAME_ENGL == "Serbia") %>%
    dplyr::select(.data$NAME_ENGL, .data$ISO3_CODE, .data$geometry)
  
  other_countries <- gisco_countries %>%
    dplyr::filter(.data$NAME_ENGL != "Serbia") %>%
    dplyr::select(.data$NAME_ENGL, .data$ISO3_CODE, .data$geometry)
  
  # Create Serbia without Kosovo by taking difference
  serbia_minus_kosovo <- tryCatch({
    sf::st_difference(serbia, kosovo_ne)
  }, error = function(e) {
    warning("Failed to subtract Kosovo from Serbia: ", e$message)
    serbia  # Return original Serbia if subtraction fails
  })
  
  # Combine all countries
  countries_with_kosovo <- dplyr::bind_rows(
    other_countries,
    serbia_minus_kosovo,
    kosovo_ne
  )
  
  # Save if requested
  if (!is.null(save_file)) {
    if (is.null(data_path)) {
      paths <- euiss_configure_paths()
      data_path <- paths$gisco
    }
    
    save_path <- file.path(data_path, save_file)
    saveRDS(countries_with_kosovo, save_path)
    message("Saved Kosovo-compatible data to: ", save_path)
  }
  
  message("Created countries dataset with ", nrow(countries_with_kosovo), 
         " countries including Kosovo as separate entity")
  
  return(countries_with_kosovo)
}

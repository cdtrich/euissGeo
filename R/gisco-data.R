#' GISCO Data Functions
#' 
#' Functions for retrieving and managing Eurostat GISCO geographic data.
#' 
#' @name gisco-data
NULL

#' Retrieve geographic data from Eurostat GISCO service
#'
#' Fetches geographic data from Eurostat GISCO. GeoJSON is always tried first:
#' (1) online from the GISCO distribution server, (2) a local GeoJSON copy,
#' and only then (3) the legacy local shapefile zip archives.
#'
#' @param res Resolution of map data: "60", "20", "10", "03", "01" (million scale).
#'   Ignored for `shp = "LB"`.
#' @param verbose Print additional information during execution? (default: TRUE)
#' @param type "countries" (default), "communes" or "coastlines".
#'   Communes are fixed to 2016 / 01M; coastlines to RG / 2016.
#' @param shp Shape of data: "RG" (regions), "BN" (boundaries), "LB" (labels).
#' @param bordertype Boundary type for "BN" shape: "INLAND", "COASTL", or "ALL"
#'   (the plain, unsuffixed boundary file).
#' @param year Reference year: "2024", "2020", "2016", "2013", "2010", "2006", "2003".
#' @param crs Coordinate reference system: "4326" (WGS84), "3035" (ETRS89), "3857" (Web Mercator).
#' @param data_path Base path for local GISCO data. If NULL, uses configured paths
#'   (only resolved if the online route fails or is skipped).
#' @param source Where to look: "auto" (online, then local; default), "online"
#'   (online only), or "local" (local only).
#' @param timeout Download timeout in seconds (default: 120).
#'
#' @return Simple Features (sf) object with requested geographic data.
#'
#' @details
#' Online files follow
#' \code{https://gisco-services.ec.europa.eu/distribution/v2/countries/geojson/CNTR_RG_20M_2024_4326.geojson}.
#' Local files are searched recursively under \code{data_path} by file name,
#' so flat and sub-foldered layouts both work.
#' The legacy zip route expects \code{ref-countries-YYYY-SSm.shp.zip}.
#'
#' @examples
#' \dontrun{
#' countries <- euiss_gisco(res = "20")
#' boundaries <- euiss_gisco(res = "10", shp = "BN", bordertype = "COASTL")
#' labels <- euiss_gisco(shp = "LB")
#' coast <- euiss_gisco(res = "10", type = "coastlines")
#' offline <- euiss_gisco(res = "20", source = "local")
#' }
#'
#' @import dplyr
#' @import sf
#' @importFrom utils unzip download.file
#'
#' @export
euiss_gisco <- function(res = "60",
                        verbose = TRUE,
                        type = "countries",
                        shp = "RG",
                        bordertype = "INLAND",
                        year = "2024",
                        crs = "4326",
                        data_path = NULL,
                        source = c("auto", "online", "local"),
                        timeout = 120) {
  
  source <- match.arg(source)
  
  # --- Input validation ------------------------------------------------------
  valid_type <- c("countries", "communes", "coastlines")
  if (!type %in% valid_type) {
    stop("`type` must be one of: ", paste(valid_type, collapse = ", "), call. = FALSE)
  }
  
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
  
  if (type == "countries") {
    valid_years <- c("2024", "2020", "2016", "2013", "2010", "2006", "2003")
    if (!year %in% valid_years) {
      stop("`year` must be one of: ", paste(valid_years, collapse = ", "), call. = FALSE)
    }
  }
  
  if (type == "countries" && shp == "BN") {
    valid_bordertype <- c("INLAND", "COASTL", "ALL")
    if (!bordertype %in% valid_bordertype) {
      stop("`bordertype` must be one of: ", paste(valid_bordertype, collapse = ", "), call. = FALSE)
    }
  }
  
  # --- Fixed parameters for non-country types -------------------------------
  if (type == "communes") {
    year <- "2016"
    res <- "01"
  } else if (type == "coastlines") {
    year <- "2016"
    shp <- "RG"
  }
  
  geojson_name <- .gisco_geojson_name(type, shp, res, year, crs, bordertype)
  
  # --- 1. Online GeoJSON -----------------------------------------------------
  data <- NULL
  
  if (source %in% c("auto", "online")) {
    url <- .gisco_geojson_url(type, geojson_name)
    if (verbose) message("Trying online GeoJSON: ", geojson_name)
    data <- .gisco_read_online(url, timeout, verbose)
    
    if (is.null(data) && source == "online") {
      stop("Could not retrieve GeoJSON from: ", url, call. = FALSE)
    }
  }
  
  # --- 2./3. Local GeoJSON, then legacy zip ----------------------------------
  if (is.null(data)) {
    
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
    
    local_file <- .gisco_find_local(data_path, geojson_name)
    
    if (!is.null(local_file)) {
      if (verbose) message("Loading local GeoJSON: ", local_file)
      data <- sf::st_read(local_file, quiet = TRUE)
      
    } else if (type != "coastlines") {
      # Legacy zip route
      zip_filename <- sprintf("ref-%s-%s-%sm.shp.zip", type, year, res)
      zip_path <- file.path(data_path, zip_filename)
      
      if (file.exists(zip_path)) {
        if (verbose) message("GeoJSON not found locally; loading legacy zip: ", zip_filename)
        path_zip <- if (type == "communes") "COMM" else "CNTR"
        data <- .load_gisco_nested_zip(
          zip_path, path_zip, shp, res, year, crs, bordertype, verbose
        )
      }
    }
    
    if (is.null(data)) {
      stop(
        "GISCO data not found.\n",
        "  Online: ", if (source == "local") "skipped" else "failed", "\n",
        "  Local GeoJSON: ", geojson_name, " not found under ", data_path, "\n",
        if (type != "coastlines") paste0(
          "  Legacy zip: ", sprintf("ref-%s-%s-%sm.shp.zip", type, year, res), " not found\n"
        ),
        "Check parameters, connection, or download the file to the GISCO data folder.",
        call. = FALSE
      )
    }
  }
  
  if (verbose) {
    message(
      "Successfully loaded: ", nrow(data), " features",
      if (shp != "LB") paste0(" at ", res, "M resolution")
    )
  }
  
  data
}

#' Build GISCO GeoJSON file name
#' @keywords internal
#' @noRd
.gisco_geojson_name <- function(type, shp, res, year, crs, bordertype) {
  
  prefix <- c(countries = "CNTR", communes = "COMM", coastlines = "COAS")[[type]]
  
  core <- if (shp == "LB") {
    sprintf("%s_LB_%s_%s", prefix, year, crs)
  } else {
    sprintf("%s_%s_%sM_%s_%s", prefix, shp, res, year, crs)
  }
  
  if (type == "countries" && shp == "BN" && bordertype != "ALL") {
    core <- paste0(core, "_", bordertype)
  }
  
  paste0(core, ".geojson")
}

#' Build GISCO GeoJSON URL
#' @keywords internal
#' @noRd
.gisco_geojson_url <- function(type, filename) {
  folder <- c(countries = "countries", communes = "communes", coastlines = "coas")[[type]]
  sprintf(
    "https://gisco-services.ec.europa.eu/distribution/v2/%s/geojson/%s",
    folder, filename
  )
}

#' Download and read a GeoJSON; returns NULL on any failure
#' @keywords internal
#' @noRd
.gisco_read_online <- function(url, timeout, verbose) {
  
  tmp <- tempfile(fileext = ".geojson")
  old_opts <- options(timeout = timeout)
  on.exit({
    options(old_opts)
    unlink(tmp)
  }, add = TRUE)
  
  ok <- tryCatch({
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    file.exists(tmp) && file.size(tmp) > 0
  },
  error = function(e) {
    if (verbose) message("  Online download failed: ", conditionMessage(e))
    FALSE
  },
  warning = function(w) {
    if (verbose) message("  Online download failed: ", conditionMessage(w))
    FALSE
  })
  
  if (!isTRUE(ok)) return(NULL)
  
  tryCatch(
    sf::st_read(tmp, quiet = TRUE),
    error = function(e) {
      if (verbose) message("  Downloaded file could not be read: ", conditionMessage(e))
      NULL
    }
  )
}

#' Find a GeoJSON file by name anywhere under data_path; NULL if absent
#' @keywords internal
#' @noRd
.gisco_find_local <- function(data_path, filename) {
  files <- list.files(data_path, pattern = "\\.geojson$", recursive = TRUE, full.names = TRUE)
  hit <- files[tolower(basename(files)) == tolower(filename)]
  if (length(hit) == 0) NULL else hit[1]
}

#' Load GISCO data from nested zip structure (legacy)
#' @keywords internal
#' @noRd
.load_gisco_nested_zip <- function(outer_zip, path_zip, shp, res, year, crs, 
                                   bordertype, verbose) {
  
  temp_outer <- tempfile()
  temp_inner <- tempfile()
  
  on.exit({
    unlink(temp_outer, recursive = TRUE)
    unlink(temp_inner, recursive = TRUE)
  }, add = TRUE)
  
  tryCatch({
    utils::unzip(outer_zip, exdir = temp_outer)
  }, error = function(e) {
    stop(
      "Failed to extract outer zip: ", basename(outer_zip), "\n",
      "Error: ", e$message,
      call. = FALSE
    )
  })
  
  inner_filename <- .build_inner_filename(path_zip, shp, res, year, crs, bordertype)
  inner_zip_path <- file.path(temp_outer, inner_filename)
  
  if (!file.exists(inner_zip_path)) {
    available_inner <- list.files(temp_outer, pattern = "\\.zip$")
    stop(
      "Inner zip not found: ", inner_filename, "\n",
      "Available in archive: ", paste(available_inner, collapse = ", "),
      call. = FALSE
    )
  }
  
  tryCatch({
    utils::unzip(inner_zip_path, exdir = temp_inner)
  }, error = function(e) {
    stop(
      "Failed to extract inner zip: ", inner_filename, "\n",
      "Error: ", e$message,
      call. = FALSE
    )
  })
  
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

#' Build inner zip filename based on shape type (legacy)
#' @keywords internal
#' @noRd
.build_inner_filename <- function(path_zip, shp, res, year, crs, bordertype) {
  
  if (shp == "BN") {
    base <- sprintf("%s_%s_%sM_%s_%s", path_zip, shp, res, year, crs)
    if (bordertype == "ALL") sprintf("%s.shp.zip", base)
    else sprintf("%s_%s.shp.zip", base, bordertype)
  } else if (shp == "LB") {
    sprintf("%s_%s_%s_%s.shp.zip", path_zip, shp, year, crs)
  } else if (shp == "RG") {
    sprintf("%s_%s_%sM_%s_%s.shp.zip", path_zip, shp, res, year, crs)
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
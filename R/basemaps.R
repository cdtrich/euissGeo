#' Basemap Creation Functions
#' 
#' Functions for creating comprehensive basemaps with raster elevation data,
#' vector layers, and EUISS styling.
#' 
#' @name basemaps
NULL

#' Create EUISS-styled basemap with elevation, roads, and urban areas
#'
#' This function creates a comprehensive basemap with raster elevation data,
#' roads, urban areas, and country boundaries using EUISS visual identity.
#' Supports both GISCO and Natural Earth data sources.
#'
#' @param country Country name to focus the map on
#' @param ras Raster type. One of "GRAY_HR_SR_OB_DR", "GRAY_HR_SR_W", "Blue-Earth-Bathymetry"  
#' @param data_path Base path for raster and vector data. If NULL, uses configured paths
#' @param db Database source. Either "gisco" for European focus or "ne" for global
#' @param crs Coordinate reference system. Use "+proj=robin" for global, or "+proj=laea" for regional
#' @param buffer Buffer distance around country in meters. Default 100km
#' @param col Named list of colors for map elements. If NULL, uses EUISS colors
#' @param show_progress Logical. Show progress bar during processing?
#'
#' @return A ggplot2 object representing the styled basemap
#'
#' @examples
#' \dontrun{
#' # Basic basemap for Albania
#' map <- euiss_basemap("Albania")
#' 
#' # Custom styling and projection
#' map <- euiss_basemap(
#'   country = "Kosovo",
#'   ras = "GRAY_HR_SR_W", 
#'   db = "ne",
#'   crs = "+proj=laea +lat_0=42 +lon_0=20",
#'   buffer = 50000
#' )
#' }
#'
#' @import dplyr
#' @import sf  
#' @import ggplot2
#' @import terra
#' @importFrom progress progress_bar
#' @importFrom euissR euiss_get_fonts
#' @importFrom euissR theme_euiss
#'
#' @export
euiss_basemap <- function(country = "Albania",
                         ras = "GRAY_HR_SR_OB_DR", 
                         data_path = NULL,
                         db = "gisco",
                         crs = "+proj=robin",
                         buffer = 100000,
                         col = NULL,
                         show_progress = TRUE) {
  
  # Input validation
  valid_ras <- c("GRAY_HR_SR_OB_DR", "GRAY_HR_SR_W", "Blue-Earth-Bathymetry")
  if (!ras %in% valid_ras) {
    stop("ras must be one of: ", paste(valid_ras, collapse = ", "))
  }
  
  valid_db <- c("gisco", "ne")
  if (!db %in% valid_db) {
    stop("db must be either 'gisco' or 'ne'")
  }
  
  if (!is.character(country) || length(country) != 1) {
    stop("country must be a single character string")
  }
  
  if (!is.numeric(buffer) || buffer < 0) {
    stop("buffer must be a positive number")
  }
  
  # Configure colors
  if (is.null(col)) {
    col <- euiss_get_basemap_colors()
  }
  
  # Configure data paths
  paths <- euiss_configure_basemap_paths(data_path)
  
  # Initialize progress bar
  if (show_progress) {
    pb <- progress::progress_bar$new(
      format = "Building basemap [:bar] :percent :etas",
      total = 7, clear = FALSE, width = 60
    )
  }
  
  # Step 1: Load and validate country boundaries
  if (show_progress) pb$tick()
  countries_data <- euiss_load_countries(country, db, buffer, paths)
  
  # Step 2: Load raster data
  if (show_progress) pb$tick()  
  raster_data <- euiss_load_raster_data(ras, countries_data, crs, paths)
  
  # Step 3: Determine CRS if needed
  if (show_progress) pb$tick()
  if (is.na(crs) || crs == "auto") {
    crs <- euiss_calculate_optimal_crs(countries_data)
  }
  
  # Step 4: Load vector layers
  if (show_progress) pb$tick()
  roads_data <- euiss_load_roads(countries_data, paths)
  
  if (show_progress) pb$tick()
  urban_data <- euiss_load_urban_areas(countries_data, paths)
  
  if (show_progress) pb$tick()
  urban_labels <- euiss_load_urban_labels(countries_data, paths)
  
  if (show_progress) pb$tick()
  lakes_data <- euiss_load_lakes(countries_data, paths)
  
  # Create the map
  map <- euiss_create_basemap_plot(
    countries = countries_data,
    raster = raster_data,
    roads = roads_data,
    urban = urban_data, 
    urban_labels = urban_labels,
    lakes = lakes_data,
    target_country = country,
    ras_name = ras,
    crs = crs,
    colors = col
  )
  
  return(map)
}

#' Get default EUISS basemap colors
#' @return Named list of colors for basemap elements
euiss_get_basemap_colors <- function() {
  
  # Try to get colors from euissR, fall back to defaults
  default_colors <- list(
    water = "#00A0C1",
    borders = "#1D1D1B", 
    urban = "#C6C6C6",
    urban_label = "#1D3956"
  )
  
  if (requireNamespace("euissR", quietly = TRUE)) {
    try({
      # Get updated colors from euissR if available
      teal <- euissR::pal_cat_euiss_enhanced(6)[2]
      col_axis <- "#000"
      col_grid <- euissR::pal_seq_v_euiss(1)
      teal3 <- euissR::pal_seq_b_euiss(4)[2]
      
      default_colors$water <- teal
      default_colors$borders <- col_axis
      default_colors$urban <- col_grid
      default_colors$urban_label <- teal3
    }, silent = TRUE)
  }
  
  return(default_colors)
}

#' Configure basemap data paths with intelligent fallbacks
#' @param data_path Optional user-specified data path
#' @return List of configured paths for different data sources
euiss_configure_basemap_paths <- function(data_path = NULL) {
  
  if (is.null(data_path)) {
    # Try environment variable first
    env_path <- Sys.getenv("EUISS_DATA_PATH")
    if (env_path != "") {
      data_path <- env_path
    }
  }
  
  # Define possible locations for each data type
  if (is.null(data_path)) {
    base_paths <- c(
      "D:/data",           # Original path structure
      "~/data",            # User home data
      "./data",            # Relative to working directory  
      tempdir()            # Last resort
    )
    data_path <- Find(dir.exists, base_paths)
  }
  
  if (is.null(data_path)) {
    data_path <- tempdir()
    warning("No data directory found. Using temporary directory: ", data_path)
  }
  
  paths <- list(
    base = data_path,
    raster = file.path(data_path, "raster"),
    natural_earth = file.path(data_path, "natural earth"), 
    gisco = file.path(data_path, "giscoR")
  )
  
  return(paths)
}

#' Load country boundaries data
#' @param country Target country name
#' @param db Database source ("gisco" or "ne")  
#' @param buffer Buffer distance in meters
#' @param paths List of data paths
#' @return sf object with cropped country boundaries
#' @export
euiss_load_countries <- function(country, db, buffer, paths) {
  
  if (db == "ne") {
    # Use rnaturalearth
    if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
      stop("rnaturalearth package required for db = 'ne'")
    }
    
    countries_raw <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
    
    if (!any(countries_raw$name == country)) {
      available_countries <- sort(countries_raw$name)
      stop("Country '", country, "' not found in Natural Earth data.\n",
           "Available countries: ", paste(head(available_countries, 10), collapse = ", "), 
           "...\nSee rnaturalearth::ne_countries() for full list.")
    }
    
    target_country <- countries_raw %>%
      dplyr::filter(.data$name == country)
    
    countries_crop <- countries_raw %>%
      sf::st_crop(sf::st_buffer(target_country, buffer)) %>%
      dplyr::select(country = .data$name)
      
  } else if (db == "gisco") {
    # Use GISCO data - this would call euiss_gisco() 
    tryCatch({
      countries_raw <- euiss_gisco(res = "10", verbose = FALSE)
    }, error = function(e) {
      stop("Failed to load GISCO data. Ensure data is available or use db = 'ne'. Error: ", e$message)
    })
    
    if (!any(countries_raw$NAME_ENGL == country)) {
      available_countries <- sort(countries_raw$NAME_ENGL)
      stop("Country '", country, "' not found in GISCO data.\n",
           "Available countries: ", paste(head(available_countries, 10), collapse = ", "),
           "...")
    }
    
    target_country <- countries_raw %>%
      dplyr::filter(.data$NAME_ENGL == country)
    
    countries_crop <- countries_raw %>%
      sf::st_crop(sf::st_buffer(target_country, buffer)) %>%
      dplyr::select(country = .data$NAME_ENGL)
  }
  
  return(countries_crop)
}

#' Load and crop raster elevation data
#' @param ras Raster type
#' @param countries_crop Cropped country boundaries
#' @param crs Target CRS
#' @param paths Data paths
#' @return Tibble with raster data
#' @export
euiss_load_raster_data <- function(ras, countries_crop, crs, paths) {
  
  # Determine raster file path
  if (ras == "GRAY_HR_SR_OB_DR") {
    ras_path <- file.path(paths$raster, ras, paste0(ras, ".tif"))
  } else if (ras == "GRAY_HR_SR_W") {
    ras_path <- file.path(paths$raster, ras, ras, paste0(ras, ".tif"))
  } else if (ras == "Blue-Earth-Bathymetry") {
    ras_path <- file.path(paths$raster, ras, ras, paste0(ras, ".tif"))
    ras <- "Blue.Earth.Bathymetry"  # Fix variable name for ggplot
  }
  
  if (!file.exists(ras_path)) {
    warning("Raster file not found: ", ras_path, ". Skipping raster layer.")
    return(tibble::tibble(x = numeric(0), y = numeric(0)))
  }
  
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("terra package required for raster processing")
  }
  
  # Load and process raster
  tryCatch({
    raster <- terra::rast(ras_path)
    
    raster_crop <- raster %>%
      terra::crop(countries_crop) %>%
      terra::project(crs) %>%
      terra::as.data.frame(xy = TRUE) %>%
      tibble::as_tibble()
    
    # Ensure proper column name for ggplot
    if (!"Blue.Earth.Bathymetry" %in% names(raster_crop) && ras == "Blue.Earth.Bathymetry") {
      # Find the actual raster column name and rename it
      raster_cols <- names(raster_crop)[!names(raster_crop) %in% c("x", "y")]
      if (length(raster_cols) > 0) {
        names(raster_crop)[names(raster_crop) == raster_cols[1]] <- "Blue.Earth.Bathymetry"
      }
    }
    
    return(raster_crop)
    
  }, error = function(e) {
    warning("Failed to process raster data: ", e$message, ". Skipping raster layer.")
    return(tibble::tibble(x = numeric(0), y = numeric(0)))
  })
}

#' Load roads data
#' @param countries_crop Cropped boundaries
#' @param paths Data paths
#' @return sf object with roads
#' @export
euiss_load_roads <- function(countries_crop, paths) {
  
  roads_path <- file.path(paths$natural_earth, "ne_10m_roads")
  
  if (!dir.exists(roads_path) && !file.exists(paste0(roads_path, ".shp"))) {
    warning("Roads data not found at: ", roads_path, ". Skipping roads layer.")
    return(sf::st_sf(geometry = sf::st_sfc()))
  }
  
  tryCatch({
    roads <- sf::st_read(roads_path, quiet = TRUE) %>%
      sf::st_crop(countries_crop)
    return(roads)
  }, error = function(e) {
    warning("Failed to load roads: ", e$message, ". Skipping roads layer.")
    return(sf::st_sf(geometry = sf::st_sfc()))
  })
}

#' Load urban areas data
#' @param countries_crop Cropped boundaries
#' @param paths Data paths
#' @return sf object with urban areas
#' @export
euiss_load_urban_areas <- function(countries_crop, paths) {
  
  urban_path <- file.path(paths$natural_earth, "ne_10m_urban_areas")
  
  if (!dir.exists(urban_path) && !file.exists(paste0(urban_path, ".shp"))) {
    warning("Urban areas data not found. Skipping urban areas layer.")
    return(sf::st_sf(geometry = sf::st_sfc()))
  }
  
  tryCatch({
    urban <- sf::st_read(urban_path, quiet = TRUE) %>%
      sf::st_crop(countries_crop)
    return(urban)
  }, error = function(e) {
    warning("Failed to load urban areas: ", e$message, ". Skipping urban areas layer.")
    return(sf::st_sf(geometry = sf::st_sfc()))
  })
}

#' Load urban labels/populated places
#' @param countries_crop Cropped boundaries  
#' @param paths Data paths
#' @return sf object with place labels
#' @export
euiss_load_urban_labels <- function(countries_crop, paths) {
  
  labels_path <- file.path(paths$natural_earth, "ne_10m_populated_places_simple")
  
  if (!dir.exists(labels_path) && !file.exists(paste0(labels_path, ".shp"))) {
    warning("Urban labels data not found. Skipping labels layer.")
    return(sf::st_sf(name = character(0), geometry = sf::st_sfc()))
  }
  
  tryCatch({
    labels <- sf::st_read(labels_path, quiet = TRUE) %>%
      sf::st_crop(countries_crop)
    return(labels)
  }, error = function(e) {
    warning("Failed to load urban labels: ", e$message, ". Skipping labels layer.")
    return(sf::st_sf(name = character(0), geometry = sf::st_sfc()))
  })
}

#' Load lakes data
#' @param countries_crop Cropped boundaries
#' @param paths Data paths  
#' @return sf object with lakes
#' @export
euiss_load_lakes <- function(countries_crop, paths) {
  
  lakes_path1 <- file.path(paths$natural_earth, "ne_10m_lakes")
  lakes_path2 <- file.path(paths$natural_earth, "ne_10m_lakes_europe")
  
  lakes <- sf::st_sf(geometry = sf::st_sfc())
  
  # Try to load regular lakes
  if (dir.exists(lakes_path1) || file.exists(paste0(lakes_path1, ".shp"))) {
    tryCatch({
      lakes1 <- sf::st_read(lakes_path1, quiet = TRUE)
      lakes <- lakes1
    }, error = function(e) {
      warning("Failed to load lakes: ", e$message)
    })
  }
  
  # Try to load Europe lakes and bind
  if (dir.exists(lakes_path2) || file.exists(paste0(lakes_path2, ".shp"))) {
    tryCatch({
      lakes2 <- sf::st_read(lakes_path2, quiet = TRUE)
      if (nrow(lakes) > 0) {
        lakes <- dplyr::bind_rows(lakes, lakes2)
      } else {
        lakes <- lakes2
      }
    }, error = function(e) {
      warning("Failed to load Europe lakes: ", e$message)
    })
  }
  
  # Crop to area of interest
  if (nrow(lakes) > 0) {
    tryCatch({
      lakes <- lakes %>%
        sf::st_make_valid() %>%
        sf::st_crop(countries_crop)
    }, error = function(e) {
      warning("Failed to crop lakes: ", e$message)
      lakes <- sf::st_sf(geometry = sf::st_sfc())
    })
  }
  
  return(lakes)
}

#' Calculate optimal CRS for the area of interest
#' @param countries_crop Country boundaries
#' @return CRS string
#' @export
euiss_calculate_optimal_crs <- function(countries_crop) {
  
  # Calculate centroid
  centroid <- countries_crop %>%
    sf::st_union() %>%
    sf::st_centroid() %>%
    sf::st_coordinates()
  
  # Lambert Azimuthal Equal Area centered on the area
  crs <- paste0(
    "+proj=laea +lat_0=", centroid[, "Y"], 
    " +lon_0=", centroid[, "X"],
    " +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  )
  
  return(crs)
}

#' Create the final basemap plot
#' @param countries Country boundaries
#' @param raster Raster elevation data
#' @param roads Roads data
#' @param urban Urban areas
#' @param urban_labels Urban labels
#' @param lakes Lakes data
#' @param target_country Target country name
#' @param ras_name Raster variable name
#' @param crs Coordinate system
#' @param colors Color scheme
#' @return ggplot2 object
#' @export
euiss_create_basemap_plot <- function(countries, raster, roads, urban, urban_labels, 
                                     lakes, target_country, ras_name, crs, colors) {
  
  # Fix raster name for ggplot
  ras_col <- gsub("-", ".", ras_name)
  
  # Start building the plot
  p <- ggplot2::ggplot(countries)
  
  # Add raster layer if available
  if (nrow(raster) > 0 && ras_col %in% names(raster)) {
    p <- p + 
      ggplot2::geom_raster(
        data = raster,
        ggplot2::aes(x = .data$x, y = .data$y, fill = .data[[ras_col]]),
        alpha = 0.5,
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_gradientn(
        colors = c("white", "white", "white", colors$urban, colors$borders)
      )
  }
  
  # Add lakes
  if (nrow(lakes) > 0) {
    p <- p + 
      ggplot2::geom_sf(
        data = lakes,
        fill = colors$water,
        color = NA
      )
  }
  
  # Add roads
  if (nrow(roads) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = roads,
        color = colors$urban,
        alpha = 0.5,
        linewidth = 1  # Updated from size
      )
  }
  
  # Add urban areas
  if (nrow(urban) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = urban,
        fill = colors$urban,
        alpha = 0.5,
        color = NA
      )
  }
  
  # Add country boundaries - other countries
  other_countries <- countries %>%
    dplyr::filter(.data$country != target_country)
  
  if (nrow(other_countries) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = other_countries,
        fill = "white",
        alpha = 0.75,
        color = NA
      )
  }
  
  # Add target country boundary
  target_country_geom <- countries %>%
    dplyr::filter(.data$country == target_country)
  
  p <- p +
    ggplot2::geom_sf(
      data = target_country_geom,
      fill = NA,
      color = colors$borders,
      linewidth = 1
    )
  
  # Add urban labels
  if (nrow(urban_labels) > 0 && "name" %in% names(urban_labels)) {
    p <- p +
      ggplot2::geom_sf_text(
        data = urban_labels,
        ggplot2::aes(label = .data$name),
        hjust = "left",
        color = colors$urban_label,
        size = 2
      )
  }
  
  # Add country labels
  p <- p +
    ggplot2::geom_sf_text(
      ggplot2::aes(label = toupper(.data$country)),
      hjust = "left",
      size = 2,
      color = colors$borders
    )
  
  # Apply coordinate system and theme
  p <- p +
    ggplot2::coord_sf(crs = crs, datum = NA) +
    ggplot2::theme_void()
  
  return(p)
}
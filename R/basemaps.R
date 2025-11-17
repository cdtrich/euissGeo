#' Basemap Creation Functions
#' 
#' Functions for creating comprehensive basemaps with raster elevation data,
#' vector layers, and EUISS styling.
#' 
#' @name basemaps
NULL

#' Create EUISS-styled basemap with selectable layers
#'
#' Creates a comprehensive basemap with optional layers including elevation data,
#' roads, urban areas, administrative boundaries, rivers, and labels using EUISS styling.
#'
#' @param country Country name to focus the map on (default: "Belgium").
#' @param ras Raster type: "GRAY_HR_SR_OB_DR", "GRAY_HR_SR_W", "Blue-Earth-Bathymetry".
#' @param data_path Base path for raster and vector data. If NULL, uses configured paths.
#' @param db Database source: "gisco" (European) or "ne" (global).
#' @param crs Coordinate reference system (default: "+proj=robin").
#' @param buffer Buffer distanceeui around country in meters (default: 100000).
#' @param shaded_relief Include shaded relief raster? (default: FALSE)
#' @param boundaries Show country boundaries? (default: TRUE)
#' @param roads Show road network? (default: FALSE)
#' @param urban_areas Show urban area polygons? (default: TRUE)
#' @param urban_labels Show city name labels? (default: TRUE)
#' @param lakes Show lakes? (default: FALSE)
#' @param rivers Show rivers? (default: FALSE)
#' @param admin_boundaries Show administrative boundaries (provinces/states)? (default: FALSE)
#' @param admin_level GADM admin level for boundaries: 1 (provinces) or 2 (districts).
#' @param graticule Show lat/lon grid? (default: FALSE)
#' @param col Named list of colors for map elements. If NULL, uses EUISS colors.
#' @param show_progress Show progress bar? (default: TRUE)
#'
#' @return A ggplot2 object representing the styled basemap.
#'
#' @examples
#' \dontrun{
#' # Basic map - boundaries and urban areas only
#' map <- euiss_basemap("Belgium")
#' 
#' # Everything enabled
#' map_full <- euiss_basemap(
#'   "Belgium",
#'   shaded_relief = TRUE,
#'   roads = TRUE,
#'   lakes = TRUE,
#'   rivers = TRUE,
#'   admin_boundaries = TRUE,
#'   graticule = TRUE
#' )
#' 
#' # Minimal - just boundaries
#' map_minimal <- euiss_basemap(
#'   "Belgium",
#'   urban_areas = FALSE,
#'   urban_labels = FALSE
#' )
#' }
#'
#' @import dplyr
#' @import sf
#' @import ggplot2
#' @importFrom progress progress_bar
#'
#' @export
euiss_basemap <- function(country = "Belgium",
                          ras = "GRAY_HR_SR_OB_DR",
                          data_path = NULL,
                          db = "gisco",
                          crs = "+proj=robin",
                          buffer = 100000,
                          shaded_relief = FALSE,
                          boundaries = TRUE,
                          roads = FALSE,
                          urban_areas = TRUE,
                          urban_labels = TRUE,
                          lakes = FALSE,
                          rivers = FALSE,
                          admin_boundaries = FALSE,
                          admin_level = 1,
                          graticule = FALSE,
                          col = NULL,
                          show_progress = TRUE) {
  
  # Input validation
  valid_ras <- c("GRAY_HR_SR_OB_DR", "GRAY_HR_SR_W", "Blue-Earth-Bathymetry")
  if (!ras %in% valid_ras) {
    stop("`ras` must be one of: ", paste(valid_ras, collapse = ", "), call. = FALSE)
  }
  
  valid_db <- c("gisco", "ne")
  if (!db %in% valid_db) {
    stop("`db` must be 'gisco' or 'ne'", call. = FALSE)
  }
  
  if (!is.character(country) || length(country) != 1) {
    stop("`country` must be a single character string", call. = FALSE)
  }
  
  .validate_positive_number(buffer, "buffer", allow_zero = TRUE)
  
  if (!admin_level %in% c(1, 2)) {
    stop("`admin_level` must be 1 or 2", call. = FALSE)
  }
  
  # Configure colors
  if (is.null(col)) {
    col <- .get_basemap_colors()
  }
  
  # Configure paths
  paths <- euiss_configure_paths(data_path)
  
  # Count layers to load
  n_layers <- sum(c(
    boundaries,
    shaded_relief,
    roads,
    urban_areas,
    urban_labels,
    lakes,
    rivers,
    admin_boundaries
  ))
  
  # Initialize progress
  pb <- if (show_progress) {
    .create_progress_bar(TRUE, n_layers, "Building basemap [:bar] :percent :etas")
  } else {
    NULL
  }
  
  # Step 1: ALWAYS load country boundaries (needed for cropping)
  .tick_progress(pb)
  countries_data <- euiss_load_countries(country, db, buffer, paths)
  
  # Step 2: Load optional raster
  raster_data <- NULL
  if (shaded_relief) {
    .tick_progress(pb)
    raster_data <- euiss_load_raster_data(ras, countries_data, crs, paths)
  }
  
  # Step 3: Determine CRS if auto
  if (is.na(crs) || crs == "auto") {
    crs <- .suggest_crs(countries_data, preference = "equal_area")
  }
  
  # Step 4: Load optional vector layers
  roads_data <- NULL
  if (roads) {
    .tick_progress(pb)
    roads_data <- euiss_load_roads(countries_data, paths)
  }
  
  urban_data <- NULL
  if (urban_areas) {
    .tick_progress(pb)
    urban_data <- euiss_load_urban_areas(countries_data, paths)
  }
  
  labels_data <- NULL
  if (urban_labels) {
    .tick_progress(pb)
    labels_data <- euiss_load_urban_labels(countries_data, paths)
  }
  
  lakes_data <- NULL
  if (lakes) {
    .tick_progress(pb)
    lakes_data <- euiss_load_lakes(countries_data, paths)
  }
  
  rivers_data <- NULL
  if (rivers) {
    .tick_progress(pb)
    rivers_data <- euiss_load_rivers(countries_data, paths)
  }
  
  admin_data <- NULL
  if (admin_boundaries) {
    .tick_progress(pb)
    admin_data <- euiss_load_admin_boundaries(country, admin_level, countries_data, paths)
  }
  
  # Create the map
  map <- euiss_create_basemap_plot(
    countries = countries_data,
    raster = raster_data,
    roads = roads_data,
    urban = urban_data,
    urban_labels = labels_data,
    lakes = lakes_data,
    rivers = rivers_data,
    admin = admin_data,
    target_country = country,
    ras_name = ras,
    crs = crs,
    colors = col,
    show_graticule = graticule
  )
  
  map
}

#' Get default EUISS basemap colors
#' @keywords internal
#' @noRd
.get_basemap_colors <- function() {
  
  default_colors <- list(
    water = "#00A0C1",
    borders = "#1D1D1B",
    urban = "#C6C6C6",
    urban_label = "#1D3956",
    roads = "#8C8C8C",
    admin = "#666666"
  )
  
  # Try euissR colors
  if (requireNamespace("euissR", quietly = TRUE)) {
    tryCatch({
      teal <- euissR::pal_cat_euiss_enhanced(6)[2]
      col_axis <- "#000"
      col_grid <- euissR::pal_seq_v_euiss(1)
      teal3 <- euissR::pal_seq_b_euiss(4)[2]
      
      default_colors$water <- teal
      default_colors$borders <- col_axis
      default_colors$urban <- col_grid
      default_colors$urban_label <- teal3
    }, error = function(e) invisible())
  }
  
  default_colors
}

#' Load country boundaries data
#' @param country Country name to focus map on
#' @param db Database source: "gisco" or "ne"  
#' @param buffer Buffer distance around country in meters
#' @param paths List of configured data paths
#' @keywords internal
#' @noRd
euiss_load_countries <- function(country, db, buffer, paths) {
  
  if (db == "ne") {
    # Natural Earth
    if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
      stop("rnaturalearth package required for db='ne'", call. = FALSE)
    }
    
    countries_raw <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
    
    if (!any(countries_raw$name == country)) {
      available <- sort(countries_raw$name)
      stop(
        "Country '", country, "' not found in Natural Earth.\n",
        "Available: ", paste(head(available, 10), collapse = ", "), "...",
        call. = FALSE
      )
    }
    
    target_country <- countries_raw %>%
      dplyr::filter(.data$name == country)
    
    countries_crop <- countries_raw %>%
      sf::st_crop(sf::st_buffer(target_country, buffer)) %>%
      dplyr::select(country = .data$name, .data$geometry)
    
  } else {
    # GISCO
    countries_raw <- tryCatch({
      euiss_gisco(res = "10", verbose = FALSE)
    }, error = function(e) {
      stop(
        "Failed to load GISCO data. Ensure data is available or use db='ne'.\n",
        "Error: ", e$message,
        call. = FALSE
      )
    })
    
    if (!any(countries_raw$NAME_ENGL == country)) {
      available <- sort(countries_raw$NAME_ENGL)
      stop(
        "Country '", country, "' not found in GISCO.\n",
        "Available: ", paste(head(available, 10), collapse = ", "), "...",
        call. = FALSE
      )
    }
    
    target_country <- countries_raw %>%
      dplyr::filter(.data$NAME_ENGL == country)
    
    countries_crop <- countries_raw %>%
      sf::st_crop(sf::st_buffer(target_country, buffer)) %>%
      dplyr::select(country = .data$NAME_ENGL, .data$geometry)
  }
  
  countries_crop
}

#' Load and crop raster elevation data
#' @param ras Raster type identifier
#' @param countries_crop sf object with country boundaries
#' @param crs Target coordinate reference system
#' @param paths List of configured data paths
#' @keywords internal
#' @noRd
euiss_load_raster_data <- function(ras, countries_crop, crs, paths) {
  
  # Build raster path
  if (ras == "GRAY_HR_SR_OB_DR") {
    ras_path <- file.path(paths$raster, ras, paste0(ras, ".tif"))
  } else if (ras == "GRAY_HR_SR_W") {
    ras_path <- file.path(paths$raster, ras, ras, paste0(ras, ".tif"))
  } else if (ras == "Blue-Earth-Bathymetry") {
    ras_path <- file.path(paths$raster, ras, ras, paste0(ras, ".tif"))
    ras <- "Blue.Earth.Bathymetry"
  }
  
  if (!file.exists(ras_path)) {
    warning("Raster not found: ", ras_path, ". Skipping relief layer.", call. = FALSE)
    return(tibble::tibble(x = numeric(0), y = numeric(0)))
  }
  
  if (!requireNamespace("terra", quietly = TRUE)) {
    warning("terra package required for raster. Skipping relief layer.", call. = FALSE)
    return(tibble::tibble(x = numeric(0), y = numeric(0)))
  }
  
  tryCatch({
    raster <- terra::rast(ras_path) %>%
      terra::crop(countries_crop) %>%
      terra::project(crs) %>%
      terra::as.data.frame(xy = TRUE) %>%
      tibble::as_tibble()
    
    # Ensure proper column naming
    raster_cols <- setdiff(names(raster), c("x", "y"))
    if (length(raster_cols) > 0) {
      names(raster)[names(raster) == raster_cols[1]] <- gsub("-", ".", ras)
    }
    
    raster
    
  }, error = function(e) {
    warning("Failed to process raster: ", e$message, ". Skipping relief.", call. = FALSE)
    tibble::tibble(x = numeric(0), y = numeric(0))
  })
}

#' Load roads data
#' @param countries_crop sf object for cropping
#' @param paths List of configured data paths
#' @keywords internal
#' @noRd
euiss_load_roads <- function(countries_crop, paths) {
  
  roads_path <- file.path(paths$natural_earth, "ne_10m_roads")
  
  if (!dir.exists(roads_path) && !file.exists(paste0(roads_path, ".shp"))) {
    warning("Roads data not found. Skipping roads layer.", call. = FALSE)
    return(sf::st_sf(geometry = sf::st_sfc()))
  }
  
  tryCatch({
    sf::st_read(roads_path, quiet = TRUE) %>%
      sf::st_crop(countries_crop)
  }, error = function(e) {
    warning("Failed to load roads: ", e$message, call. = FALSE)
    sf::st_sf(geometry = sf::st_sfc())
  })
}

#' Load urban areas data
#' @param countries_crop sf object for cropping
#' @param paths List of configured data paths
#' @keywords internal
#' @noRd
euiss_load_urban_areas <- function(countries_crop, paths) {
  
  urban_path <- file.path(paths$natural_earth, "ne_10m_urban_areas")
  
  if (!dir.exists(urban_path) && !file.exists(paste0(urban_path, ".shp"))) {
    warning("Urban areas data not found. Skipping urban areas layer.", call. = FALSE)
    return(sf::st_sf(geometry = sf::st_sfc()))
  }
  
  tryCatch({
    sf::st_read(urban_path, quiet = TRUE) %>%
      sf::st_crop(countries_crop)
  }, error = function(e) {
    warning("Failed to load urban areas: ", e$message, call. = FALSE)
    sf::st_sf(geometry = sf::st_sfc())
  })
}

#' Load urban labels/populated places
#' @param countries_crop sf object for cropping
#' @param paths List of configured data paths
#' @keywords internal
#' @noRd
euiss_load_urban_labels <- function(countries_crop, paths) {
  
  labels_path <- file.path(paths$natural_earth, "ne_10m_populated_places_simple")
  
  if (!dir.exists(labels_path) && !file.exists(paste0(labels_path, ".shp"))) {
    warning("Urban labels data not found. Skipping labels layer.", call. = FALSE)
    return(sf::st_sf(name = character(0), geometry = sf::st_sfc()))
  }
  
  tryCatch({
    sf::st_read(labels_path, quiet = TRUE) %>%
      sf::st_crop(countries_crop)
  }, error = function(e) {
    warning("Failed to load urban labels: ", e$message, call. = FALSE)
    sf::st_sf(name = character(0), geometry = sf::st_sfc())
  })
}

#' Load lakes data
#' @param countries_crop sf object for cropping
#' @param paths List of configured data paths
#' @keywords internal
#' @noRd
euiss_load_lakes <- function(countries_crop, paths) {
  
  lakes_path1 <- file.path(paths$natural_earth, "ne_10m_lakes")
  lakes_path2 <- file.path(paths$natural_earth, "ne_10m_lakes_europe")
  
  lakes <- sf::st_sf(geometry = sf::st_sfc())
  
  # Try regular lakes
  if (dir.exists(lakes_path1) || file.exists(paste0(lakes_path1, ".shp"))) {
    lakes1 <- tryCatch({
      sf::st_read(lakes_path1, quiet = TRUE)
    }, error = function(e) NULL)
    
    if (!is.null(lakes1)) lakes <- lakes1
  }
  
  # Try Europe lakes
  if (dir.exists(lakes_path2) || file.exists(paste0(lakes_path2, ".shp"))) {
    lakes2 <- tryCatch({
      sf::st_read(lakes_path2, quiet = TRUE)
    }, error = function(e) NULL)
    
    if (!is.null(lakes2)) {
      lakes <- if (nrow(lakes) > 0) dplyr::bind_rows(lakes, lakes2) else lakes2
    }
  }
  
  # Crop
  if (nrow(lakes) > 0) {
    lakes <- tryCatch({
      lakes %>%
        sf::st_make_valid() %>%
        sf::st_crop(countries_crop)
    }, error = function(e) {
      warning("Failed to crop lakes: ", e$message, call. = FALSE)
      sf::st_sf(geometry = sf::st_sfc())
    })
  }
  
  lakes
}

#' Load rivers data
#' @param countries_crop sf object for cropping
#' @param paths List of configured data paths
#' @keywords internal
#' @noRd
euiss_load_rivers <- function(countries_crop, paths) {
  
  rivers_path <- file.path(paths$natural_earth, "ne_10m_rivers_lake_centerlines")
  
  if (!dir.exists(rivers_path) && !file.exists(paste0(rivers_path, ".shp"))) {
    warning("Rivers data not found. Skipping rivers layer.", call. = FALSE)
    return(sf::st_sf(geometry = sf::st_sfc()))
  }
  
  tryCatch({
    sf::st_read(rivers_path, quiet = TRUE) %>%
      sf::st_crop(countries_crop)
  }, error = function(e) {
    warning("Failed to load rivers: ", e$message, call. = FALSE)
    sf::st_sf(geometry = sf::st_sfc())
  })
}

#' Load administrative boundaries from GADM
#' @param country Country name
#' @param admin_level GADM admin level (1 or 2)
#' @param countries_crop sf object for cropping
#' @param paths List of configured data paths
#' @keywords internal
#' @noRd
euiss_load_admin_boundaries <- function(country, admin_level, countries_crop, paths) {
  
  if (!requireNamespace("geodata", quietly = TRUE)) {
    warning(
      "geodata package required for GADM admin boundaries.\n",
      "Install with: install.packages('geodata')\n",
      "Skipping admin boundaries layer.",
      call. = FALSE
    )
    return(sf::st_sf(geometry = sf::st_sfc()))
  }
  
  # Get ISO3 code for country
  iso3 <- .standardize_country_codes(country, origin = "country.name", 
                                     destination = "iso3c", warn = FALSE)
  
  if (is.na(iso3)) {
    warning("Could not determine ISO3 code for '", country, "'. Skipping admin boundaries.", 
            call. = FALSE)
    return(sf::st_sf(geometry = sf::st_sfc()))
  }
  
  # Load GADM data
  admin_data <- tryCatch({
    geodata::gadm(country = iso3, level = admin_level, path = paths$gadm) %>%
      sf::st_as_sf() %>%
      sf::st_crop(countries_crop)
  }, error = function(e) {
    warning("Failed to load GADM data for ", country, ": ", e$message, call. = FALSE)
    sf::st_sf(geometry = sf::st_sfc())
  })
  
  admin_data
}

#' Create the final basemap plot
#' @param countries sf object with country boundaries
#' @param raster Raster data as tibble (optional)
#' @param roads sf object with roads (optional)
#' @param urban sf object with urban areas (optional)
#' @param urban_labels sf object with labels (optional)
#' @param lakes sf object with lakes (optional)
#' @param rivers sf object with rivers (optional)
#' @param admin sf object with admin boundaries (optional)
#' @param target_country Name of target country
#' @param ras_name Name of raster layer
#' @param crs Coordinate reference system
#' @param colors Named list of colors
#' @param show_graticule Show lat/lon grid?
#' @export
euiss_create_basemap_plot <- function(countries, raster, roads, urban, urban_labels,
                                      lakes, rivers, admin, target_country, ras_name,
                                      crs, colors, show_graticule) {
  
  # Start plot
  p <- ggplot2::ggplot(countries)
  
  # Layer order (fixed): relief → lakes → rivers → urban → roads → admin → boundaries → labels → graticule
  
  # 1. Raster relief (bottom layer)
  if (!is.null(raster) && nrow(raster) > 0) {
    ras_col <- gsub("-", ".", ras_name)
    if (ras_col %in% names(raster)) {
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
  }
  
  # 2. Lakes
  if (!is.null(lakes) && nrow(lakes) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = lakes,
        fill = colors$water,
        color = NA
      )
  }
  
  # 3. Rivers
  if (!is.null(rivers) && nrow(rivers) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = rivers,
        color = colors$water,
        linewidth = 0.3,
        alpha = 0.7
      )
  }
  
  # 4. Urban areas
  if (!is.null(urban) && nrow(urban) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = urban,
        fill = colors$urban,
        alpha = 0.5,
        color = NA
      )
  }
  
  # 5. Roads
  if (!is.null(roads) && nrow(roads) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = roads,
        color = colors$roads,
        alpha = 0.5,
        linewidth = 0.5
      )
  }
  
  # 6. Administrative boundaries
  if (!is.null(admin) && nrow(admin) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = admin,
        fill = NA,
        color = colors$admin,
        linewidth = 0.3,
        alpha = 0.6
      )
  }
  
  # 7. Other country boundaries (faded)
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
  
  # 8. Target country boundary (prominent)
  target_country_geom <- countries %>%
    dplyr::filter(.data$country == target_country)
  
  p <- p +
    ggplot2::geom_sf(
      data = target_country_geom,
      fill = NA,
      color = colors$borders,
      linewidth = 1
    )
  
  # 9. Urban labels
  if (!is.null(urban_labels) && nrow(urban_labels) > 0 && "name" %in% names(urban_labels)) {
    p <- p +
      ggplot2::geom_sf_text(
        data = urban_labels,
        ggplot2::aes(label = .data$name),
        hjust = "left",
        color = colors$urban_label,
        size = 2
      )
  }
  
  # 10. Country labels
  p <- p +
    ggplot2::geom_sf_text(
      ggplot2::aes(label = toupper(.data$country)),
      hjust = "left",
      size = 2,
      color = colors$borders
    )
  
  # Apply CRS and theme
  if (show_graticule) {
    p <- p + ggplot2::coord_sf(crs = crs, datum = sf::st_crs(4326))
  } else {
    p <- p + ggplot2::coord_sf(crs = crs, datum = NA)
  }
  
  p <- p + ggplot2::theme_void()
  
  p
}
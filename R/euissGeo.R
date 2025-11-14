#' \code{euissGeo} package
#'
#' Enhanced geospatial analysis package for the EU Institute for Security Studies.
#' Provides comprehensive tools for spatial data processing, visualization, and analysis
#' with seamless integration to the euissR styling package.
#'
#' @details
#' The euissGeo package offers specialized functions for:
#' \itemize{
#'   \item Loading and processing GISCO and Natural Earth geographic data
#'   \item Spatial data conversion and coordinate transformations
#'   \item Advanced spatial joining with country code standardization
#'   \item Geocoding and reverse geocoding services
#'   \item Creating cartograms and spike maps
#'   \item Raster data processing and visualization
#'   \item Comprehensive basemap creation with multiple data sources
#' }
#'
#' Key Features:
#' \itemize{
#'   \item \strong{Data Integration}: Seamless access to GISCO and Natural Earth data
#'   \item \strong{Smart Geocoding}: Robust geocoding with fallback strategies
#'   \item \strong{Flexible Visualization}: Create cartograms, spike maps, and styled basemaps
#'   \item \strong{EUISS Integration}: Full compatibility with euissR styling
#'   \item \strong{Error Resilience}: Comprehensive error handling and fallback mechanisms
#'   \item \strong{Performance Optimized}: Efficient processing of large spatial datasets
#' }
#'
#' @section Data Sources:
#' The package works with multiple geographic data sources:
#' \itemize{
#'   \item \strong{GISCO}: Eurostat's geographic information system (European focus)
#'   \item \strong{Natural Earth}: Global geographic data at multiple scales
#'   \item \strong{OpenStreetMap}: Geocoding services via Nominatim
#'   \item \strong{Custom Rasters}: Support for elevation, terrain, and thematic rasters
#' }
#'
#' @section Configuration:
#' Use \code{euiss_configure_paths()} to set up data directories and
#' \code{euiss_check_setup()} to verify package configuration.
#'
#' @section Getting Started:
#' \preformatted{
#' # Configure data paths
#' euiss_check_setup()
#' 
#' # Load country data
#' countries <- euiss_gisco(res = "20")
#' 
#' # Create a basemap
#' map <- euiss_basemap("France", db = "gisco")
#' 
#' # Geocode locations
#' locations <- data.frame(city = c("Paris", "Lyon"))
#' geocoded <- euiss_geocode(locations, loc = "city")
#' }
#'
#' @docType package
#' @name euissGeo-package
#' @aliases euissGeo
#' 
#' @import dplyr
#' @import sf
#' @import ggplot2
#' @import terra
#' @importFrom magrittr %>%
#' 
#' @author CD Trich
#' @references 
#' \itemize{
#'   \item GISCO: \url{https://ec.europa.eu/eurostat/web/gisco}
#'   \item Natural Earth: \url{https://www.naturalearthdata.com/}
#'   \item EUISS: \url{https://www.iss.europa.eu/}
#' }
#' 
#' @seealso 
#' Useful links:
#' \itemize{
#'   \item \code{\link{euiss_gisco}} for loading European geographic data
#'   \item \code{\link{euiss_basemap}} for creating comprehensive basemaps
#'   \item \code{\link{euiss_geocode}} for geocoding addresses and places
#'   \item \code{\link{euiss_spikemap}} for creating spike map visualizations
#'   \item \code{\link{euiss_cartogram}} for creating cartograms
#' }
#'
"_PACKAGE"

## Quiets concerns of R CMD check re: the .'s that appear in pipelines and NSE
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", ".data"))
}

## Package environment for storing configuration and cached data
.euissGeo_env <- new.env(parent = emptyenv())

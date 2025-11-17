#' euissGeo: Enhanced Geospatial Analysis for EUISS
#'
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
#' @section Key Features:
#' \itemize{
#'   \item \strong{Data Integration}: Seamless access to GISCO and Natural Earth data
#'   \item \strong{Smart Geocoding}: Robust geocoding with fallback strategies
#'   \item \strong{Flexible Visualization}: Cartograms, spike maps, and styled basemaps
#'   \item \strong{EUISS Integration}: Full compatibility with euissR styling
#'   \item \strong{Error Resilience}: Comprehensive error handling and fallbacks
#'   \item \strong{Performance}: Efficient processing of large spatial datasets
#' }
#'
#' @section Data Sources:
#' \itemize{
#'   \item \strong{GISCO}: Eurostat's geographic information system (European focus)
#'   \item \strong{Natural Earth}: Global geographic data at multiple scales
#'   \item \strong{OpenStreetMap}: Geocoding via Nominatim
#'   \item \strong{Raster Data}: Elevation, terrain, and thematic rasters
#' }
#'
#' @section Configuration:
#' Use \code{\link{euiss_configure_paths}} to set up data directories and
#' \code{\link{euiss_check_setup}} to verify package configuration.
#'
#' @section Getting Started:
#' \preformatted{
#' # Check setup
#' euiss_check_setup()
#' 
#' # Load country data
#' countries <- euiss_gisco(res = "20")
#' 
#' # Create a basemap
#' map <- euiss_basemap("France", db = "gisco")
#' 
#' # Geocode locations
#' cities <- data.frame(city = c("Paris", "Lyon"))
#' geocoded <- euiss_geocode(cities, loc = "city")
#' }
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
#'   \item \code{\link{euiss_gisco}} - Load European geographic data
#'   \item \code{\link{euiss_basemap}} - Create comprehensive basemaps
#'   \item \code{\link{euiss_geocode}} - Geocode addresses and places
#'   \item \code{\link{euiss_spikemap}} - Create spike map visualizations
#'   \item \code{\link{euiss_cartogram}} - Create cartograms
#'   \item \code{\link{euiss_left_join}} - Enhanced spatial joins
#'   \item \code{\link{euiss_coords_to_sf}} - Convert coordinates to sf objects
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
"_PACKAGE"

## Quiets R CMD check concerns about NSE
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", ".data", ".env"))
}
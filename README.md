# euissGeo

<!-- badges: start -->
[![R-CMD-check](https://github.com/cdtrich/euissGeo/workflows/R-CMD-check/badge.svg)](https://github.com/cdtrich/euissGeo/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

Enhanced geospatial analysis package for the EU Institute for Security Studies (EUISS).

## Overview

euissGeo provides comprehensive tools for spatial data processing, visualization, and analysis with seamless integration to the euissR styling package. The package offers specialized functions for working with European and global geographic data sources.

## Installation

```r
# Install from GitHub (when available)
# devtools::install_github("cdtrich/euissGeo")

# For now, install from local source:
devtools::install("path/to/euissGeo")
```

## Key Features

- 🗺️ **Multi-Source Data Integration**: Access GISCO (European) and Natural Earth (global) geographic data
- 🌍 **Robust Geocoding**: Multiple geocoding services with intelligent fallbacks  
- 📊 **Advanced Visualizations**: Create cartograms, spike maps, and styled basemaps
- 🎨 **EUISS Styling Integration**: Full compatibility with euissR visual identity
- 🖥️ **Cross-Platform Support**: Works on Windows, macOS, and Linux
- ⚙️ **Flexible Configuration**: Intelligent path detection and configuration management

## Quick Start

```r
library(euissGeo)

# Check package setup and configuration
euiss_check_setup()

# Load European countries data
countries <- euiss_gisco(res = "20")

# Create a styled basemap  
map <- euiss_basemap("France", db = "gisco")
print(map)

# Geocode locations
cities <- data.frame(city = c("Paris", "Lyon", "Marseille"))
geocoded <- euiss_geocode(cities, loc = "city")

# Create spike map visualization
countries$population <- runif(nrow(countries), 1e6, 1e8)
spike_data <- euiss_spikemap(countries, val = population, fact = 1000)
```

## Main Functions

### 🗂️ Data Loading & Configuration
- `euiss_gisco()` - Load GISCO European geographic data
- `euiss_load_raster()` - Enhanced raster loading with validation  
- `euiss_configure_paths()` - Configure data directories
- `euiss_check_setup()` - Verify package configuration

### 🔄 Spatial Conversion & Processing
- `euiss_coords_to_sf()` - Convert coordinate strings to sf objects
- `euiss_coords_char2dec()` - Parse various coordinate formats (DMS, DM, decimal)
- `euiss_crop_raster()` - Advanced raster cropping and projection
- `euiss_left_join()` - Enhanced spatial joins with country standardization

### 🌐 Geocoding Services  
- `euiss_geocode()` - Robust geocoding with retry logic
- `euiss_batch_geocode()` - Multi-service batch geocoding
- `euiss_reverse_geocode()` - Convert coordinates to place names

### 🗺️ Visualization & Cartography
- `euiss_basemap()` - Create comprehensive styled basemaps
- `euiss_spikemap()` - Generate spike map data for ggplot2  
- `euiss_cartogram()` - Create cartograms (continuous, Dorling, etc.)

## Configuration

### Data Paths
euissGeo can automatically detect data directories or you can configure them manually:

```r
# Auto-configure with defaults
paths <- euiss_configure_paths()

# Set custom base path
euiss_set_data_path("/path/to/your/data")

# Check current configuration
config <- euiss_get_config()
print(config)
```

### Environment Variables
Set persistent configuration in your `.Renviron` file:
```bash
EUISS_DATA_PATH=/path/to/your/data
EUISS_GISCO_PATH=/path/to/gisco/data
EUISS_RASTER_PATH=/path/to/raster/data
```

## Data Sources

### 🇪🇺 GISCO (Eurostat)
High-quality European administrative boundaries and statistical data.
```r
# Multiple resolutions available
countries_60m <- euiss_gisco(res = "60")  # 1:60M scale
countries_10m <- euiss_gisco(res = "10")  # 1:10M scale  
countries_01m <- euiss_gisco(res = "01")  # 1:1M scale

# Different boundary types
boundaries <- euiss_gisco(shp = "BN", bordertype = "COASTL")
labels <- euiss_gisco(shp = "LB")
```

### 🌍 Natural Earth
Global geographic data at multiple scales via rnaturalearth integration.
```r
# Use Natural Earth data for global coverage
global_map <- euiss_basemap("Brazil", db = "ne")
```

### 🗺️ OpenStreetMap (Nominatim)
Geocoding services for address and place name resolution.
```r
# Geocoding with automatic retry and rate limiting
locations <- euiss_geocode(city_data, loc = "city_name", verbose = TRUE)
```

## Examples

### Creating a Regional Map

```r
library(euissGeo)
library(ggplot2)
library(dplyr)

# Load Balkan countries with Kosovo handling
balkans <- c("Albania", "Bosnia and Herzegovina", "Croatia", 
            "Kosovo", "Montenegro", "North Macedonia", "Serbia", "Slovenia")

# Get GISCO data  
countries <- euiss_gisco(res = "10", verbose = TRUE) %>%
  filter(NAME_ENGL %in% balkans)

# Create styled map
ggplot(countries) +
  geom_sf(fill = "lightblue", color = "white", linewidth = 0.5) +
  theme_void() +
  labs(title = "Balkan Peninsula",
       caption = "Source: Eurostat GISCO")
```

### Spike Map Visualization

```r
# Add population data  
countries$population <- c(2.8e6, 3.3e6, 4.1e6, 1.9e6, 6.3e5, 2.1e6, 6.9e6, 2.1e6)

# Create spike data
spike_data <- euiss_spikemap(
  countries, 
  val = population, 
  fact = 0.001,
  polygon_format = TRUE
)

# Create visualization
ggplot() +
  geom_sf(data = countries, fill = "lightgray", color = "white") +
  geom_polygon(data = spike_data, 
               aes(x = x, y = y, group = spike_id, fill = population),
               color = "darkred", alpha = 0.7) +
  scale_fill_viridis_c(name = "Population", labels = scales::comma) +
  theme_void() +
  labs(title = "Population Spike Map")
```

### Cartogram Creation

```r
# Create Dorling cartogram
cartogram_data <- euiss_cartogram(
  countries, 
  weight = population,
  type = "dorling",
  k = 0.05
)

# Plot comparison
original <- ggplot(countries) + 
  geom_sf(aes(fill = population)) + 
  labs(title = "Original Geography")

cartogram <- ggplot(cartogram_data) + 
  geom_sf(aes(fill = population)) +
  labs(title = "Population Cartogram")

# Combine plots (requires patchwork)
# original + cartogram
```

### Comprehensive Basemap

```r
# Create detailed basemap with multiple layers
albania_map <- euiss_basemap(
  country = "Albania",
  ras = "GRAY_HR_SR_W",           # Elevation raster
  db = "gisco",                   # Data source
  crs = "+proj=laea +lat_0=41 +lon_0=20",  # Equal-area projection
  buffer = 50000,                 # 50km buffer
  show_progress = TRUE
)

print(albania_map)
```

### Batch Geocoding

```r
# Prepare location data
cities <- data.frame(
  name = c("Tirana", "Durrës", "Vlorë", "Shkodër", "Elbasan"),
  country = "Albania"
)

# Geocode with fallback strategies
geocoded <- euiss_batch_geocode(
  paste(cities$name, cities$country, sep = ", "),
  services = c("nominatim"),
  verbose = TRUE
)

# Convert to sf for mapping
cities_sf <- euiss_coords_to_sf(
  geocoded, 
  coords = "coords", 
  sep = ", "
)
```

## Dependencies

### Required
```r
# Core spatial packages
install.packages(c("sf", "terra", "dplyr", "ggplot2"))
```

### Suggested  
```r
# Enhanced functionality
install.packages(c(
  "rnaturalearth",    # Global geographic data
  "tmaptools",        # Geocoding services  
  "countrycode",      # Country standardization
  "cartogram",        # Cartogram creation
  "progress"          # Progress bars
))

# EUISS styling integration
devtools::install_github("cdtrich/euissR")
```

### System Requirements
Spatial libraries (automatically handled by sf/terra on most systems):
- **GDAL** (>= 3.0.0) - Geospatial Data Abstraction Library
- **GEOS** (>= 3.8.0) - Geometry Engine Open Source  
- **PROJ** (>= 6.0.0) - Cartographic Projections Library

## Integration with euissR

euissGeo automatically detects and integrates with the euissR package:

```r
# Install both packages
devtools::install_github("cdtrich/euissR")
library(euissR)   # Load first for styling
library(euissGeo) # Will detect euissR and use EUISS colors

# Basemaps automatically use EUISS color scheme
map <- euiss_basemap("Kosovo")  # Uses teal, grey, etc. from euissR
```

## Performance Tips

### Large Datasets
```r
# Use lower resolution for faster processing
countries_fast <- euiss_gisco(res = "60")  # vs res = "10"

# Aggregate rasters for performance
elevation <- euiss_crop_raster(
  my_raster,
  crop_extent = study_area, 
  aggregate_fact = 4,  # 4x4 aggregation
  crs = target_crs
)
```

### Batch Processing
```r
# Process multiple raster files efficiently
dem_files <- list.files("dem_folder", pattern = "\\.tif$", full.names = TRUE)

processed <- euiss_batch_raster_process(
  dem_files,
  output_dir = "processed_dems",
  target_crs = 3857,
  aggregate_fact = 2
)
```

## Troubleshooting

### Common Issues

**Data not found:**
```r
# Check configuration
euiss_check_setup()

# Configure paths manually
euiss_set_data_path("/your/data/path")
```

**Geocoding failures:**
```r
# Check internet connection
# Try with verbose output
result <- euiss_geocode(data, loc = "location", verbose = TRUE)
```

**CRS issues:**
```r
# Check current CRS
sf::st_crs(your_sf_object)

# Transform to common CRS
your_sf_object <- sf::st_transform(your_sf_object, 4326)
```

**System dependencies on Linux:**
```bash
sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev
```

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)  
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Development Setup
```r
# Clone and setup development environment
devtools::install_deps()          # Install dependencies
devtools::document()              # Generate documentation  
devtools::check()                 # Run checks
devtools::test()                  # Run tests
```

## Citation

```bibtex
@software{euissGeo,
  title = {euissGeo: Enhanced Geospatial Analysis for EUISS Research},
  author = {CD Trich},
  year = {2024},
  version = {0.1.0},
  url = {https://github.com/cdtrich/euissGeo}
}
```

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Links

- 📧 **Email**: christian.g.dietrich@gmail.com
- 🐛 **Issues**: [GitHub Issues](https://github.com/cdtrich/euissGeo/issues)
- 📖 **euissR Package**: [cdtrich/euissR](https://github.com/cdtrich/euissR)
- 🌍 **EUISS**: [iss.europa.eu](https://www.iss.europa.eu/)
# euissGeo Package Organization Plan

## Core Functions by Category

### 📍 **Geographic Data Retrieval**
- `euiss_gisco()` - Enhanced GISCO data access
- `euiss_gisco_kosovo()` - Kosovo-specific handling

### 🗺️ **Cartographic & Visualization**  
- `euiss_basemap()` - Comprehensive base map creation
- `euiss_cartogram()` - Cartogram visualization
- `euiss_spikemap()` - Spike map creation
- `geom_relief_euiss()` - Relief/elevation geom

### 🛠️ **Spatial Operations**
- `euiss_crop_raster()` - Raster cropping utilities
- `euiss_load_raster()` - Raster loading with error handling
- `euiss_left_join()` - Spatial joins with country codes

### 📐 **Coordinate Operations**
- `euiss_coords_to_sf()` - Convert coordinates to SF objects
- `euiss_coords_char2dec()` - Parse coordinate strings 
- `euiss_sf_to_coords()` - Extract coordinates from SF
- `euiss_geocode()` - Geocoding functionality

### 🔧 **General Utilities** 
- `euiss_substring()` - String manipulation helper
- `euiss_unzip()` - Interactive file extraction
- `euiss_date_from_excel()` - Excel date conversion
- `euiss_locator()` - Interactive point selection

## Package Structure
```
euissGeo/
├── DESCRIPTION
├── NAMESPACE  
├── LICENSE
├── README.md
├── R/
│   ├── data-retrieval.R       # GISCO functions
│   ├── cartography.R          # Basemap, cartogram, spikemap
│   ├── spatial-operations.R   # Crop, load, joins
│   ├── coordinates.R          # Coordinate transformations
│   ├── geom-relief.R          # ggplot2 relief geom
│   └── utilities.R            # General helper functions
├── man/                       # Documentation
├── tests/
└── vignettes/
    ├── getting-started.Rmd
    ├── creating-basemaps.Rmd
    └── coordinate-operations.Rmd
```

## Integration with euissR

### Color & Theme Consistency
```r
# All euissGeo functions should use euissR colors
library(euissR)  # Always imported as dependency

# Example in basemap function:
euiss_basemap <- function(..., 
                         colors = list(
                           water = get("teal", envir = .euiss_env),
                           borders = get("col_axis", envir = .euiss_env),
                           urban = get("col_grid", envir = .euiss_env)
                         )) {
  # Function implementation
}
```

### Documentation Cross-References
- All euissGeo help files reference euissR for styling
- README shows integrated workflow examples
- Vignettes demonstrate combined usage

## Development Priorities

### High Priority (Core Functionality)
1. `euiss_gisco()` - Clean up path dependencies, add error handling
2. `euiss_basemap()` - Remove global variable assignments, modularize
3. `euiss_left_join()` - Simple but essential function
4. `euiss_geocode()` - Geographic workflows essential

### Medium Priority (Enhanced Features)
1. `euiss_spikemap()` - Good visualization tool
2. Coordinate conversion functions 
3. Raster utilities

### Low Priority (Nice to Have)
1. Interactive utilities (`euiss_unzip`, `euiss_locator`)
2. Date/string helpers

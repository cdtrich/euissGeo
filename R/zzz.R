#' Package initialization functions
#' 
#' Functions that run when the package is loaded or attached.
#' 
#' @name package-hooks
#' @keywords internal
NULL

#' Initialize package environment and check dependencies
#' @param libname Library name (unused)
#' @param pkgname Package name (unused)
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  
  # Initialize package environment variables
  assign("initialized", FALSE, envir = .euissGeo_env)
  assign("data_paths", list(), envir = .euissGeo_env)
  assign("config_cache", list(), envir = .euissGeo_env)
  
  # Check for critical dependencies silently
  critical_deps <- c("sf", "dplyr", "ggplot2")
  missing_critical <- character(0)
  
  for (pkg in critical_deps) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_critical <- c(missing_critical, pkg)
    }
  }
  
  # Store dependency status
  assign("missing_critical_deps", missing_critical, envir = .euissGeo_env)
  
  # Try to configure default paths silently
  tryCatch({
    default_paths <- euiss_configure_paths(create_missing = FALSE, verbose = FALSE)
    assign("default_paths", default_paths, envir = .euissGeo_env)
  }, error = function(e) {
    # Fail silently during load
  })
  
  # Mark as initialized
  assign("initialized", TRUE, envir = .euissGeo_env)
}

#' Package attachment message
#' @param libname Library name (unused)  
#' @param pkgname Package name (unused)
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  
  # Get package version
  pkg_version <- utils::packageVersion("euissGeo")
  
  # Basic welcome message
  packageStartupMessage(
    paste0("euissGeo ", pkg_version, " loaded successfully."),
    "\nEnhanced geospatial analysis for EUISS research."
  )
  
  # Check for missing critical dependencies
  if (exists("missing_critical_deps", envir = .euissGeo_env)) {
    missing_critical <- get("missing_critical_deps", envir = .euissGeo_env)
    
    if (length(missing_critical) > 0) {
      packageStartupMessage(
        "Warning: Missing critical dependencies: ", 
        paste(missing_critical, collapse = ", "),
        "\nInstall with: install.packages(c(", 
        paste0('"', missing_critical, '"', collapse = ", "), "))"
      )
    }
  }
  
  # Check euissR integration
  if (requireNamespace("euissR", quietly = TRUE)) {
    packageStartupMessage("euissR integration available - enhanced styling enabled.")
  } else {
    packageStartupMessage(
      "Install euissR for enhanced styling: devtools::install_github('cdtrich/euissR')"
    )
  }
  
  # Suggest configuration check
  packageStartupMessage(
    "Use euiss_check_setup() to verify package configuration."
  )
}

#' Package detachment cleanup
#' @param libpath Library path (unused)
#' @keywords internal  
.onUnload <- function(libpath) {
  
  # Clean up package environment
  if (exists(".euissGeo_env")) {
    rm(list = ls(envir = .euissGeo_env), envir = .euissGeo_env)
  }
  
  # Clean up any temporary files or connections
  # (Add specific cleanup code here if needed)
}

#' Check if package is properly initialized
#' @return Logical indicating initialization status
#' @keywords internal
.is_initialized <- function() {
  exists("initialized", envir = .euissGeo_env) && 
    get("initialized", envir = .euissGeo_env)
}

#' Get package environment for testing/debugging
#' @return Package environment
#' @keywords internal
.get_pkg_env <- function() {
  .euissGeo_env
}

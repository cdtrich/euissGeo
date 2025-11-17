#' Package Initialization and Hooks
#' 
#' Functions that run when the package is loaded or attached.
#' 
#' @name package-hooks
#' @keywords internal
NULL

# Package environment for storing configuration and cached data
.euissGeo_env <- new.env(parent = emptyenv())

#' Package load hook
#' @param libname Library name (unused)
#' @param pkgname Package name (unused)
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  
  # Initialize package environment
  assign("initialized", FALSE, envir = .euissGeo_env)
  assign("data_paths", list(), envir = .euissGeo_env)
  assign("config_cache", list(), envir = .euissGeo_env)
  
  # Check critical dependencies silently
  critical_deps <- c("sf", "dplyr", "ggplot2")
  missing_critical <- critical_deps[!sapply(critical_deps, requireNamespace, quietly = TRUE)]
  
  assign("missing_critical_deps", missing_critical, envir = .euissGeo_env)
  
  # Try to configure default paths silently
  default_paths <- tryCatch({
    euiss_configure_paths(create_missing = FALSE, verbose = FALSE)
  }, error = function(e) NULL)
  
  assign("default_paths", default_paths, envir = .euissGeo_env)
  assign("initialized", TRUE, envir = .euissGeo_env)
  
  invisible()
}

#' Package attach hook
#' @param libname Library name (unused)  
#' @param pkgname Package name (unused)
#' @keywords internal
#' @noRd
.onAttach <- function(libname, pkgname) {
  
  pkg_version <- utils::packageVersion("euissGeo")
  
  packageStartupMessage(
    "euissGeo ", pkg_version, "\n",
    "Enhanced geospatial analysis for EUISS research"
  )
  
  # Check for missing critical dependencies
  missing_critical <- get0("missing_critical_deps", envir = .euissGeo_env, ifnotfound = character(0))
  
  if (length(missing_critical) > 0) {
    packageStartupMessage(
      "WARNING: Missing critical dependencies: ", 
      paste(missing_critical, collapse = ", "), "\n",
      "Install with: install.packages(c('", 
      paste(missing_critical, collapse = "', '"), "'))"
    )
  }
  
  # Check euissR integration
  if (requireNamespace("euissR", quietly = TRUE)) {
    packageStartupMessage("euissR integration: enabled")
  } else {
    packageStartupMessage(
      "For EUISS styling: devtools::install_github('cdtrich/euissR')"
    )
  }
  
  packageStartupMessage("Use euiss_check_setup() to verify configuration")
  
  invisible()
}

#' Package unload hook
#' @param libpath Library path (unused)
#' @keywords internal  
#' @noRd
.onUnload <- function(libpath) {
  
  # Clean up package environment
  if (exists(".euissGeo_env")) {
    rm(list = ls(envir = .euissGeo_env), envir = .euissGeo_env)
  }
  
  invisible()
}

#' Check if package is properly initialized
#' @return Logical indicating initialization status
#' @keywords internal
#' @noRd
.is_initialized <- function() {
  exists("initialized", envir = .euissGeo_env) && 
    isTRUE(get("initialized", envir = .euissGeo_env))
}

#' Get package environment (for testing/debugging)
#' @return Package environment
#' @keywords internal
#' @noRd
.get_pkg_env <- function() {
  .euissGeo_env
}
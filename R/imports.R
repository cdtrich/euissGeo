#' Package imports and global variables
#' 
#' @keywords internal
#' @noRd
NULL

# Import operators
#' @importFrom magrittr %>%
#' @importFrom rlang .data .env
#' @importFrom stats setNames
#' @importFrom rlang :=
NULL

# Quiets R CMD check notes about NSE and pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".",
    ".data",
    ".env",
    # Add any other NSE variables used in the package
    "geometry",
    "x",
    "y",
    "val",
    "spike_id",
    "x_poly",
    "y_poly",
    "coords"
  ))
}
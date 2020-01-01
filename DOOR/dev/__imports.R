#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


check_tibble <- function() {
  if (!is_installed("tibble")) {
    abort("The tibble package must be installed")
  }
}

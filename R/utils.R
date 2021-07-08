
#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' @importFrom methods as
NULL

#' @importFrom utils unzip download.file read.csv
NULL

#' Inverse of in call for convenience
#' Calculates the set of entries not present in the second vector
#'
#' @param a First vector
#' @param b Second vector
#' @keywords internal
#' @noRd
#' @author Martin Jung

`%notin%` = function(a, b){!(a %in% b)}

#' Custom logging function for scripts
#'
#' @param title The title in the log output
#' @param ... Any additional outputs or words for display
#' @keywords internal
#' @noRd
#' @author Martin Jung

myLog <- function(title = "[Processing]",...) {
  cat(paste0(title,' ', Sys.time(), " | ", ..., "\n"))
}

#' Semantic labelled vector class
#'
#' @name haven_labelled_defined
#'
#' @description
#' The class `haven_labelled_defined` represents a semantically
#' enriched labelled vector created by [defined()].
#'
#' Objects of this class inherit from:
#'
#' \itemize{
#'   \item `[haven:labelled]()` for numeric,
#'     character, and factor data;
#'   \item `Date` for temporal dates;
#'   \item `POSIXct` and `POSIXt` for timestamps;
#'   \item `logical` for boolean data.
#' }
#'
#' The class tag enables S3 method dispatch for:
#'
#' \itemize{
#'   \item `[as_character()]`
#'   \item `[as_numeric()]`
#'   \item `[as_logical()]`
#'   \item \code{as.Date}
#'   \item \code{as.POSIXct}
#'   \item \code{\link{print.haven_labelled_defined}}
#'   \item \code{\link{summary.haven_labelled_defined}}
#' }
#'
#' Users normally do not construct this class directly; it is returned
#' as the result of calling [defined()].
#'
#' @return No value is returned. This documentation page exists so that
#'   the class can be referenced in help topics.
#'
#' @keywords internal
NULL

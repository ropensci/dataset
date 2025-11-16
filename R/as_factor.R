#' Coerce a defined vector to a factor
#'
#' @description
#' `as_factor()` converts a [`defined()`][defined] vector into a standard R
#' factor. If value labels are present, they are turned into factor levels
#' via [haven::as_factor()]. Otherwise, the underlying values are converted
#' with [base::factor()].
#' @details
#' Use \code{strip_attributes = TRUE} when flattening or preparing data for
#' external pipelines, but keep the default when working with defined
#' vectors directly.
#' @param x A vector created with [defined()].
#' @param strip_attributes Logical; should semantic metadata attributes
#'   (such as \code{label}, \code{unit}, \code{definition}, and
#'   \code{namespace}) be removed from the returned vector?
#'   Defaults to \code{FALSE}.
#' @param ... Reserved for future extensions.
#'
#' @return A factor vector.
#'
#' @examples
#' sex <- defined(
#'   c(0, 1, 1, 0),
#'   label  = "Sex",
#'   labels = c("Female" = 0, "Male" = 1)
#' )
#'
#' as_factor(sex)
#' as_factor(sex, strip_attributes = FALSE)
#'
#' @export
as_factor <- function(x, ...) {
  UseMethod("as_factor")
}

#' @export
#' @importFrom haven as_factor labelled
#' @importFrom vctrs vec_data
as_factor.haven_labelled_defined <- function(
    x,
    strip_attributes = TRUE,
    ...) {
  vals <- vctrs::vec_data(x)
  lbls <- attr(x, "labels", exact = TRUE)

  # CASE 1: value labels present → labelled → haven::as_factor()
  if (!is.null(lbls)) {
    fac <- haven::as_factor(
      haven::labelled(vals, labels = lbls),
      ...
    )
  } else {
    # CASE 2: no value labels → plain factor on values
    fac <- factor(vals)
  }

  # (We don't touch class(fac): it's already a plain factor)
  # Optionally copy semantic metadata
  if (!strip_attributes) {
    attr(fac, "unit")      <- attr(x, "unit",      exact = TRUE)
    attr(fac, "concept")   <- attr(x, "concept",   exact = TRUE)
    attr(fac, "namespace") <- attr(x, "namespace", exact = TRUE)
  }

  fac
}


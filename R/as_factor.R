#' Coerce a defined vector to a factor
#'
#' @description
#' `as_factor()` converts a [`defined()`][defined] vector into a standard R
#' factor. If value labels are present, they are turned into factor levels
#' via [haven::as_factor()]. Otherwise, the underlying values are converted
#' with [base::factor()].
#'
#' If `preserve_attributes = TRUE`, semantic metadata (`unit`, `concept`,
#' `namespace`) is copied to the resulting factor, but the
#' `"haven_labelled_defined"` class itself is dropped.
#'
#' @param x A vector created with [defined()].
#' @param preserve_attributes Logical; whether to retain semantic
#'   metadata on the factor. Defaults to `FALSE`.
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
#' as_factor(sex, preserve_attributes = TRUE)
#'
#' @export
as_factor <- function(x, ...) {
  UseMethod("as_factor")
}

#' @export
#' @importFrom haven as_factor labelled
#' @importFrom vctrs vec_data
as_factor.haven_labelled_defined <- function(x,
                                             preserve_attributes = FALSE,
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
  if (preserve_attributes) {
    attr(fac, "unit")      <- attr(x, "unit",      exact = TRUE)
    attr(fac, "concept")   <- attr(x, "concept",   exact = TRUE)
    attr(fac, "namespace") <- attr(x, "namespace", exact = TRUE)
  }

  fac
}


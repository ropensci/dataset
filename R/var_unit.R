#' Get or Set a Unit of Measure
#'
#' Adds or retrieves a unit of measure (UoM) attribute to a vector. Units
#' provide semantic meaning for numeric or character data <U+2014> such as currency,
#' weight, or time <U+2014> helping prevent incorrect operations like merging values
#' measured in incompatible units.
#'
#' @param x A vector.
#' @param value A character string specifying the unit of measure, or `NULL` to
#'   remove the unit.
#' @param ... Further arguments for method extensions.
#'
#' @details The `"unit"` attribute stores a machine-readable representation of a
#' unit of measure (e.g., `"kg"`, `"USD"`, `"days"`). This is useful when
#' working with linked open data or when combining data from multiple sources
#' where silent mismatches in units could cause errors.
#'
#' For full integration with semantic metadata (e.g., labels, concepts,
#' namespaces), use [defined()] vectors or [dataset_df()] objects.
#'
#' `get_variable_units()` is an alias for `var_unit()`.
#'
#' See `vignette("defined", package = "dataset")` for end-to-end examples
#' involving semantic enrichment.
#'
#' @return
#' - `var_unit(x)` returns the `"unit"` attribute as a character string.
#' - `var_unit(x) <- value` sets, updates, or removes the unit and returns
#' the modified vector invisibly.
#'
#' @examples
#' # Retrieve the unit of measure (if defined)
#' var_unit(orange_df$circumference)
#'
#' # Regular data.frame columns have no unit by default
#' var_unit(mtcars$wt)
#'
#' # Add a unit to a column
#' var_unit(mtcars$wt) <- "1000 lbs"
#'
#' # Remove the unit
#' var_unit(mtcars$wt) <- NULL
#'
#' @family defined metadata methods and functions
#' @export
var_unit <- function(x, ...) {
  # rlang::check_dots_used()
  UseMethod("var_unit")
}

#' @export
var_unit.default <- function(x, ...) {
  attr(x, "unit", exact = TRUE)
}

#' @rdname var_unit
#'
#' @description
#' The `var_unit<-` assignment method sets, updates, or removes the `"unit"`
#' attribute of a vector. This can be used with `defined()` vectors or base
#' vectors to ensure consistent semantic annotation.
#'
#' @param value A character string defining the unit of measure, or `NULL` to
#'   remove the unit.
#'
#' @return
#' The modified object `x`, returned invisibly with the updated `"unit"`
#' attribute.
#'
#' @export
`var_unit<-` <- function(x, value) {
  UseMethod("var_unit<-")
}


#' @rdname var_unit
#' @export
`var_unit<-.default` <- function(x, value) {
  unit_attribute(x) <- value
  x
}


#' @rdname var_unit
#' @export
get_variable_units <- var_unit


#' @rdname var_unit
#'
#' @description
#' `unit_attribute()` is a low-level helper to directly access the `"unit"`
#' attribute of a vector, without applying fallback logic. It is mainly used
#' internally.
#'
#' @return
#' The `"unit"` attribute of the object `x`, or `NULL` if not set.
#'
#' @export
unit_attribute <- function(x) {
  attr(x, "unit", exact = TRUE)
}

#' @rdname var_unit
#'
#' @description
#' `get_unit_attribute()` is an alias for `unit_attribute()`, included for naming
#' consistency in codebases that distinguish getter/setter patterns.
#'
#' @export
get_unit_attribute <- function(x) {
  unit_attribute(x)
}


#' @rdname var_unit
#'
#' @description
#' `set_unit_attribute()` is the low-level assignment function that sets or
#' removes the `"unit"` attribute of an object. Used internally by
#' `unit_attribute<-`.
#'
#' @param value A single character string or `NULL`. If not of length one, an
#'   error is thrown.
#'
#' @return The object `x` with updated `"unit"` attribute.
#'
#' @export
set_unit_attribute <- function(x, value) {
  if ((!is.character(value) && !is.null(value)) || length(value) > 1) {
    stop(
      "`unit` should be a single character string or NULL",
      call. = FALSE,
      domain = "R-dataset"
    )
  }
  attr(x, "unit") <- value
  x
}

#' @rdname var_unit
#' @export
`unit_attribute<-` <- set_unit_attribute

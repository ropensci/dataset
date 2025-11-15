#' Create a semantically enriched vector with variable-level metadata
#'
#' `defined()` constructs a vector that behaves like a base R vector but carries
#' semantic metadata used for documentation, validation, and interoperability.
#' The resulting object inherits from [`haven::labelled()`] (for numeric,
#' character, and factor data) or from base date/time classes, and adds:
#'
#' * a human-readable variable label,
#' * an optional unit of measurement,
#' * a **concept URI** identifying the meaning of the variable
#'   (formerly called *definition*),
#' * an optional namespace used for value-level URI expansion,
#' * optional labelled values (where supported).
#'
#' The `concept` attribute is a general semantic anchor and may refer to:
#' a measure or dimension concept (SDMX-style), a property IRI (e.g. Wikibase),
#' or any URI that defines or describes the variable.
#'
#' `defined()` vectors preserve their metadata during subsetting, printing,
#' summarizing, comparisons, and many tidyverse operations. They integrate
#' smoothly with [`dataset_df()`] objects and can be safely flattened via
#' [`as.data.frame()`], [`as_tibble()`], or coercion helpers such as
#' [`as_numeric()`] and [`as_character()`].
#'
#' @section Supported input types:
#' * numeric (integer or double)
#' * character
#' * factor (converted via [`labelled::to_labelled()`])
#' * [`Date`]
#' * [`POSIXct`]
#' * [`haven::labelled()`]
#' * logical (with restrictions: logical vectors **cannot** have value labels)
#'
#' @param x A vector to annotate.
#' @param labels Optional named vector of value labels. Only supported for
#'   numeric or character vectors (not for logical).
#' @param label A short human-readable variable label (character of length 1).
#' @param unit Unit of measurement (character length 1) or `NULL`.
#' @param concept A URI or identifier describing the meaning or definition
#'   of the variable. This replaces the deprecated `definition` argument.
#' @param namespace Optional string or named character vector used to generate
#'   value-level URIs via substitution (`$1` macro).
#' @param ... For backward compatibility; the deprecated `definition`
#'   argument is still accepted and mapped to `concept`.
#'
#' @return A vector of class `"haven_labelled_defined"` or `"datetime_defined"`,
#'   depending on the input type.
#'
#' @importFrom labelled is.labelled
#'
#' @seealso
#'   [`is.defined()`],
#'   [`as_numeric()`],
#'   [`as_character()`],
#'   [`as_logical()`],
#'   [`strip_defined()`],
#'   [`dataset_df()`]
#'
#' @examples
#' gdp_vector <- defined(
#'   c(3897, 7365, 6753),
#'   label = "Gross Domestic Product",
#'   unit = "million dollars",
#'   concept = "http://data.europa.eu/83i/aa/GDP"
#' )
#'
#' is.defined(gdp_vector)
#' print(gdp_vector)
#' summary(gdp_vector)
#' gdp_vector[1:2]
#'
#' @export
# Main generic ---------------------------------------------------------------
defined <- function(x,
                    labels = NULL,
                    label = NULL,
                    unit = NULL,
                    concept = NULL,
                    namespace = NULL,
                    ...) {
  dots <- list(...)

  # ------------------------------------------------------------------
  # DEPRECATED ARGUMENT SUPPORT
  # ------------------------------------------------------------------
  if (!is.null(dots$definition)) {
    warning("`definition` is deprecated; use `concept` instead.", call. = FALSE)
    if (is.null(concept)) {
      concept <- dots$definition
    }
  }

  # ------------------------------------------------------------------
  # ARGUMENT VALIDATION
  # ------------------------------------------------------------------
  if (!is.null(label) && (!is.character(label) || length(label) != 1)) {
    stop("`label` must be a character(1) or NULL.", call. = FALSE)
  }

  if (!is.null(unit) && (!is.character(unit) || length(unit) != 1)) {
    stop("`unit` must be a character(1) or NULL.", call. = FALSE)
  }

  if (!is.null(concept) && (!is.character(concept) || length(concept) != 1)) {
    stop("`concept` must be a character(1) or NULL.", call. = FALSE)
  }

  if (!is.null(namespace) &&
    (!is.character(namespace) || length(namespace) != 1)) {
    stop("`namespace` must be a character(1) or NULL.", call. = FALSE)
  }


  # ------------------------------------------------------------------
  # DISPATCH BY TYPE
  # ------------------------------------------------------------------

  # LOGICAL -----------------------------------------------------------
  if (is.logical(x)) {
    if (!is.null(labels)) {
      stop(
        "defined(logical): value labels are not supported for logical vectors.\n",
        "Use factor or character instead.",
        call. = FALSE
      )
    }

    tmp <- x
    attr(tmp, "label") <- label
    attr(tmp, "unit") <- unit
    attr(tmp, "concept") <- concept
    attr(tmp, "namespace") <- namespace
    class(tmp) <- c("haven_labelled_defined", "logical")

    return(tmp)
  }

  # NUMERIC / INTEGER ------------------------------------------------
  if (is.numeric(x)) {
    x_raw <- vctrs::vec_data(x)
    labels <- vec_cast_named(labels, x_raw, x_arg = "labels", to_arg = "x")

    return(new_labelled_defined(
      x_raw,
      labels = labels,
      label = label,
      unit = unit,
      concept = concept,
      namespace = namespace
    ))
  }

  # CHARACTER ---------------------------------------------------------
  if (is.character(x)) {
    return(new_labelled_defined(
      x,
      labels = labels,
      label = label,
      unit = unit,
      concept = concept,
      namespace = namespace
    ))
  }

  # FACTOR ------------------------------------------------------------
  if (is.factor(x)) {
    labelled_x <- labelled::to_labelled(x)
    var_unit(labelled_x) <- unit
    var_concept(labelled_x) <- concept
    var_namespace(labelled_x) <- namespace

    class(labelled_x) <- c("haven_labelled_defined", class(labelled_x))
    return(labelled_x)
  }

  # HAVEN LABELLED ----------------------------------------------------
  if (is.labelled(x)) {
    x2 <- x
    var_unit(x2) <- unit
    var_concept(x2) <- concept
    var_namespace(x2) <- namespace

    class(x2) <- c("haven_labelled_defined", class(x2))
    return(x2)
  }

  # DATE --------------------------------------------------------------
  if (inherits(x, "Date")) {
    tmp <- x
    attr(tmp, "label") <- label
    attr(tmp, "unit") <- unit
    attr(tmp, "concept") <- concept
    attr(tmp, "namespace") <- namespace
    class(tmp) <- c("haven_labelled_defined", "Date")
    return(tmp)
  }

  # POSIXct -----------------------------------------------------------
  if (inherits(x, "POSIXct")) {
    tmp <- x
    attr(tmp, "label") <- label
    attr(tmp, "unit") <- unit
    attr(tmp, "concept") <- concept
    attr(tmp, "namespace") <- namespace
    class(tmp) <- c("haven_labelled_defined", "POSIXct", "POSIXt")
    return(tmp)
  }

  # UNSUPPORTED -------------------------------------------------------
  stop(
    "defined(x): `x` must be logical, numeric, character, factor, Date, ",
    "POSIXct, or a labelled vector.",
    call. = FALSE
  )
}



#' @rdname defined
#' @export
is.defined <- function(x) {
  any(inherits(x, "haven_labelled_defined"), inherits(x, "datetime_defined"))
}

#' @keywords internal
vec_ptype_abbr.haven_labelled_defined <- function(x, ...) {
  "defined"
}

#' @keywords internal
vec_ptype2.double.haven_labelled_defined <- function(x, y, ...) double()

#' @keywords internal
vec_cast.double.haven_labelled_defined <- function(x, to, ...) vctrs::vec_data(x)

#' @keywords internal
vec_cast.character.haven_labelled_defined <- function(x, to, ...) vctrs::vec_data(x)

#' From haven
#' @keywords internal
#' @importFrom vctrs vec_cast
vec_cast_named <- function(x, to, ...) {
  stats::setNames(vctrs::vec_cast(x, to, ...), names(x))
}

#' @importFrom tibble new_tibble
#' @importFrom haven labelled
#' @keywords internal
new_labelled_defined <- function(x = double(),
                                 labels = NULL,
                                 label = NULL,
                                 unit = NULL,
                                 concept = NULL,
                                 namespace = NULL) {
  if (!is.null(unit) && (!is.character(unit) || length(unit) != 1)) {
    stop("defined(..., unit): 'unit' must be a character vector of length one.")
  }

  if (!is.null(concept) && (!is.character(concept) || length(concept) != 1)) {
    stop("defined(..., defintion): 'concept' must be a character vector of length one or NULL.")
  }

  if (!is.null(label) && (!is.character(label) || length(label) != 1)) {
    stop("defined(..., label): 'label' must be a character vector of length one or NULL.")
  }

  if (!is.null(namespace) && (!is.character(namespace) || length(namespace) != 1)) {
    stop("defined(..., namespace): 'namespace' must be a character vector of length one or NULL.")
  }

  tmp <- haven::labelled(x, labels = labels, label = label)

  attr(tmp, "unit") <- unit
  attr(tmp, "concept") <- concept
  attr(tmp, "namespace") <- namespace
  attr(tmp, "class") <- c("haven_labelled_defined", class(tmp))

  tmp
}

#' @importFrom tibble new_tibble
#' @keywords internal
new_datetime_defined <- function(x,
                                 label = NULL,
                                 unit = NULL,
                                 concept = NULL,
                                 namespace = NULL) {
  if (!is.null(unit) && (!is.character(unit) || length(unit) != 1)) {
    stop("defined(..., unit): 'unit' must be a character vector of length one.")
  }

  if (!is.null(concept) && (!is.character(concept) || length(concept) != 1)) {
    stop("defined(..., defintion): 'concept' must be a character vector of length one.")
  }

  if (!is.null(label) && (!is.character(label) || length(label) != 1)) {
    stop("defined(..., label): 'label' must be a character vector of length one.")
  }

  if (!is.null(namespace) && (!is.character(namespace) || length(namespace) != 1)) {
    stop("defined(..., namespace): 'namespace' must be a character vector of length one or NULL.")
  }

  tmp <- x

  attr(tmp, "unit") <- unit
  attr(tmp, "concept") <- concept
  attr(tmp, "namespace") <- namespace
  attr(tmp, "class") <- c("datetime_defined", class(tmp))

  tmp
}


## Subsetting ------------------------------------------------------

#' @export
`[.haven_labelled_defined` <- function(x, i, ...) {
  result <- NextMethod("[")
  most_attrs <- c("label", "unit", "concept", "namespace", "labels")
  for (attr_name in most_attrs) {
    attr(result, attr_name) <- attr(x, attr_name)
  }
  class(result) <- class(x)
  result
}

#' @export
#' @importFrom vctrs vec_data
`[[.haven_labelled_defined` <- function(x, i, ...) {
  defined(vctrs::vec_data(x)[[i]],
    label = var_label(x),
    unit = var_unit(x),
    concept = var_concept(x),
    namespace = var_namespace(x),
    labels = attr(x, "labels")
  )
}


#' @export
#' @importFrom vctrs vec_data
Ops.haven_labelled_defined <- function(e1, e2) {
  # Comparisons work as expected
  lhs <- if (inherits(e1, "haven_labelled_defined")) vctrs::vec_data(e1) else e1
  rhs <- if (inherits(e2, "haven_labelled_defined")) vctrs::vec_data(e2) else e2
  .Generic <- .Generic
  do.call(.Generic, list(lhs, rhs))
}

#' @export
#' @importFrom vctrs vec_data
length.haven_labelled_defined <- function(x) {
  length(vctrs::vec_data(x))
}

#' @export
#' @importFrom vctrs vec_data
head.haven_labelled_defined <- function(x, n = 6L, ...) {
  x[seq_len(min(n, length(x)))]
}

#' @export
#' @importFrom vctrs vec_data
tail.haven_labelled_defined <- function(x, n = 6L, ...) {
  x[seq.int(to = length(x), length.out = min(n, length(x)))]
}


## Print & Summary --------------------------------------------------

#' @export
print.haven_labelled_defined <- function(x, ...) {
  has_def <- !is.null(var_concept(x)) && !is.na(var_concept(x)) && nzchar(var_concept(x))
  has_unit <- !is.null(var_unit(x)) && !is.na(var_unit(x)) && nzchar(var_unit(x))
  has_label <- !is.null(var_label(x))

  cat(deparse(substitute(x)))

  if (has_label) cat(paste0(": ", var_label(x)))
  cat("\n")

  if (has_def && has_unit) {
    msg <- paste0("Defined as ", var_concept(x), ", measured in ", var_unit(x))
  } else if (has_def) {
    msg <- paste0("Defined as ", var_concept(x))
  } else if (has_unit) {
    msg <- paste0("Measured in ", var_unit(x))
  } else {
    msg <- "Defined vector"
  }

  cat(msg, "\n")
  print(vctrs::vec_data(x), ...)
  invisible(x)
}


#' @export
format.haven_labelled_defined <- function(x, ...) {
  base <- format(vec_data(x), ...)
  unit <- var_unit(x)
  def <- var_concept(x)

  if (!is.null(unit) && nzchar(unit)) {
    suffix <- paste0(" (", unit, ")")
  } else if (!is.null(def) && nzchar(def) && nchar(def) < 30) {
    suffix <- paste0(" [", def, "]")
  } else {
    suffix <- ""
  }

  paste0(base, suffix)
}

#' @rdname defined
#' @param object An R object to be summarised.
#' @export
summary.haven_labelled_defined <- function(object, ...) {
  label <- var_label(object)
  unit <- var_unit(object)

  if (!is.null(label) && nzchar(label)) {
    if (!is.null(unit) && nzchar(unit)) {
      cat(paste0(label, " (", unit, ")\n"))
    } else {
      cat(paste0(label, "\n"))
    }
  }
  NextMethod()
}

#' Combine defined vectors with metadata checks
#'
#' The `c()` method for `defined` vectors ensures that all semantic metadata
#' (label, unit, concept, namespace, and value labels) match exactly. This
#' prevents accidental loss or mixing of incompatible definitions during
#' concatenation.
#'
#' All input vectors must:
#' - Have identical `label` attributes
#' - Have identical `unit`, `concept`, and `namespace`
#' - Have identical value labels (or none)
#'
#' @param ... One or more vectors created with [defined()].
#'
#' @return A single `defined` vector with concatenated values and retained
#'   metadata.
#'
#' @examples
#' a <- defined(1:3, label = "Length", unit = "meter")
#' b <- defined(4:6, label = "Length", unit = "meter")
#' c(a, b)
#'
#' @seealso [defined()]
#' @export
c.haven_labelled_defined <- function(...) {
  dots <- list(...)

  var_labels <- unlist(lapply(dots, var_label))
  val_labels <- lapply(dots, function(x) attr(x, "labels"))
  units <- unlist(lapply(dots, var_unit))
  concepts <- unlist(lapply(dots, var_concept))
  namespaces <- unlist(lapply(dots, namespace_attribute))

  all_identical <- function(l) {
    all(mapply(identical, head(l, 1), tail(l, -1)))
  }

  if (length(unique(as.character(var_labels))) > 1) {
    stop("c(): var_label must be identical or NULL across inputs")
  }

  if (length(unique(as.character(units))) > 1) {
    stop("c(): unit must be identical or NULL across inputs")
  }

  if (length(unique(as.character(concepts))) > 1) {
    stop("c(): concept must be identical or NULL across inputs")
  }

  if (length(unique(as.character(namespaces))) > 1) {
    stop("c(): namespace must be identical or NULL across inputs")
  }

  if (!all_identical(val_labels)) {
    stop("c(): value labels must be identical or NULL across inputs")
  }

  defined(
    unname(do.call(c, lapply(dots, vctrs::vec_data))),
    label = var_labels[[1]],
    labels = val_labels[[1]],
    concept = concepts[[1]],
    namespace = namespaces[[1]],
    unit = units[[1]]
  )
}

## Coercion ----------------------------------------------------------
## as_numeric, as_character, as_factor in separate files

#' @export
as.list.haven_labelled_defined <- function(x, ...) {
  lapply(seq_along(x), function(i) x[[i]])
}

#' @export
as.vector.haven_labelled_defined <- function(x, mode = "any") {
  as.vector(vctrs::vec_data(x), mode = mode)
}

#' @title Strip the class from a defined vector
#' @description Converts a `defined` vector to a base R numeric or character,
#'   retaining metadata as passive attributes.
#' @param x A `defined` vector.
#' @return A base R vector with attributes (`label`, `unit`, etc.) intact.
#' @seealso [as_numeric()], [as_character()]
#' @examples
#' gdp <- defined(c(3897L, 7365L), label = "GDP", unit = "million dollars")
#' strip_defined(gdp)
#'
#' fruits <- defined(c("apple", "avocado", "kiwi"),
#'   label = "Fruit", unit = "kg"
#' )
#'
#' strip_defined(fruits)
#' @export
strip_defined <- function(x) {
  if (!inherits(x, "haven_labelled_defined")) {
    return(x)
  }

  base_class <- typeof(vctrs::vec_data(x)) # typically "double" or "integer"
  class(x) <- base_class
  x
}

#' @importFrom pillar type_sum tbl_sum
#' @export
type_sum.haven_labelled_defined <- function(x) {
  "defined"
}

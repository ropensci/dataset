#' Add lightweight semantic mappings to a vector
#'
#' `prelabel()` applies lightweight semantic mappings to a vector
#' before formal definition with [defined()].
#'
#' It is intended for:
#'
#' - provisional harmonisation;
#' - contextual grouping;
#' - lightweight classification;
#' - semantic preprocessing workflows.
#'
#' Unlike [defined()], `prelabel()` does not enforce
#' formal semantic definitions, namespaces, or units.
#'
#' Semantic mappings may be supplied as:
#'
#' - named vectors;
#' - named lists;
#' - two-column data frames.
#'
#' These mappings are normalised internally with
#' [as_value_key()].
#'
#' @param x A vector.
#'
#' @param labels Candidate semantic mappings describing
#' provisional semantic assertions.
#'
#' `labels` is internally normalised with [as_value_key()] and
#' may therefore be supplied as:
#'
#' - a named vector;
#' - a named list;
#' - a two-column data frame or tibble.
#'
#' @param unmatched Behaviour for unmatched observational values.
#'
#' One of:
#'
#' - `"keep"` (default): preserve unmatched values as
#'   self-describing semantic assertions;
#' - `"na"`: operationalise unmatched values as `NA` during
#'   semantic coercion.
#'
#' @param missing_label Semantic assertion used internally for
#' missing observational values.
#'
#' @return
#' A vector with:
#'
#' - class `"prelabelled"`;
#' - attached provisional semantic vocabulary stored in the
#'   `"prelabel"` attribute.
#'
#' @details
#' `prelabelled` vectors intentionally separate:
#'
#' - observational evidence;
#' - semantic operationalisation;
#' - contextual semantic refinement.
#'
#' The original observational values remain unchanged while
#' semantic assertions may evolve through iterative refinement
#' workflows.
#'
#' Semantic operationalisation is provided with:
#'
#' - [as.character()] for lightweight semantic coercion;
#' - [as_character()] for provenance-preserving semantic
#'   workspaces.
#'
#' @examples
#'
#' x <- c(
#'   "R",
#'   "png",
#'   "csv",
#'   "unknown"
#' )
#'
#' extension_map <- c(
#'   R = "functional_programming",
#'   png = "visualisation",
#'   csv = "tabular_data"
#' )
#'
#' x <- prelabel(
#'   x,
#'   labels = extension_map
#' )
#'
#' x
#'
#' as.character(x)
#'
#' semantic_workspace <- as_character(x)
#'
#' attributes(semantic_workspace)
#'
#' @importFrom stats setNames na.omit
#' @export
prelabel <- function(
  x,
  labels,
  unmatched = "keep",
  missing_label = "<NA>"
) {
  if (!is.vector(x)) {
    stop(
      "x must be a vector",
      call. = FALSE
    )
  }

  labels <- as_value_key(labels)

  unmatched <- match.arg(
    unmatched,
    c(
      "keep",
      "na"
    )
  )

  out <- x

  matched <- x %in% names(labels)

  out_labels <- labels[
    match(
      x,
      names(labels)
    )
  ]


  labels <- as_value_key(labels)

  # Preserve unmatched values as self-describing assertions
  unmatched_values <-
    setdiff(
      unique(as.character(stats::na.omit(x))),
      names(labels)
    )

  passthrough <- stats::setNames(
    unmatched_values,
    unmatched_values
  )

  labels <- c(
    labels,
    passthrough
  )

  # Explicit semantic missing assertion
  if (any(is.na(x))) {
    labels <- c(
      labels,
      stats::setNames(
        missing_label,
        "<NA>"
      )
    )
  }

  attr(out, "prelabel") <- labels

  class(out) <- unique(
    c(
      "prelabelled",
      class(x)
    )
  )

  out
}


#' Test if a vector is prelabelled
#'
#' Determine whether an object inherits from the
#' `"prelabelled"` class.
#'
#' @inheritParams prelabel
#'
#' @return
#' Logical scalar.
#'
#' @export
is.prelabelled <- function(x) {
  inherits(x, "prelabelled")
}

#' Semantic character coercion
#'
#' Convert objects into semantic character representations.
#'
#' @param x An object.
#' @param ... Additional arguments.
#'
#' @export
as_character <- function(x, ...) {
  UseMethod("as_character")
}

#' Coerce prelabelled vectors to semantic character workspace
#'
#' Convert a `prelabelled` vector into a semantic character
#' workspace suitable for iterative refinement and inference
#' workflows.
#'
#' Unlike base [as.character()], this method preserves:
#'
#' - original observational values;
#' - semantic label mappings;
#' - refinement metadata;
#' - additional custom attributes.
#'
#' The method operationalises provisional semantic labels into
#' working character values while retaining the original observed
#' vector for reversibility and provenance-aware workflows.
#'
#' This allows workflows such as:
#'
#' - `prelabel() %>% as.character() %>% refine()`
#' - `prelabel() %>% as.character() %>% infer()`
#' - iterative semantic harmonisation;
#' - contextual semantic derivation.
#'
#' The original observational values are preserved in the
#' `"original_values"` attribute.
#'
#' @param x A `prelabelled` vector.
#'
#' @param ... Unused.
#'
#' @return
#' A character vector where:
#'
#' - values are derived from the semantic labels;
#' - original observations are preserved as attributes;
#' - refinement metadata is retained;
#' - the `"prelabelled"` class is removed.
#'
#' @details
#' The method intentionally separates:
#'
#' - observational evidence;
#' - semantic operationalisation.
#'
#' This distinction is important for provenance-aware semantic
#' refinement workflows where semantic interpretations may evolve
#' while original observations remain stable.
#'
#' The coercion is therefore semantically reversible.
#'
#' @examples
#'
#' x <- prelabel(
#'   c(
#'     "r",
#'     "pdf",
#'     "qmd"
#'   ),
#'   labels = c(
#'     r = "software development",
#'     pdf = "publication",
#'     qmd = "documentation"
#'   )
#' )
#'
#' x
#'
#' y <- as_character(x)
#'
#' y
#'
#' attributes(y)
#'
#' attr(
#'   y,
#'   "original_values"
#' )
#'
#' @export
#' @export
as_character.prelabelled <- function(
  x,
  ...
) {
  # Preserve observational evidence ----------------------

  original_values <- unclass(x)

  # Canonical semantic operationalization ----------------

  out <- as.character(x)

  # Preserve semantic vocabulary -------------------------

  attr(
    out,
    "prelabel"
  ) <- attr(
    x,
    "prelabel"
  )

  # Preserve observational provenance --------------------

  attr(
    out,
    "original_values"
  ) <- original_values

  # Preserve additional attributes -----------------------

  extra_attrs <-
    attributes(x)[
      !names(attributes(x)) %in%
        c(
          "class",
          "prelabel"
        )
    ]

  mostattributes(out) <-
    c(
      attributes(out),
      extra_attrs
    )

  # Remove semantic wrapper class ------------------------

  class(out) <-
    setdiff(
      class(x),
      "prelabelled"
    )

  out
}


#' @export
#' @export
as.character.prelabelled <- function(
  x,
  ...
) {
  labels <- attr(
    x,
    "prelabel"
  )

  keys <- base::as.character(
    unclass(x)
  )

  keys[is.na(x)] <- "<NA>"

  out <- labels[
    match(
      keys,
      names(labels)
    )
  ]

  out[is.na(x)] <- NA_character_

  unname(out)
}

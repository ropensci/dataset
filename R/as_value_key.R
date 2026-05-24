#' Coerce semantic mappings to canonical key-value form
#'
#' Convert semantic mapping carriers into a canonical
#' named character vector representation.
#'
#' @details
#' `as_value_key()` standardises lightweight semantic mappings
#' supplied as:
#'
#' - named vectors;
#' - named lists;
#' - two-column data frames or tibbles.
#'
#' The resulting object is suitable for:
#'
#' - [prelabel()];
#' - semantic grouping;
#' - contextual reconstruction;
#' - relational projection workflows.
#'
#' Named lists may contain one-to-many mappings:
#'
#' \preformatted{
#' list(
#'   conceptualisation = c(
#'     "D:/_package/alpha",
#'     "D:/_markdown/alpha-methodology"
#'   ),
#'   betaR = c(
#'     "D:/_packages/beta",
#'     "D:/_packages/prebeta"
#'   )
#' )
#' }
#'
#' where multiple values share the same semantic key.
#'
#' The function is intentionally lightweight. It does not:
#'
#' - validate ontologies;
#' - enforce uniqueness;
#' - resolve semantic conflicts;
#' - construct graph objects;
#' - perform recursive inheritance logic.
#'
#' @param x A semantic mapping carrier:
#'
#' - named character vector;
#' - named list;
#' - two-column data frame or tibble.
#'
#' @return
#' A named character vector representing canonical semantic
#' key-value mappings.
#'
#' @examples
#'
#' # named vector
#' as_value_key(
#'   c(
#'     R = "functional_programming",
#'     png = "visualisation"
#'   )
#' )
#'
#' # named list
#' as_value_key(
#'   list(
#'     R = "functional_programming",
#'     png = "visualisation"
#'   )
#' )
#'
#' # tibble
#' mapping_tbl <- tibble::tibble(
#'   extension = c("R", "png"),
#'   activity = c(
#'     "functional_programming",
#'     "visualisation"
#'   )
#' )
#'
#' as_value_key(mapping_tbl)
#'
#' @export
as_value_key <- function(x) {
  # ------------------------------------------------------------
  # Named vector
  # ------------------------------------------------------------

  if (
    is.atomic(x) &&
      !is.null(names(x))
  ) {
    out <- as.character(x)

    names(out) <- names(x)

    return(out)
  }

  # ------------------------------------------------------------
  # Named list
  # ------------------------------------------------------------

  if (
    is.list(x) &&
      !is.data.frame(x) &&
      !is.null(names(x))
  ) {
    out <- unlist(
      x,
      recursive = TRUE,
      use.names = FALSE
    )

    out_names <- rep(
      names(x),
      lengths(x)
    )

    out <- as.character(out)

    names(out) <- out_names

    return(out)
  }

  # ------------------------------------------------------------
  # Two-column data frame
  # ------------------------------------------------------------

  if (
    is.data.frame(x) &&
      ncol(x) == 2
  ) {
    out <- as.character(x[[2]])

    names(out) <- as.character(x[[1]])

    return(out)
  }

  stop(
    paste(
      "`x` must be:",
      "- a named vector;",
      "- a named list;",
      "- or a two-column data frame."
    ),
    call. = FALSE
  )
}

#' Invert semantic key-value mappings
#'
#' Convert semantic mappings into a two-column relational form
#' suitable for joins, inspection, and roundtrip conversion with
#' [as_value_key()].
#'
#' @rdname as_value_key
#' @export
#' @importFrom tibble tibble
invert_value_key <- function(x) {
  x <- as_value_key(x)

  tibble::tibble(
    key = names(x),
    value = unname(x)
  )
}

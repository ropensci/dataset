#' Get or Set a Variable Label
#'
#' Adds or retrieves a human-readable label as a metadata attribute for a
#' variable or vector. This label is useful for making variables easier to
#' understand than their programmatic names (e.g., column names).
#'
#' @details
#' This interface builds on [labelled::var_label()] and is compatible with
#' the `defined()` infrastructure for semantic metadata (labels, namespaces,
#' units, and variable identifiers).
#'
#' See [labelled::var_label()] for low-level usage. For a comprehensive
#' guide to working with variable labels and semantic metadata, see:
#' `vignette("defined", package = "dataset")`.
#'
#' @param x A vector or data frame.
#' @param value A character string or `NULL` to remove the label.
#'   When used with `var_labels()` on a data frame, this can also be a named list
#'   or a character vector of the same length as the number of columns in `x`.
#' @param ... Further arguments passed to or used by methods.
#'
#' @return
#' - `var_label(x)` returns the `"label"` attribute of `x` as a character string.
#' - `var_label(x) <- value` sets, removes, or replaces the label attribute of `x`,
#' returning the updated object invisibly.
#'
#' @examples
#' # Retrieve the label attribute
#' var_label(orange_df$circumference)
#'
#' # Set or update the label attribute
#' var_label(orange_df$circumference) <- "circumference (breast height)"
#'
#' @seealso [labelled::var_label()], [var_labels()], [defined()]
#' @family defined metadata methods and functions
#'
#' @name var_label
#' @export
#' @importFrom labelled var_label
#' @importFrom labelled `var_label<-`
NULL

#' @rdname var_label
#' @export
#' @importFrom labelled `var_label<-`
var_label.defined <- function(x, ...) {
  NextMethod()
}
#' @rdname var_label
#'
#' @description
#' `label_attribute()` is a low-level helper that retrieves the `"label"` attribute
#' of an object without any fallback or printing logic. It is primarily used internally.
#'
#' @return
#' A character string if the `"label"` attribute exists, or `NULL` if not present.
#'
#' @export
#' @importFrom labelled label_attribute
label_attribute <- function(x) {
  attr(x, "label", exact = TRUE)
}

#' @rdname var_label
#'
#' @description
#' The `var_label<-` assignment method sets or removes the `"label"` attribute
#' of a vector or data frame column. This allows attaching human-readable
#' descriptions to variables for interpretability and downstream metadata use.
#'
#' @param value A character string to assign as the label, or `NULL` to remove it.
#'
#' @return
#' The modified object `x`, returned invisibly with the updated `"label"` attribute.
#'
#' @export
`var_label<-` <- function(x, value) {
  UseMethod("var_label<-")
}

#' @rdname var_label
#' @export
#' @importFrom labelled `var_label<-`
`var_label<-.haven_labelled_defined` <- function(x, value) {
  attr(x, "label") <- value
  x
}

#' @rdname var_label
#'
#' @param unlist For data frames, return a named vector instead of a list.
#' @param null_action For data frames, controls how to handle columns without a variable label.
#'   Options are:
#'   - `"keep"` (default): keep `NULL` for unlabeled columns
#'   - `"fill"`: use the column name as a fallback
#'   - `"skip"`: exclude columns with no label from the result
#'   - `"na"`: use `NA_character_`
#'   - `"empty"`: use an empty string `""`
#' @param recurse If `TRUE`, applies `var_label()` recursively on packed columns
#'   (as created by [tidyr::pack()]) to retrieve sub-column labels. If `FALSE`,
#'   only the outer (grouped) column label is returned.
#' @examples
#' # Example: Retrieve variable labels from a dataset_df
#' df <- dataset_df(
#'   id = defined(1:3, label = "Observation ID"),
#'   temp = defined(c(22.5, 23.0, 21.8), label = "Temperature (<U+00B0>C)"),
#'   site = defined(c("A", "B", "A"))
#' )
#'
#' # List form (default)
#' var_label(df)
#'
#' # Character vector form
#' var_label(df, unlist = TRUE, null_action = "empty")
#'
#' # Exclude variables without labels
#' var_label(df, null_action = "skip")
#'
#' # Replace missing labels with column names
#' var_label(df, null_action = "fill")
#' @export
var_label.dataset_df <- function(x,
                                 unlist = FALSE,
                                 null_action = c("keep", "fill", "skip", "na", "empty"),
                                 recurse = FALSE,
                                 ...) {
  null_action <- match.arg(null_action)

  result <- lapply(names(x), function(name) {
    col <- x[[name]]

    if (recurse && inherits(col, "data.frame")) {
      return(var_label(col, unlist = unlist, null_action = null_action, recurse = recurse, ...))
    }

    label <- var_label(col)

    if (is.null(label)) {
      label <- switch(null_action,
        keep = NULL,
        fill = name,
        skip = NULL,
        na = NA_character_,
        empty = ""
      )
    }

    label
  })

  names(result) <- names(x)

  if (null_action == "skip") {
    result <- result[!vapply(result, is.null, logical(1))]
  }

  if (unlist) {
    return(unlist(result, use.names = TRUE))
  }

  result
}

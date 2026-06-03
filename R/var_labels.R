#' @title Get or set all variable labels on a dataset
#'
#' @description
#' Retrieve or assign labels for all variables (columns) in a dataset.
#'
#' @details
#' This is the dataset-level equivalent of [var_label()].
#' It works with any `data.frame`-like object, including [dataset_df()], and
#' returns/sets the `"label"` attribute of each column.
#'
#' Labels are useful for storing human-readable descriptions of variables that
#' may have short or cryptic column names.
#'
#' For internal purposes, this function uses the `"var_labels"` dataset
#' attribute and delegates to [var_label()] and
#' [var_label<-()] on individual columns.
#'
#' @param x A `data.frame` or `dataset_df` object.
#' @param value
#'   - For setting: a named list or named character vector of labels.
#'     Names must match column names in `x`.
#'     Unnamed elements are ignored.
#'   - For getting: ignored.
#' @param unlist Logical; if `TRUE`, return a named character vector instead
#'   of a list. Defaults to `FALSE`.
#' @param null_action How to handle columns without labels. One of:
#'   - `"keep"` (default): keep `NULL` values for unlabeled columns.
#'   - `"fill"`: use the column name as a fallback label.
#'   - `"skip"`: exclude unlabeled columns from the result.
#'   - `"na"`: use `NA_character_` for unlabeled columns.
#'   - `"empty"`: use an empty string `""` for unlabeled columns.
#'
#' @return
#' * Getter: a named list (or vector if `unlist = TRUE`) of variable labels.
#' * Setter: the modified `x` with updated labels, returned invisibly.
#'
#' @examples
#' df <- dataset_df(
#'   id = defined(1:3, label = "Observation ID"),
#'   temp = defined(c(22.5, 23.0, 21.8), label = "Temperature (<U+00B0>C)"),
#'   site = defined(c("A", "B", "A"))
#' )
#'
#' # Get all variable labels
#' var_labels(df)
#'
#' # Set multiple labels at once
#' var_labels(df) <- list(site = "Site code")
#'
#' # Return as a named vector with empty string for unlabeled vars
#' var_labels(df, unlist = TRUE, null_action = "empty")
#'
#' @seealso [var_label()]
#' @family defined metadata methods and functions
#' @export
var_labels <- function(x,
                       unlist = FALSE,
                       null_action = c("keep", "fill", "skip", "na", "empty")) {
  null_action <- match.arg(null_action)
  stopifnot(is.data.frame(x))

  result <- lapply(names(x), function(name) {
    lbl <- var_label(x[[name]])
    if (is.null(lbl)) {
      lbl <- switch(null_action,
        keep = NULL,
        fill = name,
        skip = NULL,
        na = NA_character_,
        empty = ""
      )
    }
    lbl
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

#' @rdname var_labels
#' @export
`var_labels<-` <- function(x, value) {
  stopifnot(is.data.frame(x))
  stopifnot(is.list(value) || is.character(value))

  # Convert character vector to named list if needed
  if (is.character(value) && !is.list(value)) {
    if (is.null(names(value))) {
      stop("Character vector for var_labels() must be named.")
    }
    value <- as.list(value)
  }

  # Only set labels for matching names
  for (nm in intersect(names(x), names(value))) {
    var_label(x[[nm]]) <- value[[nm]]
  }

  # Update dataset-level "var_labels" attribute too
  x <- set_var_labels(x, value)

  invisible(x)
}

#' @keywords internal
set_var_labels <- function(dataset, var_labels) {
  var_label_list <- list()
  var_label_list <- lapply(colnames(dataset), function(i) i)
  names(var_label_list) <- colnames(dataset)

  for (rn in which(names(var_label_list) %in% names(var_labels))) {
    var_label_list[[rn]] <- var_labels[[which(names(var_label_list)[rn] == names(var_labels))]]
  }

  attr(dataset, "var_labels") <- var_label_list

  dataset
}

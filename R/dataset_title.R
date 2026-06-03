#' @title Get or Set the Title of a Dataset
#'
#' @description Retrieve or assign the main title of a dataset, typically used
#' as the primary label in metadata exports (e.g., DataCite or Dublin Core).
#'
#' @details According to the [Dublin Core specification for
#' [title](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/title/),
#' the title represents the name by which the resource is formally known.
#'
#' The DataCite metadata schema supports multiple titles (e.g., translated,
#' alternative), but this function currently supports only a single main title.
#'
#' @param x A dataset object created by [dataset_df()] or [as_dataset_df()].
#' @param value A character string representing the new title. If `NULL`, a
#'   placeholder value `":tba"` is assigned. If `value` is a character vector of
#'   length > 1, an error is raised.
#' @param overwrite Logical. If `TRUE`, the existing title is replaced. If
#'   `FALSE` (default) and a title is already present, a warning is issued and
#'   the title is not changed.
#'
#' @return `dataset_title()` returns the current dataset title as a character
#' string. `dataset_title<-()` returns the updated dataset object (invisible).
#'
#' @examples
#' dataset_title(orange_df)
#'
#' # Set a new title with overwrite = TRUE
#' dataset_title(orange_df, overwrite = TRUE) <- "The Growth of Orange Trees"
#' dataset_title(orange_df)
#'
#' @family bibliographic helper functions
#' @rdname dataset_title
#' @export

dataset_title <- function(x) {
  if (!is.dataset_df(x)) {
    stop("dataset_title(x) must be created with dataset() or as_dataset_df().")
  }

  ds_bibentry <- get_bibentry(x)
  ds_bibentry$title
}

#' @rdname dataset_title
#' @importFrom stats setNames
#' @export
`dataset_title<-` <- function(x, overwrite = FALSE, value) {
  if (!is.dataset_df(x)) {
    stop("title(x) <- x must be created with dataset() or as_dataset_df().")
  }

  ds_bibentry <- invisible(get_bibentry(x))

  if (is.null(value)) {
    ds_bibentry$title <- ":tba"
    attr(x, "dataset_bibentry") <- ds_bibentry
    return(x)
  }

  if (inherits(value, "factor") || inherits(value, "character")) {
    if (length(value) > 1) {
      stop(
        "title(x) <- value: if you have multiple titles, use dataset_title_create()"
        )
    }
  }

  if (!ds_bibentry$title %in% c(":unas", ":tba", "") && !overwrite) {
    warning("The dataset already has a title: ", ds_bibentry$title)
  } else {
    ds_bibentry$title <- value
  }

  attr(x, "dataset_bibentry") <- ds_bibentry

  invisible(x)
}

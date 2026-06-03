#' @title Bind strictly defined rows
#' @description Add rows of dataset `y` to dataset `x`, validating all
#'   semantic metadata. Metadata (labels, units, concept definitions,
#'   namespaces) must match exactly. Additional dataset-level metadata such as
#'   title and creator can be overridden using `...`.
#'
#' @details This function combines two semantically enriched datasets created
#'   with `dataset_df()`. All variable-level attributes <U+2014> including labels,
#' units, concept definitions, and namespaces <U+2014> must match. If \code{strict =
#' TRUE} (the default), the row identifier namespace (used in the \code{rowid}
#'   column) must also match exactly.
#'
#'   If `strict = FALSE`, row identifiers from `y` may differ and will
#'   be ignored; the output will inherit `x`'s row identifier scheme.
#'
#' @param x A `dataset_df` object.
#' @param y A `dataset_df` object to bind to `x`.
#' @param ... Optional dataset-level attributes such as `title` or
#'   [creator()] to override.
#' @param strict Logical. If `TRUE` (default), require full semantic
#'   compatibility, including rowid.
#'
#' @return A new `dataset_df` object with rows from `x` and `y`, combined
#'   semantically.
#'
#' @export
#' @examples
#' A <- dataset_df(
#'   length = defined(c(10, 15),
#'     label = "Length",
#'     unit = "cm", namespace = "http://example.org"
#'   ),
#'   identifier = c(id = "http://example.org/dataset#"),
#'   dataset_bibentry = dublincore(
#'     title = "Dataset A",
#'     creator = person("Alice", "Smith")
#'   )
#' )
#'
#' B <- dataset_df(
#'   length = defined(c(20, 25),
#'     label = "Length",
#'     unit = "cm", namespace = "http://example.org"
#'   ),
#'   identifier = c(id = "http://example.org/dataset#")
#' )
#'
#' bind_defined_rows(A, B) # succeeds
#'
#' C <- dataset_df(
#'   length = defined(c(30, 35),
#'     label = "Length",
#'     unit = "cm", namespace = "http://example.org"
#'   ),
#'   identifier = c(id = "http://another.org/dataset#")
#' )
#'
#' \dontrun{
#' bind_defined_rows(A, C, strict = TRUE) # fails: mismatched rowid
#' }
#'
#' bind_defined_rows(A, C, strict = FALSE) # succeeds: rowid inherited
bind_defined_rows <- function(x, y, ..., strict = FALSE) {
  dots <- list(...)

  if (!inherits(x, "dataset_df")) {
    stop(
      "`x` must be a dataset_df object. Got: ",
      paste(class(x), collapse = ", ")
    )
  }

  if (!inherits(y, "dataset_df")) {
    stop(
      "`y` must be a dataset_df object. Got: ",
      paste(class(y), collapse = ", ")
    )
  }


  # Strict mode: also validate rowid identifier
  if (strict) {
    ns_x <- namespace_attribute(x[["rowid"]])
    ns_y <- namespace_attribute(y[["rowid"]])

    if (!identical(ns_x, ns_y)) {
      stop("Row identifier namespaces must match in strict mode.")
    }
  }

  # Exclude rowid from semantic checks
  vars <- setdiff(names(x), "rowid")

  if (!identical(names(x)[vars], names(y)[vars])) {
    stop("Error: Column names must match between datasets.")
  }

  if (!identical(var_label(x[vars]), var_label(y[vars]))) {
    stop("Error: Variable labels must match in the two datasets.")
  }

  if (!identical(lapply(x[vars], var_unit), lapply(y[vars], var_unit))) {
    stop("Error: Variable units must match in the two datasets.")
  }

  if (!identical(
    lapply(x[vars], function(i) attr(i, "concept")),
    lapply(y[vars], function(j) attr(j, "concept"))
  )) {
    stop("Variable concept definitions must match in the two datasets.")
  }

  if (!identical(
    lapply(x[vars], namespace_attribute),
    lapply(y[vars], namespace_attribute)
  )
  ) {
    stop("Variable namespaces must match in the two datasets.")
  }

  # Bind only variable columns (exclude rowid)
  new_data <- mapply(function(a, b) c(a, b), x[vars], y[vars], SIMPLIFY = FALSE)

  # Use the rowid namespace from x; allow override later if needed
  new_dataset <- do.call(dataset_df, c(new_data, list(identifier = namespace_attribute(x[["rowid"]]))))

  # Handle title/creator if supplied
  if (!is.null(dots$title)) dataset_title(new_dataset, overwrite = TRUE) <- dots$title
  if (!is.null(dots$creator)) {
    creator(new_dataset) <- dots$creator
  } else {
    creator(new_dataset) <- compare_creators(x, y)
  }

  attr(new_dataset, "dataset_bibentry") <- attr(x, "dataset_bibentry")
  new_dataset
}

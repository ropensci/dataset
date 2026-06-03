#' @title Get or set contributors
#'
#' @description `contributor()` is a lightweight wrapper around [creator()] that
#'   works only with contributors. It retrieves or updates only the contributor
#'   entries in the dataset's bibliographic metadata.
#'
#' @details All people are stored in the `author` slot of the underlying
#'   [utils::bibentry()]. This helper preserves primary creators and filters or
#'   updates only those entries that represent contributors.
#'
#'   A *contributor* is defined as:
#'
#' * a person with `role == "ctb"`, **or**
#' * a person with a `comment[["contributorType"]]`.
#'
#'   Primary creators (authors) typically have `role %in% c("aut", "cre")`.
#'
#'   Contributors can be further annotated with metadata in `comment`, for
#'   example:
#'
#'  `comment = c(contributorType = "hostingInstitution",`
#'  `           ORCID = "0000-0000-0000-0000")`
#'
#' @param x A dataset object created with [dataset_df()] or [as_dataset_df()].
#' @param value A [utils::person()] object representing a single contributor. If
#'   the `role` field is missing, it will be set to `"ctb"`. If `NULL`, the
#'   dataset is returned unchanged.
#' @param overwrite Logical. If `TRUE`, replace all existing contributors with
#'   `value`. If `FALSE`, append `value` to the existing contributors. Defaults
#'   to `FALSE`.
#'
#' @return
#' * `contributor()` returns a [utils::person()] or a list of such objects
#' corresponding to contributors.
#' * `contributor<-()` returns the updated dataset (invisibly).
#'
#' @examples
#' df <- dataset_df(data.frame(x = 1))
#' creator(df) <- person("Jane", "Doe", role = "aut")
#'
#' # Add a contributor
#' contributor(df, overwrite = FALSE) <-
#'   person("GitHub",
#'     role = "ctb",
#'     comment = c(contributorType = "hostingInstitution")
#'   )
#'
#' # Replace all contributors
#' contributor(df) <- person("Support", "Team", role = "ctb")
#'
#' # Inspect only contributors
#' contributor(df)
#'
#' @family bibliographic helper functions
#' @importFrom utils person
#' @export
contributor <- function(x) {
  assertthat::assert_that(
    is.dataset_df(x),
    msg =
      "contributor(x): x must be created with dataset_df() or as_dataset_df()."
  )
  auth <- get_bibentry(x)$author
  if (is.null(auth)) {
    return(auth)
  }

  as_list <- as.list(auth)
  is_ctb <- function(p) {
    inherits(p, "person") &&
      (identical(p$role, "ctb") ||
        (!is.null(p$comment) && !is.null(p$comment[["contributorType"]])))
  }
  keep <- vapply(as_list, is_ctb, logical(1))
  if (!any(keep)) {
    return(auth[0])
  }
  utils::as.person(as_list[keep])
}


#' @rdname contributor
#' @export
`contributor<-` <- function(x, overwrite = FALSE, value) {
  assertthat::assert_that(
    is.dataset_df(x),
    msg = "contributor(x) <- value: x must be a dataset_df object."
  )
  if (is.null(value)) {
    return(x)
  }
  if (!inherits(value, "person")) {
    stop("contributor(x) <- value: `value` must be a utils::person object.")
  }
  if (is.null(value$role)) value$role <- "ctb"
  if (!identical(value$role, "ctb")) {
    stop("contributor(x) <- value: role must be 'ctb'.")
  }

  all_people <- get_bibentry(x)$author
  non_ctb <- Filter(function(p) !identical(p$role, "ctb"), as.list(all_people))

  new_auth <- if (overwrite) {
    c(non_ctb, value)
  } else {
    c(all_people, value)
  }

  databibentry <- attr(x, "dataset_bibentry")
  databibentry$author <- utils::as.person(new_auth)
  attr(x, "dataset_bibentry") <- databibentry
  invisible(x)
}

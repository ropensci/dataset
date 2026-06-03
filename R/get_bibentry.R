#' @title Get or set the bibentry
#'
#' @description
#' Retrieve or replace the bibliographic entry stored in a dataset's attributes.
#' The entry is a [utils::bibentry()] used to hold citation metadata for
#' [dataset_df()] objects.
#'
#' @details
#' New datasets are initialized with reasonable defaults. To build a new
#' bibentry with sensible defaults and field names, use [datacite()] (DataCite)
#' or [dublincore()] (Dublin Core), then assign it with
#' `set_bibentry(dataset) <- value`.
#'
#' See the vignette for more background:
#' `vignette("bibentry", package = "dataset")`.
#'
#' @param dataset A dataset created with [dataset_df()].
#' @param value A [utils::bibentry()] to store on the dataset. If `NULL`, a
#'   minimal default entry is created.
#'
#' @return
#' * `get_bibentry(dataset)` returns the [utils::bibentry()] stored in
#'   `dataset`'s attributes.
#' * `set_bibentry(dataset) <- value` sets the attribute and returns the
#'   modified dataset invisibly.
#'
#' @examples
#' # Get the bibentry of a dataset_df object:
#' be <- get_bibentry(orange_df)
#'
#' # Create a well-formed bibentry (DataCite-style):
#' be2 <- datacite(
#'   Creator = person("Jane", "Doe"),
#'   Title = "The Orange Trees Dataset",
#'   Publisher = "MyOrg"
#' )
#'
#' # Assign the new bibentry:
#' set_bibentry(orange_df) <- be2
#'
#' # Inspect in different notations:
#' as_datacite(orange_df, type = "list")
#' as_dublincore(orange_df, type = "list")
#'
#' @family bibliographic helper functions
#' @importFrom utils bibentry person
#' @export
get_bibentry <- function(dataset) {
  assertthat::assert_that("dataset_bibentry" %in% names(attributes(dataset)),
    msg = "Error: get_bibentry(dataset): dataset has no dataset_bibentry attribute"
  )
  attr(dataset, "dataset_bibentry")
}

#' @rdname get_bibentry
#' @export
`set_bibentry<-` <- function(dataset, value) {
  sys_time <- Sys.time()
  year <- substr(as.character(sys_time), 1, 4)

  if (is.null(value)) {
    value <- dublincore(
      title = "Untitled Dataset",
      creator = person("Unknown Author"),
      dataset_date = year
    )
  }

  if (is.null(value$year)) value$year <- year

  attr(dataset, "dataset_bibentry") <- value
  invisible(dataset)
}

#' @keywords internal
set_default_bibentry <- function() {
  sys_time <- Sys.time()
  year <- substr(as.character(sys_time), 1, 4)
  Title <- "Untitled Dataset"
  Creator <- person("Unknown", "Author")
  datacite(Title = Title, Creator = Creator, PublicationYear = year)
}

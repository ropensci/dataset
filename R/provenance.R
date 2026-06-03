#' @title Get or update provenance information
#'
#' @description
#' Retrieve or append provenance statements (in N<U+2011>Triples form) stored on a
#' [dataset_df()] object.
#'
#' @details
#' Provenance is stored in the `"prov"` attribute as N<U+2011>Triples text. Use
#' [n_triple()] or [n_triples()] to construct valid statements that follow
#' PROV<U+2011>O (e.g., `prov:wasGeneratedBy`, `prov:wasInformedBy`).
#'
#' @param x A dataset created with [dataset_df()].
#' @param value Character vector of N<U+2011>Triples created by [n_triple()] or
#'   [n_triples()] to append to existing provenance.
#'
#' @return
#' * `provenance(x)` returns the contents of the `"prov"` attribute (character
#'   vector of N<U+2011>Triples), or `NULL` if none is set.
#' * `provenance(x) <- value` appends `value` to the `"prov"` attribute and
#'   returns the modified dataset invisibly.
#'
#' @examples
#' provenance(orange_df)
#'
#' # Add a provenance statement:
#' provenance(orange_df) <- n_triple(
#'   "https://doi.org/10.5281/zenodo.10396807",
#'   "http://www.w3.org/ns/prov#wasInformedBy",
#'   "http://example.com/source#1"
#' )
#'
#' @importFrom utils citation
#' @export
provenance <- function(x) {
  if (!is.dataset_df(x)) {
    stop("provenance(x): x must be a dataset_df object with standardised provenance metadata.")
  }
  attr(x, "prov")
}

#' @rdname provenance
#' @export
`provenance<-` <- function(x, value) {
  if (!is.dataset_df(x)) {
    stop("provenance(x)<- : x must be a dataset object created with dataset_df() or as_dataset_df().")
  }

  old_provenance <- provenance(x)
  new_provenance <- old_provenance

  attr(x, "prov") <- c(new_provenance, value)
  invisible(x)
}

#' @title Build default provenance bundle
#' @description
#' Construct a small PROV bundle (as N<U+2011>Triples) describing the dataset, the
#' software agent, and an optional creation time.
#'
#' @details
#' This helper is used internally to seed provenance metadata. It emits a set of
#' PROV statements including an `Entity` for the dataset, an `Activity` for
#' creation, and `SoftwareAgent` entries for the package citation.
#'
#' @param dataset_id Base IRI for the dataset (used as the `Entity` subject).
#' @param author Optional creator/author agent.
#' @param dtm Optional data team/maintainer agent.
#' @param generated_at_time Optional POSIXct time; defaults to [Sys.time()].
#'
#' @return A character vector of N<U+2011>Triples suitable for the `"prov"` attribute.
#'
#' @keywords internal
#' @importFrom utils citation
default_provenance <- function(dataset_id = "http://example.com/dataset#",
                               author = NULL,
                               dtm = NULL,
                               generated_at_time = NULL) {
  cite_dataset <- utils::citation("dataset")

  # See prov_author in n_triple.R
  agent_triples <- c()
  if (!is.null(author)) agent_triples <- c(agent_triples, prov_author(author))
  if (!is.null(dtm)) agent_triples <- c(agent_triples, prov_author(dtm))

  if (is.null(generated_at_time)) {
    generated_at_time <- Sys.time()
  }

  bundle_id <- gsub("#", "_prov.nt", dataset_id)

  prov <- n_triples(
    c(
      n_triple(bundle_id, "a", "http://www.w3.org/ns/prov#Bundle"),
      n_triple(dataset_id, "a", "http://www.w3.org/ns/prov#Entity"),
      n_triple(dataset_id, "a", "http://purl.org/linked-data/cube#DataSet"),
      agent_triples,
      n_triple("https://doi.org/10.32614/CRAN.package.dataset", "a", "http://www.w3.org/ns/prov#SoftwareAgent"),
      n_triple("http://example.com/creation", "a", "http://www.w3.org/ns/prov#Activity"),
      n_triple("http://example.com/creation", "http://www.w3.org/ns/prov#generatedAtTime", xsd_convert(generated_at_time)),
      n_triple(paste0("https://doi.org/", cite_dataset[[2]]$doi), "a", "http://www.w3.org/ns/prov#SoftwareAgent")
    )
  )

  prov
}

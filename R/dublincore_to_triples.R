#' @keywords internal
dublincore_to_triples <- function(
  dclist,
  dataset_id = "http://example.com/dataset"
) {
  if (is.null(dclist) || is.null(dclist$title) || nchar(dclist$title) == 0) {
    stop("Error: dublincore_to_triples(dclist, dataset_id): no title found in dclist")
  }

  # Normalize type field URI if needed
  if (!is.null(dclist$type)) {
    dclist$type <- gsub("DCMITYPE:", "http://purl.org/dc/terms/DCMIType", dclist$type)
  }

  # Accumulate all triples (many Dublin Core fields may have multiple values):
  dctriples <- c(
    expand_triples(dataset_id, "http://purl.org/dc/terms/title", dclist$title),
    expand_triples(dataset_id, "http://purl.org/dc/terms/description", dclist$description),
    expand_triples(dataset_id, "http://purl.org/dc/terms/creator", dclist$creator),
    expand_triples(dataset_id, "http://purl.org/dc/terms/contributor", dclist$contributor),
    expand_triples(dataset_id, "http://purl.org/dc/terms/publisher", dclist$publisher),
    expand_triples(dataset_id, "http://purl.org/dc/terms/identifier", dclist$identifier),
    expand_triples(dataset_id, "http://purl.org/dc/terms/subject", dclist$subject),
    expand_triples(dataset_id, "http://purl.org/dc/terms/type", dclist$type),
    expand_triples(dataset_id, "http://purl.org/dc/terms/date", dclist$date),
    expand_triples(dataset_id, "http://purl.org/dc/terms/language", dclist$language),
    expand_triples(dataset_id, "http://purl.org/dc/terms/relation", dclist$relation),
    expand_triples(dataset_id, "http://purl.org/dc/terms/format", dclist$format),
    expand_triples(dataset_id, "http://purl.org/dc/terms/rights", dclist$rights),
    expand_triples(dataset_id, "http://purl.org/dc/terms/source", dclist$datasource),
    expand_triples(dataset_id, "http://purl.org/dc/terms/coverage", dclist$coverage)
  )

  n_triples(dctriples)
}

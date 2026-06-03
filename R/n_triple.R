#' Create N-Triples
#'
#' Create RDF triple statements to annotate your dataset with standard,
#' interoperable metadata.
#'
#' N-Triples is a line-based serialization format for RDF. It is easy to parse
#' and widely supported. For details, see the
#' [W3C RDF 1.2 N-Triples specification](https://www.w3.org/TR/rdf12-n-triples/).
#'
#' @param triples A character vector of concatenated N-Triples, created with
#'   [n_triple()].
#'
#' @return A character vector of unique N-Triple strings.
#'
#' @examples
#' triple_1 <- n_triple(
#'   "http://example.org/show/218",
#'   "http://www.w3.org/2000/01/rdf-schema#label",
#'   "That Seventies Show"
#' )
#'
#' triple_2 <- n_triple(
#'   "http://example.org/show/218",
#'   "http://example.org/show/localName",
#'   '"Cette S<U+00E9>rie des Ann<U+00E9>es Septante"@fr-be'
#' )
#'
#' n_triples(c(triple_1, triple_2, triple_1))
#'
#' @export


n_triples <- function(triples) {
  unique(triples)
}

#' @title Create an N-Triple
#' @description Create a single N-Triple triple.
#' @details N-Triples is an easy to parse line-based subset of Turtle to serialize
#' RDF. An N-Triple triple is a sequence of RDF terms representing the subject,
#'  predicate and object of an RDF Triple. Use [n_triples()] to serialize
#'  multiple statements.
#' @source [RDF 1.1 N-Triples](https://www.w3.org/TR/n-triples/)
#' @param s The subject of a triplet.
#' @param p The predicate of a triplet.
#' @param o The object of a triplet.
#' @return A character vector containing one N-Triple string.
#' @examples
#' s <- "http://example.org/show/218"
#' p <- "http://www.w3.org/2000/01/rdf-schema#label"
#' o <- "That Seventies Show"
#' n_triple(s, p, o)
#' @export
n_triple <- function(s, p, o) {
  if (length(o) != 1) {
    stop("n_triple(): object 'o' must be a scalar (length = 1), got: ", length(o))
  }

  s <- create_iri(s)
  p <- create_iri(p)

  # Don't convert `o` if it's already quoted with ^^<...>
  if (grepl('^".+"\\^\\^<.+>$', o) || grepl("^<.+>$", o)) {
    # o is already a well-formed literal or URI
  } else {
    o <- create_iri(o)
  }

  sprintf("%s %s %s .", s, p, o)
}


#' @keywords internal
create_iri <- function(x) {
  if (length(x) != 1) {
    stop("create_iri(): input must be a scalar value (length = 1).")
  }

  if (any(c("list", "data.frame", "tbl", "data.table") %in% class(x))) {
    stop("Error: create_iri(x) must be a scalar URI, string, integer, double, Date, dateTime, or person.")
  }

  double_string <- "^^<http://www.w3.org/2001/XMLSchema#double>"
  integer_string <- "^^<http://www.w3.org/2001/XMLSchema#integer>"
  character_string <- "^^<http://www.w3.org/2001/XMLSchema#string>"
  date_string <- "^^<http://www.w3.org/2001/XMLSchema#date>"
  datetime_string <- "^^<http://www.w3.org/2001/XMLSchema#dateTime>"

  if (inherits(x, "person")) {
    if ("isni" %in% tolower(names(x$comment))) {
      x <- paste0("https://isni.org/isni/", x$comment[which(tolower(names(x$comment)) == "isni")])
    } else if ("orcid" %in% tolower(names(x$comment))) {
      x <- paste0("https://orcid.org/", x$comment[which(tolower(names(x$comment)) == "orcid")])
    } else if ("viaf" %in% tolower(names(x$comment))) {
      x <- paste0("https://viaf.org/viaf/", x$comment[which(tolower(names(x$comment)) == "viaf")])
    } else if ("wikidata" %in% tolower(names(x$comment))) {
      qid <- x$comment[which(tolower(names(x$comment)) == "wikidata")]
      qid <- gsub("https://www.wikidata.org/wiki/", "", qid)
      x <- paste0("https://www.wikidata.org/wiki/", qid)
    } else {
      name_parts <- c(x$given, x$family)
      role_part <- if (!is.null(x$role)) paste0("[", x$role, "]") else ""
      x <- paste(trimws(paste(name_parts, collapse = " ")), role_part)
    }
  }

  if (is.integer(x)) {
    sprintf('"%s"%s', as.character(x), integer_string)
  } else if (inherits(x, "POSIXct")) {
    xsd_convert(x)
  } else if (is.character(x) && substr(x, 1, 5) %in% c("http:", "https")) {
    sprintf("<%s>", as.character(x))
  } else if (grepl("^_\\:", x)) {
    x # return blank node unquoted
  } else if (grepl("@", x)) {
    sprintf('"%s"', x)
  } else if (inherits(x, "Date")) {
    sprintf('"%s"%s', as.character(x), date_string)
  } else if (is.numeric(x)) {
    sprintf('"%s"%s', as.character(x), double_string)
  } else if (x == "a") {
    "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
  } else if (is.character(x)) {
    x <- gsub("DCMITYPE\\:", "http://purl.org/dc/dcmitype/", x)
    sprintf('"%s"%s', as.character(x), character_string)
  }
}


#' @keywords internal
prov_author <- function(author_person) {
  # Handle multiple authors
  if (length(author_person) > 1) {
    return(unlist(lapply(author_person, prov_author), use.names = FALSE))
  }

  person_iri <- NULL # <-- initialize early
  print_name <- ""

  if (inherits(author_person, "person")) {
    print_name <- "_:"
    if (!is.null(author_person$family)) print_name <- paste0(print_name, tolower(author_person$family))
    if (!is.null(author_person$given)) print_name <- paste0(print_name, tolower(author_person$given))
    person_iri <- get_person_iri(author_person)
  } else if (is.character(attr(author_person, "person"))) {
    print_name <- paste0(attr(author_person, "person"), ": ")
  }

  if (!is.null(person_iri)) {
    n_triple(person_iri, "a", "http://www.w3.org/ns/prov#Agent")
  } else {
    n_triple(print_name, "a", "http://www.w3.org/ns/prov#Agent")
  }
}

#' @keywords internal
get_person_iri <- function(p) {
  assertthat::assert_that(inherits(p, "person"),
    msg = "Error: get_person_iri(p): p is not a utils::person object."
  )

  if (!is.null(p$comment)) {
    comment_names <- tolower(names(p$comment))
    comment_values <- p$comment

    if ("orcid" %in% comment_names) {
      id <- comment_values[which(comment_names == "orcid")[1]]
      if (!grepl("^https://orcid.org/", id)) id <- paste0("https://orcid.org/", id)
      return(id)
    } else if ("isni" %in% comment_names) {
      id <- comment_values[which(comment_names == "isni")[1]]
      if (!grepl("^https://isni.org/isni/", id)) id <- paste0("https://isni.org/isni/", id)
      return(id)
    } else if ("viaf" %in% comment_names) {
      id <- comment_values[which(comment_names == "viaf")[1]]
      if (!grepl("^https://viaf.org/viaf/", id)) id <- paste0("https://viaf.org/viaf/", id)
      return(id)
    } else if ("wikidata" %in% comment_names) {
      qid <- comment_values[which(comment_names == "wikidata")[1]]
      qid <- sub("^https://www.wikidata.org/wiki/", "", qid) # strip if already full
      return(paste0("https://www.wikidata.org/wiki/", qid))
    }
  }

  NULL
}


#' @keywords internal
expand_triples <- function(dataset_id, predicate_uri, values) {
  if (is.null(values)) {
    return(character(0))
  }

  # Normalize: wrap scalar in list
  if (!is.list(values)) values <- as.list(values)

  # Filter invalid/placeholder values
  values <- Filter(function(x) {
    !is.null(x) &&
      length(x) == 1 &&
      !is.na(x) &&
      !(as.character(x) %in% c("", ":unas", ":tba"))
  }, values)

  if (length(values) == 0) {
    return(character(0))
  }

  vapply(values, function(val) {
    if (inherits(val, "person")) {
      val <- format(val)
    }
    n_triple(dataset_id, predicate_uri, val)
  }, character(1))
}

#' @title Dataset to triples (three columns or N-Triples)
#' @description Converts a dataset to RDF-style triples with subject, predicate,
#'   and object columns. Supports semantic expansion via variable metadata.
#' @param x A `dataset_df` or `data.frame`.
#' @param idcol Name or index of the subject column. If NULL, defaults to
#'   `"rowid"` or rownames.
#' @param expand_uri Logical; if TRUE, expands URIs using namespaces and
#'   definitions.
#' @param format Output format: `"data.frame"` (default) or `"nt"` for
#'   N-Triples.
#' @return Either a `data.frame` with columns `s`, `p`, and `o`, or a character
#'   vector of N-Triple lines.
#' @export
#' @details
#' For publishing examples, a minimal serverless scaffold is provided at
#' <https://github.com/dataobservatory-eu/dataset-template>, which shows how
#' to host CSV + RDF serialisations on GitHub Pages without any server setup.
#'
#' @note A simple, serverless scaffolding for publishing `dataset_df` objects
#'   on the web (with HTML + RDF exports) is available at
#'   <https://github.com/dataobservatory-eu/dataset-template>.
#'
#' @examples
#' # A minimal example with just rowid and geo
#' data("gdp", package = "dataset")
#' small_geo <- dataset_df(
#'   geo = defined(
#'     gdp$geo[1:3],
#'     label = "Geopolitical entity",
#'     concept = "http://example.com/prop/geo",
#'     namespace = "https://dd.eionet.europa.eu/vocabulary/eurostat/geo/$1"
#'   )
#' )
#'
#' # View as triple table
#' dataset_to_triples(small_geo)
#'
#' # View as N-Triples
#' dataset_to_triples(small_geo, format = "nt")
dataset_to_triples <- function(x,
                               idcol = NULL,
                               expand_uri = TRUE,
                               format = "data.frame") {
  is_dataset <- inherits(x, "dataset_df")
  if (!is_dataset) {
    stop("Works only with dataset_df objects.")
  }

  # Determine subject (s)
  if (is.null(idcol)) {
    if ("rowid" %in% names(x)) {
      idcol <- "rowid"
    } else {
      x <- cbind(rowid = row.names(x))
      idcol <- "rowid"
    }
  }

  idcol_pos <- idcol_find(x, idcol)
  data_cols <- setdiff(seq_along(x), idcol_pos)

  # Expand subject URIs using namespace
  s_vec <- as.character(x[[idcol]])
  ns <- tryCatch(var_namespace(x[[idcol]]), error = function(e) NULL)

  if (expand_uri && !is.null(ns) && nzchar(ns)) {
    if (grepl("\\$1", ns)) {
      s_vec <- vapply(s_vec, function(id) sub("\\$1", id, ns), character(1))
    } else {
      s_vec <- paste0(ns, s_vec)
    }
  }


  # Construct triple list
  triple_list <- lapply(data_cols, function(i) {
    triples_column_generate(s_vec, x[[i]], names(x)[i])
  })

  out <- do.call(rbind, triple_list)

  # Add metadata if available
  if (is_dataset) {
    attr(out, "bibentry") <- get_bibentry(x)
    attr(out, "title") <- paste0(dataset_title(x), " [triple form]")
    attr(out, "subject") <- subject(x)
  }

  row.names(out) <- NULL

  # Return either data.frame or N-Triples
  if (format %in% c("nt", "ntriples", "N-Triples")) {
    triples_to_ntriples(out)
  } else {
    out
  }
}

#' @title Internal: Expand multi-valued DC fields to RDF triples
#' @description Converts scalar or vector fields into RDF triples.
#' @param dataset_id The subject URI
#' @param predicate_uri The RDF predicate URI
#' @param values A scalar, character vector, or list (e.g., person objects)
#' @return A character vector of RDF triples
#' @keywords internal
expand_triples <- function(dataset_id, predicate_uri, values) {
  if (is.null(values)) {
    return(character(0))
  }

  # Ensure list for consistency
  if (!is.list(values)) values <- as.list(values)

  vapply(values, function(val) {
    if (inherits(val, "person")) {
      val <- format(val)
    }
    n_triple(dataset_id, predicate_uri, val)
  }, character(1))
}

#' @title Internal: Generate RDF triples for a single column
#' @description Create subject-predicate-object triples from one column of a dataset
#' @param s_vec A character vector of subject URIs (length = number of rows)
#' @param col The column vector (e.g., \code{x[[i]]})
#' @param colname The name of the column (used as fallback for predicate)
#' @return A data.frame with columns s, p, o
#' @keywords internal
triples_column_generate <- function(s_vec, col, colname) {
  def <- tryCatch(var_concept(col), error = function(e) NULL)
  ns <- tryCatch(var_namespace(col), error = function(e) NULL)

  # predicate: use definition or fallback
  pred_uri <- if (!is.null(def) && nzchar(def)) {
    def
  } else {
    paste0("http://example.com/prop/", colname)
  }

  # object: URI from namespace or typed literal via xsd_convert
  if (!is.null(ns) && nzchar(ns)) {
    o_val <- vapply(as.character(col), function(val) {
      if (grepl("\\$1", ns)) {
        sub("\\$1", val, ns)
      } else {
        paste0(ns, val)
      }
    }, character(1))
  } else {
    o_val <- xsd_convert(col)
  }

  data.frame(
    s = s_vec,
    p = rep(pred_uri, length(o_val)),
    o = o_val,
    stringsAsFactors = FALSE
  )
}


#' @title Internal: Convert triple data.frame to N-Triples format
#' @description Turns a data.frame with `s`, `p`, `o` columns into N-Triples strings.
#' @param df A data.frame with columns `s`, `p`, and `o`.
#' @return A character vector of N-Triple lines.
#' @keywords internal
triples_to_ntriples <- function(df) {
  stopifnot(all(c("s", "p", "o") %in% names(df)))
  vapply(seq_len(nrow(df)), function(i) {
    n_triple(df$s[i], df$p[i], df$o[i])
  }, character(1))
}

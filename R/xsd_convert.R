#' Convert to XML Schema Definition (XSD) Types
#'
#' Converts R vectors, data frames, and `dataset_df` objects to
#' [XML Schema Definition (XSD)](https://www.w3.org/TR/xmlschema11-2/)
#' compatible string representations such as `xsd:decimal`, `xsd:boolean`,
#' `xsd:date`, and `xsd:dateTime`.
#'
#' This is primarily used for generating RDF-compatible typed literals.
#'
#' @details
#' - For **vectors**, returns a character vector of typed literals.
#' - For **data frames** or tibbles, returns a data frame with the same
#'   structure but with all values converted to XSD strings.
#' - For `dataset_df` objects, behaves like the data frame method but
#'   preserves dataset-level attributes.
#'
#' @param x An object (vector, data frame, tibble, or `dataset_df`).
#' @param idcol Column name or position to use as row (observation) identifier.
#'   If `NULL`, row names are used.
#' @param shortform Logical. If `TRUE` (default), datatypes are abbreviated with
#'   the `xsd:` prefix (e.g. `"42"^^<xsd:integer>`). If `FALSE`, datatypes are
#'   expanded to full URIs (e.g.
#'   `"42"^^<http://www.w3.org/2001/XMLSchema#integer>`).
#' @param ... Additional arguments passed to methods.
#'
#' @return A character vector or data frame with values serialized as
#' XSD-compatible RDF literals.
#'
#' @examples
#' # Simple data frame with mixed types
#' df <- data.frame(
#'   id     = 1:2,
#'   value  = c(3.14, 2.71),
#'   active = c(TRUE, FALSE),
#'   date   = as.Date(c("2020-01-01", "2020-12-31"))
#' )
#'
#' # Short vs long-form URI:
#' xsd_convert(120L, shortform = TRUE)
#' xsd_convert(121L, shortform = FALSE)
#'
#' @section Class-specific examples:
#' ```r
#' xsd_convert(42L)                   # integer -> xsd:integer
#' xsd_convert(c(TRUE, FALSE, NA))    # logical -> xsd:boolean
#' xsd_convert(Sys.Date())            # Date -> xsd:date
#' xsd_convert(Sys.time())            # POSIXct -> xsd:dateTime
#' xsd_convert(factor("apple"))       # factor -> xsd:string
#' xsd_convert(c("apple", "banana"))  # character -> xsd:string
#' ```
#' @family RDF and linked data helpers
#' @export

xsd_convert <- function(x, idcol = NULL, shortform = TRUE, ...) {
  UseMethod("xsd_convert", x)
}


#' @keywords internal
get_type <- function(t, shortform = TRUE) {
  base_type <- if (any(class(t) %in% c("numeric", "double"))) {
    "decimal"
  } else if (any(class(t) == "integer")) {
    "integer"
  } else if (any(class(t) %in% c("character", "factor"))) {
    "string"
  } else if (any(class(t) == "logical")) {
    "boolean"
  } else if (any(class(t) == "Date")) {
    "date"
  } else if (any(class(t) == "POSIXct")) {
    "dateTime"
  } else if (any(class(t) == "difftime")) {
    "duration"
  } else {
    "string"
  }

  xsd_uri(base_type, shortform = shortform)
}

#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.haven_labelled_defined <- function(x, idcol = NULL,
                                               shortform = TRUE, ...) {
  type <- get_type(x, shortform)

  if (grepl("decimal$", type)) {
    return(xsd_convert(as_numeric(x), shortform = shortform, ...))
  }
  if (grepl("integer$", type)) {
    return(xsd_convert(as_numeric(x), shortform = shortform, ...))
  }
  if (grepl("string$", type)) {
    return(xsd_convert(as_character(x), shortform = shortform, ...))
  }
  if (grepl("boolean$", type)) {
    return(xsd_convert(as.logical(as_numeric(x)), shortform = shortform, ...))
  }

  stop("Unsupported haven_labelled_defined type: ", paste(class(x), collapse = ", "))
}

#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.data.frame <- function(x, idcol = NULL, shortform = TRUE, ...) {
  # Identify ID column (or default to row names)
  if (!is.null(idcol)) {
    id_idx <- idcol_find(x, idcol)
    ids <- as.character(x[[id_idx]])
    id_name <- names(x)[id_idx]
  } else {
    ids <- row.names(x)
    id_name <- ".rowid"
  }

  convert_cols <- setdiff(
    seq_along(x),
    if (!is.null(idcol)) id_idx else integer(0)
  )

  # Apply xsd_convert to all non-ID columns, forwarding shortform + ...
  xsd_list <- lapply(
    convert_cols,
    function(c) xsd_convert(x[[c]], shortform = shortform, ...)
  )
  names(xsd_list) <- names(x)[convert_cols]

  # Assemble result: ID column first, then converted columns
  result <- c(
    setNames(list(ids), id_name),
    xsd_list
  )

  as.data.frame(result, stringsAsFactors = FALSE)
}


#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.dataset_df <- function(x, idcol = "rowid",
                                   shortform = TRUE, ...) {
  NextMethod("xsd_convert", shortform = shortform, ...)
}

#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.tbl_df <- function(x, idcol = NULL, shortform = TRUE, ...) {
  if (!inherits(x, "data.frame")) {
    stop("xsd_convert.tbl_df: input must be a data frame or tibble.")
  }
  NextMethod("xsd_convert", shortform = shortform, ...)
}

#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.character <- function(x, idcol = NULL,
                                  shortform = TRUE, ...) {
  var_type <- xsd_uri("string", shortform)

  if (length(x) == 0) {
    return(paste0('""^^<', var_type, ">"))
  }

  ifelse(is.na(x),
    NA_character_,
    paste0('"', x, '"^^<', var_type, ">")
  )
}


#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.numeric <- function(x, idcol = NULL,
                                shortform = TRUE, ...) {
  var_type <- xsd_uri("decimal", shortform)
  if (length(x) == 0) {
    return(paste0('""^^<', var_type, ">"))
  }

  formatted <- ifelse(is.na(x), NA_character_, {
    str <- format(x, scientific = FALSE, trim = TRUE, justify = "none")
    sub("\\.0$", "", str)
  })

  ifelse(is.na(formatted),
    NA_character_,
    paste0('"', formatted, '"^^<', var_type, ">")
  )
}

#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.integer <- function(x, idcol = NULL, shortform = TRUE, ...) {
  var_type <- xsd_uri("integer", shortform)

  if (length(x) == 0) {
    return(paste0('""^^<', var_type, ">"))
  }

  ifelse(is.na(x),
    NA_character_,
    paste0('"', as.character(x), '"^^<', var_type, ">")
  )
}


#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.logical <- function(x, idcol = NULL, shortform = TRUE, ...) {
  var_type <- xsd_uri("boolean", shortform)

  if (length(x) == 0) {
    return(paste0('""^^<', var_type, ">"))
  }

  ifelse(
    is.na(x),
    NA_character_,
    paste0('"', tolower(as.character(x)), '"^^<', var_type, ">")
  )
}


#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.factor <- function(x, idcol = NULL, shortform = TRUE, ...) {
  args <- list(...)
  codelist <- args$codelist %||% NULL

  if (length(x) == 0) {
    return(paste0('""^^<', xsd_uri("string", shortform), ">"))
  }

  if (is.null(codelist)) {
    var_type <- xsd_uri("string", shortform)
    ifelse(
      is.na(x),
      NA_character_,
      paste0('"', as.character(x), '"^^<', var_type, ">")
    )
  } else {
    ifelse(
      is.na(x),
      NA_character_,
      paste0(codelist, ":", as.character(x))
    )
  }
}


#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.POSIXct <- function(x, idcol = NULL, shortform = TRUE, ...) {
  var_type <- xsd_uri("dateTime", shortform)

  if (length(x) == 0) {
    return(paste0('""^^<', var_type, ">"))
  }

  time_string <- strftime(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  ifelse(
    is.na(x),
    NA_character_,
    paste0('"', time_string, '"^^<', var_type, ">")
  )
}

#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.Date <- function(x, idcol = NULL, shortform = TRUE, ...) {
  var_type <- xsd_uri("date", shortform)

  if (length(x) == 0) {
    return(paste0('""^^<', var_type, ">"))
  }

  date_str <- format(x, format = "%Y-%m-%d")

  ifelse(
    is.na(x),
    NA_character_,
    paste0('"', date_str, '"^^<', var_type, ">")
  )
}


#' @rdname xsd_convert
#' @exportS3Method
xsd_convert.difftime <- function(x, idcol = NULL, shortform = TRUE, ...) {
  var_type <- xsd_uri("duration", shortform)

  if (length(x) == 0) {
    return(paste0('""^^<', var_type, ">"))
  }

  seconds <- unclass(x)
  unit <- attr(x, "units")

  multiplier <- switch(unit,
    "secs" = 1,
    "mins" = 60,
    "hours" = 3600,
    "days" = 86400,
    stop("Unsupported difftime unit: ", unit)
  )
  seconds <- seconds * multiplier

  to_iso8601 <- function(s) {
    if (is.na(s)) {
      return(NA_character_)
    }
    h <- floor(s / 3600)
    m <- floor((s %% 3600) / 60)
    sec <- round(s %% 60)
    paste0(
      "PT",
      if (h > 0) paste0(h, "H") else "",
      if (m > 0) paste0(m, "M") else "",
      if (sec > 0 || (h == 0 && m == 0)) paste0(sec, "S") else ""
    )
  }

  iso_strs <- vapply(seconds, to_iso8601, character(1))

  ifelse(
    is.na(seconds),
    NA_character_,
    paste0('"', iso_strs, '"^^<', var_type, ">")
  )
}

#' @keywords internal
xsd_uri <- function(type, shortform = TRUE) {
  base <- "http://www.w3.org/2001/XMLSchema#"
  if (shortform) {
    paste0("xsd:", type)
  } else {
    paste0(base, type)
  }
}

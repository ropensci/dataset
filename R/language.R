#' @title Set the Primary Language of a Dataset
#'
#' @description
#' Assign the primary language of a semantically rich dataset object using an
#' ISO 639 language code or full language name. This sets the `language`
#' attribute in the dataset's metadata.
#'
#' @details
#' This function supports recognition of:
#' - 2-letter codes (ISO 639-1, e.g., `"en"`, `"fr"`)
#' - 3-letter codes from both:
#'   - `Alpha_3_B` (bibliographic, e.g., `"fre"`)
#'   - `Alpha_3_T` (terminologic, e.g., `"fra"`)
#' - Full language names (e.g., `"English"`, `"French"`)
#'
#' For compatibility with open science repositories and modern metadata
#' standards, this function **returns the terminologic code** (`Alpha_3_T`)
#' when available. If `Alpha_3_T` is missing for a language, the legacy
#' bibliographic code (`Alpha_3_B`) is used as a fallback.
#'
#' Full language names (e.g., `"English"`, `"Spanish"`) are matched
#' case-insensitively against the ISO 639-2 Name field. Exact matches are
#' attempted first; if none are found, a prefix match is used. For example:
#' - `"English"` returns `"eng"`
#' - `"English, Old"` returns `"ang"`
#'
#' This means that:
#' - Both `"fra"` (terminologic) and `"fre"` (bibliographic) will be accepted
#'   as valid input for French
#' - The resulting value stored and returned will be `"fra"`
#'
#' This behaviour aligns with:
#' - [DataCite Metadata Schema 4.4](https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties#9-language)
#' - [schema.org](https://schema.org/inLanguage)
#' - Common repository practices (Zenodo, OSF, Figshare)
#'
#' If `value` is `NULL`, the language is marked as `":unas"` (unspecified).
#'
#' In some cases<U+2014>especially for historical or moribund languages<U+2014>multiple
#' similar names may exist. In such cases, it is safer to use a specific
#' language code (e.g., `"ang"` instead of `"English, Old"` and `"enm"`
#'  for `"English, Middle (1100-1500)"`). You can also
#' refer directly to the definitions in [ISOcodes::ISO_639_2]
#' for clarity.
#'
#' @param x A dataset object created by [dataset_df()] or [as_dataset_df()].
#' @param value A 2-letter or 3-letter language code (ISO 639-1 or ISO 639-2),
#'   or a full language name (case-insensitive).
#' @param iso_639_code A character string indicating the desired return format:
#'   either `"639-3"` (default; terminologic) or `"639-1"` (2-letter code).
#'
#' @return The dataset with an updated `language` attribute, typically an ISO
#' 639-2/T code (`Alpha_3_T`) such as `"fra"`, `"eng"`, `"spa"`, etc.
#'
#' @examples
#' df <- dataset_df(data.frame(x = 1:3))
#'
#' language(df) <- "English" # Returns "eng"
#' language(df) <- "fre" # Legacy code; returns "fra"
#' language(df) <- "fra" # Returns "fra"
#' language(df, iso_639_code = "639-1") <- "fra" # Returns "fr"
#'
#' language(df) <- NULL # Sets ":unas"
#'
#' @family bibliographic helper functions
#' @export

language <- function(x) {
  assert_that(is.dataset_df(x),
    msg = "language(x): x must be a dataset object created with dataset() or as_dataset_df()."
  )

  ds_bibentry <- get_bibentry(x)
  as.character(ds_bibentry$language)
}


#' @rdname language
#' @export
`language<-` <- function(x, iso_639_code = "639-3", value) {
  assert_that(
    is.dataset_df(x),
    msg = "language(x)<- value: x must be a dataset object created with dataset() or as_dataset_df()."
  )

  ds_bibentry <- get_bibentry(x)

  # Handle NULL input by assigning placeholder
  if (is.null(value)) {
    ds_bibentry$language <- ":unas"
    attr(x, "dataset_bibentry") <- ds_bibentry
    return(x)
  }

  # Load and normalize ISO language data
  ISO_639 <- ISOcodes::ISO_639_2
  ISO_639 <- as.data.frame(
    lapply(ISO_639, as.character),
    stringsAsFactors = FALSE
  )

  value <- tolower(value)
  lang_entry <- data.frame()

  # Try matching by Alpha_2 (2-letter code)
  if (nchar(value) == 2) {
    lang_entry <- ISO_639[which(tolower(ISO_639$Alpha_2) == value), ]
  }

  # Try matching by Alpha_3_B or Alpha_3_T (3-letter code)
  if (nrow(lang_entry) == 0 && nchar(value) == 3) {
    lang_entry <- ISO_639[
      tolower(ISO_639$Alpha_3_B) == value |
        tolower(ISO_639$Alpha_3_T) == value,
    ]
  }

  # Try matching by full language name
  if (nrow(lang_entry) == 0) {
    lang_entry <- ISO_639[which(tolower(ISO_639$Name) == value), ]
  }

  # If Still not found, fail gracefully
  if (nrow(lang_entry) == 0) {
    stop(paste0(
      "Language='", value,
      "' is not a valid ISO 639 language code."
    ))
  }

  # Assign value according to iso_639_code preference
  if (iso_639_code == "639-1") {
    ds_bibentry$language <- as.character(lang_entry$Alpha_2[1])
  } else {
    # Prefer terminologic code if present and non-empty
    lang3t <- as.character(lang_entry$Alpha_3_T[1])
    lang3b <- as.character(lang_entry$Alpha_3_B[1])

    ds_bibentry$language <- if (!is.na(lang3t) && lang3t != "") {
      lang3t
    } else {
      lang3b
    }
  }

  attr(x, "dataset_bibentry") <- ds_bibentry
  invisible(x)
}

#' @rdname language
#' @export
`language<-` <- function(x, iso_639_code = "639-3", value) {
  assert_that(
    is.dataset_df(x),
    msg = "language(x)<- value: x must be a dataset object created with dataset() or as_dataset_df()."
  )

  ds_bibentry <- get_bibentry(x)

  # Handle NULL input by assigning placeholder
  if (is.null(value)) {
    ds_bibentry$language <- ":unas"
    attr(x, "dataset_bibentry") <- ds_bibentry
    return(x)
  }

  # Load and normalize ISO language data
  ISO_639 <- ISOcodes::ISO_639_2
  ISO_639 <- as.data.frame(
    lapply(ISO_639, as.character),
    stringsAsFactors = FALSE
  )

  value <- tolower(value)
  lang_entry <- data.frame()

  # Try matching by Alpha_2 (2-letter code)
  if (nchar(value) == 2) {
    matches <- which(tolower(ISO_639$Alpha_2) == value)
    if (length(matches) > 0) {
      lang_entry <- ISO_639[matches, ]
    }
  }

  # Try matching by Alpha_3_B or Alpha_3_T (3-letter code)
  if (nrow(lang_entry) == 0 && nchar(value) == 3) {
    matches <- which(
      tolower(ISO_639$Alpha_3_B) == value |
        tolower(ISO_639$Alpha_3_T) == value
    )
    if (length(matches) > 0) {
      lang_entry <- ISO_639[matches, ]
    }
  }

  # Try matching by full language name
  if (nrow(lang_entry) == 0) {
    matches <- which(value == tolower(ISO_639$Name))
    if (length(matches) == 0) {
      matches <- which(grepl(paste0("^", value), tolower(ISO_639$Name)))
    }
    lang_entry <- ISO_639[matches, ]
  }

  # If still not found, fail gracefully
  if (nrow(lang_entry) == 0) {
    stop(
      paste0(
        "Language='", value,
        "' is not a valid ISO 639 language code or name."
      )
    )
  }

  # Assign value according to iso_639_code preference
  if (iso_639_code == "639-1") {
    ds_bibentry$language <- as.character(lang_entry$Alpha_2[1])
  } else {
    # Prefer terminologic code if present and non-empty
    lang3t <- as.character(lang_entry$Alpha_3_T[1])
    lang3b <- as.character(lang_entry$Alpha_3_B[1])

    ds_bibentry$language <- if (!is.na(lang3t) && lang3t != "") {
      lang3t
    } else {
      lang3b
    }
  }

  attr(x, "dataset_bibentry") <- ds_bibentry
  invisible(x)
}

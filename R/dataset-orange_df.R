#' Growth of Orange Trees
#'
#' A dataset recording the growth of orange trees, replicated from the classic
#' [datasets::Orange](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/Orange.html)
#' dataset and implemented as a `dataset_df` S3 class with enhanced semantic metadata.
#'
#' @format A data frame with 35 rows and 4 variables:
#' - `rowid`: A unique identifier for each row (character)
#' - `tree`: Tree identifier (ordered factor)
#' - `age`: Age of the tree in days (numeric)
#' - `circumference`: Trunk circumference in mm (numeric)
#'
#' @details
#' This is a semantically enriched version of the classic Orange dataset,
#' constructed using the `dataset_df()` and `dublincore()` constructors.
#' Each column includes semantic metadata such as units, labels, concepts,
#' or namespace identifiers. The dataset also embeds a machine-readable citation
#' for reproducibility and provenance tracking.
#'
#' ### Constructor Example
#' ```r
#' orange_bibentry <- dublincore(
#'   title = "Growth of Orange Trees",
#'   creator = c(
#'     person(
#'       given = "N.R.",
#'       family = "Draper",
#'       role = "cre",
#'       comment = c(VIAF = "http://viaf.org/viaf/84585260")
#'     ),
#'     person(
#'       given = "H",
#'       family = "Smith",
#'       role = "cre"
#'     )
#'   ),
#'   contributor = person(
#'     given = "Antal",
#'     family = "Daniel",
#'     role = "dtm"
#'   ),
#'   publisher = "Wiley",
#'   datasource = "https://isbnsearch.org/isbn/9780471170822",
#'   dataset_date = 1998,
#'   identifier = "https://doi.org/10.5281/zenodo.14917851",
#'   language = "en",
#'   description = "The Orange data frame has 35 rows and 3 columns of records of the growth of orange trees."
#' )
#'
#' orange_df <- dataset_df(
#'   rowid = defined(paste0("orange:", row.names(Orange)),
#'     label = "ID in the Orange dataset",
#'     namespace = c("orange" = "datasets::Orange")
#'   ),
#'   tree = defined(Orange$Tree,
#'     label = "The number of the tree"
#'   ),
#'   age = defined(Orange$age,
#'     label = "The age of the tree",
#'     unit = "days since 1968/12/31"
#'   ),
#'   circumference = defined(Orange$circumference,
#'     label = "circumference at breast height",
#'     unit = "milimeter",
#'     concept = "https://www.wikidata.org/wiki/Property:P2043"
#'   ),
#'   dataset_bibentry = orange_bibentry
#' )
#'
#' orange_df$rowid <- defined(orange_df$rowid,
#'   namespace = "https://doi.org/10.5281/zenodo.14917851"
#' )
#' ```
#' @references
#' - Draper, N. R. & Smith, H. (1998). *Applied Regression Analysis* (3rd ed.). Wiley.
#' - Pinheiro, J. C. & Bates, D. M. (2000). *Mixed-effects Models in S and S-PLUS*. Springer.
#' - Becker, R. A., Chambers, J. M. & Wilks, A. R. (1988). *The New S Language*. Wadsworth & Brooks/Cole.
#'
#' @examples
#' # Print with semantic citation and data preview
#' print(orange_df)
#'
#' # Access semantic metadata associated with variables
#' print(orange_df$age)
#'
#' # Retrieve the embedded bibliographic record
#' as_dublincore(orange_df)
#'
#' @keywords datasets
"orange_df"

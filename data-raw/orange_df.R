## code to prepare `orange_df` dataset goes here
orange_bibentry <- dublincore(
  title = "Growth of Orange Trees",
  creator = c(
    person(
      given = "N.R.",
      family = "Draper",
      role = "cre",
      comment = c(VIAF = "http://viaf.org/viaf/84585260")
    ),
    person(
      given = "H",
      family = "Smith",
      role = "cre"
    )
  ),
  contributor = person(
    given = "Antal",
    family = "Daniel",
    role = "dtm"
  ), # Add data manager
  publisher = "Wiley",
  datasource = "https://isbnsearch.org/isbn/9780471170822",
  dataset_date = 1998,
  identifier = "https://doi.org/10.5281/zenodo.14917851",
  language = "en",
  description = "The Orange data frame has 35 rows and 3 columns of records of the growth of orange trees."
)

orange_df <- dataset_df(
  rowid = defined(paste0("orange:", row.names(Orange)),
    label = "ID in the Orange dataset",
    namespace = c("orange" = "datasets::Orange")
  ),
  tree = defined(Orange$Tree,
    label = "The number of the tree"
  ),
  age = defined(Orange$age,
    label = "The age of the tree",
    unit = "days since 1968/12/31"
  ),
  circumference = defined(Orange$circumference,
    label = "circumference at breast height",
    unit = "milimeter",
    concept = "https://www.wikidata.org/wiki/Property:P2043"
  ),
  dataset_bibentry = orange_bibentry
)


orange_df$rowid <- defined(orange_df$rowid,
  namespace = "https://doi.org/10.5281/zenodo.14917851"
)
write.csv(orange_df, file = "orange_df.csv", row.names = FALSE)
saveRDS(orange_df, file = "orange_df.rds")

usethis::use_data(orange_df, overwrite = TRUE)

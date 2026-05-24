reprextemplates::concat_text_files(
  source_dir = here::here("R"), extension = "R", recursive = F,
  output_file = "data-raw/dataset-review-R.txt",
  TRUE
)

reprextemplates::concat_text_files(
  source_dir = here::here("vignettes"), extension = "Rmd",
  output_file = "data-raw/dataset-vignettes.txt",
  TRUE
)

reprextemplates::concat_text_files(
  source_dir = here::here(), extension = "bak", recursive = T,
  output_file = "data-raw/dataset-review-bak.txt",
  TRUE
)


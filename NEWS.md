# dataset 0.4.2

Fixing a a brittle unit test that assumed a stable printed representation of 
`utils::bibentry()`.

# dataset 0.4.1

This release strengthens the handling of semantically enriched vectors and improves coercion across base R and tidyverse workflows.

## Enhancements

- New S3 methods for semantically enriched logical, `Date`, and `POSIXct` types.
- Expanded coercion support:
  - `as_numeric()`, `as_character()`, `as_logical()`, `as_factor()`
  - Optional preservation of semantic metadata (`label`, `unit`, `concept`, `namespace`).
- Rewritten coercion logic for all `defined()` vector types, ensuring stable and predictable behaviour.

## dataset_df improvements

- `as.data.frame.dataset_df()` and `as_tibble.dataset_df()`:
- Correct handling of numeric, character, factor, and date-time columns.
- Label-aware coercion for categorical variables.
- Clear separation between attribute stripping and preservation.

## Testing and robustness

- Significant increase in test coverage, including tests for all coercion paths, metadata stripping, and temporal types.
- Improved error messaging for invalid type coercion.
- More consistent printing and formatting of `defined` vectors.

This update improves reliability, consistency, and interoperability of semantically enriched datasets in R.

# dataset 0.4.0

A new CRAN release with much improved unit testing and documentation to 
meet the rOpenSci standards and better methods for the main s3 classes of
the package.

- Rewritten vignettes.
- Improved print, summary methods for `dataset_df` and `defined`.
- Better handling of multible contributors in `bibrecord`. 
- A new `dataset_to_triples` and `xsd_convert` for better serialisation.
- A better handling of empty nodes in RDF.
- Many bug fixes in the way semantic information is translated to RDF.
- `var_labels()` now similar to `labelled::var_lables()` behavior, generally 
haven_labelled_defined as an s3 class works better in the tidyverse.
- New bibliographic helper functions for `dataset_format()` and `contributor()`.
- Countless small bug fixes to convert to various metadata schemas edge cases, 
like missing contributors, formatted subjects, etc.
- Better handling of structured metadata with `subject()`

# dataset 0.3.9

- New CRAN release with many bug fixes, and improvements from peer-review.
- The `definition` attributes is renamed to `concept`.
- Improved printing for `defined` and `dataset_df` classes.
- Improved compatibility and coercion methods for base R character and numeric types.
- A clearer `bibrecord` class for extending `utils::person` and `utils::bibentry` classes for more modern and cleaner bibliographic references. 

# dataset 0.3.4027

- The new `bibrecord()` class is handles is the superclass of the `dublincore` and
`datacite()` classes; these classes have a new print method and they are conforming
the current library standard DCTERMS and current repository standard DataCite;
unlike `utils::bibentry()`, they handle contributors and their roles, identifiers, 
and many other attributes.
- Breaking change: the `definition` metadata field in the `defined()` class is 
changed to the more understandable `concept` name.
- The `defined()` vectors print nicely, and the `dataset_df()` class is more 
readable, too.
- The missing examples are present, including examples on the use of the 
semantically richer `orange_df` example dataset.
- Many code quality improvements and new tests.

# dataset 0.3.4023

- Changed `iris_df` to `orange_df` in all examples.
- `xsd_convert()` handles difftime classes and edge cases.
- Small errors fixed in examples.
- Test coverage increased.
- The `master` branch is renamed to `main`.

# dataset 0.3.4021

- Added support for generic vector methods: `length()`, `head()`, `tail()`, `as.vector()`, `as.list()`, and subsetting (`[`, `[[`).
- Implemented comparison methods (`==`, `<`, `>`, etc.) that operate on the underlying data while maintaining semantic integrity.
- Introduced custom `print()` and `format()` methods that summarise metadata (label, unit, definition) in a concise and human-readable manner.
- Improved the `summary()` method for `defined` vectors to display variable metadata and integrate seamlessly with base R statistics.
- Enhanced the `c()` method to validate compatibility across all semantic attributes (`label`, `unit`, `definition`, `namespace`) before concatenation.
- Extended vignette with richer examples and explanations of semantic validation, namespaces, and metadata access.
- `compare_creators()` internal function to add all creators to joined datasets.

This update significantly improves the usability and robustness of semantically enriched vectors in both interactive and programmatic workflows.

# dataset 0.3.4

- New release on CRAN.

# dataset 0.3.0

- Released on CRAN.
- 0.3.1. Is a minor bug fix with units test on old R releases. It does not affect the functionality of the package.

# dataset 0.2.9

- `dataset_ttl_write()`: write datasets to turtle format; 
- with helper functions `get_prefix()`,  `get_resource_identifier()`, `xsd_convert()`, and `dataset_to_triples()`.

# dataset 0.2.8

New vignettes on

- [x] [Richer Semantics for the Dataset's Variables](https://dataset.dataobservatory.eu/articles/defined.html)


# dataset 0.2.7 

* Released on CRAN

The devel branch contains new code that is not is validated, but as a whole the package is not working consistently.

# dataset 0.2.6
* All tests are passing, all examples are running.

# dataset 0.2.5 
* `datacite()` has a new interface and an  `as_datacite()` retrieval version. See the `Working with DataCite Metadata` vignette.
* `dublincore()` has a new interface and an  `as_dublincore()` version. See the `Working with Dublin Core Metadata` vignette.

# dataset 0.2.4
All tests are passing but documentation is not rewritten yet.

# dataset 0.2.3 
new subject class for recording subjects

# dataset 0.2.2 
New s3 classes for DataCite and Dublin Core bibliographic entries.

# dataset 0.2.1

A minor correction to avoid vignettes downloading data from the Eurostat data warehouse on CRAN. Small readability improvements in the vignette articles.

# dataset 0.2.0

* New methods for the `dataset()` s3 class: `print.dataset()`, `summary.dataset()`, `subset.dataset`, `[.dataset`, `as.data.frame()`.
* New vignette on how to use the [dataspice](https://github.com/ropensci/dataspice) package programmatically for publishing dataset documentation.
* Released on CRAN.

# dataset 0.1.9

* Incorporating minor changes from the [rOpenSci](https://github.com/ropensci/software-review/issues/553) and CRAN peer-reviews.

# dataset 0.1.7
[![Status at rOpenSci Software Peer Review](https://badges.ropensci.org/553_status.svg)](https://github.com/ropensci/software-review/issues/553)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6992467.svg)](https://doi.org/10.5281/zenodo.6992467)

* After reviewing CRAN submission comments, and correcting documentation issues, submitted to [rOpenSci](https://github.com/ropensci/software-review/issues/553) for review before re-submitting to CRAN.

# dataset 0.1.6.0001
* Add `dataset_local_id()` and `dataset_uri()` to the dataset functions.

# dataset 0.1.6.
* A release candidate on CRAN after small documentation improvements.

# dataset 0.1.4.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6950435.svg)](https://doi.org/10.5281/zenodo.6950435) Development version available on Zenodo.

* `dataset_export()` is implemented with filetype = 'csv'.
* Replacement functions are added to simple properties `identifier()`, `publisher()`, `publication_year()`, `language()`, `description()`,  `datasource_get()` and `datasource_set()` [to avoid confusion with the base R source() function], `geolocation()`, `rights()`, `version()`.
* Functions to work with structured referential metadata: `dataset_title()`, `subject()`, `subject_create()`.

# dataset 0.1.3.

* Vignette articles started to develop and consult the development plan of the project. See  [From dataset To RDF](https://dataset.dataobservatory.eu/articles/rdf.html), _Export and Publish A dataset Object_, _Datasets with FAIR metadata_.
* New functions: `download_dataset()`, `datacite()`, and the `dataset()` constructor.

# dataset 0.1.2.

* The definition of the `dataset()` class, an improved data.frame (tibble, DT) R object with standardized structure and metadata.
* Adding and reading [DublinCore](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/) metadata and [DataCite](https://support.datacite.org/docs/datacite-metadata-schema-44/) mandatory and recommended [FAIR metadata](https://www.go-fair.org/fair-principles/) metadata.

# dataset 0.1.0.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6703765.svg)](https://doi.org/10.5281/zenodo.6703765) First development version release.

* Added the `Motivation of the dataset package` vignette article, which is later
replaced with [Design Principles & Future Work Semantically Enriched, Standards-Aligned Datasets in R](https://dataset.dataobservatory.eu/articles/design.html).


<!-- README.md is generated from README.Rmd. Please edit that file -->

# The dataset R Package <a href='https://dataset.dataobservatory.eu/'><img src="man/figures/logo.png" align="right"/></a>

<!-- badges: start -->

<!-- badges: start -->

[![rhub](https://github.com/ropensci/dataset/actions/workflows/rhub.yaml/badge.svg)](https://github.com/ropensci/dataset/actions/workflows/rhub.yaml)
[![devel-version](https://img.shields.io/badge/devel%20version-0.4.0-blue.svg)](https://github.com/ropensci/dataset)
[![Codecov test
coverage](https://codecov.io/gh/ropensci/dataset/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci/dataset?branch=main)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/dataset)](https://cran.r-project.org/package=dataset)
[![CRAN_time_from_release](https://www.r-pkg.org/badges/ago/dataset)](https://cran.r-project.org/package=dataset)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/681_status.svg)](https://github.com/ropensci/software-review/issues/681)
[![DOI](https://img.shields.io/badge/DOI-10.32614%2FCRAN.package.dataset-blue)](https://doi.org/10.32614/CRAN.package.dataset)
[![dataobservatory](https://img.shields.io/badge/ecosystem-dataobservatory.eu-3EA135.svg)](https://dataobservatory.eu/)
<!-- badges: end -->

# Overview

The `dataset` package helps you create **semantically rich**,
**machine-readable**, and **interoperable datasets** in R. It introduces
S3 classes that extend data frames, vectors, and bibliographic entries
with formal metadata structures inspired by:

- **SDMX** (Statistical Data and Metadata eXchange), widely used in
  official statistics  
- **Dublin Core** and **DataCite**, for FAIR-compliant depositing and
  reuse in scientific and open data repositories  
- **Open Science publishing practices**, to support transparent and
  reproducible research

The goal is to preserve metadata when reusing statistical and repository
datasets, improve interoperability, and make it easy to turn tidy data
frames into web-ready, publishable datasets that comply with ISO and W3C
standards.

## Installation

You can install the latest released version of **`dataset`** from
[CRAN](https://cran.r-project.org/package=dataset) with:

``` r
install.packages("dataset")
```

To install the development version from GitHub with `pak` or `remotes`:

``` r
# install.packages("pak")
pak::pak("dataobservatory-eu/dataset")

# install.packages("remotes")
remotes::install_github("dataobservatory-eu/dataset")
```

## Minimal Example

``` r
library(dataset)
df <- dataset_df(
  country = defined(
    c("AD", "LI"),
    label = "Country",
    namespace = "https://www.geonames.org/countries/$1/"
  ),
  gdp = defined(c(3897, 7365),
    label = "GDP",
    unit = "million euros"
  ),
  dataset_bibentry = dublincore(
    title = "GDP Dataset",
    creator = person("Jane", "Doe", role = "aut"),
    publisher = "Small Repository"
  )
)
print(df)
#> Doe (2025): GDP Dataset [dataset]
#>   rowid     country   gdp       
#>   <defined> <defined> <defined>
#> 1 obs1      AD        3897     
#> 2 obs2      LI        7365
```

Export as RDF triples:

<style type="text/css">
.smaller .table {
  font-size: 11px;
}
&#10;.smaller pre,
.smaller code {
  font-size: 11px;
  line-height: 1.2;
}
</style>

``` r
dataset_to_triples(df, format = "nt")
```

<div class="smaller">

    #> [1] "<http://example.com/dataset#obsobs1> <http://example.com/prop/country> <https://www.geonames.org/countries/AD/> ."
    #> [2] "<http://example.com/dataset#obsobs2> <http://example.com/prop/country> <https://www.geonames.org/countries/LI/> ."
    #> [3] "<http://example.com/dataset#obsobs1> <http://example.com/prop/gdp> \"3897\"^^<xsd:decimal> ."                     
    #> [4] "<http://example.com/dataset#obsobs2> <http://example.com/prop/gdp> \"7365\"^^<xsd:decimal> ."

</div>

Retain automatically recorded provenance:

``` r
provenance(df)
```

<div class="smaller">

    #> [1] "<http://example.com/dataset_prov.nt> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Bundle> ."                  
    #> [2] "<http://example.com/dataset#> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Entity> ."                         
    #> [3] "<http://example.com/dataset#> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.org/linked-data/cube#DataSet> ."                 
    #> [4] "_:doejane <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> ."                                              
    #> [5] "<https://doi.org/10.32614/CRAN.package.dataset> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#SoftwareAgent> ."
    #> [6] "<http://example.com/creation> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Activity> ."                       
    #> [7] "<http://example.com/creation> <http://www.w3.org/ns/prov#generatedAtTime> \"2025-09-02T07:19:30Z\"^^<xsd:dateTime> ."

</div>

## Contributing

We welcome contributions and discussion!

- Please see our
  [CONTRIBUTING.md](https://github.com/dataobservatory-eu/dataset/blob/main/CONTRIBUTING.md)
  guide.
- Ideas, bug reports, and feedback are welcome via [GitHub
  issues](https://github.com/dataobservatory-eu/dataset/issues).
- The design principles and ideas for futher development are explained
  in [Design Principles & Future Work Semantically Enriched,
  Standards-Aligned Datasets in
  R](https://dataset.dataobservatory.eu/articles/design.html).

## Code of Conduct

This project follows the [rOpenSci Code of
Conduct](https://ropensci.org/code-of-conduct/). By participating, you
are expected to uphold these guidelines.

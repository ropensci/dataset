
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The dataset R Package <a href='https://dataset.dataobservatory.eu/'><img src="man/figures/logo.png" align="right"/></a>

<!-- badges: start -->

[![rhub](https://github.com/ropensci/dataset/actions/workflows/rhub.yaml/badge.svg)](https://github.com/ropensci/dataset/actions/workflows/rhub.yaml)
[![devel-version](https://img.shields.io/badge/devel%20version-0.4.5-blue.svg)](https://github.com/ropensci/dataset)
[![Codecov test
coverage](https://codecov.io/gh/ropensci/dataset/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci/dataset?branch=main)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/dataset)](https://cran.r-project.org/package=dataset)
[![CRAN_time_from_release](https://www.r-pkg.org/badges/ago/dataset)](https://cran.r-project.org/package=dataset)
<!-- [![DOI](https://img.shields.io/badge/DOI-10.32614/CRAN.package.dataset-blue)](https://doi.org/10.32614/CRAN.package.dataset) --->
[![dataobservatory](https://img.shields.io/badge/ecosystem-dataobservatory.eu-3EA135.svg)](https://dataobservatory.eu/)
<!-- badges: end -->

# Overview

The `dataset` package extends tidyverse workflows with lightweight
semantic metadata, provenance tracking, and interoperable dataset
structures.

It supports gradual semantic stabilization ranging from lightweight
semantic mappings to formally defined variables and semantically
enriched datasets suitable for FAIR, machine-readable, and
standards-aligned data exchange.

The package draws inspiration from:

- **SDMX** and statistical data cube models
- **Dublin Core** and **DataCite**
- **FAIR and reproducible research workflows**

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

In real analytical workflows, we often encounter inconsistent semantic
labeling. Early in a project, inconsistencies may be easy to detect,
such as mixing `AD` and `Andorra`, but later we may join datasets that
use different coding systems, for example the ISO-3166 alpha-3 `AND`
code for the same country.

``` r
library(dataset)

x <- prelabel(
  c("AD", "Andorra", "LI"),
  labels = c(
    Andorra = "AD",
    Liechtenstein = "LI"
  )
)

as.character(x)
#> [1] "AD" "AD" "LI"
```

The `prelabel()` constructor supports lightweight semantic stabilization
while preserving the original observational values and provisional
semantic mappings:

``` r
attr(x, "prelabel")
#>       Andorra Liechtenstein            AD            LI 
#>          "AD"          "LI"          "AD"          "LI"
```

Once semantic assumptions become sufficiently stable, variables can be
formalized with `defined()` and combined into a semantically enriched
`dataset_df()` object:

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
#> Doe (2026): GDP Dataset [dataset]
#>   rowid country   gdp 
#>   <chr> <chr>   <dbl>
#> 1 obs1  AD       3897
#> 2 obs2  LI       7365
```

Because semantic assumptions and provenance are preserved explicitly,
semantically enriched datasets can be exported as interoperable RDF
triples without manually reconstructing metadata at publication time.

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
    #> [7] "<http://example.com/creation> <http://www.w3.org/ns/prov#generatedAtTime> \"2026-05-24T18:48:53Z\"^^<xsd:dateTime> ."

</div>

## Contributing

We welcome contributions and discussion!

- Please see our
  [CONTRIBUTING.md](https://github.com/ropensci/dataset/blob/main/CONTRIBUTING.md)
  guide.
- Ideas, bug reports, and feedback are welcome via [GitHub
  issues](https://github.com/ropensci/dataset/issues).
- The design principles and ideas for futher development are explained
  in [Design Principles & Future Work Semantically Enriched,
  Standards-Aligned Datasets in
  R](https://dataset.dataobservatory.eu/articles/design.html).

Please refer to this package as:

Daniel Antal. (2026). *dataset: Create Data Frames that are Easier to
Exchange and Reuse (0.4.4)*. The Comprehensive R Archive Network.
<https://zenodo.org/records/17621464>, DOI:
10.32614/CRAN.package.dataset

See contributors on the website and in the DESCRIPTION file.

## Code of Conduct

This project follows the [rOpenSci Code of
Conduct](https://ropensci.org/code-of-conduct/). By participating, you
are expected to uphold these guidelines.

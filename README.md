
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wbdataset

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/wbdataset)](https://CRAN.R-project.org/package=wbdataset)
[![devel-version](https://img.shields.io/badge/devel%20version-0.1.1029-blue.svg)](https://github.com/dataobservatory-eu/wbdataset)
\[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13972192.svg)
[![Codecov test
coverage](https://codecov.io/gh/dataobservatory-eu/wbdataset/graph/badge.svg)](https://app.codecov.io/gh/dataobservatory-eu/wbdataset)
<!-- badges: end -->

The goal of `wbdataset` is to create tidy datasets from Wikidata or a
Wikibase instance. The `wbdataset` package is an extension of the
`dataset`, which in turn is an R package that helps to exchange, publish
and combine datasets more easily by improving their semantics. The
`wbdataset` extends the usability of dataset by connecting the Wikibase
API with the R statistical environment.

## Installation

You can install the development version of wbdataset from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("antaldaniel/dataset")
devtools::install_github("dataobservatory-eu/wbdataset")
```

Once installed,

``` r
library(wbdataset)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(wbdataset)
small_countries_dataset <- get_wikidata_item(
  qid = c("Q228", "Q347"),
  language = c("en", "nl"),
  data_curator = person("Jane Doe"),
  title = "Small Countries"
)
#> Downloaded Q228
#> Downloaded Q347

small_countries_dataset
#> Jane Doe (2025). "Small Countries."
#>   rowid      qid_on_wikidata label         description                  language
#>   <hvn_lbl_> <hvn_lbl_>      <hvn_lbl_>    <hvn_lbl_>                   <hvn_lb>
#> 1 eg:1       Q228            Andorra       sovereign microstate betwee… en      
#> 2 eg:2       Q228            Andorra       land in Europa               nl      
#> 3 eg:3       Q347            Liechtenstein country in Central Europe    en      
#> 4 eg:4       Q347            Liechtenstein land in Europa               nl
```

The *wbdataset* package has three tutorial articles:

- [Importing from Wikidata or
  Wikibase](https://wbdataset.dataobservatory.eu/articles/wikidata-import.html)
  shows you how to import data from Wikidata.
- [Authenticated MediaWiki API
  Access](https://wbdataset.dataobservatory.eu/articles/Wikibase_API.html)
  shows how you log in to a Wikibase instance. If you use `wbdataset` to
  retrieve information only from Wikidata, you do not need this.
- [Getting Started To Create a Graph on
  Wikibase](https://wbdataset.dataobservatory.eu/articles/start.html)
  shows how to add data to a Wikibase graph database.

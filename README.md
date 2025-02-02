
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wbdataset

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/wbdataset)](https://CRAN.R-project.org/package=wbdataset)
\[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13972192.svg)\]
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

``` r
library(wbdataset)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(wbdataset)
get_item(
  qid = c("Q228", "Q347"),
  language = c("en", "nl"),
  creator = person("Jane Doe"),
  title = "Small Countries"
)
#> Downloaded Q228
#> Downloaded Q347
#> Jane Doe (????). "Small Countries."
#>   rowid      qid        label         description                       language
#>   <hvn_lbl_> <hvn_lbl_> <hvn_lbl_>    <hvn_lbl_>                        <hvn_lb>
#> 1 eg:1       Q228       Andorra       sovereign microstate between Fra… en      
#> 2 eg:2       Q228       Andorra       land in Europa                    nl      
#> 3 eg:3       Q347       Liechtenstein country in Central Europe         en      
#> 4 eg:4       Q347       Liechtenstein land in Europa                    nl
```

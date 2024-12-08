
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wbdataset

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/wbdataset)](https://CRAN.R-project.org/package=wbdataset)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13972192.svg)](https://zenodo.org/record/13972192)
[![Codecov test
coverage](https://codecov.io/gh/dataobservatory-eu/wbdataset/graph/badge.svg)](https://app.codecov.io/gh/dataobservatory-eu/wbdataset)
<!-- badges: end -->

The goal of wbdataset is to create tidy datasets from Wikidata or a
Wikibase instance.

## Installation

You can install the development version of wbdataset from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dataobservatory-eu/wbdataset")
```

``` r
library(wbdataset)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(wbdataset)
get_item(qid=c("Q228", "Q347"), 
         language=c("en", "nl"), 
         creator=person("Jane Doe"), 
         title="Small Countries")
#> Downloaded Q228
#> Downloaded Q347
#> Jane Doe (2024). "Small Countries."
#>   qid        label         description                                  language
#>   <hvn_lbl_> <hvn_lbl_>    <hvn_lbl_>                                   <hvn_lb>
#> 1 Q228       Andorra       sovereign microstate between France and Spa… en      
#> 2 Q228       Andorra       land in Europa                               nl      
#> 3 Q347       Liechtenstein country in Central Europe                    en      
#> 4 Q347       Liechtenstein land in Europa                               nl
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

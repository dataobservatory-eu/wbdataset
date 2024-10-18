
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wbdataset

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/wbdataset)](https://CRAN.R-project.org/package=wbdataset)
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
#>    qid         label
#> 1 Q228       Andorra
#> 2 Q228       Andorra
#> 3 Q347 Liechtenstein
#> 4 Q347 Liechtenstein
#>                                                        description language
#> 1 sovereign microstate between France and Spain, in Western Europe       en
#> 2                                                   land in Europa       nl
#> 3                                        country in Central Europe       en
#> 4                                                   land in Europa       nl
#> Further metadata: describe(x)
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

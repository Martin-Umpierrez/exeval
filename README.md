
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![R-CMD-check](https://github.com/Martin-Umpierrez/exeval/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Martin-Umpierrez/exeval/actions/workflows/R-CMD-check.yaml)

# exeval <img align="right" src = "man/figures/exeval_logohex.png" width="135px">

# exeval: An R-package for External Evaluation of popPKPD Models.

`exeval` package performs the *external evaluation process* of a popPKPD
model. The external evaluation is done using an independent dataset from
which the original popPKPD model was developed. Currently, model fit is
based on [mapbayr](https://github.com/FelicienLL/mapbayr) package.

External evaluation for a single model from can be done with:

- a population PKPD model
  ([mrgsolve](https://github.com/metrumresearchgroup/mrgsolve) format),
- a data set with concentrations (NONMEM format)

## Installation

You can install the development version of exeval from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("Martin-Umpierrez/exeval")
```


<!-- README.md is generated from README.Rmd. Please edit that file -->

[![R-CMD-check](https://github.com/Martin-Umpierrez/exeval/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Martin-Umpierrez/exeval/actions/workflows/R-CMD-check.yaml)

# exeval <img align="right" src = "man/figures/logo_new.png" width="135px">

# exeval: An R-package for Robust External Evaluation of popPKPD Models.

exeval is a free and open source package that automatize the process of
*external evaluation process* using an independent dataset from which
the original popPKPD model was developed.

Currently, the user can use the package choose to use the package based
on the [mapbayr](https://github.com/FelicienLL/mapbayr) package

You can perform an external evaluation for a single model from :

- a population PKPD model (coded in
  [mrgsolve](https://github.com/metrumresearchgroup/mrgsolve),
- a data set with concentrations (NONMEM format)

## Installation

You can install the development version of exeval from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("Martin-Umpierrez/exeval")
```

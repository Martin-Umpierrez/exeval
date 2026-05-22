
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![R-CMD-check](https://github.com/Martin-Umpierrez/exeval/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Martin-Umpierrez/exeval/actions/workflows/R-CMD-check.yaml)

# <img align="right" src = "man/figures/exeval_logohex.png" width="135px">

# exeval: An R-package for External Evaluation of popPKPD Models.

`exeval` provides a reproducible workflow for *external evaluation* of
population pharmacokinetic (popPK) and pharmacokinetic-pharmacodynamic
(PKPD) models.

`exeval` package provedas a reporducible worflow for *external
evaluation* of a population pharmacokinetic (popPK) andpopPKPD model.
The external evaluation is done using an independent dataset from which
the original popPKPD model was developed. Currently, model fit is based
on [mapbayr](https://github.com/FelicienLL/mapbayr) package.

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

`exeval` provides a reproducible workflow for *external evaluation* of
population pharmacokinetic (popPK) and pharmacokinetic-pharmacodynamic
(PKPD) models.

The package supports model-informed evaluation workflows based on:

- Maximum a posteriori (MAP) estimation via `mapbayr`
- posterior model updating
- a priori and Bayesian forecasting simulations
- predictive performance metrics
- model comparison across candidate models
- graphical diagnostics and fit screening

Models can be supplied as:

- `mrgsolve` model code
- compiled `mrgmod` objects
- built-in curated models included in `exeval`

Input datasets should follow standard pharmacometric event-table
structure (e.g. ID, TIME, DV, AMT, EVID, CMT, OCC), with helper
functions available for data preparation.

## Installation

Install the development version from GitHub:

``` r
install.packages("remotes")
remotes::install_github("Martin-Umpierrez/exeval")
```

## Quick start

``` r
library(exeval)

data("tacrolimus_pk1_kidney")
data("exeval_models")

dd <- tacrolimus_pk1_kidney |> subset(ID < 6)

res <- exeval_ppk(
  model = "TAC_Zuo2013",
  data = dd,
  evaluation_type = "sequential_updating",
  assessment = "Complete"
)

print(res)
plot(res, type = "IF_plot")
summary(res)
```

## Workflow

The main external evaluation workflow is:

``` r
run_MAP_estimations()
→ update_map_models()
→ run_pk_simulations()
→ metrics_occ()
```

or, more simply:

``` r
exeval_ppk(...)
```

## Included datasets

`exeval` includes:

- `exeval_models`: curated built-in PK/PKPD models
- `tacrolimus_pk1_kidney`: external evaluation dataset example

## License

MIT

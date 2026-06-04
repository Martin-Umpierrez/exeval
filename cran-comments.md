## Resubmission

This is a resubmission. In this version we have:

* Removed the hardcoded `set.seed()` from `run_pk_simulations()` and added
  a `seed = NULL` argument instead.
* Fixed the `Title` field to use title case.
* Removed the `Maintainer` field (redundant with `Authors@R`).
* Moved `mapbayr` from `Depends` to `Imports`.
* Replaced `tidyverse` in `Suggests` with specific packages.
* Removed `LazyData: true` as the package has no datasets.
* Removed `Config/roxygen2/version` from DESCRIPTION.


## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.
## Submission notes

This is an update (0.1.2 -> 0.2.0). The behaviour of `sgmean()` is unchanged;
the new version adds functions that build on it:

* Inference for the proportional trimmed mean: `se_trimmed()`, `ci_trimmed()`,
  `ttest_trimmed()`, `ttest_rel_trimmed()`, `ttest_ind_trimmed()` (Yuen-type
  tests returning `htest` objects) and `effect_size_trimmed()`.
* Diagnostics: `diagnose_truncation()` and `plot_trim_profile()`.
* Descriptive statistics: `sg_summary()`, `winsorized_mean()`,
  `winsorized_sigma()`, `sg_quantile()`, `quartiles()`, `iqr()`,
  `geometric_mean()`, `dam()`, `sbi()`, `skewness()`, `kurtosis()`, `cv()`,
  `std_error()`, `sg_mode()`, and `set_language()` for English/Spanish labels
  of the printed tables.
* The package now imports `stats` and `graphics` (base packages).
* The tests include reference values shared with a Python implementation of
  the same methods.

## Test environments

* local Windows 10 x64, R 4.5.3 (ucrt)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.
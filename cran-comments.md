## Submission notes

This is an update (0.1.1 -> 0.1.2) that fixes wrong results of `sgmean()` in
some edge cases, mainly when little data remains after trimming (for example
`sgmean(1:9, trim = 0.45)` returned 27.22 instead of 5; the cause was a range
`a:b` with `a > b`, which counts backwards in R). Results for usual settings
(small `trim`, moderate `n`) are unchanged.

Other changes:

* `na.rm = FALSE` with missing values now returns `NA`, as `mean()` does.
* Invalid `trim` values now give a clear error.
* A testthat suite (including an independent reference implementation) was added.

## Test environments

* local Windows 10 x64, R 4.5.3 (ucrt)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

  This note comes from the local machine, which could not reach a time
  server; it is not related to the package.

## Reverse dependencies

There are no reverse dependencies.


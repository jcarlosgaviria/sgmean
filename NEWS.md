# sgmean 0.1.2

## Bug fixes

* **Wrong values when little data remained after trimming.** For some
  combinations of `n` and `trim` (when `n < 2 * floor(k) + 3`, with
  `k = trim * n`, and for `n = 2` or `3` with `k < 1`) the sum of the
  interior observations used a range `a:b` with `a > b`, which counts
  backwards in R instead of being empty. Examples fixed:
  `sgmean(1:9, 0.45)` returned 27.22 (now 5), `sgmean(1:10, 0.45)` returned
  16.5 (now 5.5), `sgmean(1:2, 0.3)` returned 5.25 (now 1.5). Results for
  usual settings (small `trim`, moderate `n`) are unchanged.
* The weights are now computed as the overlap of each observation with the
  interval `[k, n - k]`, which removes the case analysis and the `:` ranges.
* `na.rm = FALSE` with missing values now returns `NA`, as `mean()` does
  (before, `sort()` silently dropped them). Invalid `trim` (length other than
  one, `NA`) now gives a clear error.

## Other changes

* Added a `testthat` suite, including an independent reference
  implementation and regression tests for the cases above.


# sgmean 0.1.1

## Bug fixes

* **Critical fix**: Corrected the proportional trimmed mean formula for
  cases where `k = trim * n >= 1`. The previous implementation (v0.1.0)
  incorrectly applied a `(1 - k)` discount to `x[1]` and `x[n]` for all
  cases, which is only correct when `k < 1` (Type A distortion). For
  `k >= 1`, this produced incorrect results.

* For exact integer `k >= 2`, `sgmean()` now correctly returns the same
  result as `mean(..., trim)` (as documented and expected).

* For non-integer `k > 1` (Type B distortion), `sgmean()` now correctly
  applies a fractional discount `(1 - delta)` to `x[floor(k) + 1]` and
  `x[n - floor(k)]`, consistent with expression (4) of the companion
  article and verified against Statgraphics output.

* The fix was discovered and verified during peer review of the companion
  article submitted to The R Journal. The corrected implementation
  produces results identical to Statgraphics for all tested cases.

## Verification

* Type A (n=15, trim=0.05, k=0.75): unchanged — 1499.074 (correct in v0.1.0)
* Type B (n=15, trim=0.10, k=1.50): corrected — 1365.833 (vs 1102.083 in v0.1.0)
* Control (k exact integer): corrected — now identical to `mean(..., trim)`

# sgmean 0.1.0

* Initial CRAN release (2026-06-03).
* Implements proportional trimmed mean compatible with Statgraphics.
* Resolves Type A (k < 1) and Type B (k > 1, non-integer) distortions
  of base R's `mean(..., trim)`.

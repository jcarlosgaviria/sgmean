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

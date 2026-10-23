#' Proportional Trimmed Mean
#'
#' Computes a proportional trimmed mean that resolves the integer
#' truncation problem of base R's \code{mean(..., trim)}. When
#' \eqn{k = \text{trim} \times n} is non-integer, a fractional
#' discount \eqn{(1 - \delta)} is applied to boundary observations,
#' where \eqn{\delta = k - \lfloor k \rfloor}.
#'
#' @param x A numeric vector.
#' @param trim Fraction of observations to trim symmetrically from
#'   each end of the sorted data. Must be in \eqn{[0, 0.5)}.
#' @param na.rm Logical. Should missing values be removed before
#'   computation? Default is \code{FALSE}; if \code{FALSE} and \code{x} contains \code{NA}, the result is \code{NA}, as in \code{mean()}.
#'
#' @return A single numeric scalar.
#'
#' @details
#' The proportional trimmed mean generalizes \code{mean(..., trim)}
#' by treating \eqn{k = \text{trim} \times n} as a continuous
#' parameter. Three cases are handled:
#' \describe{
#'   \item{k = 0}{Returns the arithmetic mean.}
#'   \item{0 < k < 1 (Type A)}{Applies a discount \eqn{(1-k)} to
#'     \eqn{X_{(1)}} and \eqn{X_{(n)}}.}
#'   \item{k exact integer}{Reduces to \code{mean(..., trim)},
#'     producing identical results to R base.}
#'   \item{k > 1, non-integer (Type B)}{Eliminates
#'     \eqn{\lfloor k \rfloor} observations from each tail and
#'     applies a fractional discount \eqn{(1-\delta)} to the new
#'     boundary observations, where \eqn{\delta = k - \lfloor k
#'     \rfloor}.}
#' }
#'
#' @references
#' Gaviria Chaverra, J. C. (2026). sgmean: A Proportional Trimmed
#' Mean for R Compatible with Statgraphics. \emph{The R Journal}.
#'
#' @export
#'
#' @examples
#' # Type A distortion: k = 0.05 * 15 = 0.75
#' x <- c(850, 920, 980, 1050, 1120, 1180, 1250,
#'        1320, 1400, 1480, 1550, 1700, 1850, 2100, 8500)
#' mean(x, trim = 0.05)    # 1816.667 — no trimming applied (k* = 0)
#' sgmean(x, trim = 0.05)  # 1499.074 — proportional discount applied
#'
#' # Type B distortion: k = 0.10 * 15 = 1.50
#' mean(x, trim = 0.10)    # 1376.923 — effective trim 6.7%
#' sgmean(x, trim = 0.10)  # 1365.833 — effective trim 10% (Statgraphics)
#'
#' # Control: k = 1/15 * 15 = 1 (exact integer)
#' mean(x, trim = 1/15)    # identical to sgmean
#' sgmean(x, trim = 1/15)  # identical to mean(..., trim)
sgmean <- function(x, trim = 0.05, na.rm = FALSE) {

  if (!is.numeric(x))
    stop("'x' must be a numeric vector.")
  if (!is.numeric(trim) || length(trim) != 1L || is.na(trim) ||
      trim < 0 || trim >= 0.5)
    stop("'trim' must be a single number in [0, 0.5).")
  if (na.rm)
    x <- x[!is.na(x)]
  else if (anyNA(x))
    return(NA_real_)

  n <- length(x)
  if (n == 0L)
    return(NaN)
  x <- sort(x)
  k <- trim * n

  # No trimming requested
  if (k == 0)
    return(mean(x))

  # Exact integer k: identical to mean(x, trim = trim)
  kf <- floor(k)
  if (k == kf)
    return(mean(x[(kf + 1):(n - kf)]))

  # General case (0 < k < 1, or k > 1 not an integer). The weight of the
  # i-th order statistic is the length of [i - 1, i] intersected with
  # [k, n - k]; the weights add up to n - 2k. Computed without ':' ranges,
  # which run backwards in R when the upper end is smaller than the lower
  # one (version 0.1.1 returned wrong values when little data remained
  # after trimming).
  i <- seq_len(n)
  w <- pmax(0, pmin(i, n - k) - pmax(i - 1, k))
  keep <- w > 0
  sum(w[keep] * x[keep]) / (n - 2 * k)
}

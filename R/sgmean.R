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
#'   computation? Default is \code{FALSE}.
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
  if (trim < 0 || trim >= 0.5)
    stop("'trim' must be in [0, 0.5).")
  if (na.rm)
    x <- x[!is.na(x)]

  x     <- sort(x)
  n     <- length(x)
  k     <- trim * n
  kf    <- floor(k)
  delta <- k - kf

  # No trimming requested
  if (k == 0)
    return(mean(x))

  # Type A: 0 < k < 1 — proportional discount on x[1] and x[n]
  if (kf == 0) {
    suma <- (1 - k) * x[1] +
            sum(x[2:(n - 1)]) +
            (1 - k) * x[n]

  # Exact integer k — identical to mean(..., trim)
  } else if (delta == 0) {
    suma <- sum(x[(kf + 1):(n - kf)])

  # Type B: k > 1, non-integer — fractional discount on boundary obs
  } else {
    suma <- (1 - delta) * x[kf + 1] +
            sum(x[(kf + 2):(n - kf - 1)]) +
            (1 - delta) * x[n - kf]
  }

  return(suma / (n - 2 * k))
}

# Winsorized mean and sigma (Statgraphics-compatible) ------------------------

.sg_winsorize <- function(x, trim, na.rm = FALSE) {
  if (!is.numeric(trim) || length(trim) != 1L || is.na(trim) ||
      trim < 0 || trim >= 0.5)
    stop("'trim' must be a single number in [0, 0.5).")
  x <- sort(.sg_clean(x, na.rm))
  n <- length(x)
  k <- floor(n * trim + 1e-9)
  w <- x
  if (k > 0) {
    w[seq_len(k)] <- x[k + 1]
    w[(n - k + 1):n] <- x[n - k]
  }
  list(w = w, k = k, n = n)
}

#' Winsorized mean
#'
#' Mean of \code{x} after replacing \code{floor(trim * n)} observations at each
#' tail: each of the \eqn{k} smallest values is replaced by the \eqn{(k+1)}-th
#' smallest and each of the \eqn{k} largest by the \eqn{(k+1)}-th largest.
#' Statgraphics Centurion was found to behave this way (integer truncation, no
#' fractional treatment) on the data sets tested.
#'
#' @param x A numeric vector.
#' @param trim Fraction replaced in each tail, in \eqn{[0, 0.5)}. Default 0.05.
#' @param na.rm Logical. If \code{FALSE} (default) an \code{NA} is an error.
#' @return A single number.
#' @seealso \code{\link{winsorized_sigma}}, \code{\link{sgmean}}
#' @export
#' @examples
#' x <- c(850, 920, 980, 1050, 1120, 1180, 1250, 1320, 1400, 1480, 1550,
#'        1700, 1850, 2100, 8500)
#' winsorized_mean(x, trim = 0.10)
winsorized_mean <- function(x, trim = 0.05, na.rm = FALSE) {
  mean(.sg_winsorize(x, trim, na.rm)$w)
}

#' Winsorized standard deviation
#'
#' Sample standard deviation of the winsorized sample multiplied by
#' \eqn{\sqrt{(n-1)/(n-2k-1)}}, with \eqn{k = \lfloor trim \cdot n \rfloor},
#' as reported by Statgraphics.
#'
#' @inheritParams winsorized_mean
#' @return A single number.
#' @seealso \code{\link{winsorized_mean}}
#' @export
#' @examples
#' x <- c(850, 920, 980, 1050, 1120, 1180, 1250, 1320, 1400, 1480, 1550,
#'        1700, 1850, 2100, 8500)
#' winsorized_sigma(x, trim = 0.10)
winsorized_sigma <- function(x, trim = 0.05, na.rm = FALSE) {
  s <- .sg_winsorize(x, trim, na.rm)
  if (s$n - 2 * s$k - 1 <= 0)
    stop("not enough observations for this trim.")
  sd(s$w) * sqrt((s$n - 1) / (s$n - 2 * s$k - 1))
}

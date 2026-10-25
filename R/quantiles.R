# Sample quantiles (Hyndman & Fan 1996, types 1-9) ---------------------------

.sg_clean <- function(x, na.rm = FALSE, min_n = 1L, name = "x") {
  x <- .sg_vec(x, na.rm, name)
  if (length(x) < min_n)
    stop(sprintf("at least %d observation(s) required%s", as.integer(min_n),
                 if (na.rm) " after removing NA" else ""), call. = FALSE)
  x
}

.sg_clamp <- function(i, n) min(max(i, 1), n)

.sg_quantile_one <- function(xs, n, p, type) {
  eps <- 4 * .Machine$double.eps
  if (type == 2L) {
    np <- n * p
    r <- round(np)
    if (abs(np - r) < 1e-9 * max(1, np)) {          # n * p integer -> average
      lo <- .sg_clamp(r, n); hi <- .sg_clamp(r + 1, n)
      return(0.5 * (xs[lo] + xs[hi]))
    }
    return(xs[.sg_clamp(ceiling(np), n)])
  }
  if (type %in% c(1L, 3L)) {
    np <- n * p
    j <- floor(np + eps)
    g <- np - j
    if (abs(g) < eps * max(1, np)) g <- 0
    if (type == 1L) {
      idx <- if (g == 0) j else j + 1
      return(xs[.sg_clamp(idx, n)])
    }
    h <- np - 0.5                                    # type 3: nearest even order stat.
    j3 <- floor(h)
    g3 <- h - j3
    idx <- if (g3 == 0 && j3 %% 2 == 0) j3 else j3 + 1
    return(xs[.sg_clamp(idx, n)])
  }
  ab <- switch(as.character(type),
               "4" = c(0, 1), "5" = c(0.5, 0.5), "6" = c(0, 0), "7" = c(1, 1),
               "8" = c(1 / 3, 1 / 3), "9" = c(3 / 8, 3 / 8))
  h <- ab[1] + (n + 1 - ab[1] - ab[2]) * p
  j <- floor(h + eps)
  g <- h - j
  lo <- xs[.sg_clamp(j, n)]
  hi <- xs[.sg_clamp(j + 1, n)]
  lo + g * (hi - lo)
}

#' Sample quantiles with the nine Hyndman-Fan definitions
#'
#' Quantiles of \code{x} using Hyndman and Fan (1996) definition \code{type}
#' (1 to 9, the numbering of \code{\link[stats]{quantile}}). The default
#' \code{type = 2} reproduces the quartiles of the Statgraphics
#' "Summary Statistics" procedure in every case tested; base R and NumPy
#' default to type 7, which gives different values.
#'
#' Position rule for \code{type = 2}: with the sorted sample and a
#' probability \eqn{p}, the position is \eqn{np + 1/2}. If it is an integer the
#' quantile is that order statistic; if it ends in .5 it is the mean of the two
#' neighbouring order statistics; otherwise it is the observation at the
#' nearest position. This equals \eqn{x_{(\lceil np \rceil)}} when \eqn{np} is
#' not an integer and the mean of \eqn{x_{(np)}} and \eqn{x_{(np+1)}} when it
#' is. It shows that Statgraphics behaves consistently with type 2 on the data
#' sets tested; it does not prove which formula the program uses internally.
#'
#' The function is called \code{sg_quantile} so that it does not mask
#' \code{stats::quantile}.
#'
#' @param x A numeric vector.
#' @param probs Probabilities in \eqn{[0, 1]}.
#' @param type Integer from 1 to 9. Default 2.
#' @param na.rm Logical. If \code{FALSE} (default) an \code{NA} in \code{x} is an
#'   error; if \code{TRUE} they are dropped.
#' @return A numeric vector of quantiles, one per element of \code{probs}.
#' @references Hyndman, R. J. and Fan, Y. (1996). Sample quantiles in
#'   statistical packages. \emph{The American Statistician}, 50(4), 361-365.
#' @seealso \code{\link{quartiles}}, \code{\link{iqr}}
#' @export
#' @examples
#' x <- c(2.35, 3.10, 4.72, 5.05, 6.48, 7.91, 8.26, 9.63, 11.07, 12.84, 15.30, 21.75)
#' sg_quantile(x, c(1/6, 5/6))   # 5/6 sextile = 14.07 (Statgraphics)
#' sg_quantile(x, 0.25, type = 7) # base R default, different value
sg_quantile <- function(x, probs = c(0.25, 0.5, 0.75), type = 2L, na.rm = FALSE) {
  if (length(type) != 1L || !(type %in% 1:9))
    stop("'type' must be an integer between 1 and 9.")
  x <- sort(.sg_clean(x, na.rm))
  if (!is.numeric(probs) || anyNA(probs) || any(probs < 0 | probs > 1))
    stop("'probs' must be in [0, 1].")
  n <- length(x)
  vapply(probs, function(p) .sg_quantile_one(x, n, p, as.integer(type)), numeric(1))
}

#' Quartiles (Q1, median, Q3)
#'
#' @inheritParams sg_quantile
#' @return A named numeric vector with \code{Q1}, \code{median} and \code{Q3}.
#'   The default \code{type = 2} matches Statgraphics.
#' @seealso \code{\link{sg_quantile}}
#' @export
#' @examples
#' quartiles(c(3, 5, 7, 8, 12, 13, 14, 18, 21))
quartiles <- function(x, type = 2L, na.rm = FALSE) {
  q <- sg_quantile(x, c(0.25, 0.5, 0.75), type, na.rm)
  names(q) <- c("Q1", "median", "Q3")
  q
}

#' Interquartile range
#'
#' Difference between the third and the first quartile computed with
#' \code{\link{quartiles}} (default \code{type = 2}). Note that
#' \code{stats::IQR} uses type 7 by default.
#'
#' @inheritParams sg_quantile
#' @return A single number.
#' @seealso \code{\link{quartiles}}
#' @export
#' @examples
#' iqr(c(3, 5, 7, 8, 12, 13, 14, 18, 21))
iqr <- function(x, type = 2L, na.rm = FALSE) {
  q <- quartiles(x, type, na.rm)
  unname(q[3] - q[1])
}

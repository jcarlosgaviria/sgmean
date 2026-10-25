# Descriptive statistics compatible with Statgraphics Centurion ---------------

#' @importFrom stats median sd var
NULL

#' Geometric mean
#'
#' \eqn{\exp(\mathrm{mean}(\log x))}; all observations must be strictly positive.
#'
#' @param x A numeric vector.
#' @param na.rm Logical. If \code{FALSE} (default) an \code{NA} is an error.
#' @return A single number.
#' @export
#' @examples
#' geometric_mean(c(1, 2, 4, 8))
geometric_mean <- function(x, na.rm = FALSE) {
  x <- .sg_clean(x, na.rm, 1L)
  if (any(x <= 0))
    stop("geometric mean requires strictly positive data.")
  exp(mean(log(x)))
}

#' DAM: median absolute deviation about the median (not rescaled)
#'
#' Unlike \code{stats::mad}, the result is not multiplied by 1.4826
#' (this is the "DAM" of Statgraphics).
#'
#' @inheritParams geometric_mean
#' @return A single number.
#' @seealso \code{\link{sbi}}
#' @export
#' @examples
#' dam(c(2, 4, 4, 5, 7, 9, 30))
dam <- function(x, na.rm = FALSE) {
  x <- .sg_clean(x, na.rm, 1L)
  median(abs(x - median(x)))
}

#' Tukey biweight scale (Sbi)
#'
#' Square root of the biweight midvariance. Centre = median, scale = unscaled
#' MAD, tuning constant \code{c = 9}. Returns \code{NA} when the MAD is zero
#' (Statgraphics leaves the value blank).
#'
#' @inheritParams geometric_mean
#' @param c Tuning constant. Default 9.
#' @return A single number, or \code{NA_real_}.
#' @seealso \code{\link{dam}}
#' @export
#' @examples
#' sbi(c(2, 4, 4, 5, 7, 9, 30))
sbi <- function(x, c = 9, na.rm = FALSE) {
  x <- .sg_clean(x, na.rm, 1L)
  m <- median(x)
  mad0 <- median(abs(x - m))
  if (mad0 == 0) return(NA_real_)
  u <- (x - m) / (c * mad0)
  inside <- abs(u) < 1
  num <- sum(inside * (x - m)^2 * (1 - u^2)^4)
  den <- abs(sum(inside * (1 - u^2) * (1 - 5 * u^2)))
  sqrt(length(x) * num) / den
}

#' Adjusted Fisher-Pearson skewness (G1)
#'
#' Needs at least 3 observations. With \code{standardized = TRUE} the value is
#' divided by \eqn{\sqrt{6/n}}.
#'
#' @inheritParams geometric_mean
#' @param standardized Logical. Default \code{FALSE}.
#' @return A single number.
#' @seealso \code{\link{kurtosis}}
#' @export
#' @examples
#' skewness(c(2, 4, 4, 5, 7, 9, 30))
skewness <- function(x, standardized = FALSE, na.rm = FALSE) {
  x <- .sg_clean(x, na.rm, 3L)
  n <- length(x)
  z <- (x - mean(x)) / sd(x)
  g1 <- n / ((n - 1) * (n - 2)) * sum(z^3)
  if (standardized) g1 / sqrt(6 / n) else g1
}

#' Adjusted excess kurtosis (G2)
#'
#' Needs at least 4 observations. With \code{standardized = TRUE} the value is
#' divided by \eqn{\sqrt{24/n}}.
#'
#' @inheritParams skewness
#' @return A single number.
#' @seealso \code{\link{skewness}}
#' @export
#' @examples
#' kurtosis(c(2, 4, 4, 5, 7, 9, 30))
kurtosis <- function(x, standardized = FALSE, na.rm = FALSE) {
  x <- .sg_clean(x, na.rm, 4L)
  n <- length(x)
  z <- (x - mean(x)) / sd(x)
  g2 <- n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum(z^4) -
    3 * (n - 1)^2 / ((n - 2) * (n - 3))
  if (standardized) g2 / sqrt(24 / n) else g2
}

#' Coefficient of variation (percent)
#'
#' \eqn{100\, s / \bar{x}}. Needs at least 2 observations.
#'
#' @inheritParams geometric_mean
#' @return A single number (in percent).
#' @export
#' @examples
#' cv(c(2, 4, 4, 5, 7, 9, 30))
cv <- function(x, na.rm = FALSE) {
  x <- .sg_clean(x, na.rm, 2L)
  100 * sd(x) / mean(x)
}

#' Standard error of the mean
#'
#' \eqn{s / \sqrt{n}}. Needs at least 2 observations. For the standard error of
#' the trimmed mean see \code{\link{se_trimmed}}.
#'
#' @inheritParams geometric_mean
#' @return A single number.
#' @export
#' @examples
#' std_error(c(2, 4, 4, 5, 7, 9, 30))
std_error <- function(x, na.rm = FALSE) {
  x <- .sg_clean(x, na.rm, 2L)
  sd(x) / sqrt(length(x))
}

#' Mode (unique most frequent value)
#'
#' Statgraphics shows the mode only when exactly one value is the most
#' frequent (also when it occurs just twice); it is left blank when every value
#' is different or when several values tie. In those cases this function
#' returns \code{NA_real_}. Called \code{sg_mode} so that it does not mask
#' \code{base::mode}.
#'
#' @inheritParams geometric_mean
#' @return A single number or \code{NA_real_}.
#' @export
#' @examples
#' sg_mode(c(1, 2, 2, 3))   # 2
#' sg_mode(c(1, 1, 2, 2))   # NA (tie)
sg_mode <- function(x, na.rm = FALSE) {
  x <- .sg_clean(x, na.rm, 1L)
  u <- unique(x)
  counts <- tabulate(match(x, u), length(u))
  top <- max(counts)
  winners <- u[counts == top]
  if (top == 1L || length(winners) > 1L) return(NA_real_)
  winners
}

.sg_summary_one <- function(x, trim, quartile_type, na.rm) {
  x <- .sg_clean(x, na.rm, 2L)
  n <- length(x)
  q <- quartiles(x, quartile_type)
  s <- sg_quantile(x, c(1 / 6, 5 / 6), quartile_type)
  sb <- sbi(x)
  list(
    count = n,
    mean = mean(x),
    median = median(x),
    mode = sg_mode(x),
    geometric_mean = if (all(x > 0)) geometric_mean(x) else NA_real_,
    trimmed_mean = sgmean(x, trim = trim),
    winsorized_mean = winsorized_mean(x, trim),
    variance = var(x),
    std = sd(x),
    cv = cv(x),
    std_error = std_error(x),
    winsorized_sigma = winsorized_sigma(x, trim),
    dam = dam(x),
    sbi = sb,
    min = min(x),
    max = max(x),
    range = max(x) - min(x),
    q1 = unname(q[1]),
    q3 = unname(q[3]),
    iqr = unname(q[3] - q[1]),
    sextile_1_6 = s[1],
    sextile_5_6 = s[2],
    intersextile_range = s[2] - s[1],
    skewness = if (n >= 3) skewness(x) else NA_real_,
    std_skewness = if (n >= 3) skewness(x, TRUE) else NA_real_,
    kurtosis = if (n >= 4) kurtosis(x) else NA_real_,
    std_kurtosis = if (n >= 4) kurtosis(x, TRUE) else NA_real_,
    sum = sum(x),
    sum_squares = sum(x^2)
  )
}

# Several variables? Return a named list of numeric vectors, else NULL.
.sg_as_columns <- function(x) {
  if (is.data.frame(x)) {
    ok <- vapply(x, is.numeric, logical(1))
    if (any(!ok))
      warning("non-numeric columns skipped: ",
              paste(names(x)[!ok], collapse = ", "), call. = FALSE)
    if (!any(ok)) stop("the data frame has no numeric columns.")
    return(as.list(x[ok]))
  }
  if (is.matrix(x) && ncol(x) > 1L) {
    nm <- colnames(x)
    if (is.null(nm)) nm <- paste0("x", seq_len(ncol(x)))
    out <- lapply(seq_len(ncol(x)), function(j) x[, j])
    names(out) <- nm
    return(out)
  }
  if (is.list(x)) {
    if (is.null(names(x))) names(x) <- paste0("x", seq_along(x))
    return(x)
  }
  NULL
}

#' Statgraphics-style summary statistics
#'
#' All the "Summary Statistics" results of Statgraphics Centurion in one call:
#' 29 values per variable. \code{x} can be a numeric vector (one variable) or a
#' data frame, a named list of columns or a matrix with several columns (one
#' column of results per variable). The function is called \code{sg_summary} so
#' that it does not mask the generic \code{base::summary}.
#'
#' \code{trim} applies to the proportional trimmed mean (\code{\link{sgmean}})
#' and to the winsorized mean and sigma (integer \code{floor(trim * n)}, as
#' Statgraphics does). Skewness needs \eqn{n \ge 3} and kurtosis \eqn{n \ge 4};
#' they are \code{NA} otherwise. The mode and Sbi are \code{NA} when
#' Statgraphics leaves them blank.
#'
#' @param x A numeric vector, data frame, list or matrix.
#' @param trim Trimming fraction in \eqn{[0, 0.5)}. Default 0.05.
#' @param quartile_type Hyndman-Fan type for quartiles and sextiles. Default 2.
#' @param lang Language of the printed labels: \code{"en"}, \code{"es"} or
#'   \code{"auto"}; see \code{\link{set_language}}.
#' @param na.rm Logical. If \code{FALSE} (default) an \code{NA} is an error; if
#'   \code{TRUE} each variable drops its own \code{NA}.
#' @return For one variable an object of class \code{"sg_summary"} (a named list
#'   of 29 numbers that prints as an aligned table). For several variables an
#'   object of class \code{"sg_summary_multi"} (a list of \code{"sg_summary"}),
#'   which can be converted with \code{as.data.frame()}.
#' @seealso \code{\link{set_language}}
#' @export
#' @examples
#' x <- c(850, 920, 980, 1050, 1120, 1180, 1250, 1320, 1400, 1480, 1550,
#'        1700, 1850, 2100, 8500)
#' s <- sg_summary(x, trim = 0.10)
#' s$trimmed_mean
#' s
#' print(s, lang = "es")
#' sg_summary(data.frame(a = x, b = rev(x)))
sg_summary <- function(x, trim = 0.05, quartile_type = 2L, lang = NULL,
                       na.rm = FALSE) {
  if (!is.numeric(trim) || length(trim) != 1L || is.na(trim) ||
      trim < 0 || trim >= 0.5)
    stop("'trim' must be a single number in [0, 0.5).")
  if (!is.null(lang)) .sg_lang(lang)   # validate
  multi <- .sg_as_columns(x)
  if (!is.null(multi)) {
    res <- lapply(multi, function(col)
      sg_summary(col, trim, quartile_type, lang, na.rm))
    return(structure(res, trim = trim, lang = lang, class = "sg_summary_multi"))
  }
  structure(.sg_summary_one(x, trim, quartile_type, na.rm),
            trim = trim, lang = lang, class = "sg_summary")
}

# Internal helpers ---------------------------------------------------------

# Accept a numeric vector or a one-column data.frame; apply na.rm.
.sg_vec <- function(x, na.rm = FALSE, name = "x") {
  if (is.data.frame(x)) {
    if (ncol(x) != 1L)
      stop(sprintf("'%s' has several columns; pass one variable.", name))
    x <- x[[1L]]
  }
  if (!is.numeric(x))
    stop(sprintf("'%s' must be a numeric vector.", name))
  x <- as.numeric(x)
  if (na.rm) x <- x[!is.na(x)]
  else if (anyNA(x))
    stop(sprintf("'%s' contains NA; use na.rm = TRUE to drop them.", name))
  x
}

.sg_check_conf <- function(conf) {
  if (!is.numeric(conf) || length(conf) != 1L || is.na(conf) ||
      conf <= 0 || conf >= 1)
    stop("'conf' must be a single number in (0, 1).")
}

.sg_prepare <- function(x, trim, na.rm = FALSE, name = "x") {
  if (!is.numeric(trim) || length(trim) != 1L || is.na(trim) ||
      trim < 0 || trim >= 0.5)
    stop("'trim' must be a single number in [0, 0.5).")
  x <- .sg_vec(x, na.rm, name)
  if (length(x) == 0L)
    stop(sprintf("'%s' must be non-empty.", name))
  x <- sort(x)
  n <- length(x)
  k <- trim * n
  g <- floor(k + 1e-9)
  delta <- k - g
  if (abs(delta) < 1e-9) delta <- 0
  h <- n - 2 * k
  if (h <= 1)
    stop("not enough observations for this trim (n - 2k must be > 1).")
  if (delta > 0 && 2 * (g + 1) >= n)
    stop("trim too large for this n (need 2 * ceiling(k) < n).")
  if (h < 8)
    warning(sprintf(paste0("very small effective sample size (n - 2k = %s); ",
                           "intervals and p-values may be unreliable"),
                    format(h, digits = 3)), call. = FALSE)
  list(x = x, n = n, g = g, delta = delta, h = h)
}

# Sum of squares of the sample winsorized with g values replaced per tail
.sg_ssw <- function(x, g) {
  n <- length(x)
  y <- x
  if (g > 0) {
    y[seq_len(g)] <- x[g + 1]
    y[(n - g + 1):n] <- x[n - g]
  }
  sum((y - mean(y))^2)
}

.sg_se <- function(p) {
  den <- p$h * (p$h - 1)
  se <- (1 - p$delta) * sqrt(.sg_ssw(p$x, p$g) / den)
  if (p$delta > 0)
    se <- se + p$delta * sqrt(.sg_ssw(p$x, p$g + 1) / den)
  se
}

.sg_pvalue <- function(tstat, df, alternative) {
  switch(alternative,
         two.sided = 2 * stats::pt(abs(tstat), df, lower.tail = FALSE),
         greater = stats::pt(tstat, df, lower.tail = FALSE),
         less = stats::pt(tstat, df))
}

.sg_interval <- function(est, se, df, conf, alternative) {
  if (alternative == "two.sided") {
    q <- stats::qt(0.5 + conf / 2, df)
    c(est - q * se, est + q * se)
  } else {
    q <- stats::qt(conf, df)
    if (alternative == "greater") c(est - q * se, Inf) else c(-Inf, est + q * se)
  }
}

# One-sample core shared by the one-sample and the paired tests
.sg_one_sample <- function(p, trim, mu0, conf, alternative) {
  est <- sgmean(p$x, trim)
  se <- .sg_se(p)
  if (se == 0)
    stop("zero standard error (constant winsorized sample).")
  tstat <- (est - mu0) / se
  df <- p$h - 1
  list(est = est, se = se, tstat = tstat, df = df,
       p.value = .sg_pvalue(tstat, df, alternative),
       ci = structure(.sg_interval(est, se, df, conf, alternative),
                      conf.level = conf))
}

# Exported functions -------------------------------------------------------

#' Standard error of the proportional trimmed mean
#'
#' Standard error of \code{\link{sgmean}} when \eqn{k = trim \times n} is not
#' an integer ("method D").
#'
#' @param x A numeric vector (or a one-column data frame).
#' @param trim Fraction trimmed from each tail, in \eqn{[0, 0.5)}.
#' @param na.rm Logical. Drop missing values? If \code{FALSE} (default) a
#'   missing value is an error (equivalent to \code{nan_policy = "raise"} in the
#'   Python package; \code{TRUE} is \code{nan_policy = "omit"}).
#'
#' @details
#' Let \eqn{g = \lfloor k \rfloor}, \eqn{\delta = k - g} and
#' \eqn{h = n - 2k}. With \eqn{SSW(m)} the sum of squared deviations of the
#' sample winsorized with \eqn{m} values replaced at each tail,
#' \eqn{SE(m) = \sqrt{SSW(m) / (h (h - 1))}} and
#' \deqn{SE = (1 - \delta) SE(g) + \delta SE(g + 1).}
#' When \eqn{k} is an integer this is the classical Tukey-McLaughlin standard
#' error; it is continuous in \code{trim}. For fractional \eqn{k} the
#' interpolation is a \strong{heuristic} supported by simulation, not a proven
#' result. A warning is given when \eqn{n - 2k < 8}.
#'
#' @return A single number.
#' @seealso \code{\link{ci_trimmed}}, \code{\link{ttest_trimmed}}
#' @references
#' Tukey, J. W. and McLaughlin, D. H. (1963). Less vulnerable confidence and
#' significance procedures for location based on a single sample: trimming /
#' winsorization. \emph{Sankhya A}, 25, 331-352.
#' @export
#' @examples
#' x <- c(850, 920, 980, 1050, 1120, 1180, 1250,
#'        1320, 1400, 1480, 1550, 1700, 1850, 2100, 8500)
#' se_trimmed(x, trim = 0.05)
se_trimmed <- function(x, trim = 0.05, na.rm = FALSE) {
  .sg_se(.sg_prepare(x, trim, na.rm))
}

#' Confidence interval for the proportional trimmed mean
#'
#' @inheritParams se_trimmed
#' @param conf Confidence level in (0, 1).
#' @return A named numeric vector with \code{lower} and \code{upper}.
#' @details Uses \eqn{T \pm t_{h-1} SE}, with \eqn{h = n - 2k} and the
#'   standard error of \code{\link{se_trimmed}}.
#' @importFrom stats qt
#' @export
#' @examples
#' x <- c(850, 920, 980, 1050, 1120, 1180, 1250,
#'        1320, 1400, 1480, 1550, 1700, 1850, 2100, 8500)
#' ci_trimmed(x, trim = 0.05)
ci_trimmed <- function(x, trim = 0.05, conf = 0.95, na.rm = FALSE) {
  .sg_check_conf(conf)
  p <- .sg_prepare(x, trim, na.rm)
  est <- sgmean(p$x, trim)
  se <- .sg_se(p)
  ci <- .sg_interval(est, se, p$h - 1, conf, "two.sided")
  c(lower = ci[1], upper = ci[2])
}

#' One-sample t test for the proportional trimmed mean
#'
#' Tests \eqn{H_0}: trimmed mean \eqn{= mu0}, using \code{\link{se_trimmed}}
#' and a t distribution with \eqn{h - 1} degrees of freedom.
#'
#' @inheritParams se_trimmed
#' @param mu0 Hypothesized value.
#' @param conf Confidence level of the interval stored in the result.
#' @param alternative \code{"two.sided"} (default), \code{"less"} or
#'   \code{"greater"}; one-sided tests give a one-sided interval, as
#'   \code{\link[stats]{t.test}}.
#' @return An object of class \code{"htest"}.
#' @importFrom stats pt qt
#' @export
#' @examples
#' x <- c(850, 920, 980, 1050, 1120, 1180, 1250,
#'        1320, 1400, 1480, 1550, 1700, 1850, 2100, 8500)
#' ttest_trimmed(x, mu0 = 1000, trim = 0.05)
#' ttest_trimmed(x, mu0 = 1000, trim = 0.05, alternative = "greater")
ttest_trimmed <- function(x, mu0 = 0, trim = 0.05, conf = 0.95,
                          alternative = c("two.sided", "less", "greater"),
                          na.rm = FALSE) {
  alternative <- match.arg(alternative)
  if (!is.numeric(mu0) || length(mu0) != 1L || is.na(mu0))
    stop("'mu0' must be a single number.")
  .sg_check_conf(conf)
  dname <- paste(deparse(substitute(x)), collapse = " ")
  p <- .sg_prepare(x, trim, na.rm)
  r <- .sg_one_sample(p, trim, mu0, conf, alternative)
  out <- list(
    statistic = c(t = r$tstat),
    parameter = c(df = r$df),
    p.value = r$p.value,
    conf.int = r$ci,
    estimate = c("proportional trimmed mean" = r$est),
    null.value = c("trimmed mean" = mu0),
    stderr = r$se,
    alternative = alternative,
    method = sprintf("One-sample t-test, proportional trimmed mean (trim = %g)",
                     trim),
    data.name = dname)
  class(out) <- "htest"
  out
}

#' Paired t test for the proportional trimmed mean
#'
#' One-sample test on the differences \code{x - y}. It tests whether the
#' trimmed mean of the differences is zero; this is in general \emph{not} the
#' difference of the two trimmed means.
#'
#' @inheritParams ttest_trimmed
#' @param x,y Numeric vectors of the same length (paired observations).
#' @param na.rm Logical. Drop pairs in which either value is missing?
#' @return An object of class \code{"htest"}.
#' @importFrom stats pt qt
#' @export
#' @examples
#' before <- c(12.1, 14.3, 11.8, 15.2, 13.9, 12.7, 16.0, 14.8, 13.1, 12.5)
#' after  <- c(11.2, 13.9, 11.9, 14.1, 12.8, 12.9, 14.7, 13.5, 12.2, 12.0)
#' ttest_rel_trimmed(before, after, trim = 0.1)
ttest_rel_trimmed <- function(x, y, trim = 0.05, conf = 0.95,
                              alternative = c("two.sided", "less", "greater"),
                              na.rm = FALSE) {
  alternative <- match.arg(alternative)
  .sg_check_conf(conf)
  dname <- paste(paste(deparse(substitute(x)), collapse = " "), "and",
                 paste(deparse(substitute(y)), collapse = " "))
  a <- if (is.data.frame(x)) x[[1L]] else x
  b <- if (is.data.frame(y)) y[[1L]] else y
  if (!is.numeric(a) || !is.numeric(b))
    stop("'x' and 'y' must be numeric vectors.")
  if (length(a) != length(b))
    stop("'x' and 'y' must have the same length.")
  d <- as.numeric(a) - as.numeric(b)
  p <- .sg_prepare(d, trim, na.rm, "x - y")
  r <- .sg_one_sample(p, trim, 0, conf, alternative)
  out <- list(
    statistic = c(t = r$tstat),
    parameter = c(df = r$df),
    p.value = r$p.value,
    conf.int = r$ci,
    estimate = c("proportional trimmed mean of the differences" = r$est),
    null.value = c("trimmed mean of the differences" = 0),
    stderr = r$se,
    alternative = alternative,
    method = sprintf("Paired t-test, proportional trimmed mean (trim = %g)",
                     trim),
    data.name = dname)
  class(out) <- "htest"
  out
}

# Winsorized variance interpolated like the SE
.sg_wvar <- function(p) {
  v <- function(m) .sg_ssw(p$x, m) / (p$n - 1)
  (1 - p$delta) * v(p$g) + (if (p$delta > 0) p$delta * v(p$g + 1) else 0)
}

# sd of the winsorized standard normal (0.642 at 20 % trimming)
.sg_akp_constant <- function(trim) {
  if (trim == 0) return(1)
  z <- stats::qnorm(1 - trim)
  sqrt(1 - 2 * trim - 2 * z * stats::dnorm(z) + 2 * trim * z^2)
}

#' Robust standardized difference of trimmed means
#'
#' \eqn{d = c(trim) (T_x - T_y) / S_w} (Algina, Keselman and Penfield, 2005),
#' with \eqn{S_w} the pooled winsorized standard deviation and \eqn{c(trim)} the
#' standard deviation of the winsorized standard normal (0.642 at 20 \%). With
#' \code{trim = 0} it is Cohen's \emph{d} with the pooled standard deviation.
#' For fractional \eqn{k} the winsorized variance is interpolated between
#' \eqn{\lfloor k \rfloor} and \eqn{\lfloor k \rfloor + 1} (a \strong{heuristic},
#' like the standard error).
#'
#' @inheritParams se_trimmed
#' @param x,y Numeric vectors.
#' @return A single number.
#' @references
#' Algina, J., Keselman, H. J. and Penfield, R. D. (2005). An alternative to
#' Cohen's standardized mean difference effect size. \emph{Psychological
#' Methods}, 10, 317-328.
#' @importFrom stats qnorm dnorm
#' @export
#' @examples
#' x <- c(850, 920, 980, 1050, 1120, 1180, 1250,
#'        1320, 1400, 1480, 1550, 1700, 1850, 2100, 8500)
#' y <- c(900, 1010, 1100, 1180, 1230, 1300, 1390, 1450, 1520, 1600, 1680,
#'        1760, 1900, 2050, 2200, 2400, 2650, 3000, 3400, 4100, 5200, 9800)
#' effect_size_trimmed(x, y, trim = 0.1)
effect_size_trimmed <- function(x, y, trim = 0.05, na.rm = FALSE) {
  px <- .sg_prepare(x, trim, na.rm, "x")
  py <- .sg_prepare(y, trim, na.rm, "y")
  v1 <- .sg_wvar(px)
  v2 <- .sg_wvar(py)
  pooled <- ((px$n - 1) * v1 + (py$n - 1) * v2) / (px$n + py$n - 2)
  if (pooled == 0)
    stop("zero pooled winsorized variance.")
  .sg_akp_constant(trim) * (sgmean(px$x, trim) - sgmean(py$x, trim)) /
    sqrt(pooled)
}

#' Two-sample (Yuen-type) test with proportional trimming
#'
#' \eqn{t = (T_x - T_y) / \sqrt{se_x^2 + se_y^2}} with the standard errors of
#' \code{\link{se_trimmed}} and Welch-Satterthwaite degrees of freedom based
#' on \eqn{h - 1}. With integer \eqn{k} in both samples it reduces to Yuen's
#' (1974) test; for fractional \eqn{k} it is a \strong{heuristic} extension
#' supported by simulation.
#'
#' @inheritParams ttest_trimmed
#' @param x,y Numeric vectors.
#' @param trim Fraction trimmed from each tail (same for both samples).
#' @param conf Confidence level of the interval for the difference.
#' @param alternative \code{"two.sided"}, \code{"less"} or \code{"greater"}
#'   (for \eqn{x - y}).
#' @return An object of class \code{"htest"}; it also carries
#'   \code{effect_size} (see \code{\link{effect_size_trimmed}}) when defined.
#' @references
#' Yuen, K. K. (1974). The two-sample trimmed t for unequal population
#' variances. \emph{Biometrika}, 61, 165-170.
#' @importFrom stats pt qt
#' @export
#' @examples
#' x <- c(850, 920, 980, 1050, 1120, 1180, 1250,
#'        1320, 1400, 1480, 1550, 1700, 1850, 2100, 8500)
#' y <- c(900, 1010, 1100, 1180, 1230, 1300, 1390, 1450, 1520, 1600, 1680,
#'        1760, 1900, 2050, 2200, 2400, 2650, 3000, 3400, 4100, 5200, 9800)
#' ttest_ind_trimmed(x, y, trim = 0.05)
ttest_ind_trimmed <- function(x, y, trim = 0.05, conf = 0.95,
                              alternative = c("two.sided", "less", "greater"),
                              na.rm = FALSE) {
  alternative <- match.arg(alternative)
  .sg_check_conf(conf)
  dname <- paste(paste(deparse(substitute(x)), collapse = " "), "and",
                 paste(deparse(substitute(y)), collapse = " "))
  px <- .sg_prepare(x, trim, na.rm, "x")
  py <- .sg_prepare(y, trim, na.rm, "y")
  s1 <- .sg_se(px)
  s2 <- .sg_se(py)
  v <- s1^2 + s2^2
  if (v == 0)
    stop("zero standard error.")
  tx <- sgmean(px$x, trim)
  ty <- sgmean(py$x, trim)
  d <- tx - ty
  se <- sqrt(v)
  df <- v^2 / (s1^4 / (px$h - 1) + s2^4 / (py$h - 1))
  tstat <- d / se
  out <- list(
    statistic = c(t = tstat),
    parameter = c(df = df),
    p.value = .sg_pvalue(tstat, df, alternative),
    conf.int = structure(.sg_interval(d, se, df, conf, alternative),
                         conf.level = conf),
    estimate = c("trimmed mean of x" = tx, "trimmed mean of y" = ty),
    null.value = c("difference in trimmed means" = 0),
    stderr = se,
    alternative = alternative,
    method = sprintf(
      "Yuen-type two-sample test, proportional trimmed means (trim = %g)",
      trim),
    data.name = dname)
  es <- tryCatch(suppressWarnings(effect_size_trimmed(px$x, py$x, trim)),
                 error = function(e) NULL)
  if (!is.null(es)) out$effect_size <- es
  class(out) <- "htest"
  out
}

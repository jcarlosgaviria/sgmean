#' Diagnose integer truncation of the trimmed mean
#'
#' Compares \code{\link{sgmean}} with base R's \code{mean(x, trim = trim)},
#' which silently uses \code{floor(trim * n)} observations per tail.
#'
#' @param x A numeric vector without missing values.
#' @param trim Fraction trimmed from each tail, in \eqn{[0, 0.5)}.
#' @param na.rm Logical. Drop missing values? If \code{FALSE} (default) a
#'   missing value is an error.
#' @return An object of class \code{"sg_truncation"}: a list with \code{n},
#'   \code{trim}, \code{k} (\eqn{trim \times n}), \code{k_floor},
#'   \code{delta}, \code{effective_trim}, \code{proportional},
#'   \code{floor_mean}, \code{difference} (\code{floor_mean - proportional}),
#'   \code{relative_difference} and \code{truncated} (\code{TRUE} when
#'   \eqn{k} is not an integer).
#' @export
#' @examples
#' x <- c(850, 920, 980, 1050, 1120, 1180, 1250,
#'        1320, 1400, 1480, 1550, 1700, 1850, 2100, 8500)
#' diagnose_truncation(x, trim = 0.05)
diagnose_truncation <- function(x, trim = 0.05, na.rm = FALSE) {
  if (!is.numeric(trim) || length(trim) != 1L || is.na(trim) ||
      trim < 0 || trim >= 0.5)
    stop("'trim' must be a single number in [0, 0.5).")
  x <- .sg_vec(x, na.rm)
  if (length(x) == 0L)
    stop("'x' must be non-empty.")
  n <- length(x)
  k <- trim * n
  g <- floor(k)
  delta <- k - g
  if (abs(delta) < 1e-9) delta <- 0
  prop <- sgmean(x, trim)
  fl <- mean(x, trim = trim)
  diff <- fl - prop
  out <- list(n = n, trim = trim, k = k, k_floor = g, delta = delta,
              effective_trim = g / n, proportional = prop, floor_mean = fl,
              difference = diff,
              relative_difference = if (prop != 0) diff / prop else NaN,
              truncated = delta > 0)
  class(out) <- "sg_truncation"
  out
}

#' @export
print.sg_truncation <- function(x, ...) {
  cat("Truncation diagnostic (n = ", x$n, ", trim = ", x$trim, ")\n", sep = "")
  cat("  k = trim * n          : ", format(x$k), "\n", sep = "")
  cat("  floor(k) used by mean(): ", x$k_floor,
      "  (effective trim ", format(round(100 * x$effective_trim, 2)),
      "% per tail)\n", sep = "")
  cat("  proportional (sgmean) : ", format(x$proportional), "\n", sep = "")
  cat("  floor rule (mean)     : ", format(x$floor_mean), "\n", sep = "")
  cat("  difference            : ", format(x$difference), " (",
      format(round(100 * x$relative_difference, 2)), "%)\n", sep = "")
  cat(if (x$truncated) "  -> k is not an integer: mean(x, trim) truncates.\n"
      else "  -> k is an integer: both rules agree.\n")
  invisible(x)
}

#' Plot the trimmed mean against the trim proportion
#'
#' Draws the proportional trimmed mean (smooth) and base R's
#' \code{mean(x, trim = )} (step function, integer truncation) over a grid
#' of trims.
#'
#' @param x A numeric vector (or one-column data frame).
#' @param trim Optional chosen trim, drawn as a vertical dotted line.
#' @param trims Optional grid of trims in \eqn{[0, 0.5)}; default 200 points.
#' @param na.rm Logical. Drop missing values? Default \code{FALSE} (error).
#' @param ... Further arguments passed to \code{\link[graphics]{plot}}.
#' @return Invisibly, a data frame with columns \code{trim},
#'   \code{proportional} and \code{floor_mean}.
#' @importFrom graphics plot lines abline legend
#' @export
#' @examples
#' x <- c(850, 920, 980, 1050, 1120, 1180, 1250,
#'        1320, 1400, 1480, 1550, 1700, 1850, 2100, 8500)
#' plot_trim_profile(x, trim = 0.05)
plot_trim_profile <- function(x, trim = NULL, trims = NULL, na.rm = FALSE, ...) {
  x <- .sg_vec(x, na.rm)
  if (length(x) < 3L)
    stop("'x' needs at least 3 observations.")
  n <- length(x)
  if (is.null(trims)) trims <- seq(0, 0.5, length.out = 201)[-201]
  trims <- trims[trims >= 0 & trims < 0.5 & trims * n < (n - 1) / 2 + 1e-12]
  prop <- vapply(trims, function(t) sgmean(x, t), numeric(1))
  flo <- vapply(trims, function(t) mean(x, trim = t), numeric(1))
  plot(trims, flo, type = "s", col = "#c0504d", lwd = 2,
       ylim = range(prop, flo), xlab = "trim (proportion per tail)",
       ylab = "trimmed mean",
       main = paste0("Trimmed mean vs trim, n = ", n), ...)
  lines(trims, prop, col = "#1f4e79", lwd = 2)
  if (!is.null(trim)) abline(v = trim, lty = 3, col = "grey40")
  legend("topright", bty = "n", lwd = 2, col = c("#c0504d", "#1f4e79"),
         legend = c("floor(k): mean(x, trim)", "proportional: sgmean"))
  invisible(data.frame(trim = trims, proportional = prop, floor_mean = flo))
}

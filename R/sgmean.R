#' Trimmed Mean Compatible with Statgraphics
#'
#' @description
#' Computes the trimmed mean using a proportional discount method on the
#' extremes, replicating the behavior of Statgraphics software.
#' Unlike the built-in mean() with trim, this method applies a
#' weighted reduction to boundary values rather than removing them entirely.
#'
#' @param x A numeric vector. Does not need to be pre-sorted.
#' @param trim Trim fraction between 0 and 0.5 (default 0.05 for 5%).
#'
#' @return A single numeric value with the trimmed mean.
#'
#' @examples
#' x <- c(2, 4, 6, 8, 100)
#' sgmean(x, trim = 0.05)
#' mean(x, trim = 0.05)
#'
#' @export
sgmean <- function(x, trim = 0.05) {
  if (!is.numeric(x)) stop("'x' must be a numeric vector.")
  if (trim < 0 || trim >= 0.5) stop("'trim' must be between 0 and 0.5.")
  x <- sort(x)
  n <- length(x)
  k <- trim * n
  primer_valor <- x[1] * (1 - k)
  ultimo_valor <- x[n] * (1 - k)
  datos_centrales <- x[2:(n - 1)]
  suma_total <- sum(datos_centrales) + primer_valor + ultimo_valor
  nuevo_n <- n - 2 * k
  return(suma_total / nuevo_n)
}

# Printable tables with English / Spanish labels ------------------------------
# Only the labels of the printed table are translated; list names and function
# names are the same in every language. Language priority: the 'lang' argument,
# set_language(), the SGMEAN_LANG environment variable, and finally English.

.sg_env <- new.env(parent = emptyenv())
.sg_env$lang <- NULL

.sg_langs <- c("en", "es", "auto")

.sg_lang <- function(lang) {
  if (!is.character(lang) || length(lang) != 1L || !(tolower(lang) %in% .sg_langs))
    stop("'lang' must be one of \"en\", \"es\", \"auto\".")
  tolower(lang)
}

# key -> c(English, Spanish); "{t}" is replaced by the trimming percentage.
# Non-ASCII characters are written as \u escapes to keep the source portable.
.sg_labels <- list(
  count = c("Count", "Recuento"),
  mean = c("Mean", "Promedio"),
  median = c("Median", "Mediana"),
  mode = c("Mode", "Moda"),
  geometric_mean = c("Geometric Mean", "Media Geom\u00e9trica"),
  trimmed_mean = c("Trimmed Mean {t}%", "Media Recortada {t}%"),
  winsorized_mean = c("Winsorized Mean {t}%", "Media Winsorizada {t}%"),
  variance = c("Variance", "Varianza"),
  std = c("Standard Deviation", "Desviaci\u00f3n Est\u00e1ndar"),
  cv = c("Coefficient of Variation", "Coeficiente de Variaci\u00f3n"),
  std_error = c("Standard Error", "Error Est\u00e1ndar"),
  winsorized_sigma = c("Winsorized Sigma {t}%", "Sigma Winsorizada {t}%"),
  dam = c("DAM (median abs. deviation)", "DAM"),
  sbi = c("Sbi (biweight scale)", "Sbi"),
  min = c("Minimum", "M\u00ednimo"),
  max = c("Maximum", "M\u00e1ximo"),
  range = c("Range", "Rango"),
  q1 = c("Lower Quartile", "Cuartil Inferior"),
  q3 = c("Upper Quartile", "Cuartil Superior"),
  iqr = c("Interquartile Range", "Rango Intercuart\u00edlico"),
  sextile_1_6 = c("1/6 Sextile", "1/6 sextil"),
  sextile_5_6 = c("5/6 Sextile", "5/6 sextil"),
  intersextile_range = c("Intersextile Range", "Rango Intersextil"),
  skewness = c("Skewness", "Sesgo"),
  std_skewness = c("Standardized Skewness", "Sesgo Estandarizado"),
  kurtosis = c("Kurtosis", "Curtosis"),
  std_kurtosis = c("Standardized Kurtosis", "Curtosis Estandarizada"),
  sum = c("Sum", "Suma"),
  sum_squares = c("Sum of Squares", "Suma de Cuadrados")
)

#' Set the default language of printed tables
#'
#' Only the labels of the printed tables (\code{\link{sg_summary}}) are
#' translated; list names and function names do not change. Priority, from
#' highest to lowest: the \code{lang} argument of the printing call,
#' \code{set_language()}, the \code{SGMEAN_LANG} environment variable, and
#' English. \code{"auto"} follows the system locale.
#'
#' @param lang \code{"en"}, \code{"es"}, \code{"auto"} or \code{NULL} to reset.
#' @return Invisibly, the previous setting.
#' @export
#' @examples
#' old <- set_language("es")
#' set_language(old)
set_language <- function(lang) {
  old <- .sg_env$lang
  if (!is.null(lang)) lang <- .sg_lang(lang)
  .sg_env$lang <- lang
  invisible(old)
}

.sg_get_language <- function(lang = NULL) {
  chosen <- lang
  if (is.null(chosen)) chosen <- .sg_env$lang
  if (is.null(chosen)) {
    env <- Sys.getenv("SGMEAN_LANG", "")
    if (nzchar(env)) chosen <- env
  }
  if (is.null(chosen)) chosen <- "en"
  chosen <- .sg_lang(chosen)
  if (chosen == "auto") {
    loc <- tolower(Sys.getlocale("LC_CTYPE"))
    chosen <- if (grepl("^es|spanish", loc)) "es" else "en"
  }
  chosen
}

# Pad by characters (formatC counts bytes, which breaks accented labels)
.sg_pad <- function(x, w, left = TRUE) {
  pad <- strrep(" ", pmax(0L, w - nchar(x)))
  if (left) paste0(x, pad) else paste0(pad, x)
}

.sg_fmt <- function(key, value, lang) {
  if (is.null(value) || length(value) != 1L || is.na(value)) return("")
  if (key == "count") return(as.character(as.integer(value)))
  text <- trimws(formatC(as.numeric(value), digits = 6, format = "g"))
  if (key == "cv") text <- paste0(text, "%")
  if (lang == "es") text <- sub(".", ",", text, fixed = TRUE)
  text
}

.sg_label <- function(key, lang, trim) {
  i <- if (lang == "es") 2L else 1L
  t <- trimws(formatC(trim * 100, digits = 6, format = "g"))
  gsub("{t}", t, .sg_labels[[key]][i], fixed = TRUE)
}

#' @export
format.sg_summary <- function(x, lang = NULL, ...) {
  lang <- .sg_get_language(if (is.null(lang)) attr(x, "lang") else lang)
  trim <- attr(x, "trim")
  keys <- names(x)
  lab <- vapply(keys, .sg_label, "", lang = lang, trim = trim)
  val <- vapply(keys, function(k) .sg_fmt(k, x[[k]], lang), "")
  w1 <- max(nchar(lab)); w2 <- max(nchar(val))
  title <- if (lang == "es") "Resumen Estad\u00edstico (n = %d)"
           else "Summary Statistics (n = %d)"
  title <- sprintf(title, as.integer(x$count))
  c(title, strrep("-", w1 + w2 + 2),
    paste0(.sg_pad(lab, w1), "  ", .sg_pad(val, w2, FALSE)))
}

#' @export
print.sg_summary <- function(x, lang = NULL, ...) {
  cat(format(x, lang = lang), sep = "\n")
  invisible(x)
}

#' @export
format.sg_summary_multi <- function(x, lang = NULL, ...) {
  lang <- .sg_get_language(if (is.null(lang)) attr(x, "lang") else lang)
  trim <- attr(x, "trim")
  nms <- names(x)
  keys <- names(x[[1L]])
  lab <- vapply(keys, .sg_label, "", lang = lang, trim = trim)
  cols <- lapply(nms, function(nm)
    vapply(keys, function(k) .sg_fmt(k, x[[nm]][[k]], lang), ""))
  w0 <- max(nchar(lab))
  ws <- vapply(seq_along(nms), function(j) max(nchar(nms[j]), nchar(cols[[j]])), 1L)
  head <- paste0(strrep(" ", w0),
                 paste0("  ", .sg_pad(nms, ws, FALSE), collapse = ""))
  title <- paste0(if (lang == "es") "Resumen Estad\u00edstico" else "Summary Statistics",
                  " (", length(nms), if (lang == "es") " variables)" else " variables)")
  body <- vapply(seq_along(keys), function(r)
    paste0(.sg_pad(lab[r], w0),
           paste0("  ", .sg_pad(vapply(cols, `[`, "", r), ws, FALSE), collapse = "")),
    "")
  c(title, strrep("-", nchar(head)), head, strrep("-", nchar(head)), body)
}

#' @export
print.sg_summary_multi <- function(x, lang = NULL, ...) {
  cat(format(x, lang = lang), sep = "\n")
  invisible(x)
}

#' @export
as.data.frame.sg_summary <- function(x, ...) {
  data.frame(statistic = names(x), value = unlist(x, use.names = FALSE),
             stringsAsFactors = FALSE)
}

#' @export
as.data.frame.sg_summary_multi <- function(x, ...) {
  m <- vapply(x, function(s) unlist(s, use.names = FALSE), numeric(length(x[[1L]])))
  rownames(m) <- names(x[[1L]])
  as.data.frame(m)
}

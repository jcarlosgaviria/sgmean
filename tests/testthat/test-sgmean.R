# Independent reference: weight of the i-th order statistic = length of
# [i - 1, i] intersected with [k, n - k], divided by n - 2k (explicit loop).
ref_sgmean <- function(x, trim) {
  x <- sort(x); n <- length(x); k <- trim * n
  s <- 0
  for (i in seq_len(n)) {
    lo <- max(i - 1, k); hi <- min(i, n - k)
    if (hi > lo) s <- s + (hi - lo) * x[i]
  }
  s / (n - 2 * k)
}

x15 <- c(850, 920, 980, 1050, 1120, 1180, 1250,
         1320, 1400, 1480, 1550, 1700, 1850, 2100, 8500)

test_that("published examples are reproduced", {
  expect_equal(sgmean(x15, 0.05), 1499.074074, tolerance = 1e-8)
  expect_equal(sgmean(x15, 0.10), 1365.833333, tolerance = 1e-8)
  expect_equal(sgmean(x15, 1/15), mean(x15, trim = 1/15))
})

test_that("regression: little data left after trimming (v0.1.1 bug)", {
  # For 1:n the correct answer is the median, (n + 1) / 2.
  expect_equal(sgmean(1:9, 0.45), 5)
  expect_equal(sgmean(1:9, 0.4999), 5)
  expect_equal(sgmean(1:5, 0.45), 3)
  expect_equal(sgmean(1:10, 0.45), 5.5)
  expect_equal(sgmean(1:3, 0.4), 2)
  expect_equal(sgmean(1:2, 0.3), 1.5)
  expect_equal(sgmean(1:11, 0.45), 6)
  expect_equal(sgmean(7, 0.3), 7)
})

test_that("matches the independent reference over a grid of n and trim", {
  set.seed(1)
  for (n in 1:60) {
    for (tr in c(0, 0.01, 0.05, 0.1, 0.13, 0.2, 0.25, 0.3, 0.35, 0.45, 0.4999)) {
      x <- rnorm(n)
      expect_equal(sgmean(x, tr), ref_sgmean(x, tr), tolerance = 1e-10,
                   info = paste("n =", n, "trim =", tr))
    }
  }
})

test_that("integer k reduces to mean(x, trim)", {
  set.seed(2)
  x <- rnorm(20)
  expect_equal(sgmean(x, 0.1), mean(x, trim = 0.1))
  expect_equal(sgmean(x, 0.25), mean(x, trim = 0.25))
  expect_equal(sgmean(x, 0), mean(x))
})

test_that("order of the input does not matter and result lies in range", {
  x <- c(31, 2, 80, 5, 45, 7, 11, 24, 13, 20)
  expect_equal(sgmean(x, 0.15), sgmean(sort(x), 0.15))
  expect_gte(sgmean(x, 0.35), min(x))
  expect_lte(sgmean(x, 0.35), max(x))
  expect_equal(sgmean(x, 0.15), 18.714285714, tolerance = 1e-8)
})

test_that("missing values and invalid input", {
  expect_true(is.na(sgmean(c(1, NA, 3), 0.1)))
  expect_equal(sgmean(c(1, NA, 3, 4, 100), 0.2, na.rm = TRUE),
               sgmean(c(1, 3, 4, 100), 0.2))
  expect_error(sgmean("a"))
  expect_error(sgmean(1:10, trim = 0.5))
  expect_error(sgmean(1:10, trim = -0.1))
  expect_error(sgmean(1:10, trim = c(0.1, 0.2)))
  expect_true(is.nan(sgmean(numeric(0), 0.1)))
})


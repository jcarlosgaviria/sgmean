x15 <- c(850, 920, 980, 1050, 1120, 1180, 1250, 1320, 1400, 1480, 1550,
         1700, 1850, 2100, 8500)
y22 <- c(900, 1010, 1100, 1180, 1230, 1300, 1390, 1450, 1520, 1600, 1680,
         1760, 1900, 2050, 2200, 2400, 2650, 3000, 3400, 4100, 5200, 9800)

# independent classical Tukey-McLaughlin SE for integer k
classical_se <- function(x, k) {
  x <- sort(x); n <- length(x); y <- x
  if (k > 0) { y[seq_len(k)] <- x[k + 1]; y[(n - k + 1):n] <- x[n - k] }
  h <- n - 2 * k
  sqrt(sum((y - mean(y))^2) / (h * (h - 1)))
}

test_that("se_trimmed equals the classical SE when k is an integer", {
  set.seed(1)
  for (cs in list(c(20, 0.1, 2), c(20, 0.2, 4), c(30, 0.1, 3), c(25, 0.2, 5),
                  c(12, 0, 0))) {
    x <- rnorm(cs[1])
    expect_equal(se_trimmed(x, cs[2]), classical_se(x, cs[3]))
  }
})

test_that("values agree with the Python implementation (sgmean 0.3.0)", {
  expect_equal(se_trimmed(x15, 0.05), 221.49676512313223)
  expect_equal(unname(ci_trimmed(x15, 0.05)),
               c(1018.6064048126743, 1979.541743335474))
  r <- ttest_trimmed(x15, mu0 = 1000, trim = 0.05)
  expect_equal(unname(r$statistic), 2.2531889971243326)
  expect_equal(r$p.value, 0.04291856623834929)
  expect_equal(unname(r$parameter), 12.5)
  expect_equal(se_trimmed(x15, 0.10), 118.43840460512877)
  r2 <- ttest_ind_trimmed(x15, y22, trim = 0.05)
  expect_equal(unname(r2$statistic), -1.6219980625502974)
  expect_equal(r2$p.value, 0.11489226234042599)
  expect_equal(unname(r2$parameter), 31.122261338703513)
  expect_equal(as.numeric(r2$conf.int),
               c(-1347.1807081402821, 153.51067447024832))
  r3 <- ttest_ind_trimmed(x15, y22, trim = 0.10)
  expect_equal(unname(r3$statistic), -2.1269466407836255)
  expect_equal(unname(r3$parameter), 22.57487220078266)
})

test_that("trim = 0 reproduces t.test", {
  r <- ttest_trimmed(x15, mu0 = 1000, trim = 0)
  ref <- t.test(x15, mu = 1000)
  expect_equal(unname(r$statistic), unname(ref$statistic))
  expect_equal(r$p.value, ref$p.value)
  expect_equal(as.numeric(r$conf.int), as.numeric(ref$conf.int))
  r2 <- ttest_ind_trimmed(x15, y22, trim = 0)
  ref2 <- t.test(x15, y22)
  expect_equal(unname(r2$statistic), unname(ref2$statistic))
  expect_equal(unname(r2$parameter), unname(ref2$parameter))
  expect_equal(r2$p.value, ref2$p.value)
})

test_that("SE is continuous in trim", {
  set.seed(2)
  x <- rlnorm(40)
  for (a in c(0.05, 0.1, 0.2))
    expect_equal(se_trimmed(x, a + 1e-7), se_trimmed(x, a - 1e-7),
                 tolerance = 1e-4)
})

test_that("interval and test are consistent", {
  ci <- ci_trimmed(x15, 0.05)
  est <- sgmean(x15, 0.05)
  expect_true(unname(ci[1]) < est && est < unname(ci[2]))
  expect_equal(est - unname(ci[1]), unname(ci[2]) - est)
  expect_equal(ttest_trimmed(x15, mu0 = est, trim = 0.05)$p.value, 1)
  expect_equal(ttest_trimmed(x15, mu0 = unname(ci[2]), trim = 0.05)$p.value,
               0.05)
  expect_s3_class(ttest_trimmed(x15, 1000, 0.05), "htest")
})

test_that("input validation", {
  expect_error(se_trimmed(c(1, 2, 3), 0.5))
  expect_error(se_trimmed(c(1, 2, NA, 4), 0.1))
  expect_error(se_trimmed(c(1, 2), 0.45))
  expect_error(se_trimmed("a"))
  expect_error(ci_trimmed(x15, 0.05, conf = 1.5))
})

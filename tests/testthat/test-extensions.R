x15 <- c(850, 920, 980, 1050, 1120, 1180, 1250, 1320, 1400, 1480, 1550,
         1700, 1850, 2100, 8500)
y22 <- c(900, 1010, 1100, 1180, 1230, 1300, 1390, 1450, 1520, 1600, 1680,
         1760, 1900, 2050, 2200, 2400, 2650, 3000, 3400, 4100, 5200, 9800)
before <- c(12.1, 14.3, 11.8, 15.2, 13.9, 12.7, 16.0, 14.8, 13.1, 12.5)
after  <- c(11.2, 13.9, 11.9, 14.1, 12.8, 12.9, 14.7, 13.5, 12.2, 12.0)

test_that("alternative matches t.test when trim = 0", {
  for (alt in c("two.sided", "less", "greater")) {
    r <- ttest_trimmed(x15, mu0 = 1500, trim = 0, alternative = alt)
    ref <- t.test(x15, mu = 1500, alternative = alt)
    expect_equal(r$p.value, ref$p.value)
    expect_equal(as.numeric(r$conf.int), as.numeric(ref$conf.int))
    expect_equal(r$alternative, alt)
    r2 <- ttest_ind_trimmed(x15, y22, trim = 0, alternative = alt)
    ref2 <- t.test(x15, y22, alternative = alt)
    expect_equal(r2$p.value, ref2$p.value)
    expect_equal(as.numeric(r2$conf.int), as.numeric(ref2$conf.int))
    r3 <- ttest_rel_trimmed(before, after, trim = 0, alternative = alt)
    ref3 <- t.test(before, after, paired = TRUE, alternative = alt)
    expect_equal(r3$p.value, ref3$p.value)
    expect_equal(as.numeric(r3$conf.int), as.numeric(ref3$conf.int))
  }
})

test_that("one-sided p-values add up", {
  p_less <- ttest_trimmed(x15, 1500, 0.1, alternative = "less")$p.value
  p_greater <- ttest_trimmed(x15, 1500, 0.1, alternative = "greater")$p.value
  expect_equal(p_less + p_greater, 1)
  expect_error(ttest_trimmed(x15, 1500, 0.1, alternative = "other"))
})

test_that("paired test is the one-sample test on the differences", {
  d <- before - after
  r <- ttest_rel_trimmed(before, after, trim = 0.1)
  r1 <- ttest_trimmed(d, 0, trim = 0.1)
  expect_equal(unname(r$statistic), unname(r1$statistic))
  expect_equal(r$p.value, r1$p.value)
  expect_equal(as.numeric(r$conf.int), as.numeric(r1$conf.int))
  expect_error(ttest_rel_trimmed(before, after[-1], trim = 0.1))
})

test_that("na.rm drops missing values (pairs for the paired test)", {
  xa <- c(x15, NA)
  expect_error(se_trimmed(xa, 0.1))
  expect_equal(se_trimmed(xa, 0.1, na.rm = TRUE), se_trimmed(x15, 0.1))
  expect_equal(unname(ci_trimmed(xa, 0.1, na.rm = TRUE)),
               unname(ci_trimmed(x15, 0.1)))
  b2 <- c(before, 12); a2 <- c(after, NA)
  expect_error(ttest_rel_trimmed(b2, a2, trim = 0.1))
  r <- ttest_rel_trimmed(b2, a2, trim = 0.1, na.rm = TRUE)
  expect_equal(unname(r$statistic),
               unname(ttest_rel_trimmed(before, after, trim = 0.1)$statistic))
  expect_equal(sgmean(xa, 0.1), NA_real_)
})

test_that("one-column data frames are accepted, several columns are not", {
  expect_equal(se_trimmed(data.frame(v = x15), 0.1), se_trimmed(x15, 0.1))
  expect_error(se_trimmed(data.frame(a = x15, b = x15), 0.1))
})

test_that("effect size equals Cohen's d at trim = 0 and uses the AKP constant", {
  n1 <- length(x15); n2 <- length(y22)
  sp <- sqrt(((n1 - 1) * var(x15) + (n2 - 1) * var(y22)) / (n1 + n2 - 2))
  expect_equal(effect_size_trimmed(x15, y22, trim = 0),
               (mean(x15) - mean(y22)) / sp)
  akp <- if (exists(".sg_akp_constant")) .sg_akp_constant else sgmean:::.sg_akp_constant
  expect_equal(akp(0.2), 0.6419, tolerance = 1e-3)
  r <- ttest_ind_trimmed(x15, y22, trim = 0.1)
  expect_equal(r$effect_size, effect_size_trimmed(x15, y22, trim = 0.1))
})

test_that("warning when the effective sample size is very small", {
  expect_warning(se_trimmed(c(1, 2, 4, 7, 11, 16, 22, 30), 0.1))
})

test_that("diagnose_truncation and plot_trim_profile accept na.rm", {
  xa <- c(x15, NA)
  expect_error(diagnose_truncation(xa, 0.1))
  expect_equal(diagnose_truncation(xa, 0.1, na.rm = TRUE)$proportional,
               diagnose_truncation(x15, 0.1)$proportional)
  expect_error(plot_trim_profile(xa))
})

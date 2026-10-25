x15 <- c(850, 920, 980, 1050, 1120, 1180, 1250, 1320, 1400, 1480, 1550,
         1700, 1850, 2100, 8500)

test_that("diagnose_truncation flags the truncated case", {
  d <- diagnose_truncation(x15, 0.05)           # k = 0.75, floor = 0
  expect_s3_class(d, "sg_truncation")
  expect_true(d$truncated)
  expect_equal(d$k_floor, 0)
  expect_equal(d$floor_mean, mean(x15))
  expect_equal(d$proportional, sgmean(x15, 0.05))
  expect_equal(d$difference, d$floor_mean - d$proportional)
  expect_equal(d$floor_mean, 1816.6666666666667)
  expect_output(print(d), "truncates")
})

test_that("no truncation when k is an integer", {
  set.seed(3)
  d <- diagnose_truncation(rnorm(20), 0.1)
  expect_false(d$truncated)
  expect_equal(d$difference, 0, tolerance = 1e-12)
  expect_output(print(d), "agree")
})

test_that("floor mean matches Python reference at 10%", {
  d <- diagnose_truncation(x15, 0.10)
  expect_equal(d$floor_mean, 1376.923076923077)
  expect_equal(d$proportional, 1365.8333333333333)
})

test_that("plot_trim_profile runs and returns the grid", {
  pdf(NULL)
  on.exit(dev.off())
  out <- plot_trim_profile(x15, trim = 0.05)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("trim", "proportional", "floor_mean") %in% names(out)))
  expect_equal(out$proportional[1], mean(x15))
})

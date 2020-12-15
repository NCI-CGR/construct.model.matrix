context("Inverse normal transform on continuous traits")
library(construct.model.matrix)

test_that("Transformation results are normal by Shapiro-Wilk", {
  x <- runif(1000)
  x <- construct.model.matrix::inverse.normalize(x)
  expect_true(shapiro.test(x)$p.value >= 0.1)
})

test_that("Transformation not performed on wrong-type vector", {
  x <- factor(rep(c("x", "y"), each = 500))
  expect_error(construct.model.matrix::inverse.normalize(x))
})

test_that("Transformation works on data frame", {
  df <- data.frame(
    var = runif(1000),
    sex = sample(0:1, 1000, TRUE)
  )
  res <- construct.model.matrix::apply.inverse.normalization(
    df,
    list(c(TRUE, FALSE))
  )
  expect_true(shapiro.test(res[, 1])$p.value >= 0.1)
  expect_true(shapiro.test(res[res[, 2] == 0, 1])$p.value >= 0.1)
  expect_true(shapiro.test(res[res[, 2] == 1, 1])$p.value >= 0.1)
})

test_that("Transformation requires 'sex' variable for stratification", {
  df <- data.frame(var = runif(1000))
  expect_error(construct.model.matrix::apply.inverse.normalization(
    df,
    list(c(TRUE))
  ))
})

test_that("Transformation respects variable inclusion criteria", {
  df <- data.frame(
    var1 = runif(1000),
    var2 = runif(1000),
    sex = sample(0:1, 1000, TRUE)
  )
  target <- shapiro.test(df[, 2])$p.value
  res <- construct.model.matrix::apply.inverse.normalization(
    df,
    list(c(TRUE, FALSE, FALSE))
  )
  expect_true(shapiro.test(res[, 1])$p.value >= 0.1)
  expect_equal(shapiro.test(res[, 2])$p.value, target)
})

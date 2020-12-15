context("Filtering controls based on inclusion/exclusion criteria")
library(construct.model.matrix)

test_that("Filtering includes included controls", {
  inclusion.list <- list(
    list(
      var.name = "var1",
      var.levels = c(1, 2, 3)
    ),
    list(
      var.name = "var2",
      var.levels = c(1, 4)
    )
  )
  df <- data.frame(
    pheno = sample(0:1, 1000, TRUE),
    var3 = rnorm(1000),
    var2 = sample(0:4, 1000, TRUE),
    var1 = sample(1:10, 1000, TRUE)
  )
  target <- df[df$pheno == 1 |
    (df$pheno == 0 &
      (df$var1 == 1 | df$var1 == 2 | df$var1 == 3) &
      (df$var2 == 1 | df$var2 == 4)), ]
  res <- construct.model.matrix::apply.inc.exc(
    df,
    "pheno",
    TRUE,
    inclusion.list,
    list()
  )
  expect_identical(res, target)
})

test_that("Filtering excludes excluded controls", {
  exclusion.list <- list(
    list(
      var.name = "var1",
      var.levels = c(1, 2, 3)
    ),
    list(
      var.name = "var2",
      var.levels = c(1, 4)
    )
  )
  df <- data.frame(
    pheno = sample(0:1, 1000, TRUE),
    var3 = rnorm(1000),
    var2 = sample(0:4, 1000, TRUE),
    var1 = sample(1:10, 1000, TRUE)
  )
  target <- df[df$pheno == 1 |
    (df$pheno == 0 &
      (df$var1 != 1 & df$var1 != 2 & df$var1 != 3) &
      (df$var2 != 1 & df$var2 != 4)), ]
  res <- construct.model.matrix::apply.inc.exc(
    df,
    "pheno",
    TRUE,
    list(),
    exclusion.list
  )
  expect_identical(res, target)
})

test_that("Including and excluding works simultaneously", {
  inclusion.list <- list(list(
    var.name = "var1",
    var.levels = c(1, 2, 3)
  ))
  exclusion.list <- list(list(
    var.name = "var2",
    var.levels = c(1, 4)
  ))
  df <- data.frame(
    pheno = sample(0:1, 1000, TRUE),
    var3 = rnorm(1000),
    var2 = sample(0:4, 1000, TRUE),
    var1 = sample(1:10, 1000, TRUE)
  )
  target <- df[df$pheno == 1 |
    (df$pheno == 0 &
      (df$var1 == 1 | df$var1 == 2 | df$var1 == 3) &
      (df$var2 != 1 & df$var2 != 4)), ]
  res <- construct.model.matrix::apply.inc.exc(
    df,
    "pheno",
    TRUE,
    inclusion.list,
    exclusion.list
  )
  expect_identical(res, target)
})

test_that("Including and excluding a control removes the control", {
  inclusion.list <- list(list(
    var.name = "var2",
    var.levels = c(1, 2, 3)
  ))
  exclusion.list <- list(list(
    var.name = "var2",
    var.levels = c(1, 4)
  ))
  df <- data.frame(
    pheno = sample(0:1, 1000, TRUE),
    var3 = rnorm(1000),
    var2 = sample(0:4, 1000, TRUE),
    var1 = sample(1:10, 1000, TRUE)
  )
  target <- df[df$pheno == 1 |
    (df$pheno == 0 &
      (df$var2 == 1 | df$var2 == 2 | df$var2 == 3) &
      (df$var2 != 1 & df$var2 != 4)), ]
  res <- construct.model.matrix::apply.inc.exc(
    df,
    "pheno",
    TRUE,
    inclusion.list,
    exclusion.list
  )
  expect_identical(res, target)
})

test_that("filtering respects traits flagged as non-binary", {
  df <- data.frame(
    pheno = rnorm(1000),
    var3 = rnorm(1000),
    var2 = sample(0:4, 1000, TRUE),
    var1 = sample(1:10, 1000, TRUE)
  )
  inclusion.list <- list(list(
    var.name = "var1",
    var.levels = c(1, 2, 3)
  ))
  exclusion.list <- list(list(
    var.name = "var2",
    var.levels = c(1, 4)
  ))
  target <- df
  res <- construct.model.matrix::apply.inc.exc(
    df,
    "pheno",
    FALSE,
    inclusion.list,
    exclusion.list
  )
  expect_identical(res, target)
})

test_that("missing phenotype is detected correctly", {
  df <- data.frame(
    pheno = rnorm(1000),
    var3 = rnorm(1000),
    var2 = sample(0:4, 1000, TRUE),
    var1 = sample(1:10, 1000, TRUE)
  )
  inclusion.list <- list(list(
    var.name = "var1",
    var.levels = c(1, 2, 3)
  ))
  exclusion.list <- list(list(
    var.name = "var2",
    var.levels = c(1, 4)
  ))
  target <- df
  expect_error(construct.model.matrix::apply.inc.exc(
    df,
    "not.pheno",
    FALSE,
    inclusion.list,
    exclusion.list
  ))
})

context("Handling of categorical data")
library(construct.model.matrix)

test_that("Categorical reference level merging works", {
  df <- data.frame(
    fake = rnorm(1000),
    pheno = sample(0:4, 1000, TRUE),
    fake2 = rnorm(1000)
  )
  target <- df
  target <- target[target$pheno == 0 |
    target$pheno == 1 |
    target$pheno == 2, ]
  target$pheno[target$pheno == 0 |
    target$pheno == 2] <- 0
  ## unnecessary but included for completeness
  target$pheno[target$pheno == 1] <- 1
  expect_identical(
    construct.model.matrix::extract.categories(
      df,
      "pheno",
      "resources/example4.categories"
    ),
    target
  )
})

test_that("Categorical comparison level merging works", {
  df <- data.frame(
    fake = rnorm(1000),
    pheno = sample(0:4, 1000, TRUE),
    fake2 = rnorm(1000)
  )
  target <- df
  target <- target[target$pheno == 2 |
    target$pheno == 3 |
    target$pheno == 4, ]
  target$pheno[target$pheno == 2] <- 0
  target$pheno[target$pheno == 3 |
    target$pheno == 4] <- 1
  expect_identical(
    construct.model.matrix::extract.categories(
      df,
      "pheno",
      "resources/example5.categories"
    ),
    target
  )
})

test_that("Reference and comparison level merging work simultaneously", {
  df <- data.frame(
    fake = rnorm(1000),
    pheno = sample(0:4, 1000, TRUE),
    fake2 = rnorm(1000)
  )
  target <- df
  target <- target[target$pheno == 1 |
    target$pheno == 2 |
    target$pheno == 3 |
    target$pheno == 4, ]
  target$pheno[target$pheno == 1 |
    target$pheno == 3] <- 0
  target$pheno[target$pheno == 2 |
    target$pheno == 4] <- 1
  expect_identical(
    construct.model.matrix::extract.categories(
      df,
      "pheno",
      "resources/example6.categories"
    ),
    target
  )
})

test_that("Old style categories trackers are handled gracefully", {
  df <- data.frame(
    fake = rnorm(1000),
    pheno = sample(0:4, 1000, TRUE),
    fake2 = rnorm(1000)
  )
  target <- df
  target <- target[target$pheno == 0 |
    target$pheno == 1 |
    target$pheno == 2, ]
  target$pheno[target$pheno == 0] <- 0
  target$pheno[target$pheno == 1 |
    target$pheno == 2] <- 1
  expect_identical(
    construct.model.matrix::extract.categories(
      df,
      "pheno",
      "resources/example7.categories"
    ),
    target
  )
})

test_that("Missing category tracker filenames are flagged correctly", {
  df <- data.frame(
    fake = rnorm(1000),
    pheno = sample(0:4, 1000, TRUE),
    fake2 = rnorm(1000)
  )
  expect_error(construct.model.matrix::extract.categories(
    df,
    "pheno",
    "resources/DOESNTEXIST.xlsx"
  ))
})

test_that("Dummy variables are generated for a single variable", {
  df <- data.frame(
    fake = rnorm(1000),
    pheno = factor(as.vector(sample(0:5, 1000, TRUE), mode = "character")),
    fake2 = rnorm(1000)
  )
  target <- df
  reference.level <- as.numeric(names(sort(table(df$pheno),
    decreasing = TRUE
  ))[1])
  alt.levels <- as.numeric(names(sort(table(df$pheno),
    decreasing = TRUE
  ))[-1])
  for (i in alt.levels) {
    var.name <- paste("pheno.ref", reference.level, i, sep = ".")
    target[, var.name] <- 0
    target[, var.name][target$pheno == i] <- 1
  }
  target$pheno <- NULL

  res <- construct.model.matrix::binarize(
    df,
    1,
    list(
      c(FALSE, TRUE, FALSE),
      c(FALSE, FALSE, FALSE)
    )
  )
  expect_identical(res, target)
})

test_that("Dummies are generated for multiple variables simultaneously", {
  df <- data.frame(
    fake = rnorm(1000),
    pheno = factor(as.vector(sample(0:5, 1000, TRUE), mode = "character")),
    altpheno = factor(as.vector(sample(0:10, 1000, TRUE), mode = "character")),
    fake2 = rnorm(1000)
  )
  target <- df
  reference.level <- as.numeric(names(sort(table(df$pheno),
    decreasing = TRUE
  ))[1])
  alt.levels <- as.numeric(names(sort(table(df$pheno),
    decreasing = TRUE
  ))[-1])
  for (i in alt.levels) {
    if (i == reference.level) next
    var.name <- paste("pheno.ref", reference.level, i, sep = ".")
    target[, var.name] <- 0
    target[, var.name][target$pheno == i] <- 1
  }
  target$pheno <- NULL
  reference.level <- as.numeric(names(sort(table(df$altpheno),
    decreasing = TRUE
  ))[1])
  alt.levels <- as.numeric(names(sort(table(df$altpheno),
    decreasing = TRUE
  ))[-1])
  for (i in alt.levels) {
    var.name <- paste("altpheno.ref", reference.level, i, sep = ".")
    target[, var.name] <- 0
    target[, var.name][target$altpheno == i] <- 1
  }
  target$altpheno <- NULL
  res <- construct.model.matrix::binarize(
    df,
    1,
    list(
      c(FALSE, TRUE, FALSE, FALSE),
      c(FALSE, FALSE, TRUE, FALSE)
    )
  )
  expect_identical(res, target)
})

test_that("Nothing breaks if no variables are requested for binarization", {
  df <- data.frame(
    var1 = rnorm(1000),
    var2 = rnorm(1000),
    var3 = rnorm(1000)
  )
  res <- construct.model.matrix::binarize(
    df,
    1,
    list(
      c(FALSE, FALSE, FALSE),
      c(FALSE, FALSE, FALSE),
      c(FALSE, FALSE, FALSE)
    )
  )
  expect_identical(df, res)
})

test_that("Factor levels with counts below threshold are merged", {
  df <- data.frame(var = c(
    rep(10, 200),
    rep(5, 10),
    rep(4, 6),
    rep(20, 255)
  ))
  target <- data.frame(
    var.ref.20.10 = c(rep(1, 200), rep(0, 271)),
    var.ref.20.combined.other = c(
      rep(0, 200),
      rep(1, 16),
      rep(0, 255)
    )
  )
  res <- construct.model.matrix::binarize(
    df,
    20,
    list(c(TRUE))
  )
  expect_identical(res, target)
})

test_that("Dummy variable ordering is lexicographically sorted", {
  ## factor levels are sorted numerically, which doesn't
  ## match the underlying handler. the binary variables should
  ## be swapped
  df <- data.frame(
    fake = rnorm(1000),
    pheno = factor(rep(c(0, 1, 2, 10), each = 250)),
    fake2 = rnorm(1000)
  )
  target <- df
  reference.level <- as.numeric(names(sort(table(df$pheno),
    decreasing = TRUE
  ))[1])
  alt.levels <- as.numeric(names(sort(table(df$pheno),
    decreasing = TRUE
  ))[-1])
  for (i in alt.levels) {
    var.name <- paste("pheno.ref", reference.level, i, sep = ".")
    target[, var.name] <- 0
    target[, var.name][target$pheno == i] <- 1
  }
  target$pheno <- NULL

  res <- construct.model.matrix::binarize(
    df,
    1,
    list(
      c(FALSE, TRUE, FALSE),
      c(FALSE, FALSE, FALSE)
    )
  )
  ## expect the order is wrong
  expect_false(identical(res, target))
  ## now predict the correct order
  expect_identical(
    res,
    target[, c(1, 2, 3, 5, 4)]
  )
})

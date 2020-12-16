context("Load control inclusion/exclusion tracker files")
library(construct.model.matrix)

test_that("Loading two-column data works", {
  filename <- "resources/example1.control-inclusion"
  target <- list(
    list(
      var.name = "var1",
      var.levels = c("1")
    ),
    list(
      var.name = "var2",
      var.levels = c("2")
    ),
    list(
      var.name = "var3",
      var.levels = c("XY")
    )
  )
  res <- construct.model.matrix::load.inc.exc(filename)
  expect_identical(res, target)
})

test_that("Loading one-column data works", {
  filename <- "resources/example2.control-exclusion"
  target <- list(
    list(
      var.name = "var1",
      var.levels = c()
    ),
    list(
      var.name = "var2",
      var.levels = c()
    )
  )
  res <- construct.model.matrix::load.inc.exc(filename)
  expect_identical(res, target)
})

test_that("Loading two-column data with delimited levels works", {
  filename <- "resources/example3.control-inclusion"
  target <- list(
    list(
      var.name = "var1",
      var.levels = c("1", "2", "3")
    ),
    list(
      var.name = "var2",
      var.levels = c("X", "Y", "Z")
    )
  )
  res <- construct.model.matrix::load.inc.exc(filename)
  expect_identical(res, target)
})

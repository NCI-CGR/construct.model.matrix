context("End-to-end model matrix construction regression tests sorta")
library(construct.model.matrix)

test_that("Model matrix construction works for continuous trait", {
  ## seems a bit presumptuous but still
  skip_on_cran()
  install.prefix <- "/CGF/GWAS/Scans/PLCO/builds/1/plco-analysis"
  result.dir <- paste(install.prefix, "results", sep = "/")
  phenotype.name <- "bq_bmi_curr_co"
  ancestry <- "European"
  software <- "boltlmm"
  chip <- "GSA_batch1"
  phenotype.filename <- paste(install.prefix,
    "phenotypes/v10/atlas_v10.with_na.augmented.02nov2020.tsv",
    sep = "/"
  )
  result.prefix <- paste(result.dir,
    phenotype.name,
    ancestry,
    toupper(software),
    paste(phenotype.name,
      chip,
      software,
      sep = "."
    ),
    sep = "/"
  )
  chip.samplefile <- paste(result.prefix,
    "samples",
    sep = "."
  )
  covariate.list.csv <- paste("bq_age_co",
    "center",
    paste("batch",
      c("GSA", "Oncoarray", "OmniX", "Omni25"),
      sep = "."
    ),
    "is.other.asian",
    paste("PC", 1:10, sep = ""),
    sep = ","
  )
  output.filename <- tempfile(
    "tmp.testthat.mmc.cont",
    ".", ".tsv"
  )
  category.filename <- NA
  transformation <- "none"
  sex.specific <- "combined"
  control.inclusion.filename <- paste(result.prefix,
    "control-inclusion",
    sep = "."
  )
  control.exclusion.filename <- paste(result.prefix,
    "control-exclusion",
    sep = "."
  )
  previous.results <- paste(result.prefix,
    "model_matrix",
    sep = "."
  )
  ## the above files are for an actual installation of my current workspace.
  ## this is likely not available for most use cases. test if it's available
  ## and skip the tests if not
  if (!file.exists(phenotype.filename) |
    !file.exists(chip.samplefile) |
    !file.exists(control.inclusion.filename) |
    !file.exists(control.exclusion.filename) |
    !file.exists(previous.results)) {
    skip("End-to-end continuous trait files unavailable")
  }
  ## run the model matrix constructor
  construct.model.matrix::construct.model.matrix(
    phenotype.filename,
    chip.samplefile,
    ancestry,
    chip,
    phenotype.name,
    covariate.list.csv,
    output.filename,
    category.filename,
    transformation,
    sex.specific,
    control.inclusion.filename,
    control.exclusion.filename
  )
  ## load the data from file
  res <- read.table(output.filename,
    header = TRUE,
    sep = "\t", stringsAsFactors = FALSE
  )
  ## remove the current run version for privacy reasons
  file.remove(output.filename)
  ## load the previous result from file
  target <- read.table(previous.results,
    header = TRUE,
    sep = "\t", stringsAsFactors = FALSE
  )
  ## now test identity
  expect_identical(
    res[, colnames(res) != phenotype.name],
    target[, colnames(target) != phenotype.name]
  )
  ## exclusively for continuous traits, `ties.method = "random"`
  ## is in effect; allow for this, approximately
  expect_equal(res[, phenotype.name],
    target[, phenotype.name],
    tolerance = 0.0075
  )
})

test_that("Model matrix construction works for binary trait", {
  skip("Not yet implemented")
})

test_that("Model matrix construction works for categorical trait", {
  skip("Not yet implemented")
})

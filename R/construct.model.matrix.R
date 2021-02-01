#' Given configuration and phenotype data
#' create a model matrix.
#'
#' Given a series of configuration options, and a phenotype
#' dataset from a source like IMS, extract values relevant
#' to a regression model. Check the values for consistency in
#' various ways. Pull in principal components. Apply transformations
#' as needed. Subset data to match the specified analysis set.
#' Report the data, for use downstream directly or in part by
#' tools such as SAIGE, BOLT, PLINK, etc.
#'
#' @param phenotype.filename character vector, a filename
#' of a phenotype dataset (such as the v10 with_na phenotypes
#' from IMS)
#' @param chip.samplefile character vector, a filename
#' of a sample list, one sample ID per line. for reasons
#' inexplicable to me at this time, the sample file
#' format is actually `UNIQUEID_UNIQUEID` which is then
#' parsed out into the single `UNIQUEID` instance. I think
#' this is an artifact of a truly ancient version of the data,
#' and is a candidate for removal for parsimony, and also
#' to allow easier use of the pipeline on IDs with "_" in them
#' @param ancestry character vector, the ancestry of the
#' requested analysis. expected to be a GRAF-style ancestry
#' name: "African", "African_American", "East_Asian",
#' "European", "Hispanic1", "Hispanic2",
#' "Other_Asian_or_Pacific_Islander", "Other",
#' "South_Asian". note the underscore
#' in these ancestries that replaces the thoroughly inconvenient
#' whitespace in the raw GRAF ancestry names
#' @param chip character vector, the name of the platform
#' being analyzed. in practice, this ia really imputation
#' batch: for PLCO, "GSA_batch1" is valid, "GSA" is not
#' @param phenotype.name character vector, variable name of
#' target phenotype in `phenotype.filename`
#' @param covariate.list.csv character vector, a comma-delimited
#' list of covariate variable names from `phenotype.filename`,
#' or the string "NA"
#' @param output.filename character vector, the name
#' of the file to which the final model matrix will be written
#' @param category.filename character vector, the name of the
#' file containing reference and comparison category labels for
#' binary and categorical trait analysis, or NA.
#' if a file, the format is, one per line, a category from
#' the phenotype variable, and the string "reference" or
#' "comparison", separated by a tab. levels with the same
#' "reference" or "comparison" annotation will be merged into
#' a single synthetic binary phenotype in the final output matrix
#' @param transformation character vector, the type of
#' transformation to apply to the phenotype. currently accepted
#' values are "none", or "post.split.INT" for an inverse
#' normal transform after dataset partitioning. this is not
#' currently used by any analyses, and is merely a placeholder
#' for later implementations. continuous traits are always
#' inverse normal transformed. in fact, the level "none"
#' should be renamed to "default", I'll add this to the to-do list
#' @param sex.specific character vector, which type of sex-specific
#' analysis is requested for this model matrix. depending on the
#' value, the final model matrix will be subset by the phenotype
#' dataset's "sex" variable to include only the requested subjects.
#' recognized values are: "combined", "female", "male"
#' @param control.inclusion.filename character vector, the
#' name of the file containing control inclusion restrictions
#' in terms of phenotype dataset variables and optionally categories
#' within those variables; or NA. format for this
#' file is: per row, a variable name, and optionally
#' a comma-delimited list of variable categories denoting
#' valid controls. for backwards compatibility, a variant
#' of this file only containing the first column is permitted,
#' in which case all non-zero levels of the variable will be
#' considered inclusion levels. this is only applied to binary traits.
#' @param control.exclusion.filename character vector, the
#' name of the file containing control exclusion restrictions
#' in terms of phenotype dataset variables and optionally categories
#' within those variables; or NA. format for this
#' file is: per row, a variable name, and optionally
#' a comma-delimited list of variable categories denoting
#' invalid controls. for backwards compatibility, a variant
#' of this file only containing the first column is permitted,
#' in which case all non-zero levels of the variable will be
#' considered exclusion levels. this is only applied to binary traits.
#' @param cleaned.chip.dir character vector, the path to and name
#' of top-level output for the cleaned-chips-by-ancestry pipeline
#' @param ancestry.prefix character vector, the path to and name
#' of top-level output for the ancestry pipeline. note that this
#' is assumed to have a trailing "/" if appropriate, to allow some
#' filename prefix hackjob nonsense
#' @param phenotype.id.colname character vector, the name of the ID
#' column in the provided phenotype file; defaults to "plco_id"
#' @param supported.chips character vector, the names of supported
#' platforms in the current study; defaults to the four PLCO
#' chips with non-redundant subjects
#' @export
construct.model.matrix <- function(phenotype.filename,
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
                                   control.exclusion.filename,
                                   cleaned.chip.dir,
                                   ancestry.prefix,
                                   phenotype.id.colname = "plco_id",
                                   supported.chips = c(
                                     "GSA",
                                     "Oncoarray",
                                     "OmniX",
                                     "Omni25"
                                   )) {
  chip.nosubsets <- strsplit(chip, "_")[[1]][1]
  covariate.list <- unlist(strsplit(covariate.list.csv, ","))
  covariate.list <- unique(covariate.list[covariate.list != "NA"])

  ## do some mild error checking on the transformation and
  ##    sex.specific freetext options
  stopifnot(transformation == "none" |
    transformation == "post.split.INT")
  stopifnot(sex.specific == "female" |
    sex.specific == "male" |
    sex.specific == "combined")

  ## both control inclusion and control exclusion should be
  ##    comma-delimited lists of dataset variable names, or NA
  control.inclusion.list <-
    construct.model.matrix:::load.inc.exc(control.inclusion.filename)
  control.exclusion.list <-
    construct.model.matrix:::load.inc.exc(control.exclusion.filename)

  id.colname <- phenotype.id.colname
  possible.pcs <- paste("PC", 1:250, sep = "")

  ## try to read phenotype data
  h <- data.table::fread(
    file = phenotype.filename,
    sep = "\t",
    header = TRUE,
    data.table = FALSE
  )
  stopifnot(length(unique(colnames(h))) == length(colnames(h)))

  ## enforce IDs present once
  stopifnot(length(which(colnames(h) == id.colname)) == 1)

  ## enforce phenotype present once
  stopifnot(length(which(colnames(h) == phenotype.name)) == 1)

  ## enforce requested covariates
  stopifnot(length(covariate.list) == 0 |
    length(which(covariate.list %in% c(
      colnames(h),
      possible.pcs
    ))) == length(covariate.list))
  ## enforce control inclusion variables present
  stopifnot(length(which(unname(unlist(lapply(
    control.inclusion.list,
    function(i) {
      i[["var.name"]]
    }
  ))) %in% colnames(h))) ==
    length(control.inclusion.list))
  ## enforce control exclusion variables present
  stopifnot(length(which(unname(unlist(lapply(
    control.exclusion.list,
    function(i) {
      i[["var.name"]]
    }
  ))) %in% colnames(h))) ==
    length(control.exclusion.list))

  ## try to hack diagnose phenotype distribution for mild consistency checking
  unique.outcomes <- unique(h[, phenotype.name][!is.na(h[, phenotype.name])])
  trait.is.binary <- length(unique.outcomes) == 2 &
    length(c(0, 1) %in% unique.outcomes) == 2

  ## apply control filtering, if the trait is binary
  ##   and the configuration requests it
  h <- construct.model.matrix:::apply.inc.exc(
    h,
    phenotype.name,
    trait.is.binary,
    control.inclusion.list,
    control.exclusion.list
  )

  # load PCs for ANC/CHIP combo
  pc.filename <- paste(cleaned.chip.dir, "/",
    ancestry, "/", chip.nosubsets, ".step7.evec",
    sep = ""
  )
  pc.data <- data.frame()
  if (file.exists(pc.filename)) {
    pc.data <- read.table(pc.filename,
      header = FALSE,
      skip = 1,
      stringsAsFactors = FALSE
    )
    rownames(pc.data) <- pc.data[, 1]
    pc.data[, 1] <- NULL
    colnames(pc.data) <- c(
      possible.pcs[seq_len(ncol(pc.data) - 1)],
      "smartpca.condition"
    )
  } else {
    warning(paste("components do not exist for ancestry/chip combination ",
      ancestry,
      " ",
      chip.nosubsets,
      ", due to inadequate sample size",
      sep = ""
    ))
  }

  ## add component data if requested and do it messily and without plyr
  for (pc in possible.pcs[possible.pcs %in% covariate.list]) {
    if (nrow(pc.data) == 0) {
      h <- transform(h, tmp = rep(NA, nrow(h)))
    } else {
      h <- h[h[, id.colname] %in% rownames(pc.data), ]
      h <- h[order(h[, id.colname]), ]
      pc.data <- pc.data[rownames(pc.data) %in% h[, id.colname], ]
      pc.data <- pc.data[order(rownames(pc.data)), ]
      h <- transform(h, tmp = pc.data[, pc])
    }
    colnames(h)[ncol(h)] <- pc
  }

  all.chips <- supported.chips

  ## partition data down to requested chip/ancestry.
  ##   This is a bit extra, but allows preflight sample size checking
  ancestry.combined <- data.frame()
  for (chip in all.chips) {
    ancestry.data <- read.table(paste(ancestry.prefix,
      chip,
      ".graf_estimates.txt",
      sep = ""
    ),
    header = TRUE,
    sep = "\t",
    stringsAsFactors = FALSE
    )
    ancestry.combined <- rbind(ancestry.combined, ancestry.data)
  }
  ancestry.combined <- ancestry.combined[, c(1, ncol(ancestry.combined))]
  ancestry.combined[, 2] <- stringr::str_replace_all(
    ancestry.combined[, 2],
    " ",
    "_"
  )
  ancestry.combined <- ancestry.combined[ancestry.combined[, 2] == ancestry, ]
  h <- h[h[, id.colname] %in% ancestry.combined[, 1], ]




  ## apply sex-stratified inverse normal transform when:
  ##   - covariate is continuous and not age covariate, or
  ##   - when analysis is FASTGWA or BOLT on the specified non-binary trait
  h <- construct.model.matrix:::apply.inverse.normalization(
    h,
    list(
      grepl("_co$", colnames(h)) & !grepl("_age_", colnames(h)),
      grepl("bolt|fastgwa", output.filename, ignore.case = TRUE) &
        !trait.is.binary &
        colnames(h) == phenotype.name
    )
  )


  chip.samples <- read.table(chip.samplefile,
    header = FALSE,
    stringsAsFactors = FALSE
  )
  chip.samples[, 1] <- unlist(lapply(
    strsplit(chip.samples[, 1], "_"),
    function(i) {
      i[1]
    }
  ))
  h <- h[h[, id.colname] %in% chip.samples[, 1], ]



  ## adding a test: partitioning by platform causes substantial
  ##   deviations from normality even after INT is applied on full phenotype.
  ##   so I'm trying out post-dataset-split INT, which has its own problems
  ##   but guarantees normality
  ## NB: this is completely useless at this point; flagged for removal
  if (transformation == "post.split.INT") {
    h <- construct.model.matrix:::apply.inverse.normalization(
      h,
      list(
        grepl("_co$", colnames(h)) & !grepl("_age_", colnames(h)),
        grepl("bolt|fastgwa", output.filename, ignore.case = TRUE) &
          !trait.is.binary &
          colnames(h) == phenotype.name
      )
    )
  }
  ## for sex-specific analyses: split here, after the combined dataset
  ##   processing and before the variable colinearity dropout stuff below
  if (sex.specific != "combined") {
    if (sex.specific == "female") {
      h <- h[h[, "sex"] == 2, ]
    } else if (sex.specific == "male") {
      h <- h[h[, "sex"] == 1, ]
    }
  }

  ## create an output data frame just containing the relevant variables
  output.df <- h[, c(rep(id.colname, 2), phenotype.name, covariate.list)]

  ## subset to complete cases, which are all that are used by SAIGE
  output.df <- output.df[complete.cases(output.df), ]

  ## identify non-binary categoricals and turn them into dummies
  ## wow this part is a pain
  minimum.factor.level.count <- 10
  extra.categoricals <- c(
    "center",
    "sex",
    "is.other.asian",
    paste("batch",
      all.chips,
      sep = "."
    )
  )
  output.df <- construct.model.matrix:::binarize(
    output.df,
    minimum.factor.level.count,
    list(
      grepl("ca$", colnames(output.df)),
      colnames(output.df) %in% extra.categoricals
    )
  )

  ## apply category merging and extraction for categorical trait analysis
  output.df <- construct.model.matrix:::extract.categories(
    output.df,
    phenotype.name,
    category.filename
  )




  ## finally maybe report results
  colnames(output.df)[1:2] <- c("FID", "IID")
  output.df <- output.df[, (grepl(
    paste("batch",
      chip.nosubsets,
      sep = "."
    ),
    colnames(output.df)
  ) |
    !grepl("batch.", colnames(output.df)))]
  write.table(output.df,
    output.filename,
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    sep = "\t"
  )
}

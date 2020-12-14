#' Read category tracker and extract categorical levels
#'
#' This function takes the preprocessor *.categories file,
#' loads its contents, and then uses its annotations to
#' merge/filter categorical levels until a single binary
#' comparison is left. This binary comparison is what is
#' actually run through SAIGE for categorical traits.
#'
#' @param df data.frame, working data frame with phenotype
#' variable present for all relevant subjects, plus possibly
#' some extra rows or columns
#' @param phenotype.name character vector, column name of the
#' phenotype variable in `df`
#' @param category.filename character vector, name of
#' preprocessor *.categories tracker file with rows
#' of category levels and either "reference" or "comparison",
#' delimited by tabs
#' @seealso construct.model.matrix
extract.categories <- function(df,
                               phenotype.name,
                               category.filename) {
  stopifnot(is.data.frame(df))
  stopifnot(is.vector(phenotype.name, mode = "character"))
  stopifnot(length(which(colnames(df) == phenotype.name)) == 1)
  stopifnot(is.na(category.filename) |
    is.vector(category.filename, mode = "character"))
  if (!is.na(category.filename) & category.filename != "NA") {
    stopifnot(file.exists(category.filename))
    ## read the valid categories. format is expected to be:
    ##   each row has a first column that is the variable name,
    ## and a second column that is either 'reference' or 'comparison'
    ## to allow arbitrary merging of levels.
    ##   non-reference level. note that this is applied to binary as
    ##   well as non-binary categoricals; the only impact would be that
    ##   the case condition might be flipped to "control"
    ##   if N(cases) > N(controls) but that would only flip ORs, nothing else
    ##   also it's very uncommon for N(cases) > N(controls)
    valid.cat.df <- read.table(category.filename,
      header = FALSE,
      stringsAsFactors = FALSE
    )
    stopifnot(ncol(valid.cat.df) <= 2)
    ## back compatibility with previous preprocessor:
    ## behavior at the time was to specify one reference
    ## level in the first row, and all remaining rows
    ## were comparison levels
    if (ncol(valid.cat.df) == 1) {
      second.col <- c("reference", rep("comparison", nrow(valid.cat.df) - 1))
      valid.cat.df[, 2] <- second.col
    }
    stopifnot(identical(
      sort(unique(valid.cat.df[, 2])),
      c("comparison", "reference")
    ))
    ref.cat <- valid.cat.df[, 1][valid.cat.df[, 2] == "reference"]
    comp.cat <- valid.cat.df[, 1][valid.cat.df[, 2] == "comparison"]
    ## only keep subjects with a value in these levels
    output.df <- df[df[, phenotype.name] %in% c(ref.cat, comp.cat), ]
    reference.indicator <- output.df[, phenotype.name] %in% ref.cat
    ## set everyone in the reference level to 0;
    ##   this is for compatibility with code expecting binary traits
    output.df[reference.indicator, phenotype.name] <- 0
    ## set everyone else, potentially merging low count categories, to 1
    output.df[!reference.indicator, phenotype.name] <- 1
  }
  output.df
}

#' Convert categorical variables into binary dummies
#'
#' This function performs categorical variable binarization,
#' which is handled in other libraries but in this case
#' is performed with some particularities. Conditions with fewer
#' than a specified number of subjects are combined into a single
#' meta-group tagged "combined.other", which has undesirable
#' statistical properties but at the very least is less likely
#' to cause your downstream model to just implode.
#'
#' @param df data.frame, source data frame containing model matrix
#' data to be transformed
#' @param minimum.factor.level.count numeric, number of subjects
#' in a condition strictly below which the condition is merged
#' into the catch-all meta group
#' @param conditions list of logical vectors. each vector should
#' be of length `ncol(df)`, and should indicate whether a column
#' passes a particular criterion for being included in the
#' transformed set. each vector is effectively logically ORed together.
binarize <- function(df, minimum.factor.level.count, conditions) {
  ## input parameter checking
  stopifnot(is.data.frame(df))
  stopifnot(is.numeric(minimum.factor.level.count))
  stopifnot(minimum.factor.level.count > 0)
  stopifnot(is.list(conditions))
  for (element in conditions) {
    stopifnot(is.vector(element, mode = "logical"))
    stopifnot(length(element) == ncol(df))
  }

  ## flag things that need dummification
  output.df <- df
  for (cat.var in colnames(output.df)[apply(data.frame(conditions), 1, any)]) {
    ## create n-1 dummies
    ## reorder the base factor so the most populous group is the reference
    output.df[, cat.var] <- factor(as.vector(output.df[, cat.var],
      mode = "character"
    ))
    counts <- summary(output.df[, cat.var])
    output.df[, cat.var] <- factor(output.df[, cat.var],
      levels =
        names(counts[order(counts,
          decreasing = TRUE
        )])
    )
    if (length(unique(output.df[, cat.var])) == 1) {
      output.df[, cat.var] <- NULL
      next
    }
    other.name <- paste(cat.var,
      "ref",
      levels(output.df[, cat.var])[1],
      "combined.other",
      sep = "."
    )
    other.group <- rep(0, nrow(output.df))
    ## skip the reference level
    for (fac.level in levels(output.df[, cat.var])[-1]) {
      cat.name <- paste(cat.var,
        "ref",
        levels(output.df[, cat.var])[1],
        fac.level,
        sep = "."
      )
      cat.values <- rep(0, nrow(output.df))
      cat.values[output.df[, cat.var] == fac.level] <- 1
      if (sum(cat.values) < minimum.factor.level.count) {
        other.group <- other.group + cat.values
        next
      }
      output.df[, cat.name] <- cat.values
    }
    output.df[, cat.var] <- NULL
    if (sum(other.group) > 0) {
      output.df[, other.name] <- other.group
    }
  }
  output.df
}

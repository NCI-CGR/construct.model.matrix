#' Apply inverse normalization to continuous variables
#'
#' Performs the inverse (rank) normal transformation
#' on a vector of values. You can do this to binary
#' data but you're not gonna like the results lol.
#'
#' @param i numeric vector, values to be transformed
inverse.normalize <- function(i) {
  stopifnot(is.vector(i, mode = "numeric"))
  qnorm((rank(i,
    na.last = "keep",
    ties.method = "random"
  ) - 0.5) / sum(!is.na(i)))
}

#' Apply inverse normalization to selected columns of a data frame
#'
#' This function takes a data frame, constructed of
#' model matrix data from a phenotype dataset, and applies
#' the inverse normal transformation to each column matching
#' a certain set of provided conditions. The variables are
#' stratified by sex before application of the transformation.
#' An extension here would be to allow arbitrary stratification
#' variables before transformation.
#'
#' @param df data.frame, source data frame containing model matrix
#' data to be transformed
#' @param conditions list of logical vectors. each vector should
#' be of length `ncol(df)`, and should indicate whether a column
#' passes a particular criterion for being included in the
#' transformed set. each vector is effectively logically ORed together.
apply.inverse.normalization <- function(df,
                                        conditions) {
  ## input parameter checks
  stopifnot(is.data.frame(df))
  stopifnot(is.list(conditions))
  for (element in conditions) {
    stopifnot(is.vector(element, mode = "logical"))
    stopifnot(length(element) == ncol(df))
  }
  ## for each column fulfilling at least one of the inclusion conditions
  for (col.index in which(apply(data.frame(conditions), 1, any))) {
    for (i in unique(h$sex)) {
      h[, col.index][h$sex == i] <-
        inverse.normalize(h[, col.index][h$sex == i])
    }
  }
}

#' Load control inclusion or exclusion data from file
#'
#' This function loads several variants of control inclusion
#' or exclusion data from file into a list of variable data.
#' Inclusions and exclusions are specified in the same format
#' and thus handled identically by this function.
#'
#' @param filename character vector, name of a file
#' containing control inclusion or exclusion data, or the
#' string "NA". format for this
#' file is: per row, a variable name, and optionally
#' a comma-delimited list of variable categories denoting
#' valid controls. for backwards compatibility, a variant
#' of this file only containing the first column is permitted,
#' in which case all non-zero levels of the variable will be
#' considered inclusion levels.
load.inc.exc <- function(filename) {
  ## check consistency of input data
  stopifnot(is.vector(filename, mode = "character") |
    is.na(filename))
  res <- list()
  ## if the data is a string
  if (is.vector(filename, mode = "character")) {
    ## check that the file exists
    stopifnot(file.exists(filename))
    ## load the data
    data <- read.table(filename,
      header = FALSE,
      sep = "\t",
      stringsAsFactors = FALSE
    )
    ## enforce 1 or 2 columns exactly
    stopifnot(ncol(data) >= 1 & ncol(data) <= 2)
    res <- list()
    ## for each entry in the input data
    for (i in seq_len(nrow(data))) {
      current.list <- list()
      ## a single NA entry is allowed
      if (ncol(data) == 1 & is.na(data[i, 1])) {
        next
      }
      ## else if there's only one column
      if (ncol(data) == 1) {
        ## interpret this as a variable where
        ## all non-zero levels are included or excluded
        ## depending on downstream interpretation
        current.list <- list(
          var.name = data[i, 1],
          var.levels = c()
        )
      } else if (is.vector(data[, 2], mode = "numeric")) {
        ## second column containing numeric values
        ## if the entry is NA
        if (is.na(data[i, 2])) {
          ## again, interpret this as a variable where
          ## all non-zero levels are included or excluded
          ## depending on downstream interpretation
          current.list <- list(
            var.name = data[i, 1],
            var.levels = c()
          )
        } else {
          ## consider the second column entry a value
          ## repreenting a categorical data level
          current.list <- list(
            var.name = data[i, 1],
            var.levels = c(data[i, 2])
          )
        }
      } else {
        ## once again, handle NA entries
        if (is.na(data[i, 2]) |
          data[i, 2] == "" |
          data[i, 2] == "NA") {
          current.list <- list(
            var.name = data[i, 1],
            var.levels = c()
          )
        } else {
          ## split the variable levels as a comma-delimited
          ## list. this isn't actually used yet, so this may
          ## require editing later. in particular, this may
          ## need to be cast as numerics (or possibly the above
          ## variant may need to be cast as character vectors),
          ## or possibly I'm forgetting that it's a list delimited
          ## by something other than commas
          var.levels <- unlist(strsplit(data[i, 2], ","))
          current.list <- list(
            var.name = data[i, 1],
            var.levels = var.levels
          )
        }
      }
      ## append list for this inclusion/exclusion variable to the result
      index <- length(res) + 1
      res[[index]] <- current.list
    }
  }
  ## return the result. if all entries were NA, this list will be empty
  res
}

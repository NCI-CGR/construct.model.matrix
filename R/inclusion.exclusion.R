#' Using parsed control inclusion/exclusion data,
#' remove controls that don't match specified criteria.
#'
#' This function takes an under-construction data frame
#' of model matrix information, a phenotype, and control
#' inclusion/exclusion lists from `load.inc.exc`, and
#' applies the filters requested by the inclusion/exclusion
#' lists. Filters only impact controls, and by extension
#' this function does nothing at all if provided with
#' a trait that isn't binary.
#'
#' @param df data.frame, input model matrix data
#' @param phenotype.name character vector, a column
#' name from `df`
#' @param trait.is.binary logical, an indicator whether
#' the phenotype is binary (0/1, 0 == control)
#' @param control.inclusion.list list, inclusion criteria
#' for controls in terms of other categorical variables from
#' `df`, as processed by `load.inc.exc`
#' @param control.exclusion.list list, exclusion criteria
#' for controls in terms of other categorical variables from
#' `df`, as processed by `load.inc.exc`
apply.inc.exc <- function(df,
                          phenotype.name,
                          trait.is.binary,
                          control.inclusion.list,
                          control.exclusion.list,
                          case.inclusion.list,
                          case.exclusion.list) {
  stopifnot(is.data.frame(df))
  stopifnot(is.vector(phenotype.name, mode = "character"))
  stopifnot(length(which(colnames(df) == phenotype.name)) == 1)
  stopifnot(is.logical(trait.is.binary))
  stopifnot(is.list(control.inclusion.list))
  stopifnot(is.list(control.exclusion.list))
  stopifnot(is.list(case.inclusion.list))
  stopifnot(is.list(case.exclusion.list))  

  res <- df
  if (trait.is.binary) {
    ## very importantly: the filters are only applied to control subjects!
    ## inclusion variables work on the logic that specified
    ## (or alternatively non-zero)
    ##  entries should be included as possible controls
    for (list.entry in control.inclusion.list) {
      ## additional input QC test: be sure any filtering variables are
      ## actually present in the df colnames
      stopifnot(list.entry[["var.name"]] %in% colnames(df))
      if (length(list.entry[["var.levels"]]) == 0) {
        res <- res[(res[, phenotype.name] == 0 &
          res[, list.entry[["var.name"]]] != 0) |
          res[, phenotype.name] != 0, ]
      } else {
        res <- res[(res[, phenotype.name] == 0 &
          res[, list.entry[["var.name"]]] %in% list.entry[["var.levels"]]) |
          res[, phenotype.name] != 0, ]
      }
    } #end of for (list.entry in control.inclusion.list) 

    ## exclusion variables work on the logic that specified
    ##  entries should be excluded as possible controls, or if no levels are
    ##  provided, zero entries are possible controls (all nonzero are excluded)
    for (list.entry in control.exclusion.list) {
      ## additional input QC test: be sure any filtering variables are
      ## actually present in the df colnames
      stopifnot(list.entry[["var.name"]] %in% colnames(df))
      if (length(list.entry[["var.levels"]]) == 0) {
        res <- res[(res[, phenotype.name] == 0 &
          res[, list.entry[["var.name"]]] == 0) |
          res[, phenotype.name] != 0, ]
      } else {
        res <- res[(res[, phenotype.name] == 0 &
          !(res[, list.entry[["var.name"]]] %in% list.entry[["var.levels"]])) |
          res[, phenotype.name] != 0, ]
      }
    } # end of for (list.entry in control.exclusion.list) 
    
    ## case inclusion variables, works similarly as control inclusion variables,
    ## but on the cases :p
    for (list.entry in case.inclusion.list) {
      stopifnot(list.entry[["var.name"]] %in% colnames(df))
      if (length(list.entry[["var.levels"]]) == 0) {
        res <- res[(res[, phenotype.name] == 1 &
                      res[, list.entry[["var.name"]]] != 0) |
                     res[, phenotype.name] != 1, ]
      } else {
        res <- res[(res[, phenotype.name] == 1 &
                      res[, list.entry[["var.name"]]] %in% list.entry[["var.levels"]]) |
                     res[, phenotype.name] != 1, ]
      }
    } #end of for (list.entry in case.inclusion.list) 
    
    ## case exclusion variables
    for (list.entry in control.exclusion.list) {
      ## additional input QC test: be sure any filtering variables are
      ## actually present in the df colnames
      stopifnot(list.entry[["var.name"]] %in% colnames(df))
      if (length(list.entry[["var.levels"]]) == 0) {
        res <- res[(res[, phenotype.name] == 1 &
                      res[, list.entry[["var.name"]]] == 0) |
                     res[, phenotype.name] != 1, ]
      } else {
        res <- res[(res[, phenotype.name] == 1 &
                      !(res[, list.entry[["var.name"]]] %in% list.entry[["var.levels"]])) |
                     res[, phenotype.name] != 1, ]
      }
    } # end of for (list.entry in case.exclusion.list) 
    
    
  } # if (trait.is.binary) 
  res
}

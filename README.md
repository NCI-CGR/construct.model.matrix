# `R::construct.model.matrix`

## Introduction

This package came about as a replacement for the corresponding script
`shared-source/construct_model_matrix.R` within the repository
`palmercd/plco-analysis`. The goal of the package was to generate variant
of the script that is modularized, extensible, testable, installable, and just
generally better. Happily that has all seemingly been achieved approaching
v1.0.0. The addition of formal test cases in particular using `usethis::testthat`
has been a relief.

There isn't much use to installing this package for reasons other than
integration with `palmercd/plco-analysis`; but at least in this repo now, it's
theoretically possible for someone to extend this package or use it for other purposes.

## Installation

This repo is formatted as a CRAN-compliant R package and can be installed using relevant
installation methods. It's not on CRAN, but if you have a tarball of this repository,
you should be able to install it with the following command:

`R CMD INSTALL construct.model.matrix-1.0.0.tar.gz`

In theory, if this were posted to a correctly configured GitHub/GitLab repo, it would be
possible to directly install this package over a valid internet connection. Now, it's not
posted to such a repo, so that's probably not currently possible. But for posterity's sake,
I record here how one might do that. Within R:

`require(devtools)`

`devtools::install_gitlab("palmercd/construct.model.matrix")`

## Input Data and Formats

The main entry point function in this package, `construct.model.matrix`, builds a model matrix
given a series of parameter specifications. This is designed to deprecate the functionality
of the `palmercd/plco-analysis` script `shared-source/construct_model_matrix.R`.

The primary function `construct.model.matrix` accepts the following arguments:

 - `phenotype.filename`: character vector; a filename of a phenotype dataset (as of this
 writing, for example, the path and filename of the v10 IMS PLCO dataset, with "NA" missing
 values and augmented columns for batch and ancestry control).
 
 - `chip.samplefile`: character vector; a filename of a sample list, one sample ID per line.
 For reasons that doubtless made sense at some point, the sample ID format is in fact
 `UNIQUEID_UNIQUEID`, which is then parsed out into a single `UNIQUEID` instance. This is
 an artifact of a truly ancient version of the PLCO analysis process and is flagged for
 updating.
 
 - `ancestry`: character vector; the ancestry of the requested analysis. This is expected
 to be a GRAF-style ancestry name: `African`, `African_American`, `East_Asian`, `European`,
 `Hispanic1`, `Hispanic2`, `Other_Asian_or_Pacific_Islander`, `Other`, `South_Asian`. Note
 the underscore in these ancestries that replaces the whitespace in raw GRAF ancestry names.
 
 - `chip`: character vector; the name of the platform being analyzed. In practice, this is
 really imputation batch: for PLCO, "GSA_batch1" is valid, "GSA" is not.
 
 - `phenotype.name`: character vector; variable name of the target phenotype in `phenotype.filename`.
 
 - `covariate.list.csv`: character vector; a comma-delimited list of covariate variable
 names from `phenotype.filename`, or the string `"NA"`.
 
 - `output.filename`: character vector; the name of the output file to which the final
 model matrix will be written (see below for format specification).
 
 - `category.filename`: character vector; the name of the file containing reference and
 comparison category labels for binary and categorical trait analysis, or `NA`. If a file,
 the format is, one per line, a category from the phenotype variable, and the string `"reference"`
 or `"comparison"`, separated by a tab. Levels with the same `"reference"` or `"comparison"`
 annotation will be merged into a single synthetic binary phenotype in the final output matrix.
 
 - `transformation`: character vector; the type of transformation to apply to the phenotype.
 Currently accepted values are "none", or "post.split.INT" for an inverse normal
 transform after dataset partitioning. This is not currently used by any analyses
 in the PLCO "Atlas" runs, and is merely a placeholder for later implementations.
 Currently, continuous traits are *always* inverse normal transformed. In fact,
 the level "none" should be renamed to "default", and this is flagged for change. 
 
 - `sex.specific`: character vector; which type of sex-specific analysis is requested
 for this model matrix. Depending on the value, the final model matrix will be subset
 by the phenotype dataset "sex" variable to include only the requested subjects.
 Recognized values are: `"combined"`, `"female"`, `"male"`. This assumes internally
 that the dataset coding of the sex variable will use `"1"` to depict male subjects
 and `"2"` to depict female; this is astonishingly common, but nevertheless exposure
 of these values as configuration candidates is on the long term edit list.
 
 - `control.inclusion.filename`: character vector; the name of the file containing control 
 inclusion restrictions in terms of phenotype dataset variables and optionally categories 
 within those variables; or `NA`. The format for this file is: per row, a variable 
 name, and optionally a comma-delimited list of variable categories denoting valid controls.
 For backwards compatibility, a variant of this file only containing the first column 
 is permitted, in which case all non-zero levels of the variable will be considered 
 inclusion levels. This is only applied to binary traits.
 
 - `control.exclusion.filename`: character vector; the name of the file containing control 
 exclusion restrictions in terms of phenotype dataset variables and optionally categories 
 within those variables; or `NA`. The format for this file is: per row, a variable 
 name, and optionally a comma-delimited list of variable categories denoting invalid controls.
 For backwards compatibility, a variant of this file only containing the first column 
 is permitted, in which case all non-zero levels of the variable will be considered 
 exclusion levels. This is only applied to binary traits.

## Output Format

The data output format is consistent with the format established in `palmercd/plco-analysis`.
The first output row is a header of column names; the first two columns are "FID" and "IID";
though the naming convention is consistent with traditional PLINK phenotype files, the subject
IDs are by default derived from the "plco_id" column from the PLCO backend phenotype files. The
next column is always the single phenotype outcome; if the trait is continuous, this will have
been inverse normalized. The remaining columns are any additional covariates in the model,
in the order specified to `covariate.list.csv`.

The remaining rows each correspond to a single subject from the backend phenotype file,
after filtering out subjects not requested by the relevant parameters (e.g. `ancestry`,
`chip`, `category.filename`, `sex.specific`, `control.inclusion.filename`, `control.exclusion.filename`).
The subjects are guaranteed to be in the same order in which they are encountered in the
backend phenotype file. Entries are tab-delimited. There is no row ID column. String entries
are not enclosed in quotation marks. The output file is plain text, not compressed.

## Version History

16 December 2020: release candidate: v1.0.0! this has gone very smoothly.

14 December 2020: initial migration of version from `plco.analysis`.

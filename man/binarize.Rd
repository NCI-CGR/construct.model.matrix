% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/categorical.handling.R
\name{binarize}
\alias{binarize}
\title{Convert categorical variables into binary dummies}
\usage{
binarize(df, minimum.factor.level.count, conditions)
}
\arguments{
\item{df}{data.frame, source data frame containing model matrix
data to be transformed}

\item{minimum.factor.level.count}{numeric, number of subjects
in a condition strictly below which the condition is merged
into the catch-all meta group}

\item{conditions}{list of logical vectors. each vector should
be of length \code{ncol(df)}, and should indicate whether a column
passes a particular criterion for being included in the
transformed set. each vector is effectively logically ORed together.}
}
\description{
This function performs categorical variable binarization,
which is handled in other libraries but in this case
is performed with some particularities. Conditions with fewer
than a specified number of subjects are combined into a single
meta-group tagged "combined.other", which has undesirable
statistical properties but at the very least is less likely
to cause your downstream model to just implode.
}

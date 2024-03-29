% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{NLoNModel}
\alias{NLoNModel}
\title{NLoN Model.}
\usage{
NLoNModel(
  text,
  response,
  features = TriGramsAndFeatures,
  cv = "glmnet",
  repeats = 10,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{text}{A character vector containing the training text.}

\item{response}{A factor response with levels NL (for natural
language) and Not (for not natural language).}

\item{features}{A function computing the feature values (a matrix,
list of numeric vectors or data.frame) or a list of functions
computing individual feature values.}

\item{cv}{Specify whether to use 10-fold cross-validation and which
function to use (\code{\link[glmnet]{cv.glmnet}} or
\code{\link[caret]{train}})}

\item{repeats}{Number of repeats of the 10-fold
cross-validation. Currently not implemented for
\code{\link[glmnet]{cv.glmnet}}}

\item{verbose}{If TRUE run cross-validation in verbose mode.}

\item{...}{Additional parameters to pass to
\code{\link[glmnet]{glmnet}}.}
}
\value{
A \code{\link[glmnet]{glmnet}} trained model.
}
\description{
Train a NLoN model using glmnet.
}
\seealso{
\code{\link[glmnet]{glmnet}}
}

#' Training data used in the NLoN paper.
#'
#' A dataset containing 2000 lines of text from Mozilla Firefox,
#' Lucene and Kubernetes datasets alongside two response variables
#' from two different raters.
#'
#' @format A data frame with 6000 rows and 4 columns:
#' \describe{
#'   \item{source}{source from the text (mozilla, kubernetes or lucene).}
#'   \item{text}{line of text.}
#'   \item{rater1}{reponse from the first rater.}
#'   \item{rater2}{reponse from the second rater.}
#' }
#' @source \url{https://bugzilla.mozilla.org/}
#' \url{http://www.kubertenes/}
#' \url{http://lucene}
"nlon.data"

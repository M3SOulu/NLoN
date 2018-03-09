#' NLoN: Natural Language or Not
#'
#' NLoN identifies whether text lines are natural language or not
#' using a glmnet model with simple text features and character
#' 3-grams.
#'
#' @examples
#'
#' ## Training data provided in the package.
#' nlon.data
#'
#' ## Build a model with glmnet
#' model <- with(head(nlon.data, 100), NLoNModel(text, rater2))
#' \dontrun{model <- nlon.data[source == "mozilla", NLoNModel(text, rater2)]}
#'
#' ## Use the model to predict new data.
#' topredict <- c("This is natural language.", "not(natural, language);")
#' NLoNPredict(model, topredict)
#'
#' \dontrun{NLoNPredict(model, nlon.data[source != "mozilla", text])}
#'
#' @docType package
#' @name NLoN
#' @import data.table
#' @importFrom stats predict
NULL

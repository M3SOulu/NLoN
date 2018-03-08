#' NLoN: Natural Language or Not
#'
#' NLoN identifies whether text lines are natural language or not
#' using a glmnet model with simple text features and character
#' 3-grams.
#'
#' @examples
#'
#' ## Training data provided in the package.
#' data(nlon.data)
#'
#' ## Build a model with glmnet
#' model <- with(nlon.data, NLoNModel(text, rater2))
#'
#' ## Use the model to preidct new data.
#' topredict <- c("This is natural language.", "not(natural, language);")
#' NLoNPredict(model, topredict, 0.1)
#'
#' ## Train and predict in a single function call.
#' NLoN(rbind(nlon.data[, list(text, response=rater2)],
#'           list(text=topredict), fill=TRUE), 0.1)
#'
#' @docType package
#' @name NLoN
#' @import data.table
#' @importFrom stats predict
NULL

#' NLoN Model.
#'
#' Train a NLoN model using glmnet.
#'
#' @param text A character vector containing the training text.
#' @param response A factor response with levels NL (for natural
#'   language) and Not (for not natural language).
#' @param features A function computing the feature values (a matrix,
#'   list of numeric vectors or data.frame) or a list of functions
#'   computing individual feature values.
#' @param alpha The elasticnet mixing parameter used by
#'   \code{\link[glmnet]{glmnet}}.
#' @param ... Additional parameters to pass to
#'   \code{\link[glmnet]{glmnet}}.
#' @return A \code{\link[glmnet]{glmnet}} trained model.
#' @seealso \code{\link[glmnet]{glmnet}}
#' @export
NLoNModel <- function(text, response, features=TriGramsAndFeatures,
                      alpha=1, ...) {
  data <- ComputeFeatures(text, features)
  glmnet::glmnet(x=ConvertFeatures(data), y=response, family="binomial",
                 alpha=alpha, ...)
}

#' NLoN Model.
#'
#' Train a NLoN model using glmnet.
#'
#' @param model A glmnet model as returned by \code{\link{NLoNModel}}.
#' @param text A character vector containing the text to predict.
#' @param lambda Lambda parameter to pass to
#'   \code{\link[glmnet]{predict.glmnet}}.
#' @param type Type of prediction made by
#'   \code{\link[glmnet]{predict.glmnet}}.
#' @param features A function computing the feature values (a matrix,
#'   list of numeric vectors or data.frame) or a list of functions
#'   computing individual feature values.
#' @return The output of \code{\link[glmnet]{predict.glmnet}}.
#' @seealso \code{\link[glmnet]{predict.glmnet}}
#' @export
NLoNPredict <- function(model, text, lambda=NULL, type="class",
                        features=TriGramsAndFeatures) {
  data <- ConvertFeatures(ComputeFeatures(text, features))
  missing <- setdiff(rownames(model$beta), colnames(data))
  if (length(missing)) {
    data <- cbind(Matrix::sparseMatrix(i=c(), j=c(),
                                       dims=c(nrow(data), length(missing)),
                                       dimnames=list(NULL, missing)), data)
  }
  data <- data[, rownames(model$beta)]
  predict(model, data, s=lambda, type=type)
}

#' NLoN Model.
#'
#' Train a NLoN model and gives the predicton for the data without
#' response.
#'
#' The data.frame must contain a column \code{text} with both training
#' and test data and a column response with the response value (factor
#' with levels NL and Not). The response is NA for test data.
#'
#' @param data A data.frame containing the training and test data.
#' @param lambda Lambda parameter to pass to
#'   \code{\link[glmnet]{predict.glmnet}}.
#' @param type Type of prediction made by
#'   \code{\link[glmnet]{predict.glmnet}}.
#' @param features A function computing the feature values (a matrix,
#'   list of numeric vectors or data.frame) or a list of functions
#'   computing individual feature values.
#' @param ... Additional parameters to pass to \code{NLoNModel}.
#' @return A vector of length \code{sum(is.na(data$response))} with
#'   the prediction of the test data.
#' @seealso \code{\link{NLoNModel}}
#' @seealso \code{\link{NLoNPredict}}
#' @seealso \code{\link[glmnet]{glmnet}}
#' @seealso \code{\link[glmnet]{predict.glmnet}}
#' @export
NLoN <- function(data, lambda=NULL, type="class",
                 features=TriGramsAndFeatures, ...) {
  data <- as.data.table(data)
  model <- data[!is.na(response), NLoNModel(text, response, features, ...)]
  NLoNPredict(model, data[is.na(response), text], lambda, type, features)
}

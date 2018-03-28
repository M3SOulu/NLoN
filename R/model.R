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
#' @param cv Specify whether to use 10-fold cross-validation and which
#'   function to use (\code{\link[glmnet]{cv.glmnet}} or
#'   \code{\link[caret]{train}})
#' @param repeats Number of repeats of the 10-fold
#'   cross-validation. Currently not implemented for
#'   \code{\link[glmnet]{cv.glmnet}}
#' @param verbose If TRUE run cross-validation in verbose mode.
#' @param ... Additional parameters to pass to
#'   \code{\link[glmnet]{glmnet}}.
#' @return A \code{\link[glmnet]{glmnet}} trained model.
#' @seealso \code{\link[glmnet]{glmnet}}
#' @export
NLoNModel <- function(text, response, features=TriGramsAndFeatures,
                      cv="glmnet", repeats=10, verbose=TRUE, ...) {
  data <- ConvertFeatures(ComputeFeatures(text, features))
  if (!is.null(cv) && cv == "glmnet") {
    glmnet::cv.glmnet(x=data, y=response, family='binomial',
                      alpha=1, type.measure="auc", nfolds=10, ...)
  } else {
    model <- glmnet::glmnet(x=data, y=response, family="binomial", alpha=1, ...)
    if (!is.null(cv) && cv == "caret") {
      NLoNCaret(data, response, model$lambda, repeats, verbose, ...)
    } else model
  }
}

NLoNCaret <- function(data, response, lambdas, repeats, verbose, ...) {
  Summary <- function (data, lev=NULL, model=NULL) {
    c(F1=caret::F_meas(data$pred, data$obs, lev[1]),
      Precision=caret::precision(data$pred, data$obs, lev[1]),
      Recall=caret::recall(data$pred, data$obs, lev[1]),
      caret::twoClassSummary(data, lev, model))
  }
  control <- caret::trainControl(method="repeatedcv", number=10,
                                 repeats=repeats, classProb=TRUE,
                                 summaryFunction=Summary,
                                 verboseIter=verbose)
  tgrid <- data.frame(alpha=1, lambda=lambdas)
  caret::train(x=data, y=response, method="glmnet", metric="ROC",
               trControl=control, tuneGrid=tgrid, ...)
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
NLoNPredict <- function(model, text, lambda="lambda.min", type="class",
                        features=TriGramsAndFeatures) {
  if (inherits(model, "train")) {
    model <- model$finalModel
    model$lambda.min <- model$lambdaOpt
  }
  if (lambda == "lambda.min") {
    lambda <- model$lambda.min
  } else if (lambda == "lambda.1se") {
    lambda <- model$lambda.1se
  }
  if (inherits(model, "cv.glmnet")) {
    model <- model$glmnet.fit
  }
  data <- ConvertFeatures(ComputeFeatures(text, features))
  missing.cols <- setdiff(rownames(model$beta), colnames(data))
  data <- cbind(Matrix::sparseMatrix(i=c(), j=c(),
                                     dims=c(nrow(data), length(missing.cols)),
                                     dimnames=list(NULL, missing.cols)), data)
  data <- rbind(data[, rownames(model$beta)], c())
  predict(model, data, s=lambda, type=type)
}

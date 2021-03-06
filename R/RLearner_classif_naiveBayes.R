#' @export
makeRLearner.classif.naiveBayes = function() {
  makeRLearnerClassif(
    cl = "classif.naiveBayes",
    package = "e1071",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "laplace", default = 0, lower = 0)
      # makeNumericLearnerParam(id = "threshold", default = 0.001, lower = 0)
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "prob"),
    name = "Naive Bayes",
    short.name = "nbayes",
    callees = "naiveBayes"
  )
}

#' @export
trainLearner.classif.naiveBayes = function(.learner, .task, .subset, .weights = NULL,  ...) {
  data = getTaskData(.task, .subset, target.extra = TRUE)
  e1071::naiveBayes(x = as.matrix(data$data), y = data$target, ...)
}

#' @export
predictLearner.classif.naiveBayes = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "class", "raw")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}

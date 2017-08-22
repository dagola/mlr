
#' @export
makeRLearner.classif.mbmdrc = function() {
  mlr::makeRLearnerClassif(
    cl = "classif.mbmdrc",
    package = "MBMDRClassifieR",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeIntegerLearnerParam(id = "order", default = 2, lower = 1, upper = 2, when = "train", tunable = TRUE),
      ParamHelpers::makeLogicalLearnerParam(id = "order.range", default = TRUE, when = "train", tunable = TRUE),
      ParamHelpers::makeIntegerLearnerParam(id = "min.cell.size", default = 0, lower = 0, when = "train", tunable = TRUE),
      ParamHelpers::makeNumericLearnerParam(id = "alpha", lower = 0.01, upper = 1, default = 0.1, when = "train", tunable = TRUE),
      ParamHelpers::makeDiscreteLearnerParam(id = "adjustment", values = c("NONE", "ADDITIVE", "CODOMINANT"), default = "NONE", when = "train", tunable = TRUE),
      ParamHelpers::makeIntegerLearnerParam(id = "max.results", lower = 1, default = 1000, when = "train", tunable = FALSE),
      ParamHelpers::makeIntegerLearnerParam(id = "cv.top.results", lower = 1, default = 100, when = "train", tunable = FALSE),
      ParamHelpers::makeIntegerLearnerParam(id = "top.results", lower = 1, default = 100, when = "predict", tunable = FALSE),
      ParamHelpers::makeIntegerLearnerParam(id = "folds", lower = 1, default = 5, when = "train", tunable = FALSE),
      ParamHelpers::makeDiscreteLearnerParam(id = "cv.loss", values = c("auc", "bac"), default = "auc", when = "train", tunable = FALSE),
      ParamHelpers::makeLogicalLearnerParam(id = "o.as.na", default = FALSE, when = "both", tunable = TRUE)
    ),
    properties = c("twoclass", "prob", "numerics", "factors"),
    name = "MB-MDR based classification",
    short.name = "mbmdrc",
    note = "Learner param 'predict.method' maps to 'type' in predict.mbmdrc."
  )
}

#' @export
trainLearner.classif.mbmdrc = function(.learner, .task, .subset, .weights = NULL, cv.top.results, order, order.range, ...) {
  if (order.range) {
    order = 1L:order
  }
  mbmdrc(dependent.variable.name = mlr::getTaskTargetNames(.task),
         data = mlr::getTaskData(.task, .subset),
         order = order, top.results = cv.top.results, ...)
}

#' @export
predictLearner.classif.mbmdrc = function(.learner, .model, .newdata, type = NULL, ...) {
  p = stats::predict(.model$learner.model, newdata = .newdata, type = .learner$predict.type, ...)
  return(p)
}

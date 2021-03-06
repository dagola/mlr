
#' @export
makeRLearner.surv.ranger = function() {
  makeRLearnerSurv(
    cl = "surv.ranger",
    package = "ranger",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "num.trees", lower = 1L, default = 500L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeNumericLearnerParam(id = "mtry.perc", lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "min.node.size", lower = 1L, default = 3L),
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeNumericLearnerParam(id = "sample.fraction", lower = 0L, upper = 1L),
      makeNumericVectorLearnerParam(id = "split.select.weights", lower = 0, upper = 1),
      makeUntypedLearnerParam(id = "always.split.variables"),
      makeLogicalLearnerParam(id = "respect.unordered.factors", default = FALSE),
      makeDiscreteLearnerParam(id = "importance", values = c("none", "permutation"), default = "none", tunable = FALSE),
      makeLogicalLearnerParam(id = "write.forest", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "scale.permutation.importance", default = FALSE, requires = quote(importance == "permutation"), tunable = FALSE),
      makeIntegerLearnerParam(id = "num.threads", lower = 1L, when = "both", tunable = FALSE),
      makeLogicalLearnerParam(id = "save.memory", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = TRUE, when = "both", tunable = FALSE),
      makeIntegerLearnerParam(id = "seed", when = "both", tunable = FALSE),
      makeDiscreteLearnerParam(id = "splitrule", values = c("logrank", "C", "maxstat"), default = "logrank"),
      makeNumericLearnerParam(id = "alpha", lower = 0L, upper = 1L, default = 0.5, requires = quote(splitrule == "maxstat")),
      makeNumericLearnerParam(id = "minprop", lower = 0L, upper = 1L, default = 0.1, requires = quote(splitrule == "maxstat")),
      makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(num.threads = 1L, verbose = FALSE, respect.unordered.factors = TRUE),
    properties = c("numerics", "factors", "ordered", "featimp"),
    name = "Random Forests",
    short.name = "ranger",
    note = "By default, internal parallelization is switched off (`num.threads = 1`), `verbose` output is disabled, `respect.unordered.factors` is set to `TRUE`. All settings are changeable. `mtry.perc` sets `mtry` to `mtry.perc*getTaskNFeats(.task)`. Default for `mtry` is the floor of square root of number of features in task.",
    callees = "ranger"
  )
}

#' @export
trainLearner.surv.ranger = function(.learner, .task, .subset, .weights, mtry, mtry.perc, ...) {
  tn = getTaskTargetNames(.task)
  if (missing(mtry)) {
    if (missing(mtry.perc)) {
      mtry = floor(sqrt(getTaskNFeats(.task)))
    } else {
      mtry = max(1, floor(mtry.perc * getTaskNFeats(.task)))
    }
  }
  ranger::ranger(formula = NULL, dependent.variable.name = tn[1L],
    status.variable.name = tn[2L], data = getTaskData(.task, .subset), mtry = mtry, ...)
}

#' @export
predictLearner.surv.ranger = function(.learner, .model, .newdata, ...) {
  p = predict(object = .model$learner.model, data = .newdata)
  rowMeans(p$chf)
}

#' @export
getFeatureImportanceLearner.surv.ranger = function(.learner, .model, ...) {
  getFeatureImportanceLearner.classif.ranger(.learner, .model, ...)
}

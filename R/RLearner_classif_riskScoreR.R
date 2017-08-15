
#' @export
makeRLearner.classif.riskScorer = function() {
  makeRLearnerClassif(
    cl = "classif.riskScorer",
    package = "riskScoreR",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "weight", default = TRUE),
      makeUntypedLearnerParam(id = "beta", default = TRUE),
      makeNumericVectorLearnerParam(id = "importance", tunable = FALSE)
    ),
    properties = c("twoclass", "numerics", "prob"),
    name = "Risk allele scoring",
    short.name = "riskscorer",
    note = ""
  )
}

#' @export
trainLearner.classif.riskScorer <- function(.learner, .task, .subset, .weights, ...) {
  riskScoreR::riskScorer(y.name = getTaskTargetNames(.task),
             data = getTaskData(.task, .subset),
             feature.names = getTaskFeatureNames(.task), ...)
}

#' @export
predictLearner.classif.riskScorer <- function(.learner, .model, .newdata, ...) {
  p <- predict(risk.scorer = .model$learner.model, newdata = .newdata, type = .learner$predict.type)
  if(.learner$predict.type == "response") {
    return(p[[1]])
  } else {
    return(as.matrix(p))
  }
}

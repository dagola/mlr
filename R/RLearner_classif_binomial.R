#' @export
makeRLearner.classif.binomial = function() {
  makeRLearnerClassif(
    cl = "classif.binomial",
    package = "stats",
    par.set = makeParamSet(
      makeDiscreteLearnerParam("link", values = c("logit", "probit", "cloglog", "cauchit", "log"),
        default = "logit"),
      makeLogicalLearnerParam("sparse", default = TRUE, when = "train", tuneable = FALSE)
    ),
    par.vals = list(
      sparse = TRUE
    ),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "Binomial Regression",
    short.name = "binomial",
    note = "Delegates to `glm` with freely choosable binomial link function via learner parameter `link`. We set 'sparse' to TRUE by default to save memory. Thus the learner model consists of the coefficients only.",
    callees = c("glm", "binomial")
  )
}

#' @export
trainLearner.classif.binomial = function(.learner, .task, .subset, .weights = NULL, link = "logit", ...) {
  data = getTaskData(.task, .subset, target.extra = TRUE)
  fit = stats::glm.fit(y = data$target,
                 x = cbind(1, as.matrix(data$data)),
                 family = stats::binomial(link = link),
                 weights = .weights, ...)
  if(sparse) {
    return(fit["coefficients"])
  } else {
    return(fit)
  }
}

#' @export
predictLearner.classif.binomial = function(.learner, .model, .newdata, ...) {
  newmatrix = cbind(1, as.matrix(.newdata))
  coefs = as.matrix(.model$learner.model$coefficients)
  eta = newmatrix %*% coefs

  x = exp(eta)/(1+exp(eta))
  levs = .model$task.desc$class.levels
  if (.learner$predict.type == "prob") {
    x = cbind(1-x, x)
    colnames(x) = levs
  } else {
    p = as.factor(ifelse(x > 0.5, levs[2L], levs[1L]))
    unname(p)
  }
}



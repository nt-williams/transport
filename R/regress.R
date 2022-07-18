regress <- function(X, y, learners, family, folds) {
    family <- ifelse(family == "binomial", binomial(), gaussian())
    cv_control <- SuperLearner::SuperLearner.CV.control(V = folds)
    SuperLearner::SuperLearner(
        y, X,
        family = family[[1]],
        SL.library = learners,
        method = "method.NNLS",
        env = environment(SuperLearner::SuperLearner),
        cvControl = cv_control
    )
}

regress_predict <- function(fit, newdata) {
    predict(fit, newdata)$pred[, 1]
}

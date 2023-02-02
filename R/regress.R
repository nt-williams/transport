train <- function(X, y, family, learners, folds) {
    family <- ifelse(family == "binomial", binomial(), gaussian())
    cv_control <- SuperLearner::SuperLearner.CV.control(V = folds)

    envr <- new.env(parent = environment(SuperLearner::SuperLearner))
    envr$screenV <- parent.frame()$screenV

    SuperLearner::SuperLearner(
        y, X,
        family = family[[1]],
        SL.library = learners,
        method = "method.NNLS",
        env = envr,
        cvControl = cv_control
    )
}

predict_from_fit <- function(fit, newdata) {
    predict(fit, newdata, onlySL = TRUE)$pred[, 1]
}

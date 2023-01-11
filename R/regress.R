train <- function(X, y, family, learners, folds) {
    family <- ifelse(family == "binomial", binomial(), gaussian())
    cv_control <- SuperLearner::SuperLearner.CV.control(V = 2)
    SuperLearner::SuperLearner(
        y, X,
        family = family[[1]],
        SL.library = learners,
        method = "method.NNLS",
        env = environment(SuperLearner::SuperLearner),
        cvControl = cv_control
    )
    # glmnet3(X, y, family)
}

predict_from_fit <- function(fit, newdata) {
    predict(fit, newdata)$pred[, 1]
    # predict.glmnet3(fit, newdata)
}

glmnet3 <- function(X, y, family = c("gaussian", "binomial"), id = NULL) {
    if (!is.null(id)) {
        # need to match index with fold number for cv.glmnet
        folds <- origami::make_folds(
            nrow(X), fold_fun = origami::folds_vfold,
            cluster_ids = id, V = 10
        )

        foldid <- vector("numeric", nrow(X))
        for (i in 1:nrow(X)) {
            for (v in 1:10) {
                if (i %in% folds[[v]]$validation_set) {
                    foldid[i] <- v
                    break
                }
            }
        }

    } else {
        foldid <- NULL
    }

    ans <- list(covars = names(X))

    if (ncol(X) == 1) {
        x <- as.matrix(X)
        ans$fit <- glm(y ~ ., data = cbind(y = y, X), family = match.arg(family))
    } else {
        f <- as.formula(paste0("~ .^", ncol(X)))
        x <- model.matrix(f, X)[, -1]
        ans$fit <- glmnet::cv.glmnet(x, y, family = match.arg(family), foldid = foldid)
    }

    ans
}

predict.glmnet3 <- function(object, newx) {
    if (inherits(object$fit, "glm")) {
        return(predict(object$fit, newx, type = "response"))
    }

    X <- newx[, object$covars, drop = TRUE]
    f <- as.formula(paste0("~ .^", ncol(X)))
    X <- model.matrix(f, X)[, -1]
    as.vector(predict(object$fit, X, type = "response", s = "lambda.min")[, 1])
}

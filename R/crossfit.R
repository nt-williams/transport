crossfit <- function(data, target, covar, folds, assignments, outcome_type, learners, cvfolds, subset = NULL) {
    fits <- foreach(fold = folds, .options.future = list(seed = TRUE)) %dofuture% {
        t <- fold$training_set
        v <- fold$validation_set

        if (!is.null(subset)) {
            t <- (1:nrow(data) %in% t) & subset
        }

        train <- data[t, c(target, covar)]
        valid <- assign_value(data[v, covar, drop = FALSE], assignments)

        fit <- mlr3superlearner(data = train,
                                target = target,
                                library = learners,
                                outcome_type = outcome_type,
                                folds = cvfolds,
                                newdata = valid)

        weights <- fit$weights
        pred <- matrix(nrow = length(v), ncol = length(valid))

        for (j in 1:length(valid)) {
            pred[, j] <- fit$preds[[j]]
        }

        list(weights = weights,
             pred = pred)
    }

    i <- unlist(lapply(folds, function(x) x$validation_set))

    list(pred = do.call("rbind", lapply(fits, function(x) x$pred))[order(i), , drop = FALSE],
         weights = lapply(fits, function(x) x$weights))
}

assign_value <- function(data, assignments) {
    if (is.null(assignments)) {
        return(list(data))
    }

    lapply(seq_along(assignments), function(i, n) {
        if (is.null(assignments[[i]])) {
            return(data)
        }

        data[[n[i]]] <- assignments[[i]]
        data
    }, n = names(assignments))
}

sim_data <- function(n) {
    S <- rbinom(n, 1, 0.5)
    W1 <- rbinom(n, 1, 0.4 + (0.2 * S))
    W2 <- rnorm(n, 0.1*S, 1)
    W3 <- rnorm(n, 1 + (0.2*S), 1)
    # A <- rbinom(n, 1, 0.5)
    # Z0 <- rbinom(n , 1, plogis( -log(1.6) - log(1.1)*W2 -log(1.3)*W3))
    A <- rbinom(n, 1, plogis(-log(1.6) +log(4) - log(1.1)*W2  -log(1.3)*W3))
    # Z <- ifelse(A == 1, Z1, Z0)
    Y0 <- rbinom(n, 1, plogis(log(1.6) - log(1.3)*W3 - log(1.2)*W1))
    Y1 <- rbinom(n, 1, plogis(log(1.6) + log(1.9) -log(1.3)*W3 - log(1.2)*W1 + log(1.2)*W1))
    Y <- ifelse(A == 1, Y1, Y0)
    list(obs = data.frame(W1 = W1, W2 = W2, W3 = W3, S = S, A = A, Y = Y, Y1 = Y1, Y0 = Y0),
         ate = mean((Y1 - Y0)),
         ate_S0 = mean((Y1 - Y0)[S == 0]),
         ate_S1 = mean((Y1 - Y0)[S == 1]))
}

devtools::load_all()
# library(lmtp)

dat <- sim_data(10000)
obs <- dat$obs

Np <- transport_Npsem$new(dat$obs, c("W1", "W2", "W3"),
                          V = c("W1", "W2", "W3"), A = NULL,
                          Z = "A", S = "S", Y = "Y")

lr <- mlr3::lrn("classif.log_reg")
lr$predict_type <- "prob"
transport_ate(Np, lr, "binomial", 1)

rprt = lrn("classif.rpart", predict_type = "prob")
lgb = lrn("classif.lightgbm", predict_type = "prob")
lm = lrn("classif.log_reg", predict_type = "prob")

# Define level 0
level_0 =
    gunion(list(
        PipeOpLearnerCV$new(rprt, id = "rpart_l0",
                            param_vals = list(resampling.folds = 10)),
        PipeOpLearnerCV$new(lgb, id = "lightgbm_l0",
                            param_vals = list(resampling.folds = 10)),
        PipeOpLearnerCV$new(lm, id = "lm_l0",
                            param_vals = list(resampling.folds = 10))
    ))

# Create "averager" learner (and set predict type to "prob")
lrn_avg <- LearnerClassifAvg$new(id = "classif.avg")
lrn_avg$predict_type <- "prob"

# Combine level 0 and "averager" learner
level_1  <- level_0 %>>%
    PipeOpFeatureUnion$new(id = "u1") %>>%
    lrn_avg

lrn = GraphLearner$new(level_1)


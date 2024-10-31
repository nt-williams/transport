gendata <- function(n, A = NULL) {
  W <- rbinom(n, 1, 0.5)
  V <- rbinom(n, 1, 0.66)
  Z <- rbinom(n, 1, 0.33)

  if (is.null(A)) A <- rbinom(n, 1, 0.5)

  S <- rbinom(n, 1, 0.4 + 0.5*W - 0.3*Z)

  Yi <- rnorm(n, A + W + A*V + 2.5*A*Z, sqrt((0.1 + 0.8*W)^2))
  Y <- ifelse(S == 1, Yi, NA_real_)

  data.frame(W = W,
             V = V,
             Z = Z,
             S = S,
             A = A,
             Y = Y,
             Yi = Yi)
}

set.seed(123)
n <- 250

tmp <- gendata(n)

transport_ate(data = data,
              trt = "A",
              outcome = "Y",
              covar = c("W", "V", "Z"),
              pop = "S",
              estimator = "standard",
              folds = 1)

transport_ate(data = data,
              trt = "A",
              outcome = "Y",
              covar = c("W", "V", "Z"),
              pop = "S",
              estimator = "collaborative",
              folds = 1)

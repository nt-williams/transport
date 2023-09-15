gendata <- function(n, A = NULL) {
    W1 <- rbinom(n, 1, 0.5)
    W2 <- rbinom(n, 1, 0.66)


    A <- rbinom(n, 1, 0.5)
    Z <- rbinom(n, 1, A*0.9)

    S <- rbinom(n, 1, 0.4 + 0.5*W1 - 0.3*W2)

    Yi <- rnorm(n, Z + W1 + Z*W2 + 2.5*Z*W1, sqrt((0.1 + 0.8*W1)^2))
    Y <- ifelse(S == 1, Yi, NA_real_)

    data.frame(W1 = W1,
               W2 = W2,
               S = S,
               A = A,
               Z = Z,
               Y = Y,
               Yi = Yi)
}

dat <- gendata(1000)

transport_ate(dat, "Z", "Y", "S", c("W1", "W2"), cens = NULL,
                 outcome_type = "continuous",
                 id = NULL, weights = NULL)

transport_ate2(dat, "Z", "Y", "S", c("W1", "W2"), "W1", cens = NULL,
              outcome_type = "continuous",
              id = NULL, weights = NULL)

transport_ittate(dat, "A", "Z", "Y", "S", c("W1", "W2"), cens = NULL,
                 outcome_type = "continuous",
                 id = NULL, weights = NULL,
                 control = .transport_ittate_control())

transport_cate(dat, "A", "Z", "Y", "S", c("W1", "W2"), cens = NULL,
               outcome_type = "continuous",
               id = NULL, weights = NULL,
               control = .transport_cate_control())

gendata <- function(n) {
    W <- rbinom(n, 1, 0.5)
    Z <- rbinom(n, 1, 0.25)
    V <- rbinom(n, 1, 0.75)

    A <- rbinom(n, 1, 0.5)

    S <- rbinom(n, 1, 0.8 - 0.5*Z - 0.1*W)

    Yi <- rnorm(n, 1.2 + 0.25*A + 0.5*W + A*Z + 0.5*Z + 0.4*A*V)
    Y <- ifelse(S == 1, Yi, NA_real_)

    data.frame(W = W,
               Z = Z,
               V = V,
               S = S,
               A = A,
               Y = Y,
               Yi = Yi)
}

dat <- gendata(1000)

transport_ate3(dat, "A", "Y", "S", c("W", "Z", "V"), "V", "Z", cens = NULL,
               outcome_type = "continuous",
               id = NULL, weights = NULL)

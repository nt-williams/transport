describe("ATE estimators return correct values", {
  set.seed(123)
  foo <- gendata_ate(1000)

  it("transport_ate(estimator = 'standard')", {
    x <- transport_ate(
      data = foo,
      trt = "A",
      outcome = "Y",
      covar = c("W", "V", "Z"),
      pop = "S",
      estimator = "standard",
      folds = 1
    )

    expect_equal(x$psi@x, 2.684402, tolerance = 0.01)
  })

  it("transport_ate(estimator = 'collaborative')", {
    x <- transport_ate(
      data = foo,
      trt = "A",
      outcome = "Y",
      covar = c("W", "V", "Z"),
      pop = "S",
      estimator = "collaborative",
      folds = 1
    )

    expect_equal(x$psi@x, 2.778398, tolerance = 0.01)
  })
})

if (requireNamespace("ranger", quietly = TRUE)) {
  describe("ITTATE estimator returns correct value", {
    set.seed(123)
    tmp <- gendata_ittate(1000)

    it("transport_ittate", {
      set.seed(123)
      x <- transport_ittate(data = tmp,
                            trt = "Z",
                            instrument = "A",
                            outcome = "Y",
                            covar = c("W1", "W2", "W3"),
                            pop = "S",
                            folds = 1,
                            learners_instrument = "glm",
                            learners_trt = "glm",
                            learners_pop = "glm",
                            learners_outcome = "ranger")

      # 2.5% wiggle room
      expect_equal(x$psi@x, 0.054, tolerance = 0.025)
    })
  })
}

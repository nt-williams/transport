describe("estimators return correct values", {
  set.seed(123)
  foo <- gendata(1000)

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

    expect_equal(x$psi@x, 2.684402, tolerance = 0.001)
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

    expect_equal(x$psi@x, 2.778398, tolerance = 0.001)
  })
})

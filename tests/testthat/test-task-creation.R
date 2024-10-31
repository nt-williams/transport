describe("as_transport_task()", {
  set.seed(123)
  foo <- gendata(10)

  it("assertions detect errors", {
    local({
      foo$Y <- rep("c", 10)

      expect_error(
        as_transport_task(foo, "A", "Y", c("W", "V", "Z"), "S", NULL, NULL, NULL, 2)
      )
    })

    local({
      foo$Z <- NULL

      expect_error(
        as_transport_task(foo, "A", "Y", c("W", "V", "Z"), "S", NULL, NULL, NULL, 2)
      )
    })

    local({
      foo$A <- runif(10)

      expect_error(
        as_transport_task(foo, "A", "Y", c("W", "V", "Z"), "S", NULL, NULL, NULL, 2)
      )
    })

    local({
      foo$S <- runif(10)

      expect_error(
        as_transport_task(foo, "A", "Y", c("W", "V", "Z"), "S", NULL, NULL, NULL, 2)
      )
    })

  })

  it("auxilary functions work", {
    task <- as_transport_task(
      data = foo,
      A = "A",
      Y = "Y",
      W = c("W", "V", "Z"),
      S = "S",
      C = NULL,
      id = NULL,
      weights = NULL,
      folds = 2
    )

    pop <- data.frame(
      S = rep(0, 3),
      W = rep(0, 3),
      V = rep(0, 3),
      Z = c(1, 0, 1),
      A = c(1, 1, 0),
      Y = rep(NA_real_, 3),
      row.names = c(1L, 3L, 6L)
    )

    expect_equal(task$nrow(), 10)
    expect_equal(task$nfolds(), 2)
    expect_equal(task$pop("target")$data(), pop)
    expect_equal(task$pop()$reset()$data(), task$data())
    expect_equal(task$select("A")$modify("A", 1), rep(1, 10))
    expect_equal(task$reset()$select(c("A", "Z"))$data(), foo[, c("Z", "A")])
    expect_equal(task$history("A"), c("S", "W", "V", "Z"))
    expect_equal(task$history("Y"), c("A", "W", "V", "Z"))
    expect_equal(task$history("S"), c("W", "V", "Z"))
    expect_equal(task$obs()$data(), task$data())
  })
})

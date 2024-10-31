test_that("Multiplying by NA returns 0", {
  expect_equal(c(1, NA, 3) %*0% c(NA, 2, 3), c(0, 0, 9))
})

test_that("loggingparameters", {
  lp <- loggingparameters()
  expect_equal(class(lp), "list")
  expect_equal(length(lp), 30)
})

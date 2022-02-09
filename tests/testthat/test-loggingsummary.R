test_that("loggingsummary", {

  # Data loading
  data(LoggingSimulationOutputs_iter)

  expect_error(loggingsummary(x = TRUE),
               regexp = "The argument of the 'loggingsummary' function must be a list")

  rslt <- capture.output(loggingsummary(LoggingSimulationOutputs_iter))

  })

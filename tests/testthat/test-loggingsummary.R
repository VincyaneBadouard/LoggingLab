test_that("loggingsummary", {

  # Data loading
  data(LoggingSimulationOutputs)

  expect_error(loggingsummary(x = TRUE),
               regexp = "The argument of the 'loggingsummary' function must be a list")

  rslt <- capture.output(loggingsummary(LoggingSimulationOutputs))

  })

test_that("loggingsummary1", {

  # Data loading
  data(LoggingSimulationOutputs)

  expect_error(loggingsummary1(x = TRUE),
               regexp = "The argument of the 'loggingsummary1' function must be a list")


  rslt <- capture.output(loggingsummary1(LoggingSimulationOutputs))

  })

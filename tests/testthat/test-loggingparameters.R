test_that("loggingparameters", {

  expect_error(loggingparameters(ObjectiveBonus = "10"),
  regexp = "You have assigned a non-numerical value to one of the arguments of the 'loggingparameters' function
         waiting for a numerical value.")

  expect_error(loggingparameters(TreeHarvestableVolumeAllometry = "a + b * DBH^2"),
               regexp = "you want to fill in waiting for an object of function class")

  lp <- loggingparameters()

  expect_equal(class(lp), "list")
  expect_equal(length(lp), 30)
})

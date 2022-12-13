test_that("loggingparameters", {

  expect_error(loggingparameters(MinDBHValue = "10"),
  regexp = "You have assigned a non-numerical value to one of the arguments of the 'loggingparameters' function
         expects a numerical value.")

  expect_error(loggingparameters(ResamplDistDTM = 3.5),
               regexp = "You have assigned a non-integer value")

  expect_error(loggingparameters(CostMatrix = 3),
               regexp = "You must assign a list to the 'CostMatrix' argument")

  expect_error(loggingparameters(TreeHarvestableVolumeAllometry = "a + b * DBH^2"),
               regexp = "you want to fill in expects an object of function class")

  lp <- loggingparameters()

  expect_equal(class(lp), "list")
  expect_equal(length(lp), 35)
})

test_that("cleaninventory", {

  #Check the function argument
  MatrixInventory <- as.matrix(Paracou6_2016)
  expect_error(cleaninventory(MatrixInventory), regexp = "inventory must be a data.frame")

  #Create the test inventory
  TestInventory <- Paracou6_2016
  TestInventory[10, "CodeAlive"] <- "FALSE" # Some FALSE in CodeAlive
  TestInventory[4, "CircCorr"] <- 6 # DBH < 10

  #check if CodeAlive == TRUE
  expect_true(all(cleaninventory(TestInventory)$CodeAlive == "TRUE"))

  #check if DBH >= 10 (check if DBH exists in the same way)
  expect_true(all(cleaninventory(TestInventory)$DBH >= 10))

  expect_false(any(is.na(cleaninventory(TestInventory)$DBH))) #check that all the DBH are computed


  #check if the stops work
  ##Create the test inventory
  StopTestInventory <- Paracou6_2016
  StopTestInventory[1:3, "idTree"] <- "200" # create a 3 time present tree
  StopTestInventory[5, "Plot"] <- "100" # a tree in another plot
  StopTestInventory[20, "CensusYear"] <- 2017 # different years inventory

  expect_error(cleaninventory(StopTestInventory), regexp = "idTree") # check if idTree's are unique
  expect_error(cleaninventory(StopTestInventory), regexp = "Plot") # check if it is always the same plot
  expect_error(cleaninventory(StopTestInventory), regexp = "CensusYear") # and the same year


})


# only keep CodeAlive == TRUE -> check if CodeAlive == TRUE
# create DBH column and only keep DBH >= 10 -> check if DBH >= 10 (check if DBH exists in the same way)
# check if idTree's are unique
# check if it is always the same plot
# and the same year
# stop when at least one condition is not checked -> check if the stops work

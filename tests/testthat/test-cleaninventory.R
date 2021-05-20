test_that("cleaninventory", {

  #load the test inventory
  load(system.file("extdata", "BrokenParacou6_2016.rda", package = "Maria"))

  #check if CodeAlive == TRUE
  expect_true(all(cleaninventory(Paracou6_2016)$CodeAlive == "TRUE"))

  #check if DBH >= 10 (check if DBH exists in the same way)
  expect_true(all(cleaninventory(Paracou6_2016)$DBH >= 10))

  # check if idTree's are unique
  expect_false(any(duplicated(cleaninventory(Paracou6_2016)$idTree)))

  #check if it is always the same plot
  expect_false(!length(unique(cleaninventory(Paracou6_2016)$Plot))== 1)

  #and the same year
  expect_false(!length(unique(cleaninventory(Paracou6_2016)$CensusYear))== 1)

  #check if the stops work
  expect_error(cleaninventory(BrokenParacou6_2016), regexp = "idTree")

})


# test_that("cleaninventory", {
#   load(system.file("extdata", "BrokenParacou6_2016.rda", package = "Maria"))
#   BrokenParacou6_2016 <- filter(BrokenParacou6_2016, idTree != 200) # eliminate the first detected error
#
#   expect_error(cleaninventory(BrokenParacou6_2016), regexp = "Plot")
# })
# #
# test_that("cleaninventory", {
#   load(system.file("extdata", "BrokenParacou6_2016.rda", package = "Maria"))
#   BrokenParacou6_2016 <- filter(BrokenParacou6_2016, idTree != "200") # eliminate the first detected error
#   BrokenParacou6_2016 <- filter(BrokenParacou6_2016, PLOT != "100") # eliminate the first detected error
#
#
#   expect_error(cleaninventory(BrokenParacou6_2016), regexp = "CensusYear")
# })



# only keep CodeAlive == TRUE -> check if CodeAlive == TRUE
# create DBH column and only keep DBH >= 10 -> check if DBH >= 10 (check if DBH exists in the same way)
# check if idTree's are unique
# check if it is always the same plot
# and the same year
# stop when at least one condition is not checked -> check if the stops work



#I use BrockenParacou6_2016 to check error message, and Paracou6_2016 to check other actions.

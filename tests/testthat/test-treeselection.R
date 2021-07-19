test_that("treeselection", {

  # Check the function arguments

  data(Paracou6_2016)
  data(DemParacou)

  MatrixInventory <- as.matrix(Paracou6_2016)
  Matrixspeciescriteria <- as.matrix(SpeciesCriteria)


  expect_error(treeselection(MatrixInventory,
                             type ="manual", fuel = "0", objective = 20, diversification = TRUE, specieslax = FALSE, objectivelax = FALSE,
                             speciescriteria = Matrixspeciescriteria),
               regexp = "The 'inventory' and 'speciescriteria' arguments of the 'treeselection' function must be data.frame")

  expect_error(treeselection(Paracou6_2016, type ="manual", fuel = "0", diversification = TRUE,
                             objective = "RIL"),
               regexp = "The 'objective' argument of the 'treeselection' function must be numeric")

  expect_error(treeselection(Paracou6_2016, type ="manual", fuel = "0", objective = 20,
                             diversification = "1", specieslax = 2, objectivelax = "a"),
               regexp = "The 'diversification', 'specieslax' and 'objectivelax' arguments of the 'treeselection' function must be logical")

  expect_error(treeselection(Paracou6_2016, fuel = "0", diversification = TRUE, objective = 20,
                             type = "CL"),
               regexp = "The 'type' argument of the 'treeselection' function must be 'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  expect_error(treeselection(Paracou6_2016, type ="manual", objective = 20, diversification = TRUE,
                             fuel = TRUE),
               regexp = "The 'fuel' argument of the 'treeselection' function must be '0', '1', or '2'")

  expect_error(treeselection(Paracou6_2016, objective = 20, type ="manual", fuel = "2",
                             diversification = FALSE, specieslax = FALSE, objectivelax = FALSE,
                             DEM = NULL, plotslope = NULL))


  expect_error(treeselection(Paracou6_2016, objective = 20, type ="manual", fuel = "0", diversification = TRUE,
                             otherloggingparameters = as.matrix(loggingparameters())),
               regexp = "The 'otherloggingparameters' argument of the 'treeselection' function must be a list")

  # Test data preparation
  inventory = addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))

  testinventory <- suppressMessages(treeselection(inventory, objective = 20, type ="manual",
                                                  fuel = "2", diversification = TRUE, specieslax = FALSE, objectivelax = FALSE,
                                                  DEM = DemParacou, plotslope = PlotSlope, speciescriteria = SpeciesCriteria, otherloggingparameters = loggingparameters()))$inventory # , MainTrail


  otherloggingparameters = loggingparameters()

  # All alived
  if ("DeathCause" %in% names(inventory)) expect_true(all(is.null(DeathCause)))

  # All healthy: expect_true(all(VisibleDefect == "0")) A FAIRE

  # Objective Volume:
  ## hollow trees harvested
  objective <- 40
  VO <- suppressMessages(treeselection(inventory,
                                       type ="manual", fuel = "2", objective = 40, diversification = TRUE, specieslax = FALSE,
                                       objectivelax = FALSE))$VO

  expect_true(VO == objective)

  ## hollow trees non-harvested
  VO <- suppressMessages(treeselection(inventory,
                                       type ="manual", fuel = "0", objective = 40, diversification = TRUE, specieslax = FALSE,
                                       objectivelax = FALSE))$VO

  expect_true(VO == objective + otherloggingparameters$ObjectiveBonus)


  expect_true(nrow(inventory) == nrow(testinventory)) # check that the rows number don't change

})

# check args
# treeselection(inventory,
#               type ="manual", fuel = "0",objective = 20, diversification = TRUE, specieslax = FALSE,
#               objectivelax = FALSE, SpeciesCriteria, otherloggingparameters = loggingparameters())
# inventory, speciescriteria (data.frame)
# objective (numeric)
# type, fuel  (character)
# diversification, specieslax, objectivelax (logical)
# otherloggingparameters (liste)

# All alived: if ("DeathCause" %in% names(inventory)) expect_true(all(is.null(DeathCause)))
# All healthy: expect_true(all(VisibleDefect == "0"))
# Objective Volume: if (type == "RIL3fuelhollow"| (type == "manual"& fuel =="2")) {expect_true(VO = objective)
# }else{
# expect_true(VO = objective + otherloggingparameters$ObjectiveBonus)





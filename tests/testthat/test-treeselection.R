test_that("treeselection", {

  # Check the function arguments

  data(Paracou6_2016)
  Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:2000)

  data(DTMParacou)
  data("HarvestablePolygons")
  data("MainTrails")

  MatrixInventory <- as.matrix(Paracou6_2016)
  Matrixspeciescriteria <- as.matrix(SpeciesCriteria)


  expect_error(treeselection(MatrixInventory,
                             scenario ="manual", fuel = "0", objective = 20,
                             diversification = TRUE, specieslax = FALSE, objectivelax = FALSE,
                             speciescriteria = Matrixspeciescriteria),
               regexp = "The 'inventory' and 'speciescriteria' arguments of the 'treeselection' function must be data.frame")

  expect_error(treeselection(Paracou6_2016, speciescriteria = SpeciesCriteria,
                             scenario ="manual", fuel = "0", diversification = TRUE,
                             objective = "RIL"),
               regexp = "The 'objective' argument of the 'treeselection' function must be numeric")

  expect_error(treeselection(Paracou6_2016, speciescriteria = SpeciesCriteria,
                             scenario ="manual", fuel = "0", objective = 20,
                             diversification = "1"),
               regexp = "The 'diversification' argument of the 'treeselection' function must be logical or NULL")

  expect_error(treeselection(Paracou6_2016, speciescriteria = SpeciesCriteria,
                             scenario ="manual", fuel = "0", objective = 20,
                             diversification = FALSE, specieslax = 2, objectivelax = "a"),
               regexp = "The 'specieslax' and 'objectivelax' arguments of the 'treeselection' function must be logicals")

  expect_error(treeselection(Paracou6_2016, speciescriteria = SpeciesCriteria,
                             fuel = "0", diversification = TRUE, objective = 20,
                             scenario = "CL"),
               regexp = "The 'scenario' argument of the 'treeselection' function must be
         'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  expect_error(treeselection(Paracou6_2016, speciescriteria = SpeciesCriteria,
                             scenario ="manual", objective = 20, diversification = TRUE,
                             fuel = TRUE),
               regexp = "The 'fuel' argument of the 'treeselection' function must be '0', '1', or '2'")

  expect_error(treeselection(Paracou6_2016, speciescriteria = SpeciesCriteria,
                             objective = 20, scenario ="manual", fuel = "2",
                             diversification = FALSE, specieslax = FALSE, objectivelax = FALSE,
                             topography = NULL, plotslope = NULL))


  expect_error(treeselection(Paracou6_2016, speciescriteria = SpeciesCriteria,
                             objective = 20, scenario ="manual", fuel = "0", diversification = TRUE,
                             topography = DTMParacou, plotslope = PlotSlope, advancedloggingparameters = as.matrix(loggingparameters())),
               regexp = "The 'advancedloggingparameters' argument of the 'treeselection' function must be a list")

  expect_error(treeselection(Paracou6_2016, speciescriteria = SpeciesCriteria,
                             scenario = "manual",
                             objective = 20, fuel = NULL, diversification = T, topography = DTMParacou, plotslope = PlotSlope),
               regexp = "If you choose the 'manual' mode,
         you must fill in the arguments 'objective', 'fuel' and 'diversification'")

  # Test data preparation
  inventory = addtreedim(inventorycheckformat(Paracou6_2016), volumeparameters = ForestZoneVolumeParametersTable)

  testinventory <- suppressMessages(treeselection(inventory, objective = 20, scenario ="manual",
                                                  fuel = "2", diversification = TRUE,
                                                  specieslax = FALSE, objectivelax = TRUE,
                                                  topography = DTMParacou, plotslope = PlotSlope,
                                                  speciescriteria = SpeciesCriteria,
                                                  advancedloggingparameters = loggingparameters(),
                                                  MainTrails = MainTrails, harvestablepolygons = HarvestablePolygons)$inventory) # , MainTrail


  advancedloggingparameters = loggingparameters()

  # All alived
  if ("DeathCause" %in% names(inventory)) expect_true(all(is.null(DeathCause)))

  # Objective Volume:
  objective <- 20
  expected <- objective * unique(inventory$PlotArea)
  VO <- suppressMessages(treeselection(inventory, speciescriteria = SpeciesCriteria,
                                       scenario ="manual", fuel = "2", objective = objective,
                                       diversification = TRUE, specieslax = FALSE,
                                       objectivelax = TRUE, topography = DTMParacou, plotslope = PlotSlope,
                                       MainTrails = MainTrails, harvestablepolygons = HarvestablePolygons))$VO


  expect_true(VO == expected)

  expect_true(nrow(inventory) == nrow(testinventory)) # check that the rows number don't change

})

# check args
# treeselection(inventory,
#               scenario ="manual", fuel = "0",objective = 20, diversification = TRUE, specieslax = FALSE,
#               objectivelax = TRUE, SpeciesCriteria, advancedloggingparameters = loggingparameters())
# inventory, speciescriteria (data.frame)
# objective (numeric)
# scenario, fuel  (character)
# diversification, specieslax, objectivelax (logical)
# advancedloggingparameters (liste)

# All alived: if ("DeathCause" %in% names(inventory)) expect_true(all(is.null(DeathCause)))
# Objective Volume: if (scenario == "RIL3fuelhollow"| (scenario == "manual"& fuel =="2")) {expect_true(VO = objective)
# }else{
# expect_true(VO = objective + advancedloggingparameters$ObjectiveBonus)





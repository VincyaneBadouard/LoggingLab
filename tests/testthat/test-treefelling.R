test_that("treefelling", {

  # Test data
  data(Paracou6_2016)
  # Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:2000)
  MatrixInventory <- as.matrix(Paracou6_2016)

  MainTrail <- sf::st_linestring(matrix(c(286400, 583130,
                                          286400, 583250,
                                          286655, 583250,
                                          286655, 583130,
                                          286400, 583130)
                                        ,ncol=2, byrow=TRUE))

  pol1 <- list(matrix(c(286503, 583134,
                        286503, 583240,
                        286507, 583240,
                        286507, 583134,
                        286503, 583134)
                      ,ncol=2, byrow=TRUE))
  pol2 <- list(matrix(c(286650, 583134,
                        286650, 583240,
                        286654, 583240,
                        286654, 583134,
                        286650, 583134)
                      ,ncol=2, byrow=TRUE))

  PolList = list(pol1,pol2)
  ScndTrail <- st_multipolygon(PolList)

  inventory <- addtreedim(inventorycheckformat(Paracou6_2016))
  inventory <- suppressMessages(treeselection(inventory, objective = 30, scenario ="manual",
                             fuel = "2", diversification = TRUE, specieslax = FALSE,
                             objectivelax = FALSE, DEM = DemParacou, plotslope = PlotSlope,
                             speciescriteria = SpeciesCriteria,
                             advancedloggingparameters = loggingparameters())$inventory)

  testinventory <- treefelling(inventory, scenario = "manual", fuel = "2",
                               directionalfelling = "2", MainTrail = MainTrail, ScndTrail = ScndTrail,
                               advancedloggingparameters = loggingparameters())



  # Check the function arguments
  expect_error(treefelling(MatrixInventory,
                           scenario ="manual", fuel = "0", directionalfelling = "2",
                           advancedloggingparameters = loggingparameters()),
               regexp = "The 'inventory'argument of the 'treefelling' function must be data.frame")


  expect_error(treefelling(Paracou6_2016, fuel = "0", directionalfelling = "2",
                           advancedloggingparameters = loggingparameters(),
                           scenario = "CL"),
               regexp = "The 'scenario' argument of the 'treefelling' function must be
         'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  expect_error(treefelling(Paracou6_2016, scenario ="manual", directionalfelling = "2",
                           advancedloggingparameters = loggingparameters(),
                           fuel = TRUE),
               regexp = "The 'fuel' argument of the 'treefelling' function must be '0', '1', '2' or NULL")

  expect_error(treefelling(Paracou6_2016, scenario ="manual", fuel = "2",
                           advancedloggingparameters = loggingparameters(),
                           directionalfelling = TRUE),
               regexp = "The 'directionalfelling' argument of the 'treefelling' function must be '0', '1', '2' or NULL")

  expect_error(treefelling(Paracou6_2016,
                           scenario = "manual", fuel = "2", directionalfelling = "2",
                           advancedloggingparameters = loggingparameters(),
                           MainTrail = st_as_text(MainTrail), ScndTrail = st_as_text(ScndTrail)),
               regexp = "The 'MainTrail' and 'ScndTrail' arguments of the 'treefelling' function must be sfg")


  expect_error(treefelling(Paracou6_2016, scenario = "manual", fuel = "2",
                           directionalfelling = "2", MainTrail = MainTrail, ScndTrail = ScndTrail,
                           advancedloggingparameters = as.matrix(loggingparameters())),
               regexp = "The 'advancedloggingparameters' argument of the 'treefelling' function must be a list")

  expect_error(treefelling(Paracou6_2016, scenario = "manual",
                           MainTrail = MainTrail, ScndTrail = ScndTrail,
                           fuel = NULL, directionalfelling = NULL),
               regexp = "If you choose the 'manual' mode,
         you must fill in the arguments 'fuel' and 'directionalfelling'")


  expect_type(testinventory$TreePolygon, "character")


  expect_true(nrow(inventory) == nrow(testinventory)) # check that the rows number don't change


})

# check args errors
# inventory + TreeFellingOrientationSuccess (factor) + TreePolygon (character)
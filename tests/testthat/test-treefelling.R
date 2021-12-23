test_that("treefelling", {

  # Test data
  data(Paracou6_2016)
  data(DTMParacou)
  data(PlotSlope)
  data("HarvestablePolygons")
  data(MainTrails)

  MatrixInventory <- as.matrix(Paracou6_2016)
  MainTrails_no_sf <- MainTrails
  sf::st_geometry(MainTrails_no_sf) <- NULL


  pol1 <- list(matrix(c(286503, 582925,
                        286503, 583240,
                        286507, 583240,
                        286507, 582925,
                        286503, 582925) # the return
                      ,ncol=2, byrow=TRUE))
  pol2 <- list(matrix(c(286650, 582925,
                        286650, 583240,
                        286654, 583240,
                        286654, 582925,
                        286650, 582925) # the return
                      ,ncol=2, byrow=TRUE))

  PolList = list(pol1,pol2) #list of lists of numeric matrices
  ScndTrail <- sf::st_as_sf(sf::st_sfc(sf::st_multipolygon(PolList)))
  ScndTrail <- sf::st_set_crs(ScndTrail, sf::st_crs(MainTrails))

  ScndTrail_no_sf <- ScndTrail
  sf::st_geometry(ScndTrail_no_sf) <- NULL


  inventory <- addtreedim(inventorycheckformat(Paracou6_2016),
                          volumeparameters = ForestZoneVolumeParametersTable)
  inventory <- suppressMessages(treeselection(inventory, objective = 20, scenario ="manual",
                                              fuel = "2", diversification = TRUE, specieslax = FALSE,
                                              objectivelax = TRUE, topography = DTMParacou,
                                              plotslope = PlotSlope,
                                              speciescriteria = SpeciesCriteria,
                                              advancedloggingparameters = loggingparameters(),
                                              MainTrails = MainTrails, harvestablepolygons = HarvestablePolygons)$inventory)

  testinventory <- treefelling(inventory, scenario = "manual", fuel = "2", winching = "2",
                               directionalfelling = "2", MainTrails = MainTrails, ScndTrail = ScndTrail,
                               advancedloggingparameters = loggingparameters())



  # Check the function arguments
  expect_error(treefelling(MatrixInventory,
                           scenario ="manual", fuel = "0", winching = "0", directionalfelling = "2",
                           advancedloggingparameters = loggingparameters()),
               regexp = "The 'inventory'argument of the 'treefelling' function must be data.frame")


  expect_error(treefelling(Paracou6_2016, fuel = "0", winching = "0", directionalfelling = "2",
                           advancedloggingparameters = loggingparameters(),
                           scenario = "CL"),
               regexp = "The 'scenario' argument of the 'treefelling' function must be
         'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  expect_error(treefelling(Paracou6_2016, scenario ="manual", winching = "0", directionalfelling = "2",
                           advancedloggingparameters = loggingparameters(),
                           fuel = TRUE),
               regexp = "The 'fuel' argument of the 'treefelling' function must be '0', '1', '2' or NULL")

  expect_error(treefelling(Paracou6_2016, scenario ="manual", winching = "0", fuel = "2",
                           advancedloggingparameters = loggingparameters(),
                           directionalfelling = TRUE),
               regexp = "The 'directionalfelling' argument of the 'treefelling' function must be '0', '1', '2' or NULL")

  expect_error(treefelling(Paracou6_2016,
                           scenario = "manual", fuel = "2", winching = "0", directionalfelling = "2",
                           advancedloggingparameters = loggingparameters(),
                           MainTrails =  MainTrails_no_sf, ScndTrail = ScndTrail_no_sf),
               regexp = "The 'MainTrails' and 'ScndTrail' arguments of the 'treefelling' function must be sf")


  expect_error(treefelling(Paracou6_2016, scenario = "manual", winching = "0", fuel = "2",
                           directionalfelling = "2", MainTrails = MainTrails, ScndTrail = ScndTrail,
                           advancedloggingparameters = as.matrix(loggingparameters())),
               regexp = "The 'advancedloggingparameters' argument of the 'treefelling' function must be a list")

  expect_error(treefelling(Paracou6_2016, scenario = "manual", winching = "0",
                           MainTrails = MainTrails, ScndTrail = ScndTrail,
                           fuel = NULL, directionalfelling = NULL),
               regexp = "If you choose the 'manual' mode,
         you must fill in the arguments 'fuel' and 'directionalfelling'")


  expect_type(testinventory$TreePolygon, "character")


  expect_true(nrow(inventory) == nrow(testinventory)) # check that the rows number don't change


  # Mortality
  # timber exploitation
  Cutted <- testinventory %>%
    dplyr::filter(!is.na(TreePolygon) & ProbedHollow == "0")

  expect_true(all(Cutted$DeathCause == "cutted"))

  # fuel wood exploitation
  Fuel <- testinventory %>%
    dplyr::filter(!is.na(TreePolygon) & ProbedHollow == "1")

  expect_true(all(Fuel$DeathCause == "hollowfuel"))

  # Damage trees
  felttrees <- testinventory %>% # Cutted trees (timber and fuel)
    dplyr::filter(!is.na(TreePolygon)) %>%
    dplyr::select(TreePolygon)

    DeadTrees <- suppressWarnings(sf::st_intersection(
      sf::st_as_sf(testinventory, coords = c("Xutm", "Yutm")), # all the inventory trees
      sf::st_make_valid(getgeometry(felttrees, TreePolygon)) # "make valid" to avoid self-intersection
      # felt trees
  )) %>%
      dplyr::filter(is.na(TreePolygon)) %>%
      # dplyr::filter(Selected != "1"| is.na(Selected)) %>% # not already cutted trees
      # dplyr::filter((Selected != "1" & ProbedHollow != "1")| is.na(Selected)) %>%
      select(idTree)
    sf::st_geometry(DeadTrees) <- NULL # remove TreePolygon column (sf to data.frame)
    DeadTrees <- DeadTrees %>%
      unique() %>% # you only die once
      dplyr::arrange(idTree)

    treefall <- testinventory %>%
      dplyr::filter(DeathCause == "treefall2nd")

  Damage <- testinventory %>%
    dplyr::filter(is.na(TreePolygon) & !is.na(DeathCause)) %>%  # & DeadTrees == "1"
    dplyr::arrange(idTree)

  expect_true(nrow(Damage) == nrow(DeadTrees)) # damages = intersections

  expect_true(all(Damage$idTree == DeadTrees$idTree)) # damages = intersections

  expect_true(all(Damage$DeathCause == "treefall2nd")) # damages -> treefall2nd



})

# check args errors
# inventory + TreeFellingOrientationSuccess (factor) + TreePolygon (character)

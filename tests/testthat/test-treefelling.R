test_that("treefelling", {

  # Test data
  data(Paracou6_2016)
  data(DTMParacou)
  data(HarvestableAreaOutputsCable)
  data(SecondaryTrails)


  MatrixInventory <- as.matrix(Paracou6_2016)

  inventory <- SecondaryTrails$inventory %>%
    select(-DeathCause) # to remove previous deaths

  MainTrailsAccess <- SecondaryTrails$MainTrailsAccess
  SmoothedTrails <- SecondaryTrails$SmoothedTrails


  testinventory <- treefelling(inventory,
                               scenario = "manual", fuel = "2", winching = "2",
                               directionalfelling = "2",
                               maintrailsaccess = MainTrailsAccess, scndtrail = SmoothedTrails,
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
                           maintrailsaccess = MainTrailsAccess, scndtrail = MatrixInventory),
               regexp = "The 'maintrailsaccess' and 'scndtrail'arguments of the 'treefelling' function must be sf or sfc")


  expect_error(treefelling(Paracou6_2016, scenario = "manual", winching = "0", fuel = "2",
                           directionalfelling = "2", maintrailsaccess = MainTrailsAccess, scndtrail = SmoothedTrails,
                           advancedloggingparameters = as.matrix(loggingparameters())),
               regexp = "The 'advancedloggingparameters' argument of the 'treefelling' function must be a list")

  expect_error(treefelling(Paracou6_2016, scenario = "manual", winching = "0",
                           maintrailsaccess = MainTrailsAccess, scndtrail = SmoothedTrails,
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
    dplyr::filter(is.na(TreePolygon) & DeathCause == "treefall2nd") %>%  # & DeadTrees == "1"
    dplyr::arrange(idTree)

  expect_true(all(Damage$idTree %in% DeadTrees$idTree)) # damages among the trees under the felled trees

  expect_true(nrow(Damage) <= nrow(DeadTrees)) # treefall2nd are less or equal to the number of trees under the felled trees

  Treefall2ndDeathTrees <- testinventory %>%
    dplyr::filter(Treefall2ndDeath == "1")

  NotDeadTrees <- testinventory %>%
    dplyr::filter(Treefall2ndDeath == "0")

  expect_true(all(Treefall2ndDeathTrees$DeathCause == "treefall2nd"))
  expect_true(all(is.na(NotDeadTrees$DeathCause)))


})

# check args errors
# inventory + TreeFellingOrientationSuccess (factor) + TreePolygon (character)

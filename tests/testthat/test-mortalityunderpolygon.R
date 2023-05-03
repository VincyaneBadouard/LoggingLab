test_that("mortalityunderpolygon", {

  # Test data
  data(Paracou6_2016)
  data(DTMParacou)
  data(SpeciesCriteria)
  data(MainTrails)
  data(HarvestableAreaOutputsCable)
  data(SecondaryTrails)


  MatrixInventory <- as.matrix(Paracou6_2016)
  # BadInventory <- dplyr::select(Paracou6_2016, -c('idTree','Selected'))
  FrameTrails <- as.data.frame(SecondaryTrails$SmoothedTrails)
  NoCrsTrails <- st_set_crs(SecondaryTrails$SmoothedTrails, NA)

  # Input inventory
  Paracou6_2016 <- tibble::add_column(Paracou6_2016, DBH= NA) # add a DBH column
  Paracou6_2016$DBH = Paracou6_2016$CircCorr/pi # and calculate it
  Paracou6_2016 <- dplyr::filter(Paracou6_2016, DBH >= 10)

  inventory <- addtreedim(Paracou6_2016,
                          volumeparameters = ForestZoneVolumeParametersTable)

  inventory <- suppressMessages(treeselection(inventory,
                                              topography = DTMParacou,
                                              speciescriteria = SpeciesCriteria,
                                              scenario = "RIL2",
                                              specieslax = FALSE, objectivelax = TRUE,
                                              harvestablearea = HarvestableAreaOutputsCable$HarvestableArea,
                                              plotslope = HarvestableAreaOutputsCable$PlotSlope,
                                              maintrails = MainTrails,
                                              harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons
  )$inventory)


  # Check the function arguments

  expect_error(mortalityunderpolygon(inventory = MatrixInventory,
                                     polygons = SecondaryTrails$SmoothedTrails,
                                     deathcause = "2ndtrail"),
               regexp = "The 'inventory' argument of the 'mortalityunderpolygon' function must be data.frame")

  expect_error(mortalityunderpolygon(inventory = Paracou6_2016,
                                     polygons = SecondaryTrails$SmoothedTrails,
                                     deathcause = "2ndtrail"),
               regexp = "The 'idTree', 'Xutm', 'Yutm' or 'Selected' column is missing or empty in your inventory")

  expect_error(mortalityunderpolygon(inventory,
                                     polygons = FrameTrails,
                                     deathcause = "2ndtrail"),
               regexp = "The 'polygons' argument of the 'mortalityunderpolygon' function must be a sfc_POLYGON or sfc_MULTIPOLYGON")

  expect_error(mortalityunderpolygon(inventory,
                                     polygons = NoCrsTrails,
                                     deathcause = "2ndtrail"),
               regexp = "The 'polygons' argument of the 'mortalityunderpolygon' function must have a crs")

  expect_error(mortalityunderpolygon(inventory,
                                     polygons = SecondaryTrails$SmoothedTrails,
                                     deathcause = 'piste'),
               regexp = "The 'deathcause' argument of the 'mortalityunderpolygon' function must be
         'maintrail', '2ndtrail', 'treefall2nd', 'landing'")



  # Test the result

  testinventory <- mortalityunderpolygon(inventory,
                                         polygons = SecondaryTrails$SmoothedTrails,
                                         deathcause = "2ndtrail")


  expect_true(inherits(testinventory, "data.frame"))

  # DeathCause column filled with the given deathcause
  expect_true("DeathCause" %in% names(testinventory))

  expect_true("2ndtrail" %in% testinventory$DeathCause)

})

test_that("genaccesspts", {

  data(MainTrails)
  data(HarvestableAreaOutputsCable)
  data(DTMParacou)

  expect_error(genaccesspts(topography = NULL,
                            machinepolygons = NULL,
                            maintrails = NULL),
                            regexp = "The 'topography' argument of the 'genaccesspts' function must
         be RasterLayer")

  accesspts <- genaccesspts(topography = DTMParacou,
  machinepolygons = HarvestableAreaOutputsCable$MachinePolygons,
  maintrails = MainTrails,
  advancedloggingparameters = loggingparameters())

  expect_true(inherits(accesspts$PartMainTrails,"sf"))
  expect_true(inherits(accesspts$AccessPointAll,"sf"))

  expect_true(dim(accesspts$PartMainTrails)[1]==dim(accesspts$AccessPointAll)[1])

  expect_true(all(sf::st_intersects(accesspts$PartMainTrails,sf::st_cast(MainTrails,"POLYGON"),sparse = FALSE)))

  expect_true(all(sf::st_intersects(accesspts$AccessPointAll,sf::st_cast(MainTrails,"POLYGON"),sparse = FALSE)))
})

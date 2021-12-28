test_that("loggingsummary", {
data(MainTrails)
data("HarvestablePolygons")
  expect_error(loggingsummary(x = TRUE),
               regexp = "The argument of the 'loggingsummary' function must be a list")

  # x <- suppressMessages(
  #     loggingsimulation(Paracou6_2016, topography = DTMParacou,
  #                       verticalcreekheight  = DTMParacou, speciescriteria = SpeciesCriteria,
  #                       volumeparameters = ForestZoneVolumeParametersTable, scenario = "manual",
  #                       objective = 20, fuel = "2", diversification = TRUE, winching = "2",
  #                       directionalfelling = "2", specieslax = FALSE, objectivelax = TRUE, harvestablepolygons = HarvestablePolygons,
  #                       crowndiameterparameters = ParamCrownDiameterAllometry, MainTrails = MainTrails,
  #                       advancedloggingparameters = loggingparameters(), iter = 1, cores = 1)
  #   )
  #
  # rslt <- capture.output(loggingsummary(x))

  })

test_that("loggingsummary1", {

  # Data loading
  # data(Paracou6_2016) # inventory
  # data(PlotMask) # inventoried plot mask
  # data(DTMParacou) # topography
  # data(VerticalCreekHeight) # relative elevation
  # data(SpeciesCriteria) # species exploitability criteria
  # data(ForestZoneVolumeParametersTable) # volume parameters
  # data(ParamCrownDiameterAllometry) # parameters values of the crown diameter allometry

  expect_error(loggingsummary1(x = TRUE),
               regexp = "The argument of the 'loggingsummary1' function must be a list")

  # x <- suppressMessages(
  #     loggingsimulation1(Paracou6_2016, topography = DTMParacou,
  #                       verticalcreekheight  = DTMParacou, speciescriteria = SpeciesCriteria,
  #                       volumeparameters = ForestZoneVolumeParametersTable, scenario = "manual",
  #                       objective = 20, fuel = "2", diversification = TRUE, winching = "2",
  #                       directionalfelling = "2", specieslax = FALSE, objectivelax = TRUE, harvestablepolygons = HarvestablePolygons,
  #                       crowndiameterparameters = ParamCrownDiameterAllometry, MainTrails = MainTrails,
  #                       advancedloggingparameters = loggingparameters())
  #   )
  #
  # rslt <- capture.output(loggingsummary1(x))

  })

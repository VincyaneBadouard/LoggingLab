test_that("loggingsimulation", {

  # Data loading
  data(Paracou6_2016) # inventory
  Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:2000) # data reduction

  # data(PlotMask) # inventoried plot mask
  # data(PlotSlope)
  # data(DTMParacou) # topography
  # data(VerticalCreekHeight) # relative elevation
  # data(SpeciesCriteria) # species exploitability criteria
  # data(ForestZoneVolumeParametersTable) # volume parameters
  # data(ParamCrownDiameterAllometry) # parameters values of the crown diameter allometry


  # Rslt <- loggingsimulation(
  #   Paracou6_2016, plotmask = PlotMask, topography = DTMParacou,
  #   verticalcreekheight  = VerticalCreekHeight, speciescriteria = SpeciesCriteria,
  #   volumeparameters = ForestZoneVolumeParametersTable, scenario = "manual",
  #   objective = 20, fuel = "2", diversification = TRUE, winching = "2",
  #   directionalfelling = "2", specieslax = FALSE, objectivelax = TRUE,
  #   crowndiameterparameters = ParamCrownDiameterAllometry,
  #   advancedloggingparameters = loggingparameters(), iter = 2, cores = 2)

  # Check args
  expect_error(loggingsimulation(Paracou6_2016, plotmask = PlotMask, topography = DTMParacou,
                                 verticalcreekheight = RE, speciescriteria = SpeciesCriteria,
                                 volumeparameters = ForestZoneVolumeParametersTable,
                                 crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1",
                                 iter = FALSE, cores = FALSE),
               regexp = "The 'iter' and 'cores' arguments of the 'loggingsimulation' function must be numeric")


  # expect_true(class(Rslt) == "list")

})

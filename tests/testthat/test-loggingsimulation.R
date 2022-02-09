test_that("loggingsimulation", {

  # Data loading
  data(Paracou6_2016) # inventory
  Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:2000) # data reduction

  data(PlotMask) # inventoried plot mask
  data(DTMParacou) # topography
  data(CreekDistances) # relative elevation
  data(SpeciesCriteria) # species exploitability criteria
  data(ForestZoneVolumeParametersTable) # volume parameters
  data(ParamCrownDiameterAllometry) # parameters values of the crown diameter allometry


  # Rslt <- loggingsimulation(inventory = Paracou6_2016,
  #                           topography = DTMParacou, plotmask = PlotMask, creekdistances  = CreekDistances,
  #                           speciescriteria = SpeciesCriteria, volumeparameters = ForestZoneVolumeParametersTable,
  #                           scenario = "manual", objective = 20,
  #                           fuel = "2", winching = "2", directionalfelling = "2",
  #                           diversification = TRUE, specieslax = FALSE, objectivelax = TRUE,
  #                           crowndiameterparameters = ParamCrownDiameterAllometry,
  #                           advancedloggingparameters = loggingparameters(), iter = 2, cores = 2)

  # Check args
  expect_error(loggingsimulation(Paracou6_2016, plotmask = PlotMask, topography = DTMParacou,
                                 creekdistances = RE, speciescriteria = SpeciesCriteria,
                                 volumeparameters = ForestZoneVolumeParametersTable,
                                 crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1",
                                 iter = FALSE, cores = FALSE),
               regexp = "The 'iter' and 'cores' arguments of the 'loggingsimulation' function must be numeric")


  # expect_true(class(Rslt) == "list")

})

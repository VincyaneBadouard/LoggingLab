test_that("loggingsimulation", {

  # Data loading
  data(Paracou6_2016) # inventory
  data(DTMParacou) # topography
  RE <- DTMParacou # relative elevation
  data(SpeciesCriteria) # species exploitability criteria
  data(ForestZoneVolumeParametersTable) # volume parameters
  data(ParamCrownDiameterAllometry) # parameters values of the crown diameter allometry

  # Test data
  Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:2000) # data reduction

  MatrixInventory <- as.matrix(Paracou6_2016)
  Matrixspeciescriteria <- as.matrix(SpeciesCriteria)
  MatrixVolParam <- as.matrix(ForestZoneVolumeParametersTable)
  MatrixCDparam <- as.matrix(ParamCrownDiameterAllometry)

  # Rslt <- loggingsimulation(inventory = Paracou6_2016, topography = DTMParacou,
  #                           relativeelevation  = DTMParacou, speciescriteria = SpeciesCriteria,
  #                           volumeparameters = ForestZoneVolumeParametersTable, scenario = "manual",
  #                           objective = 20, fuel = "2", diversification = TRUE, winching = "2",
  #                           directionalfelling = "2", specieslax = FALSE, objectivelax = TRUE,
  #                           crowndiameterparameters = ParamCrownDiameterAllometry,
  #                           advancedloggingparameters = loggingparameters(), iter = 1, cores = 1, plotslope = PlotSlope)

  # Check the function arguments

  # inventory, speciescriteria, volumeparameters, crowndiameterparameters
  expect_error(loggingsimulation(MatrixInventory, topography = DTMParacou,
                                 relativeelevation  = RE, speciescriteria = Matrixspeciescriteria,
                                 volumeparameters = MatrixVolParam, scenario = "RIL1",
                                 crowndiameterparameters = MatrixCDparam),
               regexp = "The 'inventory', 'speciescriteria', 'volumeparameters' and 'crowndiameterparameters' arguments
         of the 'loggingsimulation' function must be data.frames")

  # topography, relativeelevation
  expect_error(loggingsimulation(Paracou6_2016, speciescriteria = SpeciesCriteria,
                                 volumeparameters = ForestZoneVolumeParametersTable,
                                 crowndiameterparameters = ParamCrownDiameterAllometry,
                                 scenario = "RIL1",
                                 topography = NULL,
                                 relativeelevation  = NULL),
               regexp = "The 'topography' and 'relativeelevation' arguments of the 'loggingsimulation' function must be RasterLayers")

  # scenario
  expect_error(loggingsimulation(Paracou6_2016, topography = DTMParacou,
                                 relativeelevation = RE, speciescriteria = SpeciesCriteria,
                                 volumeparameters = ForestZoneVolumeParametersTable,
                                 crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL"),
               regexp = "The 'scenario' argument of the 'loggingsimulation' function must be
         'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  # objective
  expect_error(loggingsimulation(Paracou6_2016, topography = DTMParacou,
                                 relativeelevation = RE, speciescriteria = SpeciesCriteria,
                                 volumeparameters = ForestZoneVolumeParametersTable,
                                 crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1", objective = "20"),
               regexp = "The 'objective' argument of the 'loggingsimulation' function must be numeric or NULL")

  # fuel
  expect_error(loggingsimulation(Paracou6_2016, topography = DTMParacou,
                                 relativeelevation = RE, speciescriteria = SpeciesCriteria,
                                 volumeparameters = ForestZoneVolumeParametersTable,
                                 crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1", fuel = "3"),
               regexp = "The 'fuel' argument of the 'loggingsimulation' function must be '0', '1', '2' or NULL")

  # diversification
  expect_error(loggingsimulation(Paracou6_2016, topography = DTMParacou,
                                 relativeelevation = RE, speciescriteria = SpeciesCriteria,
                                 volumeparameters = ForestZoneVolumeParametersTable,
                                 crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1", diversification = 2),
               regexp = "The 'diversification' argument of the 'loggingsimulation' function must be logical or NULL")

  # winching
  expect_error(loggingsimulation(Paracou6_2016, topography = DTMParacou,
                                 relativeelevation = RE, speciescriteria = SpeciesCriteria,
                                 volumeparameters = ForestZoneVolumeParametersTable,
                                 crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1", winching = "3"),
               regexp = "The 'winching' argument of the 'loggingsimulation' function must be '0', '1', '2' or NULL")

  # directionalfelling
  expect_error(loggingsimulation(Paracou6_2016, topography = DTMParacou,
                                 relativeelevation = RE, speciescriteria = SpeciesCriteria,
                                 volumeparameters = ForestZoneVolumeParametersTable,
                                 crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1", directionalfelling = "3"),
               regexp = "The 'directionalfelling' argument of the 'loggingsimulation' function must be '0', '1', '2' or NULL")


  # manual mode
  expect_error(loggingsimulation(Paracou6_2016, topography = DTMParacou,
                                 relativeelevation = RE, speciescriteria = SpeciesCriteria,
                                 volumeparameters = ForestZoneVolumeParametersTable,
                                 crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "manual"),
               regexp = "If you choose the 'manual' mode,
         you must fill in the arguments 'objective', 'fuel', 'diversification', 'winching' and 'directionalfelling'")


  # specieslax, objectivelax
  expect_error(loggingsimulation(Paracou6_2016, topography = DTMParacou,
                                 relativeelevation = RE, speciescriteria = SpeciesCriteria,
                                 volumeparameters = ForestZoneVolumeParametersTable,
                                 crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1",
                                 specieslax = 2, objectivelax = "yes"),
               regexp = "The 'specieslax' and 'objectivelax' arguments of the 'loggingsimulation' function must be logicals")


  # advancedloggingparameters
  expect_error(loggingsimulation(Paracou6_2016, topography = DTMParacou,
                                 relativeelevation = RE, speciescriteria = SpeciesCriteria,
                                 volumeparameters = ForestZoneVolumeParametersTable,
                                 crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1",
                                 advancedloggingparameters = FALSE),
               regexp = "The 'advancedloggingparameters' argument of the 'loggingsimulation' function must be a list")

  # iter,cores
  expect_error(loggingsimulation(Paracou6_2016, topography = DTMParacou,
                                 relativeelevation = RE, speciescriteria = SpeciesCriteria,
                                 volumeparameters = ForestZoneVolumeParametersTable,
                                 crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1",
                                 iter = FALSE, cores = FALSE),
               regexp = "The 'iter' and 'cores' arguments of the 'loggingsimulation' function must be numeric")





})

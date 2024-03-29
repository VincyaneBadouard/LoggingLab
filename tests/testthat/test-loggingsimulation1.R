test_that("loggingsimulation1", {

  # Data loading
  data(Paracou6_2016) # inventory
  data(PlotMask) # inventoried plot mask
  data(DTMParacou) # topography
  data(CreekDistances) # relative distances
  data(SpeciesCriteria) # species exploitability criteria
  data(ForestZoneVolumeParametersTable) # volume parameters
  data(ParamCrownDiameterAllometry) # parameters values of the crown diameter allometry

  # Test data
  Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:2000) # data reduction

  MatrixInventory <- as.matrix(Paracou6_2016)
  Matrixspeciescriteria <- as.matrix(SpeciesCriteria)
  MatrixVolParam <- as.matrix(ForestZoneVolumeParametersTable)
  MatrixCDparam <- as.matrix(ParamCrownDiameterAllometry)

  # Check the function arguments

  # inventory, speciescriteria, volumeparameters, crowndiameterparameters
  expect_error(loggingsimulation1(MatrixInventory, plotmask = PlotMask, topography = DTMParacou,
                                  creekverticaldistance = CreekDistances$distvert,
                                  creekhorizontaldistance = CreekDistances$disthorz,
                                  speciescriteria = Matrixspeciescriteria,
                                  volumeparameters = MatrixVolParam, scenario = "RIL1",
                                  crowndiameterparameters = MatrixCDparam),
               regexp = "The 'inventory', 'speciescriteria', 'volumeparameters' and 'crowndiameterparameters' arguments
         of the 'loggingsimulation' function must be data.frames")

  # plotmask
  expect_error(loggingsimulation1(Paracou6_2016, plotmask = data.frame(), topography = DTMParacou,
                                  creekverticaldistance = CreekDistances$distvert,
                                  creekhorizontaldistance = CreekDistances$disthorz,
                                  speciescriteria = SpeciesCriteria,
                                  volumeparameters = ForestZoneVolumeParametersTable, scenario = "RIL1",
                                  crowndiameterparameters = ParamCrownDiameterAllometry),
               regexp = "The 'plotmask' argument of the 'loggingsimulation' function must be a SpatialPolygons")

  # topography
  expect_error(loggingsimulation1(Paracou6_2016, plotmask = PlotMask,
                                  creekverticaldistance = CreekDistances$distvert,
                                  creekhorizontaldistance = CreekDistances$disthorz,
                                  speciescriteria = SpeciesCriteria,
                                  volumeparameters = ForestZoneVolumeParametersTable,
                                  crowndiameterparameters = ParamCrownDiameterAllometry,
                                  scenario = "RIL1",
                                  topography = NULL),
               regexp = "The 'topography' argument of the 'loggingsimulation' function must be a RasterLayer")

  # creekdistances
  expect_error(loggingsimulation1(Paracou6_2016, plotmask = PlotMask,
                                  creekverticaldistance = data.frame(),
                                  creekhorizontaldistance = CreekDistances$disthorz,
                                  speciescriteria = SpeciesCriteria,
                                  volumeparameters = ForestZoneVolumeParametersTable,
                                  crowndiameterparameters = ParamCrownDiameterAllometry,
                                  scenario = "RIL1",
                                  topography = DTMParacou),
               regexp = "The 'creekverticaldistance' argument of the 'loggingsimulation' function must be a RasterLayer")

  # scenario
  expect_error(loggingsimulation1(Paracou6_2016, plotmask = PlotMask, topography = DTMParacou,
                                  creekverticaldistance = CreekDistances$distvert,
                                  creekhorizontaldistance = CreekDistances$disthorz,
                                  speciescriteria = SpeciesCriteria,
                                  volumeparameters = ForestZoneVolumeParametersTable,
                                  crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL"),
               regexp = "The 'scenario' argument of the 'loggingsimulation' function must be
         'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  # objective
  expect_error(loggingsimulation1(Paracou6_2016, plotmask = PlotMask, topography = DTMParacou,
                                  creekverticaldistance = CreekDistances$distvert,
                                  creekhorizontaldistance = CreekDistances$disthorz,
                                  speciescriteria = SpeciesCriteria,
                                  volumeparameters = ForestZoneVolumeParametersTable,
                                  crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1", objective = "20"),
               regexp = "The 'objective' argument of the 'loggingsimulation' function must be numeric or NULL")

  # fuel
  expect_error(loggingsimulation1(Paracou6_2016, plotmask = PlotMask, topography = DTMParacou,
                                  creekverticaldistance = CreekDistances$distvert,
                                  creekhorizontaldistance = CreekDistances$disthorz,
                                  speciescriteria = SpeciesCriteria,
                                  volumeparameters = ForestZoneVolumeParametersTable,
                                  crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1", fuel = "3"),
               regexp = "The 'fuel' argument of the 'loggingsimulation' function must be '0', '1', '2' or NULL")

  # diversification
  expect_error(loggingsimulation1(Paracou6_2016, plotmask = PlotMask, topography = DTMParacou,
                                  creekverticaldistance = CreekDistances$distvert,
                                  creekhorizontaldistance = CreekDistances$disthorz,
                                  speciescriteria = SpeciesCriteria,
                                  volumeparameters = ForestZoneVolumeParametersTable,
                                  crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1", diversification = 2),
               regexp = "The 'diversification' argument of the 'loggingsimulation' function must be logical or NULL")

  # winching
  expect_error(loggingsimulation1(Paracou6_2016, plotmask = PlotMask, topography = DTMParacou,
                                  creekverticaldistance = CreekDistances$distvert,
                                  creekhorizontaldistance = CreekDistances$disthorz,
                                  speciescriteria = SpeciesCriteria,
                                  volumeparameters = ForestZoneVolumeParametersTable,
                                  crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1", winching = "3"),
               regexp = "The 'winching' argument of the 'loggingsimulation' function must be '0', '1', '2' or NULL")

  # directionalfelling
  expect_error(loggingsimulation1(Paracou6_2016, plotmask = PlotMask, topography = DTMParacou,
                                  creekverticaldistance = CreekDistances$distvert,
                                  creekhorizontaldistance = CreekDistances$disthorz,
                                  speciescriteria = SpeciesCriteria,
                                  volumeparameters = ForestZoneVolumeParametersTable,
                                  crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1", directionalfelling = "3"),
               regexp = "The 'directionalfelling' argument of the 'loggingsimulation' function must be '0', '1', '2' or NULL")


  # manual mode
  expect_error(loggingsimulation1(Paracou6_2016, plotmask = PlotMask, topography = DTMParacou,
                                  creekverticaldistance = CreekDistances$distvert,
                                  creekhorizontaldistance = CreekDistances$disthorz,
                                  speciescriteria = SpeciesCriteria,
                                  volumeparameters = ForestZoneVolumeParametersTable,
                                  crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "manual"),
               regexp = "If you choose the 'manual' mode,
         you must fill in the arguments 'objective', 'fuel', 'diversification', 'winching' and 'directionalfelling'")


  # specieslax, objectivelax
  expect_error(loggingsimulation1(Paracou6_2016, plotmask = PlotMask, topography = DTMParacou,
                                  creekverticaldistance = CreekDistances$distvert,
                                  creekhorizontaldistance = CreekDistances$disthorz,
                                  speciescriteria = SpeciesCriteria,
                                  volumeparameters = ForestZoneVolumeParametersTable,
                                  crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1",
                                  specieslax = 2, objectivelax = "yes"),
               regexp = "The 'specieslax' and 'objectivelax' arguments of the 'loggingsimulation' function must be logicals")


  # advancedloggingparameters
  expect_error(loggingsimulation1(Paracou6_2016, plotmask = PlotMask, topography = DTMParacou,
                                  creekverticaldistance = CreekDistances$distvert,
                                  creekhorizontaldistance = CreekDistances$disthorz,
                                  speciescriteria = SpeciesCriteria,
                                  volumeparameters = ForestZoneVolumeParametersTable,
                                  crowndiameterparameters = ParamCrownDiameterAllometry, scenario = "RIL1",
                                  advancedloggingparameters = FALSE),
               regexp = "The 'advancedloggingparameters' argument of the 'loggingsimulation' function must be a list")

  # expect_true(class(Rslt) == "list")

})

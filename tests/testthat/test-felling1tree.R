test_that("felling1tree", {

  #### Test data ####
  data(Paracou6_2016)
  data("HarvestablePolygons")
  data(MainTrails)
  # Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:2000)
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



  inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask),
                          volumeparameters = ForestZoneVolumeParametersTable)
  inventory <- suppressMessages(treeselection(inventory, objective = 20, scenario ="manual",
                                              fuel = "0", diversification = TRUE, specieslax = FALSE,
                                              objectivelax = TRUE, topography = DTMParacou,
                                              MainTrails = MainTrails, harvestablepolygons = HarvestablePolygons,
                                              plotslope = PlotSlope,
                                              speciescriteria = SpeciesCriteria,
                                              advancedloggingparameters = loggingparameters())$inventory)

  FutureReserveCrowns <- inventory %>% # create an object with future/reserve crowns only
    dplyr::filter(LoggingStatus == "future" | LoggingStatus == "reserve") %>%
    createcanopy() %>% # create all inventory crowns in the 'Crowns' column
    getgeometry(Crowns)


  inventory <- inventory %>%
    dplyr::filter(Selected == "1") %>%
    dplyr::select(idTree,DBH,TrunkHeight,TreeHeight,CrownHeight,
                  CrownDiameter,Selected, Xutm, Yutm)

  set.seed(1)
  dat <- inventory[1,] %>% # just 1 row (1 tree)
    tibble::add_column(TreeFellingOrientationSuccess = "1") # force the orientation success for the test

  #### Check the function arguments ####
  expect_error(felling1tree(MatrixInventory,
                            fuel = "0", winching = "0", directionalfelling = "2",
                            advancedloggingparameters = loggingparameters()),
               regexp = "The 'dat' argument of the 'felling1tree' function must be data.frame")

  expect_error(felling1tree(Paracou6_2016),
               regexp = "the data.frame given in the 'dat' argument
         of the 'felling1tree' function must contain only 1 row")

  expect_error(felling1tree(dat, winching = "0", directionalfelling = "2",
                            advancedloggingparameters = loggingparameters(),
                            fuel = TRUE),
               regexp = "The 'fuel' argument of the 'felling1tree' function must be '0', '1', '2' or NULL")

  expect_error(felling1tree(dat, fuel = "2", winching = "0",
                            advancedloggingparameters = loggingparameters(),
                            directionalfelling = TRUE),
               regexp = "The 'directionalfelling' argument of the 'felling1tree' function must be '0', '1', '2' or NULL")

  expect_error(felling1tree(dat,
                            fuel = "2", winching = "0", directionalfelling = "2",
                            advancedloggingparameters = loggingparameters(),
                            MainTrails =  MainTrails_no_sf, ScndTrail = ScndTrail_no_sf),
               regexp = "The 'MainTrails' and 'ScndTrail' arguments of the 'felling1tree' function must be sf")


  expect_error(felling1tree(dat, fuel = "2", winching = "0",
                            directionalfelling = "2", MainTrails = MainTrails, ScndTrail = ScndTrail,
                            advancedloggingparameters = as.matrix(loggingparameters())),
               regexp = "The 'advancedloggingparameters' argument of the 'felling1tree' function must be a list")

  #### Scanarios ####

  # Only to direct the foot of the tree towards the trail
  Rslt0 <- felling1tree(dat,
                        fuel = "0", winching = "0", directionalfelling = "0",
                        MainTrails = MainTrails, ScndTrail = ScndTrail,
                        FutureReserveCrowns = FutureReserveCrowns,
                        advancedloggingparameters = loggingparameters())


  # To direct the foot of the tree towards the trail + to avoid damage to future and reserve trees if possible
  Rslt1 <- felling1tree(dat,
                        fuel = "0", winching = "0", directionalfelling = "1",
                        MainTrails = MainTrails, ScndTrail = ScndTrail,
                        FutureReserveCrowns = FutureReserveCrowns,
                        advancedloggingparameters = loggingparameters())

  # To direct the foot of the tree towards the trail + to avoid damage to future and reserve trees if possible + orientation angle to the trail
  dat$Xutm <- 286537
  dat$Yutm <- 583070 # only to be sure to be able to avoid fut/res

  Rslt2 <- felling1tree(dat,
                        fuel = "0", winching = "1", directionalfelling = "2",
                        MainTrails = MainTrails, ScndTrail = ScndTrail,
                        FutureReserveCrowns = FutureReserveCrowns,
                        advancedloggingparameters = loggingparameters())

  # Grapple + cable without fuel wood exploitation
  ## Grapple case (tree < 6 m from the trail)
  dat$Xutm <- 286508
  dat$Yutm <- 582950

  Rslt2grapple <- felling1tree(dat,
                               fuel = "0", winching = "2", directionalfelling = "2",
                               MainTrails = MainTrails, ScndTrail = ScndTrail,
                               FutureReserveCrowns = FutureReserveCrowns,
                               advancedloggingparameters = loggingparameters())

  ## Cable case (tree > 6 m from the trail)
  dat$Xutm <- 286537
  dat$Yutm <- 583070

  Rslt2cable <- felling1tree(dat,
                             fuel = "0", winching = "2", directionalfelling = "2",
                             MainTrails = MainTrails, ScndTrail = ScndTrail,
                             FutureReserveCrowns = FutureReserveCrowns,
                             advancedloggingparameters = loggingparameters())



  # Fuel wood exploitation
  ## Grapple case (tree < 6 m from the trail)
  dat$Xutm <- 286508
  dat$Yutm <- 582950

  Rslt2fuelgrapple <- felling1tree(dat,
                                   fuel = "2", winching = "2", directionalfelling = "2",
                                   MainTrails = MainTrails, ScndTrail = ScndTrail,
                                   FutureReserveCrowns = FutureReserveCrowns,
                                   advancedloggingparameters = loggingparameters())

  ## Cable case (tree > 6 m from the trail)
  dat$Xutm <- 286537
  dat$Yutm <- 583070

  Rslt2fuelcable <- felling1tree(dat,
                                 fuel = "2", winching = "2", directionalfelling = "2",
                                 MainTrails = MainTrails, ScndTrail = ScndTrail,
                                 FutureReserveCrowns = FutureReserveCrowns,
                                 advancedloggingparameters = loggingparameters())


  # Grouping according to expected results
  ## 1-179° trail
  Rslts180 <- list(Rslt0 = Rslt0,
                   Rslt1 = Rslt1,
                   Rslt2grapple = Rslt2grapple,
                   Rslt2fuelgrapple = Rslt2fuelgrapple)
  ## 30-45° trail
  Rslts45 <- list(Rslt2 = Rslt2,
                  Rslt2cable = Rslt2cable,
                  Rslt2fuelcable = Rslt2fuelcable)

  ## avoid fut/res trees
  Rsltsfutres <- list(Rslt1 = Rslt1,
                      Rslt2 = Rslt2,
                      Rslt2grapple = Rslt2grapple,
                      Rslt2cable = Rslt2cable,
                      Rslt2fuelgrapple = Rslt2fuelgrapple,
                      Rslt2fuelcable = Rslt2fuelcable)


  #### Results class ####
  # $Foot is a POINT
  expect_s3_class(Rslt0$Foot, "POINT")

  # $NearestPoints is a sfc_LINESTRING,
  expect_s3_class(Rslt0$NearestPoints, "sfc_LINESTRING")

  # $TrailPt is a POINT,
  expect_s3_class(Rslt0$TrailPt, "POINT")

  # $FallenTree is a sfc_MULTIPOLYGON
  expect_s3_class(Rslt0$FallenTree, "sfc_MULTIPOLYGON")

  #### Check the angle between the tree and the trail ####

  for(rslt in Rslts45){

    rslt <- Rslt2fuelcable

    Arrival <- sf::st_point(as.numeric(unlist( # sfc to sfg
      sf::st_centroid(rslt$FallenTree))))

    OrientationA <- as.numeric(matlib::angle(c(rslt$TrailPt[1] - rslt$TrailPt[1], (rslt$TrailPt[2]+10) - rslt$TrailPt[2]),
                                             c(Arrival[1] - rslt$Foot[1], Arrival[2] - rslt$Foot[2]),
                                             degree = TRUE)) # vertical trail

    OrientationB <- as.numeric(matlib::angle(c(rslt$TrailPt[1] - rslt$TrailPt[1], rslt$TrailPt[2] - (rslt$TrailPt[2]+10)),
                                             c(Arrival[1] - rslt$Foot[1], Arrival[2] - rslt$Foot[2]),
                                             degree = TRUE)) # vertical trail

    OrientationC <- as.numeric(matlib::angle(c((rslt$TrailPt[1]+10) - rslt$TrailPt[1], rslt$TrailPt[2] - rslt$TrailPt[2]),
                                             c(Arrival[1] - rslt$Foot[1], Arrival[2] - rslt$Foot[2]),
                                             degree = TRUE)) # horizontal trail

    OrientationD <- as.numeric(matlib::angle(c(rslt$TrailPt[1] - (rslt$TrailPt[1]+10), rslt$TrailPt[2] - rslt$TrailPt[2]),
                                             c(Arrival[1] - rslt$Foot[1], Arrival[2] - rslt$Foot[2]),
                                             degree = TRUE)) # horizontal trail



    expect_true((OrientationA >= 29.9 & OrientationA <= 45) | (OrientationB >= 29.9 & OrientationB <= 45)|
                  (OrientationC >= 29.9 & OrientationC <= 45) | (OrientationD >= 29.9 & OrientationD <= 45))
  }


  for(rslt in Rslts180){

    Arrival <- sf::st_point(as.numeric(unlist( # sfc to sfg
      sf::st_centroid(rslt$FallenTree))))

    OrientationA <- as.numeric(matlib::angle(c(rslt$TrailPt[1] - rslt$TrailPt[1], (rslt$TrailPt[2]+10) - rslt$TrailPt[2]),
                                             c(Arrival[1] - rslt$Foot[1], Arrival[2] - rslt$Foot[2]),
                                             degree = TRUE))

    OrientationB <- as.numeric(matlib::angle(c(rslt$TrailPt[1] - rslt$TrailPt[1], rslt$TrailPt[2] - (rslt$TrailPt[2]+10)),
                                             c(Arrival[1] - rslt$Foot[1], Arrival[2] - rslt$Foot[2]),
                                             degree = TRUE))

    OrientationC <- as.numeric(matlib::angle(c((rslt$TrailPt[1]+10) - rslt$TrailPt[1], rslt$TrailPt[2] - rslt$TrailPt[2]),
                                             c(Arrival[1] - rslt$Foot[1], Arrival[2] - rslt$Foot[2]),
                                             degree = TRUE))

    OrientationD <- as.numeric(matlib::angle(c(rslt$TrailPt[1] - (rslt$TrailPt[1]+10), rslt$TrailPt[2] - rslt$TrailPt[2]),
                                             c(Arrival[1] - rslt$Foot[1], Arrival[2] - rslt$Foot[2]),
                                             degree = TRUE))


    expect_true((OrientationA > 0 & OrientationA < 180) | (OrientationB > 0 & OrientationB < 180)|
                  (OrientationC > 0 & OrientationC < 180) | (OrientationD > 0 & OrientationD < 180))

  }

  #### Check fut/res trees avoiding ####
  for(rslt in Rsltsfutres){

    overlaps <- sf::st_intersects(rslt$FallenTree,
                                  dplyr::summarise(FutureReserveCrowns, Crowns = sf::st_combine(Crowns))$Crowns) %>%
      lengths()

    expect_true(all(overlaps == 0))
  }

})


# library(ggplot2)
# ggplot() +
#   geom_sf(data = sf::st_set_crs(sf::st_sfc(Rslt2cable$Foot), sf::st_crs(MainTrails))) +
#   geom_sf(data = sf::st_set_crs(sf::st_as_sf(Rslt2cable$Trail), sf::st_crs(MainTrails))) +
#   geom_sf(data = Rslt2cable$NearestPoints) +
#   geom_sf(data = sf::st_set_crs(sf::st_sfc(Rslt2cable$TrailPt), sf::st_crs(MainTrails))) +
#   geom_sf(data = sf::st_set_crs(Rslt2cable$FallenTree, sf::st_crs(MainTrails))) +
#   geom_sf(data = sf::st_set_crs(FutureReserveCrowns, sf::st_crs(MainTrails))) # set a crs
# geom_sf(data = Arrival)



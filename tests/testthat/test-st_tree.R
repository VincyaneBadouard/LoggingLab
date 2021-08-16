test_that("st_tree", {

  # Test data
  data(Paracou6_2016)
  # Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:2000)
  MatrixInventory <- as.matrix(Paracou6_2016)

  pol1 <- list(matrix(c(286503, 583134,
                        286503, 583240,
                        286507, 583240,
                        286507, 583134,
                        286503, 583134)
                      ,ncol=2, byrow=TRUE))
  pol2 <- list(matrix(c(286650, 583134,
                        286650, 583240,
                        286654, 583240,
                        286654, 583134,
                        286650, 583134)
                      ,ncol=2, byrow=TRUE))

  PolList = list(pol1,pol2)
  ScndTrail <- st_multipolygon(PolList)
  MainTrail <- ScndTrail


  inventory <- addtreedim(inventorycheckformat(Paracou6_2016))
  inventory <- suppressMessages(treeselection(inventory, objective = 20, scenario ="manual",
                                              fuel = "2", diversification = TRUE, specieslax = FALSE,
                                              objectivelax = FALSE, DEM = DemParacou, plotslope = PlotSlope,
                                              speciescriteria = SpeciesCriteria,
                                              advancedloggingparameters = loggingparameters())$inventory)
  inventory <- inventory %>%
    dplyr::filter(Selected == "1") %>%
    dplyr::select(idTree,DBH,TrunkHeight,TreeHeight,CrownHeight,
                  CrownDiameter,Selected, Xutm, Yutm)
  dat <- inventory[1,] %>% # just 1 row (1 tree)
    add_column(TreeFellingOrientationSuccess = "1") # force the orientation success for the test

  Rslt <- st_tree(dat,
                  fuel = "0", directionalfelling = "2",
                  MainTrail = MainTrail, ScndTrail = ScndTrail,
                  advancedloggingparameters = loggingparameters())


  # Check the function arguments
  expect_error(st_tree(MatrixInventory,
                       fuel = "0", directionalfelling = "2",
                       advancedloggingparameters = loggingparameters()),
               regexp = "The 'dat' argument of the 'st_tree' function must be data.frame")

  expect_error(st_tree(Paracou6_2016),
               regexp = "the data.frame given in the 'dat' argument
         of the 'st_tree' function must contain only 1 row")

  expect_error(st_tree(dat, directionalfelling = "2",
                       advancedloggingparameters = loggingparameters(),
                       fuel = TRUE),
               regexp = "The 'fuel' argument of the 'st_tree' function must be '0', '1', '2' or NULL")

  expect_error(st_tree(dat, fuel = "2",
                       advancedloggingparameters = loggingparameters(),
                       directionalfelling = TRUE),
               regexp = "The 'directionalfelling' argument of the 'st_tree' function must be '0', '1', '2' or NULL")

  expect_error(st_tree(dat,
                       fuel = "2", directionalfelling = "2",
                       advancedloggingparameters = loggingparameters(),
                       MainTrail = st_as_text(MainTrail), ScndTrail = st_as_text(ScndTrail)),
               regexp = "The 'MainTrail' and 'ScndTrail' arguments of the 'st_tree' function must be sfg")


  expect_error(st_tree(dat, fuel = "2",
                       directionalfelling = "2", MainTrail = MainTrail, ScndTrail = ScndTrail,
                       advancedloggingparameters = as.matrix(loggingparameters())),
               regexp = "The 'advancedloggingparameters' argument of the 'st_tree' function must be a list")

  # Results class:
  ## $Foot is a POINT
  expect_s3_class(Rslt$Foot, "POINT")

  ## $NearestPoints is a LINESTRING,
  expect_s3_class(Rslt$NearestPoints, "sfc_LINESTRING")

  ## $TrailPt is a POINT,
  expect_s3_class(Rslt$TrailPt, "POINT")

  ## $A is multipolygons
  expect_s3_class(Rslt$A, "sfc_MULTIPOLYGON")

  # Check the angle between the tree and the trail

  Arrival <- st_point(as.numeric(unlist( # sfc to sfg
    sf::st_centroid(Rslt$A))))

  Orientation <- as.numeric(matlib::angle(c(Rslt$TrailPt[1] - Rslt$TrailPt[1], (Rslt$TrailPt[2]+10) - Rslt$TrailPt[2]),
                           c(Arrival[1] - Rslt$Foot[1], Arrival[2] - Rslt$Foot[2]),
                           degree = TRUE))


  expect_true(Orientation >= 30 & Orientation <= 45)
})


# check args errors
# results:
# $Foot is a POINT,
# $NearestPoints is a LINESTRING,
# $TrailPt is a POINT,
# $Trail is a multipolygon,
# $A is multipolygons
# Check the angle between the tree and the trail

# ggplot() +
#   geom_sf(data = Rslt$Foot) +
#   geom_sf(data = Rslt$Trail) +
#   geom_sf(data = Rslt$NearestPoints) +
#   geom_sf(data = Rslt$TrailPt) +
#   geom_sf(data = Rslt$A)+
#   geom_sf(data = Arrival)



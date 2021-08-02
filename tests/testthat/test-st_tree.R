test_that("st_tree", {

  # Test data
  data(Paracou6_2016)
  # Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:2000)
  MatrixInventory <- as.matrix(Paracou6_2016)

  MainTrail <- sf::st_linestring(matrix(c(286400, 583130,
                                          286400, 583250,
                                          286655, 583250,
                                          286655, 583130,
                                          286400, 583130)
                                        ,ncol=2, byrow=TRUE))

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


  inventory <- addtreedim(inventorycheckformat(Paracou6_2016))
  inventory <- suppressMessages(treeselection(inventory, objective = 20, scenario ="manual",
                             fuel = "2", diversification = TRUE, specieslax = FALSE,
                             objectivelax = FALSE, DEM = DemParacou, plotslope = PlotSlope,
                             speciescriteria = SpeciesCriteria,
                             advancedloggingparameters = loggingparameters())$inventory)
  inventory <- successfail(inventory,fuel = "2",directionalfelling = "2",
                           advancedloggingparameters = loggingparameters())
  inventory <- inventory %>%
    dplyr::filter(Selected == "1") %>%
    dplyr::select(idTree,DBH,TrunkHeight,TreeHeight,CrownHeight,
                  CrownDiameter,Selected, Xutm, Yutm, TreeFellingOrientationSuccess)
  dat <- inventory[1,]

  Rslt <- st_tree(dat,
                  fuel = "0", directionalfelling = "2",
                  MainTrail = MainTrail, ScndTrail = ScndTrail,
                  advancedloggingparameters = loggingparameters())


  # Check the function arguments
  expect_error(st_tree(MatrixInventory,
                       fuel = "0", directionalfelling = "2",
                       advancedloggingparameters = loggingparameters()),
               regexp = "The 'dat' argument of the 'st_tree' function must be data.frame")

  expect_error(st_tree(Paracou6_2016, directionalfelling = "2",
                       advancedloggingparameters = loggingparameters(),
                       fuel = TRUE),
               regexp = "The 'fuel' argument of the 'st_tree' function must be '0', '1', '2' or NULL")

  expect_error(st_tree(Paracou6_2016, fuel = "2",
                       advancedloggingparameters = loggingparameters(),
                       directionalfelling = TRUE),
               regexp = "The 'directionalfelling' argument of the 'st_tree' function must be '0', '1', '2' or NULL")

  expect_error(st_tree(Paracou6_2016,
                       fuel = "2", directionalfelling = "2",
                       advancedloggingparameters = loggingparameters(),
                       MainTrail = st_as_text(MainTrail), ScndTrail = st_as_text(ScndTrail)),
               regexp = "The 'MainTrail' and 'ScndTrail' arguments of the 'st_tree' function must be sfg")


  expect_error(st_tree(Paracou6_2016, fuel = "2",
                       directionalfelling = "2", MainTrail = MainTrail, ScndTrail = ScndTrail,
                       advancedloggingparameters = as.matrix(loggingparameters())),
               regexp = "The 'advancedloggingparameters' argument of the 'st_tree' function must be a list")

  # results:
  ## $Foot is a POINT
  expect_s3_class(Rslt$Foot, "POINT")

  ## $NearestPoints is a LINESTRING,
  expect_s3_class(Rslt$NearestPoints, "sfc_LINESTRING")

  ## $TrailPt is a POINT,
  expect_s3_class(Rslt$TrailPt, "POINT")

  ## $Trail is a multipolygon,
  expect_s3_class(Rslt$Trail, "MULTIPOLYGON") # 'XY'/'GEOMETRYCOLLECTION'/'sfg'

  ## $A is multipolygons
  expect_s3_class(Rslt$A, "sfc_MULTIPOLYGON")


  # check the angle


})


# check args errors
# results:
# $Foot is a POINT,
# $NearestPoints is a LINESTRING,
# $TrailPt is a POINT,
# $Trail is a multipolygon,
# $A is multipolygons
# check the angle

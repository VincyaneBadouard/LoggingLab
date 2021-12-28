test_that("cleaninventory", {

  # Data loading
  data(Paracou6_2016) # inventory
  data(PlotMask) # the inventoried plot mask

  MatrixInventory <- as.matrix(Paracou6_2016)


  # Check the function arguments

  expect_error(cleaninventory(as.matrix(Paracou6_2016), PlotMask, loggingparameters()),
               regexp = "The 'inventory' argument must be a data.frame")

  expect_error(cleaninventory(Paracou6_2016, sf::st_as_sf(PlotMask), loggingparameters()),
               regexp = "The 'plotmask' argument must be a SpatialPolygonsDataFrame")

  expect_error(cleaninventory(Paracou6_2016, PlotMask, FALSE),
               regexp = "The 'advancedloggingparameters' argument must be a list")


  # Create the test inventory
  TestInventory <- Paracou6_2016
  TestInventory[10, "CodeAlive"] <- FALSE # Some FALSE in CodeAlive
  TestInventory[4, "CircCorr"] <- 6 # DBH < 10
  CleanedInventory <- cleaninventory(TestInventory, PlotMask)

  # Check if there are only alive trees
  expect_true(all(CleanedInventory$CodeAlive == "TRUE"))

  # Check if mimimun DBH is respected (check if DBH exists in the same way)
  expect_true(all(CleanedInventory$DBH >= 10 & CleanedInventory$DBH < 900))#the largest known tree: 825 cm

  expect_false(any(is.na(CleanedInventory$DBH))) #check that all the DBH are computed

  #### Check if all the trees are in the plot ####

  plotmask <- sf::st_as_sf(PlotMask) # sp to sf (sfc_POLYGON)

  inventory <- Paracou6_2016 %>%
    dplyr::filter(CodeAlive == "TRUE") %>%
    dplyr::mutate(DBH = ifelse(is.na(CircCorr), Circ/pi, CircCorr/pi)) %>%
    dplyr::filter(DBH >= 10)

  inventory <- sf::st_as_sf(inventory, coords = c("Xutm", "Yutm")) %>% # as sf
    sf::st_set_crs(sf::st_crs(plotmask)) # set crs


  TreesIn <- suppressWarnings(sf::st_intersection(inventory, plotmask)) %>%
    tibble::add_column(TreesIn = "1") %>% # trees in the plot
    dplyr::select(idTree, TreesIn)

  sf::st_geometry(TreesIn) <- NULL # sf to data.frame
  TreesIn <- unique(TreesIn) # to be sure

  expect_true(nrow(cleaninventory(Paracou6_2016, PlotMask)) == nrow(TreesIn))

  expect_true(class(CleanedInventory) != "sf")
  expect_true("Xutm" %in% names(CleanedInventory))
  expect_true("Yutm" %in% names(CleanedInventory))


  })

# The inventory with only alive trees within the inventoried plot

test_that("addtreedim", {

  # Check the function argument

  ## check the database class
  data(Paracou6_2016)
  Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:1000)

  MatrixInventory <- as.matrix(Paracou6_2016)
  crowndiameterparameters <- as.matrix(ParamCrownDiameterAllometry)
  volumeparameters <- as.matrix(ForestZoneVolumeParametersTable)

  expect_error(addtreedim(MatrixInventory, crowndiameterparameters, volumeparameters), regexp = "The function arguments must be data.frames")

  ## check the class argument for "advancedloggingparameters"
  Matrixloggingparameters <- as.matrix(loggingparameters())
  expect_error(addtreedim(Paracou6_2016, advancedloggingparameters = Matrixloggingparameters), regexp = "The 'advancedloggingparameters' argument of the 'addtreedim' function must be a list")

  # Test data preparation
  testinventory <- Paracou6_2016 %>% # compute the new inventory
  inventorycheckformat() %>%
    addtreedim()

  TestList <- list( # list the variables to check
    testinventory$TreeHeight,
    testinventory$TreeHarvestableVolume,
    testinventory$TrunkHeight,
    testinventory$CrownHeight,
    testinventory$CrownDiameter)

  # check their class (integer)
  lapply(TestList,
         function(element) expect_type(element, "double"))

  # check that variables are not empty, or contains NAs
  lapply(TestList,
         function(element) expect_false(any(is.na(element))))

  # check if formulas are respected
  expect_true(all(testinventory$TreeHarvestableVolume == # ONF's volume formula
                    testinventory$aCoef + testinventory$bCoef * (testinventory$DBH/100)^2))

  expect_true(all(testinventory$TreeHeight == # BIOMASS's model
                    exp(0.07359191 + 1.34241216*log(testinventory$DBH) + -0.12282344*log(testinventory$DBH)^2)))


  expect_true(all(testinventory$TrunkHeight # Cylinder volume formula
                  == testinventory$TreeHarvestableVolume/(pi*(((testinventory$DBH/100)/2)^2))))


  expect_true(all(testinventory$CrownHeight == testinventory$TreeHeight - testinventory$TrunkHeight))

  # expect_true(all(testinventory$CrownDiameter # dont work
  #                 == exp(((log(testinventory$DBH)- testinventory$alpha - rnorm(length(testinventory$DBH), 0, 0.0295966977))/testinventory$beta))/testinventory$TreeHeight))


  # expect_true(all(testinventory$TreeHarvestableVolume ==
  #                   (pi*(((testinventory$DBH/100)/2)^2)) * testinventory$TrunkHeight)) # dont work
  #
  # expect_true(all(log(testinventory$DBH) #neither
  #                 == testinventory$alpha + testinventory$beta*log(testinventory$TreeHeight*testinventory$CrownDiameter) + rnorm(length(testinventory$DBH), 0, 0.0295966977)))

  # check coherence
  expect_true(all(0 < testinventory$TreeHarvestableVolume & testinventory$TreeHarvestableVolume < 1000))
  expect_true(all(0 < testinventory$TrunkHeight & testinventory$TrunkHeight < 100))
  expect_true(all(0 < testinventory$TreeHeight & testinventory$TreeHeight < 200))# the tallest known tree: 115.55 m
  expect_true(all(0 < testinventory$CrownHeight & testinventory$CrownHeight < 100))
  expect_true(all(0 < testinventory$CrownDiameter & testinventory$CrownDiameter < 200))

  expect_true(nrow(inventorycheckformat(Paracou6_2016)) == nrow(testinventory))


})


# compute for all trees:
# + TreeHeight
# + TreeHarvestableVolume : volume exploitable à partir du tarif de cubage (= a + b*DBH2) que représente chaque arbre (a et b dépendent de la localisation)
# + TrunkHeight : hauteur de fût (TreeHarvestableVolume  = π(DBH/2)² x TrunkHeight)
# + CrownDiameter : diamètre de couronne (CD) (ln(D) = 𝜶+ 𝜷 ln(H*CD) + 𝜺 (allométries de Mélaine)
# + CrownHeight : TreeHeight - TrunkHeight

# -> check if column exist
# -> check their class (double)
# -> is not empty, or contains NA's
# -> check if formulas are respected
# -> check coherence


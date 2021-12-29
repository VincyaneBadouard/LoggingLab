test_that("treefromthesky", {

  # Test data
  data(Paracou6_2016)
  inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask), volumeparameters = ForestZoneVolumeParametersTable)

  dat <- inventory[1,]
  MatrixInventory <- as.matrix(dat)
  NoCrown <- Paracou6_2016[1,]

  Rslt <- treefromthesky(dat)



  # Check the function arguments
  expect_error(treefromthesky(MatrixInventory),
               regexp = "The 'dat' argument of the 'treefromthesky' function must be a data.frame")

  expect_error(treefromthesky(inventory),
               regexp = "the data.frame given in the 'dat' argument
         of the 'treefromthesky' function must contain only 1 row")

  expect_error(treefromthesky(NoCrown),
               regexp = "CrownDiameter is not part of the dat columns")


  # Results class:
  expect_s3_class(Rslt, "sfc_POLYGON")

  })

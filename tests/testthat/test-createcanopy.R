test_that("createcanopy", {

  # Test data
  data(Paracou6_2016)
  Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:10)
  inventory <- addtreedim(inventorycheckformat(Paracou6_2016), volumeparameters = ForestZoneVolumeParametersTable)

  MatrixInventory <- as.matrix(Paracou6_2016)

  Rslt <- createcanopy(inventory)

  # Check the function arguments
  expect_error(createcanopy(MatrixInventory),
               regexp = "The 'inventory' argument of the 'createcanopy' function must be a data.frame")

  expect_error(createcanopy(Paracou6_2016),
               regexp = "CrownDiameter is not part of the inventory columns")


  # Check output
  expect_s3_class(Rslt, "data.frame")
  expect_type(Rslt$Crowns, "character")

  })


# ggplot() +
#   geom_sf(data = getgeometry(Rslt, Crowns),
#           aes(alpha = TreeHeight),
#           fill = "forestgreen")

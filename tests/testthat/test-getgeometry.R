test_that("getgeometry ", {

  # Test data
  data(Paracou6_2016)
  # Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:2000)
  MatrixInventory <- as.matrix(Paracou6_2016)

  txtinvent <- Paracou6_2016 %>%
    filter(VernName == "wacapou") %>%
      group_by(idTree) %>%
      do(Localisation = # inform geometry.
           sf::st_point(c(.$Xutm,.$Yutm)) %>%
           sf::st_as_text()) %>%
      tidyr::unnest(Localisation) # here to pass from list to character

    inventory <- left_join(Paracou6_2016, txtinvent, by = "idTree")

    Rslt <- getgeometry (inventory, Localisation)

  # Check the function arguments
  expect_error(getgeometry (MatrixInventory, var),
               regexp = "The 'inventory' argument of the 'getgeometry ' function must be data.frame")

  # expect_error(getgeometry (inventory, var = "Localisation"),
  #              regexp = "The 'var' argument of the 'getgeometry ' function must be a variable of your data.frame")


  # expect_error(getgeometry (inventory, Circ),
  #              regexp = "The column filled in the 'var' argument of the 'getgeometry ' function must be of type character")

  # column is sfc
  expect_s3_class(Rslt$Localisation, "sfc")

  # inventory is an sf object
  expect_s3_class(Rslt, "sf")

})


# check args et class de la col
# column is sfc
# inventory is an sf object

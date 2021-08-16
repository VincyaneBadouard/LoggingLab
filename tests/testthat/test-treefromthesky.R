test_that("treefromthesky", {

  # Test data
  data(Paracou6_2016)
  inventory <- addtreedim(inventorycheckformat(Paracou6_2016))

  dat <- inventory[1,]
  MatrixInventory <- as.matrix(dat)

  Rslt <- treefromthesky(dat)



  # Check the function arguments
  expect_error(treefromthesky(MatrixInventory),
               regexp = "The 'dat' argument of the 'treefromthesky' function must be a data.frame")

  expect_error(treefromthesky(inventory),
               regexp = "the data.frame given in the 'dat' argument
         of the 'treefromthesky' function must contain only 1 row")


  # Results class:
  expect_s3_class(Rslt, "sfc_POLYGON")

  })

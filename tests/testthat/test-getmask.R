test_that("getmask", {
  data(Paracou6_2016)
  data(DTMParacou)
  mask <- getmask(Paracou6_2016, DTMParacou)
  # See issue #67 for possible error
  expect_true(inherits(mask, "SpatialPolygons"))
})

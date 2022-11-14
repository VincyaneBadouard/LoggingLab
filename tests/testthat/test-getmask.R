test_that("getmask", {
  data(Paracou6_2016)
  data(DTMParacou)
  mask <- getmask(Paracou6_2016, DTMParacou)
  expect_true(inherits(mask, "SpatialPolygonsDataFrame"))
})

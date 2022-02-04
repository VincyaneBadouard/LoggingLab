test_that("maintrailextract", {
  data("DTMParacou")
  data("PlotMask")
  expect_error(maintrailextract(1),
               regexp = "The 'topography' arguments of the 'maintrailextract' function must be RasterLayer")


  Maintrails <-  maintrailextract(DTMParacou)
  expect_true(inherits(Maintrails$geometry,"sfc_LINESTRING"))

  expect_false(st_intersects(Maintrails,st_as_sf(PlotMask),sparse = FALSE))

})

test_that("harvestableareadefinition",{

  data(DTMParacou)
  data(VerticalCreekHeight)

  test_Harvestable <- harvestableareadefinition(topography = DTMParacou,
                                                verticalcreekheight = VerticalCreekHeight)
  test_HarvestablePolygons <- test_Harvestable[[1]]
  test_PlotSlope <- test_Harvestable[[2]]
  test_HarvestableArea <- test_Harvestable[[3]]


  expect_s3_class(test_HarvestablePolygons, class = 'sf')
  expect_true((all(0 <= test_HarvestablePolygons$Harvestable & test_HarvestablePolygons$Harvestable <= 1)))


  expect_s4_class(test_PlotSlope, class = 'RasterLayer')
  expect_type(test_HarvestableArea, 'double')

})

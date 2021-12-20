test_that("HarvestableAreaDefinition", {

  data(DTMParacou)
  data(VerticalCreekHeight)

  test_Harvestable <- HarvestableAreaDefinition(topography = DTMParacou,
                                                verticalcreekheight = VerticalCreekHeight)
test_HarvestablePolygons <- test_Harvestable[[1]]

  expect_s3_class(test_HarvestablePolygons, class = 'data.frame')
  expect_true((all(0 <= test_HarvestablePolygons$Harvestable &  test_HarvestablePolygons$Harvestable <= 1)))
})

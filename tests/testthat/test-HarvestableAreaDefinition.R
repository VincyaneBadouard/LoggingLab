test_that("harvestableareadefinition",{

  data(DTMParacou)
  data(CreekDistances)
  data(MainTrails)
  data(PlotMask)


  test_Harvestable0 <- harvestableareadefinition(topography = DTMParacou,
                                                 creekverticaldistance = CreekDistances$distvert,
                                                 creekhorizontaldistance = CreekDistances$disthorz,
                                                 maintrails = MainTrails,
                                                 plotmask = PlotMask,
                                                 scenario = "manual",
                                                 winching = "0")

  test_HarvestablePolygons0 <- test_Harvestable0$HarvestablePolygons
  test_PlotSlope0 <- test_Harvestable0$PlotSlope
  test_HarvestableArea0 <- test_Harvestable0$HarvestableArea

  test_Harvestable1 <- harvestableareadefinition(topography = DTMParacou,
                                                 creekverticaldistance = CreekDistances$distvert,
                                                 creekhorizontaldistance = CreekDistances$disthorz,
                                                 maintrails = MainTrails,
                                                 plotmask = PlotMask,
                                                 scenario = "RIL2")

  test_HarvestablePolygons1 <- test_Harvestable1$HarvestablePolygons
  test_PlotSlope1 <- test_Harvestable1$PlotSlope
  test_HarvestableArea1 <- test_Harvestable1$HarvestableArea

  test_Harvestable2 <- harvestableareadefinition(topography = DTMParacou,
                                                 creekverticaldistance = CreekDistances$distvert,
                                                 creekhorizontaldistance = CreekDistances$disthorz,
                                                 maintrails = MainTrails,
                                                 plotmask = PlotMask,
                                                 scenario = "manual",
                                                 winching = "2")

  test_HarvestablePolygons2 <- test_Harvestable2$HarvestablePolygons
  test_PlotSlope2 <- test_Harvestable2$PlotSlope
  test_HarvestableArea2 <- test_Harvestable2$HarvestableArea



  expect_s3_class(test_HarvestablePolygons0, class = 'sfc_MULTIPOLYGON')
  expect_s3_class(test_HarvestablePolygons1, class = 'sfc_MULTIPOLYGON')
  expect_s3_class(test_HarvestablePolygons2, class = 'sfc_MULTIPOLYGON')

  expect_s4_class(test_PlotSlope1, class = 'RasterLayer')
  expect_type(test_HarvestableArea2, 'double')

})

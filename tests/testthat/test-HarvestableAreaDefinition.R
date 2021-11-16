test_that("HarvestableAreaDefinition", {
  data(Plots)
  data(DTMParacou)
  data(VerticalCreekHeight)

  test_exploitPolygones <- HarvestableAreaDefinition(plot = Plots,
                                                     dtm = DTMParacou,
                                                     verticalcreekheight = VerticalCreekHeight,
                                                     advancedloggingparameters = loggingparameters())

  expect_s3_class(test_exploitPolygones, class = 'data.frame')
  expect_true((all(0 <= test_exploitPolygones$Exploit &  test_exploitPolygones$Exploit <= 1)))
})

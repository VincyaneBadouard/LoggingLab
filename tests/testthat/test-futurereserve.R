test_that("futurereserve", {

  inventory <- ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016))))

  harvestableOutputs <- harvestable(inventory, diversification = TRUE, specieslax = FALSE,
                                    DEM = DemParacou, plotslope = PlotSlope, otherloggingparameters = loggingparameters())

  inventory <- harvestableOutputs$inventory
  HVinit <- harvestableOutputs$HVinit

  inventory <- selected(inventory, type = "manual", fuel = "0", diversification = TRUE, specieslax = FALSE, objectivelax = FALSE,
                        otherloggingparameters = loggingparameters(), VO = 80, HVinit = HVinit)$inventory

  testinventory <- futurereserve(inventory)

  otherloggingparameters = loggingparameters()

  # Future = Commercial == "1" & (DBH >= otherloggingparameters$FutureTreesMinDiameter & DBH < MinFD)
  FutureTrees <- testinventory %>%
    filter(LoggingStatus == "future")

  expect_true(all(FutureTrees$Commercial == "1"))
  expect_true(all(FutureTrees$DBH >= otherloggingparameters$FutureTreesMinDiameter & FutureTrees$DBH < FutureTrees$MinFD))

  # Reserve
  ReserveTrees <- testinventory %>%
    filter(LoggingStatus =="reserve")

  expect_true(all(ReserveTrees$Commercial == "1")) # = Commercial == "1"
  expect_true(all(ReserveTrees$DBH >= otherloggingparameters$FutureTreesMinDiameter
                  & ReserveTrees$DBH < ReserveTrees$MinFD))# = Future trees


  # as many as the number of trees exploited
  expect_true(
    sum(as.numeric(testinventory$Selected == "1"), na.rm = TRUE)
    == sum(as.numeric(testinventory$LoggingStatus =="reserve"), na.rm = TRUE)
  )

})




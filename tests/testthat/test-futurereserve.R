test_that("futurereserve", {

  data(Paracou6_2016)
  data(DTMParacou)
  data(HarvestablePolygons)
  data(MainTrails)

  inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask), volumeparameters = ForestZoneVolumeParametersTable)
  inventory <- ONFGuyafortaxojoin(inventory, SpeciesCriteria)

  harvestableOutputs <- harvestable(inventory, diversification = TRUE, specieslax = FALSE,
                                    topography = DTMParacou, plotslope = PlotSlope,
                                    advancedloggingparameters = loggingparameters(),
                                    MainTrails = MainTrails, harvestablepolygons = HarvestablePolygons)

  inventory <- harvestableOutputs$inventory
  HVinit <- harvestableOutputs$HVinit

  inventory <- suppressMessages(selected(inventory, scenario = "manual", fuel = "0",
                                         diversification = TRUE, specieslax = FALSE, objectivelax = TRUE,
                                         topography = DTMParacou,
                                         advancedloggingparameters = loggingparameters(),
                                         VO = 125, HVinit = HVinit)$inventory)

  testinventory <- futurereserve(inventory, SpeciesCriteria)

  advancedloggingparameters = loggingparameters()

  # Future = Commercial == "1"
  FutureTrees <- testinventory %>%
    filter(LoggingStatus == "future")


  expect_true(all(FutureTrees$Commercial == "1"
                  & (
                    (FutureTrees$Up == "0" &
                       (FutureTrees$DBH >= advancedloggingparameters$FutureTreesMinDiameter & FutureTrees$DBH < FutureTrees$MinFD))
                    | (FutureTrees$Up == "1" &
                         (FutureTrees$DBH >= advancedloggingparameters$FutureTreesMinDiameter & FutureTrees$DBH < FutureTrees$UpMinFD)))))

  # Reserve
  ReserveTrees <- testinventory %>%
    filter(LoggingStatus =="reserve")

  expect_true(all(ReserveTrees$Commercial == "1"
                  & (
                    (ReserveTrees$Up == "0" &
                       (ReserveTrees$DBH >= advancedloggingparameters$FutureTreesMinDiameter & ReserveTrees$DBH < ReserveTrees$MinFD))
                    | (ReserveTrees$Up == "1" &
                         (ReserveTrees$DBH >= advancedloggingparameters$FutureTreesMinDiameter & ReserveTrees$DBH < ReserveTrees$UpMinFD)))))


  # as many as the number of trees exploited (if possible)
  StemNbr <- sum(as.numeric(testinventory$Selected == "1"), na.rm = TRUE)
  FutureNbr <- sum(as.numeric(testinventory$LoggingStatus == "future"), na.rm = TRUE)

  if(FutureNbr != 0){ # if if there are still Futures
  expect_true(
    sum(as.numeric(testinventory$Selected == "1"), na.rm = TRUE)
    == sum(as.numeric(testinventory$LoggingStatus =="reserve"), na.rm = TRUE)
  )
  }
  if(FutureNbr == 0){ # if there are no more futures
    expect_true(any(testinventory$LoggingStatus == "reserve")) # all the Futures are Reserves
  }

})




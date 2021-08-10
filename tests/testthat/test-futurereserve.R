test_that("futurereserve", {

  data(Paracou6_2016)
  Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:1000)

  data(DemParacou)

  inventory <- ONFGuyafortaxojoin(addtreedim(inventorycheckformat(Paracou6_2016)))

  harvestableOutputs <- harvestable(inventory, diversification = TRUE, specieslax = FALSE,
                                    DEM = DemParacou, plotslope = PlotSlope, advancedloggingparameters = loggingparameters())

  inventory <- harvestableOutputs$inventory
  HVinit <- harvestableOutputs$HVinit

  inventory <- suppressMessages(selected(inventory, scenario = "manual", fuel = "0", diversification = TRUE, specieslax = FALSE, objectivelax = FALSE,
                                         DEM = DemParacou, advancedloggingparameters = loggingparameters(), VO = 20, HVinit = HVinit)$inventory)

  testinventory <- futurereserve(inventory)

  advancedloggingparameters = loggingparameters()

  # Future = Commercial == "1" &
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


  # as many as the number of trees exploited
  expect_true(
    sum(as.numeric(testinventory$Selected == "1"), na.rm = TRUE)
    == sum(as.numeric(testinventory$LoggingStatus =="reserve"), na.rm = TRUE)
  )

})




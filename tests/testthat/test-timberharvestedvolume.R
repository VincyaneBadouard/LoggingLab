test_that("timberharvestedvolume", {

  # Data loading
  data(Paracou6_2016)
  data(HarvestableAreaOutputsCable)

  inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask),
                          volumeparameters = ForestZoneVolumeParametersTable)

  inventory <- suppressMessages(treeselection(inventory, topography = DTMParacou,
                                              speciescriteria = SpeciesCriteria,
                                              scenario ="manual", objective = 20,
                                              fuel = "2", diversification = TRUE,
                                              winching = "0",
                                              specieslax = FALSE, objectivelax = TRUE,
                                              harvestablearea = HarvestableAreaOutputsCable$HarvestableArea,
                                              harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons,
                                              plotslope =  HarvestableAreaOutputsCable$PlotSlope,
                                              advancedloggingparameters = loggingparameters()
  )$inventory)



  # Test data
  MatrixInventory <- as.matrix(Paracou6_2016)

  Rslt <- timberharvestedvolume(inventory, scenario = "manual", fuel = "0", # fuel !="2"
                                advancedloggingparameters = loggingparameters())

  RsltHollow <- timberharvestedvolume(inventory, scenario = "manual", fuel = "2", # fuel =="2"
                                      advancedloggingparameters = loggingparameters())

  LoggedTable <- inventory %>%
    filter(Selected == "1" & ProbedHollow == "0") # Healthy logged trees

  Healthy <- sum(LoggedTable$TreeHarvestableVolume)


  HollowTable <- inventory %>%
    filter(Selected == "1" & ProbedHollow == "1") # probed hollow trees

  advancedloggingparameters = loggingparameters()

  # Check the function arguments

  expect_error(timberharvestedvolume(MatrixInventory),
               regexp = "The 'inventory' argument of the 'timberharvestedvolume' function must be a data.frame")

  expect_error(timberharvestedvolume(inventory, scenario = "RIL"),
               regexp = "The 'scenario' argument of the 'timberharvestedvolume' function must be
         'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  expect_error(timberharvestedvolume(inventory, scenario = "manual", fuel = TRUE),
               regexp = "The 'fuel' argument of the 'timberharvestedvolume' function must be '0', '1', '2' or NULL")

  expect_error(timberharvestedvolume(inventory, scenario = "manual", fuel = "2",
                                     advancedloggingparameters = 20),
               regexp = "The 'advancedloggingparameters' argument of the 'timberharvestedvolume' function must be a list")

  # fuel !="2"
  expect_true(Rslt$NoHollowTimberLoggedVolume == Rslt$TimberLoggedVolume) # no hollow trees exploitation
  expect_true(Rslt$TimberLoggedVolume == Healthy) # the selected trees volume

  # fuel =="2"
  expect_true(RsltHollow$NoHollowTimberLoggedVolume == Healthy) # the selected healthy trees volume

  if(nrow(HollowTable) > 0)
    expect_true(RsltHollow$TimberLoggedVolume == sum(RsltHollow$NoHollowTimberLoggedVolume +
                                                       (1-advancedloggingparameters$TreeHollowPartForFuel) *
                                                       sum(HollowTable$TreeHarvestableVolume)))

  if(nrow(HollowTable) == 0)
    expect_true(RsltHollow$TimberLoggedVolume == RsltHollow$NoHollowTimberLoggedVolume) # no probed hollow trees

})

# fuel !="2"
## NoHollowTimberLoggedVolume = TimberLoggedVolume = sum(LoggedTable$TreeHarvestableVolume)

# fuel =="2"
## TimberLoggedVolume =
### - if(nrow(HollowTable) > 0): sum(NoHollowTimberLoggedVolume +
# (1-advancedloggingparameters$TreeHollowPartForFuel)*
#    HollowTable$TreeHarvestableVolume)
### - if(nrow(HollowTable) == 0): NoHollowTimberLoggedVolume

## NoHollowTimberLoggedVolume = sum(LoggedTable$TreeHarvestableVolume)






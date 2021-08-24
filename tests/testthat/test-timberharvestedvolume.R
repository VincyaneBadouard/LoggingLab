test_that("timberharvestedvolume", {

  # Data loading
  data(Paracou6_2016)
  Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:2000) # data reduction


  inventory <- addtreedim(inventorycheckformat(Paracou6_2016),
                          volumeparameters = ForestZoneVolumeParametersTable)

  inventory <- suppressMessages(treeselection(inventory, objective = 30, scenario ="manual",
                                              fuel = "2", diversification = TRUE, specieslax = FALSE,
                                              objectivelax = FALSE, topography = DTMParacou, plotslope = PlotSlope,
                                              speciescriteria = SpeciesCriteria,
                                              advancedloggingparameters = loggingparameters())$inventory)



  # Test data
  MatrixInventory <- as.matrix(Paracou6_2016)

  Rslt <- timberharvestedvolume(inventory, scenario = "manual", fuel = "0", # fuel !="2"
                                advancedloggingparameters = loggingparameters())

  RsltHollow <- timberharvestedvolume(inventory, scenario = "manual", fuel = "2", # fuel =="2"
                                      advancedloggingparameters = loggingparameters())

  LoggedTable <- inventory %>%
    filter(Selected == "1") # Logged trees

  Healthy <- sum(LoggedTable$TreeHarvestableVolume)


  HollowTable <- inventory %>%
    filter(ProbedHollow == "1") # probed hollow trees

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
  expect_true(Rslt$NoHollowLoggedVolume == Rslt$LoggedVolume) # no hollow trees exploitation
  expect_true(Rslt$LoggedVolume == Healthy) # the selected trees volume

  # fuel =="2"
  expect_true(RsltHollow$NoHollowLoggedVolume == Healthy) # the selected healthy trees volume

  if(nrow(HollowTable) > 0)
    expect_true(RsltHollow$LoggedVolume == sum(RsltHollow$NoHollowLoggedVolume +
                                                 (1-advancedloggingparameters$TreeHollowPartForFuel) *
                                                 HollowTable$TreeHarvestableVolume))

  if(nrow(HollowTable) == 0)
    expect_true(RsltHollow$LoggedVolume == RsltHollow$NoHollowLoggedVolume) # no probed hollow trees

})

# fuel !="2"
## NoHollowLoggedVolume = LoggedVolume = sum(LoggedTable$TreeHarvestableVolume)

# fuel =="2"
## LoggedVolume =
### - if(nrow(HollowTable) > 0): sum(NoHollowLoggedVolume +
# (1-advancedloggingparameters$TreeHollowPartForFuel)*
#    HollowTable$TreeHarvestableVolume)
### - if(nrow(HollowTable) == 0): NoHollowLoggedVolume

## NoHollowLoggedVolume = sum(LoggedTable$TreeHarvestableVolume)






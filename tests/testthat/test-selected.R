test_that("selected", {

  # Check the function arguments
  # Test data preparation
  data(Paracou6_2016)
  data(SpeciesCriteria)

  Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:2000)

  data(DTMParacou)
  data(PlotSlope)

  inventory <- addtreedim(inventorycheckformat(Paracou6_2016),
                          volumeparameters = ForestZoneVolumeParametersTable)
  inventory0 <- ONFGuyafortaxojoin(inventory, SpeciesCriteria)

  harvestableOutputs <- harvestable(inventory0, diversification = TRUE, specieslax = FALSE,
                                    topography = DTMParacou, plotslope = PlotSlope,
                                    advancedloggingparameters = loggingparameters())

  inventory <- harvestableOutputs$inventory
  HVinit <- harvestableOutputs$HVinit

  VO <- 20

  testinventory <- suppressMessages(selected(inventory, scenario = "manual", fuel = "0",
                                             diversification = TRUE, specieslax = FALSE,
                                             objectivelax = FALSE, topography = DTMParacou,
                                             advancedloggingparameters = loggingparameters(),
                                             VO = VO, HVinit = HVinit))$inventory

  MatrixInventory <- as.matrix(Paracou6_2016)

  # Tests
  expect_error(suppressMessages(selected(MatrixInventory)),
               regexp = "Argument 'inventory' of the 'selected' function must be a data.frame")

  expect_error(suppressMessages(selected(inventory, diversification = "1",
                                         specieslax = 2, objectivelax = "a")),
               regexp = "Arguments 'diversification', 'specieslax' and 'objectivelax'
         of the 'selected' function must be logical")

  #   expect_error(suppressMessages(selected(inventory, diversification = TRUE, scenario = "CL")),
  #                regexp = "The 'scenario' argument of the 'selected' function must be 'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  # expect_error(suppressMessages(selected(inventory, scenario = "manual", diversification = TRUE, fuel = TRUE)),
  #              regexp = "The 'fuel' argument of the 'selected' function must be '0', '1', or '2'")

  expect_error(suppressMessages(selected(inventory, scenario = "manual", fuel = "0", diversification = TRUE,
                                         advancedloggingparameters = as.matrix(loggingparameters()))),
               regexp = "Argument 'advancedloggingparameters' of the 'selected' function must be a list")

  expect_error(suppressMessages(selected(
    inventory, scenario = "manual", diversification = TRUE, fuel = "0",
    advancedloggingparameters = loggingparameters(),
    VO = "20", HVinit = HVinit)),
    regexp = "Arguments 'VO' and 'HVinit' of the 'selected' function must be numeric")

  expect_error(suppressMessages(selected(inventory, scenario = "manual",
                                         fuel = NULL, diversification = T)),
               regexp = "If you choose the 'manual' mode,
         you must fill in the arguments 'fuel' and 'diversification'")

  expect_true(all(!is.na(testinventory$Selected))) # No NA is Selected colomn


  # if HVinit == VO : all the "harvestable" trees are Selected == "1"
  VO <- HVinit
  testinventory <- suppressMessages(selected(inventory, scenario = "manual",
                                             fuel = "0", diversification = TRUE, topography = DTMParacou,
                                             advancedloggingparameters = loggingparameters(),
                                             VO = VO, HVinit = HVinit))$inventory
  if (HVinit == VO){
    TestRareWorld <- testinventory %>%
      dplyr::filter(LoggingStatus == "harvestable")

    expect_true(all(TestRareWorld$Selected == "1" |
                      (TestRareWorld$Selected == "0" & TestRareWorld$ProbedHollow == "1")))
  }


  # if HVinit < VO
  VO <- HVinit + 20

  ## if (!diversification && specieslax)
  inventory <- harvestable(inventory0,
                           diversification = FALSE, specieslax = TRUE, topography = DTMParacou,
                           plotslope = PlotSlope)$inventory

  testinventory <- suppressMessages(selected(inventory, scenario = "manual", fuel = "0",
                                             diversification = FALSE, specieslax = TRUE, objectivelax = TRUE, topography = DTMParacou,
                                             advancedloggingparameters = loggingparameters(),
                                             VO = VO, HVinit = HVinit))$inventory
  Testspecieslax <- testinventory %>%
    dplyr::filter(Selected == "1")

  expect_true(any(Testspecieslax$LoggingStatus =="harvestable2nd")) # there are "harvestable2nd" among the Selected

  ### objectivelax = FALSE
  expect_error(suppressMessages(selected(inventory, scenario = "manual", fuel = "0", diversification = FALSE, specieslax = TRUE, objectivelax = FALSE,
                                         topography = DTMParacou,
                                         advancedloggingparameters = loggingparameters(),
                                         VO = VO, HVinit = HVinit))$inventory,
               "By default or because of your choice, the simulation stops")

  # if (!diversification && !specieslax && objectivelax)
  inventory <- harvestable(inventory0,
                           diversification = FALSE,  specieslax = FALSE,
                           topography = DTMParacou, plotslope = PlotSlope)$inventory

  expect_message(selected(inventory, scenario = "manual", fuel = "0",
                          diversification = FALSE,  specieslax = FALSE, objectivelax = TRUE,
                          topography = DTMParacou, advancedloggingparameters = loggingparameters(),
                          VO = VO, HVinit = HVinit)$inventory,
                 "You have chosen to continue logging without diversifying")

  # if (diversification && objectivelax)
  inventory <- harvestable(inventory0,
                           diversification = TRUE, topography = DTMParacou, plotslope = PlotSlope)$inventory

  expect_message(selected(inventory, scenario = "manual", fuel = "0", diversification = TRUE,
                          objectivelax = TRUE,
                          topography = DTMParacou, advancedloggingparameters = loggingparameters(),
                          VO = VO, HVinit = HVinit)$inventory,
                 "You have chosen to continue logging in this case")

  ## if ((!specieslax & !objectivelax) | (diversification && !objectivelax))


  harvestableOutputs <- harvestable(inventory0,
                           diversification = TRUE, topography = DTMParacou, plotslope = PlotSlope)

  inventory <- harvestableOutputs$inventory
  HVinit <- harvestableOutputs$HVinit


  expect_error(suppressMessages(selected(inventory, scenario = "manual", fuel = "0",
                                         diversification = TRUE, objectivelax = FALSE,
                                         topography = DTMParacou,
                                         advancedloggingparameters = loggingparameters(),
                                         VO = VO, HVinit = HVinit))$inventory,
               "By default or because of your choice, the simulation stops")

  # if HVinit > VO
  VO <- HVinit - 20
  testinventory <- suppressMessages(selected(inventory, scenario = "manual", fuel = "0",
                                             diversification = FALSE, objectivelax = FALSE,
                                             topography = DTMParacou,
                                             advancedloggingparameters = loggingparameters(),
                                             VO = VO, HVinit = HVinit))$inventory

  TestUp <- testinventory %>%
    dplyr::filter(Commercial == "1")
  TestDBHUp <- testinventory %>%
    dplyr::filter(LoggingStatus =="harvestableUp")

  expect_true(any(TestUp$LoggingStatus == "harvestableUp")) # There are harvestableUp among the Commercial = "1"
  expect_true(all(TestDBHUp$DBH >= TestDBHUp$UpMinFD)) # DBH >= UpMinFD
  expect_true(all(TestDBHUp$Up =="1")) #  Up = "1"

  ## if (!diversification)
  expect_false(any(TestDBHUp$Commercial == "2")) # no Commercial = "2" among the harvestableUp
  ### if (HVupCommercial1 == VO)
  ### if (HVupCommercial1 > VO)
  ### if (HVupCommercial1 < VO)


  ## if (diversification)
  ### if HVupCommercial1 == VO
  ### if HVupCommercial1 < VO
  ### if HVupCommercial1 > VO
  #### if HVupCommercial12 == VO
  #### if HVupCommercial12 !== VO


  # Hollow (Rotten model)
  testinventory <- suppressMessages(selected(inventory, scenario = "manual", fuel = "0",
                                             diversification = TRUE,
                                             topography = DTMParacou,
                                             advancedloggingparameters = loggingparameters(),
                                             VO = VO, HVinit = HVinit))$inventory

  TestHollow <- testinventory %>%
    dplyr::filter(LoggingStatus == "harvestable" | LoggingStatus == "harvestable2nd")

  expect_true(all(!is.na(TestHollow$ProbedHollowProba))) # ProbedHollowProba for harvestable trees
  expect_true(all(TestHollow$ProbedHollow %in% c("0","1"))) # ProbedHollow = "0" ou "1" if !is.na(ProbedHollowProba)

  expect_true(all(TestHollow$ProbedHollow == "0" | TestHollow$ProbedHollow == "1")) # doest works with the check..

  # if fuel =="2" et que il y a des ProbedHollow == "1" : there are "hollowfuel" in DeathCause
  testinventory <- suppressMessages(selected(inventory, scenario = "manual", fuel = "2",
                                             diversification = TRUE, topography = DTMParacou,
                                             advancedloggingparameters = loggingparameters(),
                                             VO = VO, HVinit = HVinit))$inventory

  if (any(inventory$ProbedHollow == "1", na.rm = TRUE)) {
    expect_true(any(testinventory$DeathCause == "hollowfuel"))
  }

  # # 2 points vectors with coordinates of the probed hollow trees: "HollowTreesPoints" and "EnergywoodTreesPoints"

})




# check args
# inventory (data.frame)
# scenario "RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel", "RIL3fuelhollow" or "manual"(character)
# fuel "0","1"ou "2" (character)
# diversification (logical)
# specieslax (logical)
# objectivelax (logical)é
# advancedloggingparameters (list)
# VO (numeric value)
# HVinit (numeric value)

# if HVinit == VO : tous les "harvestable" sont Selected == "1"

# if HVinit < VO
## if !diversification && specieslax :  : il y a des "harvestable2nd" parmi les Selected
## if !diversification && !specieslax && objectivelax : message
## if diversification && objectivelax : message
## if (!specieslax & !objectivelax) | (diversification && !objectivelax) : erreur


# if HVinit > VO : ya des harvestableUp dans les commercial = "1" & DBH >= UpMinFD, Up = "1"
## if !diversification :  : pas de commercial = "2" dans les harvestableUp
### if HVupCommercial1 == VO
### if HVupCommercial1 > VO
### if HVupCommercial1 < VO


## if diversification
### if HVupCommercial1 == VO
### if HVupCommercial1 < VO
### if HVupCommercial1 > VO
#### if HVupCommercial12 == VO
#### if HVupCommercial12 !== VO

# ProbedHollowProba !is.na pour les Selected == "1" ou == "deselected" (Rotten) ah non ils ont été déselectionnés
# ProbedHollown = "0" ou "1" if !is.na(ProbedHollowProba)

# if scenario == "RIL3fuelhollow"| (scenario == "manual"& fuel =="2") : ya des "hollowfuel" dans DeathCause

# 2 points vectors with coordinates of the probed hollow trees: "HollowTreesPoints" and "EnergywoodTreesPoints"

test_that("selected", {

  # Check the function arguments
  # Test data preparation
  inventory <- harvestable(ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))),
                          diversification = TRUE, specieslax = FALSE)$inventory
  VO <- 40
  HVinit <- harvestable(ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))),
                       diversification = FALSE, specieslax = FALSE)$HVinit
  testinventory <- selected(inventory, type = "manual", fuel = "0", diversification = TRUE, specieslax = FALSE,
                            objectivelax = FALSE,
                            otherloggingparameters = loggingparameters(), VO = VO, HVinit = HVinit)$inventory

  MatrixInventory <- as.matrix(Paracou6_2016)

  # Tests
  expect_error(selected(MatrixInventory),
               regexp = "The 'inventory' argument of the 'selected' function must be a data.frame")

  expect_error(selected(inventory, diversification = "1", specieslax = 2, objectivelax = "a"),
               regexp = "The 'diversification', 'specieslax' and 'objectivelax' arguments of the 'selected' function must be logical")

  expect_error(selected(inventory, diversification = TRUE, type = "CL"),
               regexp = "The 'type' argument of the 'selected' function must be 'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  expect_error(selected(inventory, type = "manual", diversification = TRUE, fuel = TRUE),
               regexp = "The 'fuel' argument of the 'selected' function must be '0', '1', or '2'")

  expect_error(selected(inventory, type = "manual", fuel = "0", diversification = TRUE,
                        otherloggingparameters = as.matrix(loggingparameters())),
               regexp = "The 'otherloggingparameters' argument of the 'selected' function must be a list")

  expect_error(selected(
    inventory, type = "manual", diversification = TRUE, fuel = "0", otherloggingparameters = loggingparameters(),
    VO = "20", HVinit = HVinit),
    regexp = "The 'VO' and 'HVinit' arguments of the 'selected' function must be numeric")

  expect_true(all(!is.na(testinventory$Selected))) # No NA is Selected colomn


  # if HVinit == VO : all the "harvestable" trees are Selected == "1"
  VO <- HVinit
  testinventory <- selected(inventory, type = "manual", fuel = "0", diversification = TRUE,
                            otherloggingparameters = loggingparameters(), VO = VO, HVinit = HVinit)$inventory
  if (HVinit == VO){
    TestRareWorld <- testinventory %>%
      filter(LoggingStatus =="harvestable")

    expect_true(all(TestRareWorld$Selected == "1" |TestRareWorld$Selected == "deselected")) #deselected = probbedhollow trees
  }


# if HVinit < VO
VO <- HVinit + 20

## if (!diversification && specieslax)
inventory <- harvestable(ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))),
                        diversification = FALSE, specieslax = TRUE)$inventory

testinventory <- selected(inventory, type = "manual", fuel = "0", diversification = FALSE, specieslax = TRUE, objectivelax = TRUE,
                          otherloggingparameters = loggingparameters(), VO = VO, HVinit = HVinit)$inventory
Testspecieslax <- testinventory %>%
  filter(Selected == "1")

expect_true(any(Testspecieslax$LoggingStatus =="harvestable2nd")) # there are "harvestable2nd" among the Selected

### objectivelax = FALSE
expect_error(selected(inventory, type = "manual", fuel = "0", diversification = FALSE, specieslax = TRUE, objectivelax = FALSE,
                          otherloggingparameters = loggingparameters(), VO = VO, HVinit = HVinit)$inventory,
               "By default or by your choice, the simulation stops")

# if (!diversification && !specieslax && objectivelax)
inventory <- harvestable(ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))),
                        diversification = FALSE,  specieslax = FALSE)$inventory

expect_message(selected(inventory, type = "manual", fuel = "0", diversification = FALSE,  specieslax = FALSE, objectivelax = TRUE,
                        otherloggingparameters = loggingparameters(), VO = VO, HVinit = HVinit)$inventory,
               "In this case you have chosen to continue logging without diversifying your species.")

# if (diversification && objectivelax)
inventory <- harvestable(ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))),
                        diversification = TRUE)$inventory

expect_message(selected(inventory, type = "manual", fuel = "0", diversification = TRUE, objectivelax = TRUE,
                        otherloggingparameters = loggingparameters(), VO = VO, HVinit = HVinit)$inventory,
               "In this case you have chosen to continue logging.")

## if ((!specieslax & !objectivelax) | (diversification && !objectivelax))
inventory <- harvestable(ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))),
                        diversification = TRUE)$inventory

expect_error(selected(inventory, type = "manual", fuel = "0", diversification = TRUE, objectivelax = FALSE,
                      otherloggingparameters = loggingparameters(), VO = VO, HVinit = HVinit)$inventory,
             "By default or by your choice, the simulation stops.")

# if HVinit > VO
VO <- HVinit - 20
testinventory <- selected(inventory, type = "manual", fuel = "0", diversification = FALSE, objectivelax = FALSE,
                          otherloggingparameters = loggingparameters(), VO = VO, HVinit = HVinit)$inventory

TestUp <- testinventory %>%
  filter(Commercial == "1")
TestDBHUp <- testinventory %>%
  filter(LoggingStatus =="harvestableUp")

expect_true(any(TestDBHUp$LoggingStatus =="harvestableUp")) # There are harvestableUp among the Commercial = "1"
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
  testinventory <- selected(inventory, type = "manual", fuel = "0", diversification = TRUE,
                            otherloggingparameters = loggingparameters(), VO = VO, HVinit = HVinit)$inventory

  TestHollow <- testinventory %>%
    filter(Selected == "1"| Selected == "deselected")

  expect_true(all(!is.na(TestHollow$ProbedHollowProba))) # ProbedHollowProba for the Selected == "1" or == "deselected"
  expect_true(all(TestHollow$ProbedHollow == "0" || TestHollow$ProbedHollow == "1")) # ProbedHollow = "0" ou "1" if !is.na(ProbedHollowProba)


  # if fuel =="2" et que il y a des ProbedHollow == "1" : there are "hollowfuel" in DeathCause
  testinventory <- selected(inventory, type = "manual", fuel = "2", diversification = TRUE,
                            otherloggingparameters = loggingparameters(), VO = VO, HVinit = HVinit)$inventory

  if (any(inventory$ProbedHollow == "1", na.rm = TRUE)) {
    expect_true(any(testinventory$DeathCause == "hollowfuel"))
    }

  # # 2 points vectors with coordinates of the probed hollow trees: "HollowTreesPoints" and "EnergywoodTreesPoints"

})




# check args
# inventory (data.frame)
# type "RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel", "RIL3fuelhollow" or "manual"(character)
# fuel "0","1"ou "2" (character)
# diversification (logical)
# specieslax (logical)
# objectivelax (logical)é
# otherloggingparameters (list)
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

# if type == "RIL3fuelhollow"| (type == "manual"& fuel =="2") : ya des "hollowfuel" dans DeathCause

# 2 points vectors with coordinates of the probed hollow trees: "HollowTreesPoints" and "EnergywoodTreesPoints"

# test_that("selected", {

# Check the function arguments

MatrixInventory <- as.matrix(Paracou6_2016)

expect_error(selected(MatrixInventory), regexp = "The 'inventory' argument of the 'harvestable' function must be a data.frame")

expect_error(selected(Paracou6_2016, diversification = "1", specieslax = 2),
             regexp = "The 'diversification' and 'specieslax' arguments of the 'harvestable' function must be logical")

# })




# check args
  # inventory (data.frame)
  # type "RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel", "RIL3fuelhollow" or "manual"(character)
  # fuel "0","1"ou "2" (character)
  # diversification (logical)
  # specieslax (logical)
  # objectivelax (logical)
  # otherloggingparameters (list)
  # VO (numeric value)
  # HVinit (numeric value)

# si HVinit == VO : tous les "harvestable" sont "selected == "1"

# si HVinit < VO
## si !diversification && specieslax :
## si !diversification && !specieslax && objectivelax : message
## si diversification && objectivelax : message
## si (!specieslax & !objectivelax) | (diversification && !objectivelax) : message


# si HVinit > VO
## si !diversification
### si HVupCommercial1 == VO
### si HVupCommercial1 > VO
### si HVupCommercial1 < VO


## si diversification
### si HVupCommercial1 == VO
### si HVupCommercial1 < VO
### si HVupCommercial1 > VO
#### si HVupCommercial12 == VO
#### si HVupCommercial12 !== VO

# si type != "RIL3fuelhollow"| (type == "manual"& fuel !="2")

# si type == "RIL3fuelhollow"| (type == "manual"& fuel =="2")


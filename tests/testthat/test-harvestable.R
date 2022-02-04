test_that("harvestable", {

  # Data loading
  data(Paracou6_2016)
  data(DTMParacou)
  data(PlotMask)
  data(SpeciesCriteria)
  data(HarvestableAreaOutputsCable)

  Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:1000)
  MatrixInventory <- as.matrix(Paracou6_2016)

  # Check the function arguments
  expect_error(harvestable(MatrixInventory), regexp = "The 'inventory' argument of the 'harvestable' function must be a data.frame")

  expect_error(harvestable(Paracou6_2016, diversification = "1", specieslax = 2),
               regexp = "The 'diversification' and 'specieslax' arguments of the 'harvestable' function must be logical")

  expect_error(harvestable(Paracou6_2016, diversification = TRUE, specieslax = FALSE,
                           topography = NULL, plotslope = NULL))

  # LoggingStatus column exist and have no NA
  ## Test data preparation
  inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask), volumeparameters = ForestZoneVolumeParametersTable)
  inventory <- commercialcriteriajoin(inventory, SpeciesCriteria)

  Outputs1 <- harvestable(inventory,
                          topography = DTMParacou,
                          diversification = T, specieslax = F,
                          scenario = "RIL1",
                          plotslope = HarvestableAreaOutputsCable$PlotSlope,
                          harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons)
  Outputs2 <- harvestable(inventory,
                          topography = DTMParacou,
                          diversification = F, specieslax = T,
                          scenario = "RIL2",
                          plotslope = HarvestableAreaOutputsCable$PlotSlope,
                          harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons)
  Outputs3 <- harvestable(inventory,
                          topography = DTMParacou,
                          diversification = F, specieslax = F,
                          scenario = "RIL3",
                          plotslope = HarvestableAreaOutputsCable$PlotSlope,
                          harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons)

  testinventory1 <- Outputs1$inventory
  testinventory2 <- Outputs2$inventory
  testinventory3 <- Outputs3$inventory


  expect_false(any(is.na(testinventory1$LoggingStatus)))
  expect_false(any(is.na(testinventory2$LoggingStatus)))

  # CommercialLevel == "0" are  LoggingStatus =="non-harvestable"
  TestCommercial <- testinventory1 %>%
    dplyr::filter(CommercialLevel == "0")

  expect_true(all(TestCommercial$LoggingStatus =="non-harvestable"))

  TestCommercial <- testinventory2 %>%
    dplyr::filter(CommercialLevel == "0")

  expect_true(all(TestCommercial$LoggingStatus =="non-harvestable"))

  # "harvestable": DBH >= MinFD & DBH <= MaxFD

  # "harvestable": CommercialLevel == "1" ou "2" if diversification=T, or  diversification=F & specieslax=T
  testinventory1a <- testinventory1 %>%
    dplyr::filter(CommercialLevel == "2")
  testinventory2a <- testinventory2 %>%
    dplyr::filter(CommercialLevel == "2")

  expect_true(any(testinventory1a$LoggingStatus =="harvestable"))
  expect_true(any(testinventory2a$LoggingStatus =="harvestable2nd"))


  # "harvestable": CommercialLevel == "1"  diversification=F & specieslax=F
  testinventory3a <- testinventory3 %>%
    dplyr::filter(CommercialLevel != "1")
  expect_true(all(testinventory3a$LoggingStatus == "non-harvestable"))


  # "harvestable": check spatial!! "DistCriteria", "Slope", "SlopeCriteria"

  StatialTable <- testinventory1 %>%
    dplyr::filter(DBH >= MinFD & DBH <= MaxFD)

  ## Aggregative species individuals are checked for the distance between individuals of the same species
  AggregativeTable <- StatialTable %>%
    dplyr::filter(Aggregative)

  expect_true(all(!is.na(AggregativeTable$DistCriteria)))
  expect_true(all(!is.na(StatialTable$Slope)))
  expect_true(all(!is.na(StatialTable$SlopeCriteria)))


  # HVinit = sum of "TreeHarvestableVolume" values of "harvestable" trees.
  ## Test data preparation
  HVinit1 <- Outputs1$HVinit
  HVinit2 <- Outputs2$HVinit
  HVinit3 <- Outputs3$HVinit

  HarvestableTable1 <- testinventory1 %>%
    dplyr::filter(LoggingStatus == "harvestable")
  HarvestableTable2 <- testinventory2 %>%
    dplyr::filter(LoggingStatus == "harvestable")
  HarvestableTable3 <- testinventory3 %>%
    dplyr::filter(LoggingStatus == "harvestable")

  expect_true(HVinit1 == sum(HarvestableTable1$TreeHarvestableVolume))
  expect_true(HVinit2 == sum(HarvestableTable2$TreeHarvestableVolume))
  expect_true(HVinit3 == sum(HarvestableTable3$TreeHarvestableVolume))

  # All healthy:
  expect_true(all(HarvestableTable1$VisibleDefect == "0"))
  expect_true(all(HarvestableTable2$VisibleDefect == "0"))
  expect_true(all(HarvestableTable3$VisibleDefect == "0"))

  # Harvestables have the good maximum slope, and good distance between individuals for aggregative species
  expect_true(all(!HarvestableTable1$DistCriteria %in% FALSE))
  expect_true(all(!HarvestableTable1$DistCriteria %in% FALSE))
  expect_true(all(!HarvestableTable1$DistCriteria %in% FALSE))

  expect_true(all(HarvestableTable1$SlopeCriteria %in% TRUE))
  expect_true(all(HarvestableTable1$SlopeCriteria %in% TRUE))
  expect_true(all(HarvestableTable1$SlopeCriteria %in% TRUE))


})


# Checker que:
# - la classe des arguments (inventory (dataframe), diversification, specieslax (logical))
# - colonne LoggingStatus existe et qu'il n'y a pas de NA
# - les CommercialLevel == "0"sont codés "non-harvestable"
# - les "harvestable" ont leur DBH >= MinFD & DBH <= MaxFD
# - les "harvestable" sont codés CommercialLevel = "1" ou "2" si diversification=T ou si diversification=F & specieslax=T
# - les "harvestable" sont codés CommercialLevel = "1" si diversification=F & specieslax=F
# - les "harvestable" : check spatial!!
# - si specieslax=T : il y a des harvestable2nd
# - HVinit = somme des valeurs de "TreeHarvestableVolume " des arbres dont "LoggingStatus" = "harvestable".


# + Étiqueter "harvestable", dans une colonne "LoggingStatus", sélection des individus :
#   Essences :
#   - Si diversification=T ou si diversification=F & specieslax=T : = "1" & "2" dans colonne "CommercialLevel"
#   - Sinon (diversification=F & specieslax=F): = "1" dans colonne "CommercialLevel"
#   Diamètre exploitable (speciescriteria data): individus dont le DBH est compris entre la valeur dans colonne "MinFD" pour leur sp, de la table speciescriteria , et le MaxFD.
#   Distribution spatiale:
#     - Arbres sur <22% de pente et arbres sur pente >22% si à 40 m maximum d’une zone <22%.
#   - arbre non isolé : élimination des arbres à >100m des autres individus de la même espèce.
#   - hors des pistes principales : dont les coordonnées n'appartiennent pas aux multilignes "MainTrails".
# + Si diversification=F & specieslax=T, étiqueter "harvestable2nd" à la place d’"harvestable", les "CommercialLevel"= "2" dans la colonne "LoggingStatus".
# + HVinit= somme des valeurs de "TreeHarvestableVolume " des arbres dont "LoggingStatus" = "harvestable".

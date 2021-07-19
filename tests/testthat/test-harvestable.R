test_that("harvestable", {

# # Check the function arguments
#
# MatrixInventory <- as.matrix(Paracou6_2016)
#
#
# expect_error(harvestable(MatrixInventory), regexp = "The 'inventory' argument of the 'harvestable' function must be a data.frame")
#
# expect_error(harvestable(Paracou6_2016, diversification = "1", specieslax = 2),
#              regexp = "The 'diversification' and 'specieslax' arguments of the 'harvestable' function must be logical")
#
# expect_error(harvestable(Paracou6_2016, diversification = TRUE, specieslax = FALSE,
#             DEM = NULL, plotslope = NULL))
#
# # LoggingStatus column exist and have no NA
# ## Test data preparation
# testinventory1 <- harvestable(ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))),
#                              diversification = T, specieslax = F)$inventory
# testinventory2 <- harvestable(ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))),
#                               diversification = F, specieslax = T)$inventory
# testinventory3 <- harvestable(ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))),
#                               diversification = F, specieslax = F)$inventory
#
#
# expect_false(any(is.na(testinventory1$LoggingStatus)))
# expect_false(any(is.na(testinventory2$LoggingStatus)))
#
# # Commercial == "0" are  LoggingStatus =="non-harvestable"
# TestCommercial <- testinventory1 %>%
#   filter(Commercial == "0")
#
# expect_true(all(TestCommercial$LoggingStatus =="non-harvestable"))
#
# TestCommercial <- testinventory2 %>%
#   filter(Commercial == "0")
#
# expect_true(all(TestCommercial$LoggingStatus =="non-harvestable"))
#
# # "harvestable": DBH >= MinFD & DBH <= MaxFD
#
# # "harvestable": Commercial == "1" ou "2" if diversification=T, or  diversification=F & specieslax=T
# testinventory1a <- testinventory1 %>%
#   filter(Commercial == "2")
# testinventory2a <- testinventory2 %>%
#   filter(Commercial == "2")
#
# expect_true(any(testinventory1a$LoggingStatus =="harvestable"))
# expect_true(any(testinventory2a$LoggingStatus =="harvestable2nd"))
#
#
#
# # "harvestable": Commercial == "1"  diversification=F & specieslax=F
# testinventory3a <- testinventory3 %>%
#   filter(Commercial != "1")
# expect_true(all(testinventory3a$LoggingStatus =="non-harvestable"))
#
#
# # "harvestable": check spatial!! A FAIRE
#
# # HVinit = sum of "TreeHarvestableVolume" values of "harvestable" trees.
# ## Test data preparation
# HVinit1 <- harvestable(ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))),
#                               diversification = T, specieslax = F)$HVinit
# HVinit2 <- harvestable(ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))),
#                               diversification = F, specieslax = T)$HVinit
# HVinit3 <- harvestable(ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))),
#                               diversification = F, specieslax = F)$HVinit
#
# HarvestableTable1 <- testinventory1 %>%
#   filter(LoggingStatus == "harvestable")
# HarvestableTable2 <- testinventory2 %>%
#   filter(LoggingStatus == "harvestable")
# HarvestableTable3 <- testinventory3 %>%
#   filter(LoggingStatus == "harvestable")
#
# expect_true(HVinit1 == sum(HarvestableTable1$TreeHarvestableVolume))
# expect_true(HVinit2 == sum(HarvestableTable2$TreeHarvestableVolume))
# expect_true(HVinit3 == sum(HarvestableTable3$TreeHarvestableVolume))

})


# Checker que:
# - la classe des arguments (inventory (dataframe), diversification, specieslax (logical))
# - colonne LoggingStatus existe et qu'il n'y a pas de NA
# - les Commercial == "0"sont codés "non-harvestable"
# - les "harvestable" ont leur DBH >= MinFD & DBH <= MaxFD
# - les "harvestable" sont codés commercial = "1" ou "2" si diversification=T ou si diversification=F & specieslax=T
# - les "harvestable" sont codés commercial = "1" si diversification=F & specieslax=F
# - les "harvestable" : check spatial!!
# - si specieslax=T : il y a des harvestable2nd
# - HVinit = somme des valeurs de "TreeHarvestableVolume " des arbres dont "LoggingStatus" = "harvestable".


# + Étiqueter "harvestable", dans une colonne "LoggingStatus", sélection des individus :
#   Essences :
#   - Si diversification=T ou si diversification=F & specieslax=T : = "1" & "2" dans colonne "Commercial"
#   - Sinon (diversification=F & specieslax=F): = "1" dans colonne "Commercial"
#   Diamètre exploitable (speciescriteria data): individus dont le DBH est compris entre la valeur dans colonne "MinFD" pour leur sp, de la table speciescriteria , et le MaxFD.
#   Distribution spatiale:
#     - Arbres sur <22% de pente et arbres sur pente >22% si à 40 m maximum d’une zone <22%.
#   - arbre non isolé : élimination des arbres à >100m des autres individus de la même espèce.
#   - hors des pistes principales : dont les coordonnées n'appartiennent pas aux multilignes "MainTrail".
# + Si diversification=F & specieslax=T, étiqueter "harvestable2nd" à la place d’"harvestable", les "Commercial"= "2" dans la colonne "LoggingStatus".
# + HVinit= somme des valeurs de "TreeHarvestableVolume " des arbres dont "LoggingStatus" = "harvestable".

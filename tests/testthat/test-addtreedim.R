# test_that("addtreedim", {
#
#   # Test data preparation
#   if (!("DBH" %in% names(Paracou6_2016))) {add_column(Paracou6_2016, DBH = NA) #if DBH (cm) doesn't exist create it
#     Paracou6_2016$DBH = Paracou6_2016$CircCorr/pi}
#
#   # check their class (integer)
#   lapply(c("TreeHeight", "TreeHarvestableVolume", "TrunkHeight", "CrownHeight", "CrownDiameter"),
#          function(element) expect_type(addtreedim(Paracou6_2016)$element, integer))
#
#   # check that variables are not empty, or contains NA's
#   lapply(c("TreeHeight", "TreeHarvestableVolume", "TrunkHeight", "CrownHeight", "CrownDiameter"),
#          function(element) expect_vector(addtreedim(Paracou6_2016)$element, ptype = integer(), size = length(idTree)))
#
#   # check if formulas are respected
#
#   # check coherence
# })


# compute for all trees:
# + TreeBiomass
# + TreeHeight
# + TreeHarvestableVolume : volume exploitable à partir du tarif de cubage (= a + b*DBH2) que représente chaque arbre (a et b dépendent de la localisation)
# + TrunkHeight : hauteur de fût (TreeHarvestableVolume  = π(DBH/2)² x TrunkHeight)
# + CrownHeight : TreeHeight - TrunkHeight
# + CrownDiameter : diamètre de couronne (CD) (ln(D) = 𝜶+ 𝜷 ln(H*CD) + 𝜺 (allométries de Mélaine)
#                                              + CrownHeight : TreeHeight - TrunkHeight

# -> check if column exist
# -> check their class (integer)
# -> is not empty, or contains NA's
# -> check coherence

# -> check if formulas are respected

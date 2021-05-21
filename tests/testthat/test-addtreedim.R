test_that("addtreedim", {




})


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

# -> check if formules are respected

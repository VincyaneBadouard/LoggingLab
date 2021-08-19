# scenario = "manual"
# fuel = "2"
# directionalfelling = "2"
# advancedloggingparameters = loggingparameters()
#
# MainTrail <- sf::st_linestring(matrix(c(286400, 583130,
#                                         286400, 583250,
#                                         286655, 583250,
#                                         286655, 583130,
#                                         286400, 583130) # the return
#                                       ,ncol=2, byrow=TRUE))
#
# pol1 <- list(matrix(c(286503, 583134,
#                       286503, 583240,
#                       286507, 583240,
#                       286507, 583134,
#                       286503, 583134) # the return
#                     ,ncol=2, byrow=TRUE))
# pol2 <- list(matrix(c(286650, 583134,
#                       286650, 583240,
#                       286654, 583240,
#                       286654, 583134,
#                       286650, 583134) # the return
#                     ,ncol=2, byrow=TRUE))
#
# PolList = list(pol1,pol2) #list of lists of numeric matrices
# ScndTrail <- sf::st_multipolygon(PolList)
#
# inventory <- addtreedim(inventorycheckformat(Paracou6_2016))
# inventory <- suppressMessages(treeselection(inventory, objective = 30, scenario ="manual",
#                                             fuel = "2", diversification = TRUE, specieslax = FALSE,
#                                             objectivelax = FALSE, topography = DTMParacou, plotslope = PlotSlope,
#                                             speciescriteria = SpeciesCriteria,
#                                             advancedloggingparameters = loggingparameters())$inventory)
#
#
# # Compute treefelling success and fails
# inventory <- directionalfellingsuccessdef(
#   inventory,
#   fuel = fuel,
#   directionalfelling = directionalfelling,
#   advancedloggingparameters = loggingparameters())
#
#
# # Future/reserve trees to avoid
# inventory <- createcanopy(inventory) # create all inventory crowns in the 'Crowns' column
#
# FutureReserveCrowns <- inventory %>% # create an object with future/reserve crowns only
#   filter(LoggingStatus == "future" | LoggingStatus == "reserve") %>%
#   getgeometry(Crowns)
#
#
# # Treefelling
# felttrees <- inventory %>%
#   filter(!is.na(TreeFellingOrientationSuccess)) %>%
#   group_by(idTree) %>% # for each tree
#   do(TreePolygon = # inform geometry. # Filling a column from a function whose input is a table
#        felling1tree(.,
#                     fuel = fuel, directionalfelling = directionalfelling,
#                     MainTrail = MainTrail, ScndTrail = ScndTrail,
#                     FutureReserveCrowns = FutureReserveCrowns,
#                     advancedloggingparameters = loggingparameters())$FallenTree %>%
#        st_as_text()) %>% # as text to easy join with a non spacial table
#   tidyr::unnest(TreePolygon) # here to pass from list to character
#
# inventory <- left_join(inventory, felttrees, by = "idTree")
#
#
# # Mortality A FAIRE
# # Records the felled trees
# if (!("DeathCause" %in% names(inventory))){
#   inventory <- inventory %>%
#     add_column(DeathCause = NA)
# }
#
# inventory <- inventory %>%
#   mutate(DeathCause = ifelse(is.na(DeathCause) & !is.na(TreePolygon) & ProbedHollow == "0",
#                              "cutted", DeathCause)) %>% # timber exploitation
#   mutate(DeathCause = ifelse(is.na(DeathCause) & !is.na(TreePolygon) & ProbedHollow == "1",
#                              "fuelwood", DeathCause)) # fuel wood exploitation
#
#
# # Trees under the fallen trees
# DeadTrees <- sf::st_intersection(
#   getgeometry (felttrees, TreePolygon),
#   st_as_sf(inventory, coords = c("Xutm", "Yutm"))
# ) %>%
#   filter(Selected != "1") %>%
#   add_column(DeadTrees = "1") %>%
#   select(idTree, DeadTrees) # J'arrive pas à virer la colonne TreePolygon
#
# inventory <- inventory %>%
#   mutate(DeathCause = ifelse(is.na(DeathCause) & is.na(TreePolygon) & DeadTrees == "1",
#                              "treefall2nd", DeathCause)) # Damage trees
#
#
#

# data(Paracou6_2016)
# data(DTMParacou)
# data(PlotSlope)
#
# inventory <- addtreedim(inventorycheckformat(Paracou6_2016),
#                         volumeparameters = ForestZoneVolumeParametersTable)
#
# inventory <- ONFGuyafortaxojoin(inventory, SpeciesCriteria)
#
# harvestableOutputs <- harvestable(inventory, topography = DTMParacou,
#                                   diversification = TRUE, specieslax = FALSE, plotslope = PlotSlope,
#                                   advancedloggingparameters = loggingparameters())
#
# inventory <- harvestableOutputs$inventory
# HVinit <- harvestableOutputs$HVinit
#
# scenario = "manual"
# fuel = "0"
# diversification = TRUE
# objective = 20
# VO = 162.5
# HVinit = HVinit
# specieslax = FALSE
# objectivelax = FALSE
# topography = DTMParacou
# advancedloggingparameters = loggingparameters()
#
#
# # if objective achieved at the first attempt
# inventory <- inventory %>%
#   mutate(Selected = ifelse(HVinit == VO & LoggingStatus == "harvestable",
#                            "1", NA))# if we have our volume, harvestable sp = selected sp
#
# # Create a column to indicate Which sp is FD uped. "0" = no uped, "1"= uped
# inventory <- add_column(inventory, Up = "0")
#
#
# if (HVinit < VO){ #diversification is necessary, designate the secondary-economic-rank species too
#   if (!diversification && specieslax){
#     inventory <- inventory %>%
#       mutate(Condition = ifelse(LoggingStatus == "harvestable2nd"|LoggingStatus == "harvestable",TRUE, FALSE)) %>%
#       group_by(Condition) %>%
#       arrange(desc(TreeHarvestableVolume)) %>%
#       mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
#       ungroup() %>%
#       mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
#       select(-Condition, -VolumeCumSum)
#
#     HarvestableTable <- inventory %>%
#       filter(Selected == "1")
#     HVlax <- sum(HarvestableTable$TreeHarvestableVolume) # Harvestable volume, with specieslax permission
#
#
#     if (HVlax < VO && objectivelax)
#       message("The exploitable volume (= ",paste(round(HVlax, digits = 1)),"m^3) is still lower
#                 (by ",paste(round(VO- HVlax, digits = 1)),"m^3) than your objective volume
#                 despite the diversification you have allowed
#                 (without the diversification HVinit= ",paste(round(HVinit, digits = 1)),"m^3).
#                 In this case, you have chosen to continue harvesting with a volume lower than your objective.")
#
#     if (HVlax < VO && !objectivelax)
#       stop("The harvestable volume = ",paste(round(HVlax, digits = 1)),"m^3)
#              is still lower (by ",paste(round(VO- HVlax, digits = 1)),"m^3)
#              than your objective volume despite the diversification you have allowed
#              (without the diversification HVinit= ",paste(round(HVinit, digits = 1)),"m^3).
#              By default or by your choice, the simulation stops.
#              If you wish to continue the exploitation in spite of an exploitable volume lower
#              than your objective volume, you have the argument 'objectivelax'.")
#
#     if (!HVlax == VO)
#       message("The harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) was lower
#                 (by ",paste(round(VO- HVinit, digits = 1)),"m^3) than your objective volume.
#                 You have chosen to diversify your species selection in this case.
#                 The exploitation was therefore carried out on this diversified selection of species.")
#   }
#
#   if (!diversification && !specieslax && objectivelax)
#     message("The harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) is less
#               (by ",paste(round(VO- HVinit, digits = 1)),"m^3) than your objective volume.
#               In this case you have chosen to continue logging without diversifying your species.")
#   if (diversification && objectivelax)
#
#     message("The harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) is less
#               (by ",paste(round(VO- HVinit, digits = 1)),"m^3) than your objective volume.
#               In this case you have chosen to continue logging.")
#
#
#   if ((!specieslax & !objectivelax) | (diversification && !objectivelax))
#     stop("The harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) is lower
#            (by ",paste(round(VO- HVinit, digits = 1)),"m^3) than your objective volume.
#            By default or by your choice, the simulation stops.
#            If you wish to continue the exploitation in spite of a harvestable volume lower
#            than your objective volume, you can use the argument 'objectivelax'
#            or the diversification of species (if it is not already the case).")
# }
#
#
# if (HVinit > VO) {
#
#   inventory <- inventory %>%
#     mutate(LoggingStatus = ifelse(LoggingStatus == "harvestable" &
#                                     Commercial == "1" & (DBH >= UpMinFD & DBH <= MaxFD), #designate preferred individuals of first economic rank species, when the plot is species-rich.
#                                   "harvestableUp", LoggingStatus))
#
#   if (!diversification) {
#     HarvestableTable <- inventory %>%
#       filter(LoggingStatus == "harvestableUp")
#
#     HVupCommercial1 <- sum(HarvestableTable$TreeHarvestableVolume) #Compute the harvestable volume with upgraded FD individuals
#
#
#     if (HVupCommercial1 == VO){
#
#       inventory <- inventory %>%
#         mutate(Selected = ifelse(HVupCommercial1 == VO & LoggingStatus == "harvestableUp", "1", NA))# if harvestableUp individuals are sufficient to have our volume, harvestableUp ind = selected ind
#
#       message("As the harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) was higher
#                 (by ",paste(round(HVinit-VO, digits = 1)),"m^3) than the objective volume,
#                 the Minimum Falling Diameter (MinFD) of 1st economic rank species were increased.
#                 The objective volume has now been reached.")
#     }
#
#     if (HVupCommercial1 > VO){
#       # only individuals with DBH > FD are taken, but not all because their volumes > VO
#       inventory <- inventory %>%
#         mutate(Condition = ifelse(LoggingStatus == "harvestableUp", TRUE, FALSE)) %>%
#         group_by(Condition) %>%
#         arrange(desc(TreeHarvestableVolume)) %>%
#         mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
#         ungroup() %>%
#         mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
#         select(-Condition, -VolumeCumSum)
#
#       HarvestableTable <- inventory %>%
#         filter(Selected == "1")
#       HVupCommercial1adjust <- sum(HarvestableTable$TreeHarvestableVolume) #Harvestable volume, with "1" rank species and upgraded FD individuals only
#
#       message("The harvestable volume (= ",paste(round(HVupCommercial1, digits = 1)),"m^3) is always higher
#                 (by ",paste(round(HVupCommercial1-VO, digits = 1)),"m^3) than your objective volume
#                 despite the increase in Minimum Falling Diameter (MinFD)
#                 (Initial harvestable volume = ",paste(round(HVinit, digits = 1)),"m^3).
#                 In order to reach your objective volume, the trees were selected in decreasing order of volume
#                 until the objective volume was reached.")
#     }
#     if (HVupCommercial1 < VO){
#
#       inventory <- inventory %>%
#         mutate(Condition = ifelse(LoggingStatus == "harvestableUp"|LoggingStatus == "harvestable",TRUE,FALSE)) %>%
#         group_by(Condition) %>%
#         arrange(desc(TreeHarvestableVolume)) %>%
#         mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
#         ungroup() %>%
#         mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
#         select(-Condition, -VolumeCumSum)
#
#       HarvestableTable <- inventory %>%
#         filter(Selected == "1")
#       HVupCommercial1adjust <- sum(HarvestableTable$TreeHarvestableVolume) #Harvestable volume, with "1" rank species and upgraded FD individuals
#
#       message("As the harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3)
#                 was higher (by ",paste(round(HVinit-VO, digits = 1)),"m^3) than the objective volume,
#                 the Minimum Falling Diameter (MinFD) of the 1st economic rank species were increased
#                 with retention of some stems with DBH lower than the UpMinFD
#                 to ensure that the objective volume was attained.")
#     }
#   }
#
#   if (diversification) {
#
#     HarvestableTable <- inventory %>%
#       filter(LoggingStatus == "harvestableUp" | LoggingStatus == "harvestable") #the upgraded 1st rank and normal 2nd rank.
#
#     HVupCommercial1 <- sum(HarvestableTable$TreeHarvestableVolume) #173.334 m3
#
#     if (HVupCommercial1 == VO){
#       inventory <- inventory %>%
#
#         mutate(Selected = ifelse(LoggingStatus == "harvestableUp" | LoggingStatus == "harvestable",
#                                  "1", NA))# if we have our volume, harvestable ind = selected ind
#       message("As the harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) was higher (by ",paste(round(HVinit-VO, digits = 1)),"m^3) than the objective volume, the Minimum Falling Diameter (MinFD) of the 1st economic rank species were increased. The objective volume has now been reached. It was not necessary to increase the MinFD of the other economic species")
#     }
#
#     if (HVupCommercial1 < VO){
#
#       inventory <- inventory %>%
#         mutate(Condition = ifelse(LoggingStatus == "harvestableUp"|LoggingStatus == "harvestable",TRUE,FALSE)) %>%
#         group_by(Condition) %>%
#         arrange(desc(TreeHarvestableVolume)) %>%
#         mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
#         ungroup() %>%
#         mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
#         select(-Condition, -VolumeCumSum)
#
#       HarvestableTable <- inventory %>%
#         filter(Selected == "1")
#       HVupCommercial1adjust <- sum(HarvestableTable$TreeHarvestableVolume) #49.69643 Harvestable volume, with "1" rank species and upgraded FD individuals, and 2nd rank no upgraded FD individuals
#
#       message("As the harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3)
#                 was higher (by ",paste(round(HVinit-VO, digits = 1)),"m^3) than the objective volume,
#                 the Minimum Falling Diameter (MinFD) of the 1st economic rank species were increased
#                 with retention of some stems with DBH lower than the UpMinFD to ensure that the objective
#                 volume was attained. It was not necessary to raise the MinFDs of other economic species.")
#     }
#
#     if (HVupCommercial1 > VO){
#
#       inventory <- inventory %>%
#         mutate(LoggingStatus = ifelse(LoggingStatus == "harvestable" & Commercial == "2" & DBH >= UpMinFD, #designate preferred individuals of 2nd economic rank species too, when the plot is species-rich.
#                                       "harvestableUp", LoggingStatus))
#
#       HarvestableTable <- inventory %>%
#         filter(LoggingStatus == "harvestableUp")
#
#       HVupCommercial12 <- sum(HarvestableTable$TreeHarvestableVolume) #271.2342 Harvestable volume, with upgraded FD individuals
#
#       if (HVupCommercial12 == VO){
#         inventory <- inventory %>%
#           mutate(Selected = ifelse(LoggingStatus == "harvestableUp",
#                                    "1", NA))# if we have our volume, harvestable ind = selected ind
#
#       }
#       if (HVupCommercial12 != VO){
#
#         inventory <- inventory %>%
#           mutate(Condition = ifelse(LoggingStatus == "harvestableUp"|LoggingStatus == "harvestable",TRUE,FALSE)) %>%
#           group_by(Condition) %>%
#           arrange(desc(TreeHarvestableVolume)) %>%
#           mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
#           ungroup() %>%
#           mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
#           select(-Condition, -VolumeCumSum)
#
#         HarvestableTable <- inventory %>%
#           filter(Selected == "1")
#         HVupCommercial12adjust <- sum(HarvestableTable$TreeHarvestableVolume) #Harvestable volume, with upgraded FD individuals
#       }
#
#       message("As the harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3)
#                 was higher (by ",paste(round(HVinit-VO, digits = 1)),"m^3)
#                 than the objective volume, it was necessary to increase the Minimum Falling Diameter (MinFD)
#                 of all species. The objective volume has now been reached.")
#
#     }
#   }
# }
#
# # Apply S. Schmitt's "Rotten" predictive model to identify "truly" hollow designated trees.
# inventory <- inventory %>%
#   # Estimates the probability of being probed hollow
#   mutate(ProbedHollowProba = ifelse(Selected == "1", advancedloggingparameters$RottenModel(DBH), NA)) %>%
#
#   # generate either "1" or "0" randomly for each line, depending on the proba associated with the line:
#   rowwise() %>%
#   mutate(ProbedHollow = ifelse(!is.na(ProbedHollowProba),
#                                sample(c(1,0), size = 1, replace = F, # 1 = hollow tree, 0 = not hollow
#                                       prob = c(ProbedHollowProba, 1-ProbedHollowProba)), NA)) %>%
#   mutate(ProbedHollow = factor(as.numeric(ProbedHollow))) %>%
#   # Hollow probed trees are deselected
#   mutate(Selected = ifelse(ProbedHollow == "1", "deselected", Selected))
#
# # Return to objective value if Bonus > deselected volume
# HarvestableTable <- inventory %>%
#   filter(Selected == "1")
# VolxBonus <- sum(HarvestableTable$TreeHarvestableVolume)
#
# InitialObjective <- objective * unique(inventory$PlotArea)
#
# if(VolxBonus > InitialObjective){
#
#   inventory <- inventory %>%
#     select(- VolumeCumSum) %>%
#     mutate(Condition = ifelse(Selected == "1", TRUE, FALSE)) %>%
#     group_by(Condition) %>%
#     arrange(desc(TreeHarvestableVolume)) %>%
#     mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
#     ungroup() %>%
#     # remove the excess volume:
#     mutate(Selected = ifelse(Condition & VolumeCumSum > InitialObjective, "0", Selected)) %>%
#     select(-Condition, -VolumeCumSum)
#
# }
#
#
# # remplacer les arbres deselected (atteindre le vol objectif)
#
# # Upgraded MinFD species:
# # to inform for each individual if its species have been FD upgraded
# inventory <- inventory %>%
#   group_by(ONFName) %>%
#   mutate(Up = ifelse(any(LoggingStatus == "harvestableUp"), "1", Up)) %>%
#   ungroup() %>%
#
#   # No NA in Selected colomn
#   mutate(Selected = ifelse(is.na(Selected) , "0", Selected))
#
# # if there are "deselected" trees and not of 'selected = 1'
# if (any(inventory$Selected == "deselected") & !any(inventory$Selected == "1"))
#   stop("No trees were selected because they were all probed hollow
#          (",paste(round(sum(as.numeric(inventory$Selected == "deselected"), na.rm = TRUE),  digits = 1)),
#        " probed hollow trees). Your objective volume may be too low (the few trees selected were found to be
#          hollow).")
#
# # Create a POINTS VECTOR with coordinates of the probed hollow trees:
# if (any(inventory$ProbedHollow == "1", na.rm = TRUE)) {
#   HollowTreesPoints <- inventory %>%
#     filter(ProbedHollow == "1")
#
#   sp::coordinates(HollowTreesPoints) <- ~ Xutm + Yutm
#
#   sp::proj4string(HollowTreesPoints) <- raster::crs(topography)
#
#   HollowTreesPoints <- st_as_sf(as(HollowTreesPoints,"SpatialPoints"))
#
#   # OUTPUTS list
#   selectedOutputs <- list(inventory = inventory,
#                           HollowTreesPoints = HollowTreesPoints,
#                           EnergywoodTreesPoints = st_point(x = c(NA_real_, NA_real_))) # empty point
#
# } else {
#   selectedOutputs <- list(inventory = inventory,
#                           HollowTreesPoints = st_point(x = c(NA_real_, NA_real_)), # empty point
#                           EnergywoodTreesPoints = st_point(x = c(NA_real_, NA_real_)))
# }
#
# if (fuel !="2") {
#   HarvestableTable <- inventory %>%
#     filter(Selected == "1")
#   VolumewithHollowslost <- sum(HarvestableTable$TreeHarvestableVolume) #128.5047. Harvestable volume, with Hollows lost
#   VO - VolumewithHollowslost #34 m3 lost: the bonus is therefore generous here (37.5 m3 of bonus).
# }
#
# if ((fuel =="2") & any(inventory$ProbedHollow == "1", na.rm = TRUE)) {
#   # Hollow trees = fuel wood:
#   inventory <- inventory %>%
#     mutate(DeathCause = ifelse(ProbedHollow == "1", "hollowfuel", NA)) # remplacer NA par DeathCause dans le simulateur ONF
#
#   # Create a POINTS VECTOR with coordinates of the energywood trees:
#
#   EnergywoodTreesPoints <- HollowTreesPoints
#
#
#   # OUTPUTS list
#   selectedOutputs <- list(inventory = inventory,
#                           HollowTreesPoints = HollowTreesPoints,
#                           EnergywoodTreesPoints = EnergywoodTreesPoints)
#
# }

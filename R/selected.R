#' selected
#'
#' @param inventory (data.frame)
#' @param type "RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel", "RIL3fuelhollow" or "manual"(character)
#' @param fuel no  exploitation = "0" (default),
#' damage exploitation in fuelwood = "1", exploitation of hollow trees and damage in fuelwood = "2"
#' @param diversification (logical)
#' @param specieslax = FALSE by default (logical)
#' @param objectivelax = FALSE by default (logical)
#' @param otherloggingparameters (list)
#' @param VO (numeric value)
#' @param HVinit (numeric value)
#'
#' @return
#' @export
#'
#' @importFrom sf st_multipoint
#'
#' @examples
#' inventory = harvestable(ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))),
#' diversification = TRUE, specieslax = FALSE)$inventory
#' HVinit = harvestable(ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))),
#' diversification = TRUE, specieslax = FALSE)$HVinit
#' selected(inventory, type = "manual", fuel = "0", diversification = TRUE, specieslax = FALSE, objectivelax = FALSE,
#' otherloggingparameters = loggingparameters(), VO = 20, HVinit = HVinit)
#'
selected <- function(
  inventory,
  type = "manual",
  fuel,
  diversification,
  specieslax = FALSE,
  objectivelax = FALSE,
  otherloggingparameters = loggingparameters(),
  VO, # objective volume
  HVinit # initial Harvestable Volume
){

  # Arguments check

  if(!inherits(inventory, "data.frame"))
    stop("The 'inventory' argument of the 'selected' function must be a data.frame")

  if(!all(unlist(lapply(list(diversification, specieslax, objectivelax), inherits, "logical"))) && !is.null(diversification))
    stop("The 'diversification', 'specieslax' and 'objectivelax' arguments of the 'selected' function must be logical") # any() don't take a list

  if (!any(type == "RIL1" | type == "RIL2broken"| type == "RIL2"| type == "RIL3"| type == "RIL3fuel"|
           type == "RIL3fuelhollow"| type == "manual"))
    stop("The 'type' argument of the 'selected' function must be 'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  if (!any(fuel == "0" | fuel == "1"| fuel == "2" | is.null(fuel)))
    stop("The 'fuel' argument of the 'selected' function must be '0', '1', or '2'")

  if(!inherits(otherloggingparameters, "list"))
    stop("The 'otherloggingparameters' argument of the 'selected' function must be a list")

  if(!all(unlist(lapply(list(VO, HVinit), inherits, "numeric"))))
    stop("The 'VO' and 'HVinit' arguments of the 'selected' function must be numeric")

  # Redefinition of the parameters according to the chosen scenario
  scenariosparameters <- scenariosparameters(type = type, fuel = fuel, diversification = diversification)

  fuel <- scenariosparameters$fuel
  diversification <- scenariosparameters$diversification




  # if objective achieved at the first attempt
  inventory <- inventory %>%
    mutate(Selected = ifelse(HVinit == VO & LoggingStatus == "harvestable",
                             "1", NA))# if we have our volume, harvestable sp = selected sp

  inventory <- add_column(inventory, Up = 0) # Create a column to indicate Which sp is FD uped. "0" = no uped, "1"= uped dans inventory


  if (HVinit < VO){ #diversification is necessary, designate the secondary-economic-rank species too
    if (!diversification && specieslax){
      inventory <- inventory %>%
        mutate(Condition = ifelse(LoggingStatus == "harvestable2nd" | LoggingStatus == "harvestable", TRUE, FALSE)) %>%
        group_by(Condition) %>%
        arrange(desc(TreeHarvestableVolume)) %>%
        mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
        ungroup() %>%
        mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
        select(-Condition)

      HarvestableTable <- inventory %>%
        filter(Selected == "1")
      HVlax <- sum(HarvestableTable$TreeHarvestableVolume) #49.69643 Harvestable volume, with specieslax permission


      if (HVlax < VO && objectivelax) message("The exploitable volume (= ",paste(HVlax),") is still lower (by ",paste(VO- HVlax),") than your objective volume despite the diversification you have allowed (without the diversification HVinit= ",paste(HVinit),"). In this case, you have chosen to continue harvesting with a volume lower than your objective.")

      if (HVlax < VO && !objectivelax) stop("The harvestable volume = ",paste(HVlax),") is still lower (by ",paste(VO- HVlax),") than your objective volume despite the diversification you have allowed (without the diversification HVinit= ",paste(HVinit),"). By default or by your choice, the simulation stops. If you wish to continue the exploitation in spite of an exploitable volume lower than your objective volume, you have the argument 'objectivelax'.")

      if (!HVlax == VO) message("The harvestable volume (=",paste(HVinit),") was lower (by ",paste(VO- HVinit),") than your objective volume. You have chosen to diversify your species selection in this case. The exploitation was therefore carried out on this diversified selection of species.")
    }

    if (!diversification && !specieslax && objectivelax)
      message("The harvestable volume (=",paste(HVinit),") is less (by ",paste(VO- HVinit),") than your objective volume. In this case you have chosen to continue logging without diversifying your species.")

    if (diversification && objectivelax)
      message("The harvestable volume (=",paste(HVinit),") is less (by ",paste(VO- HVinit),") than your objective volume. In this case you have chosen to continue logging.")


    if ((!specieslax & !objectivelax) | (diversification && !objectivelax))
      stop("The harvestable volume (=",paste(HVinit),") is lower (by ",paste(VO- HVinit),") than your objective volume. By default or by your choice, the simulation stops. If you wish to continue the exploitation in spite of a harvestable volume lower than your objective volume, you can use the argument 'objectivelax' or the diversification of species (if it is not already the case).")
  }


  if (HVinit > VO) {

    inventory <- inventory %>%
      mutate(LoggingStatus = ifelse(
        Commercial == "1" & (DBH >= UpMinFD & DBH <= MaxFD), #designate preferred individuals of first economic rank species, when the plot is species-rich.
        "harvestableUp", LoggingStatus)) %>%

      mutate(Up = ifelse(Commercial == "1", "1", Up))# to inform that the "1" ranks have been FD upgraded. Pas mieux de le faire à l'sp?
    # test "Commercial"= "1" pour les "harvestableUp"

    if (!diversification) {
      HarvestableTable <- inventory %>%
        filter(LoggingStatus == "harvestableUp")

      HVupCommercial1 <- sum(HarvestableTable$TreeHarvestableVolume) #82.42823. compute the harvestable volume with upgraded FD individuals


      if (HVupCommercial1 == VO){

        inventory <- inventory %>%
          mutate(Selected = ifelse(HVupCommercial1 == VO & LoggingStatus == "harvestableUp", "1", NA))# if harvestableUp individuals are sufficient to have our volume, harvestableUp ind = selected ind

        message("As the harvestable volume (=",paste(HVinit),") was higher (by ",paste(HVinit-VO),") than the objective volume, the Minimum Falling Diameter (MinFD) of 1st economic rank species were increased. The objective volume has now been reached.")
      }

      if (HVupCommercial1 > VO){
        # only individuals with DBH > FD are taken, but not all because their volumes > VO
        inventory<- inventory %>%
          mutate(Condition = ifelse(LoggingStatus == "harvestableUp", TRUE, FALSE)) %>%
          group_by(Condition) %>%
          arrange(desc(TreeHarvestableVolume)) %>%
          mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
          ungroup() %>%
          mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
          select(-Condition)

        HarvestableTable <- inventory %>%
          filter(Selected == "1")
        HVupCommercial1adjust <- sum(HarvestableTable$TreeHarvestableVolume) #49.69643 Harvestable volume, with "1" rank species and upgraded FD individuals only

        message("The harvestable volume (=",paste(HVupCommercial1),") is always higher (by ",paste(HVupCommercial1-VO),") than your objective volume despite the increase in Minimum Falling Diameter (MinFD) (Initial harvestable volume = HVinit). In order to reach your objective volume, the trees were selected in decreasing order of volume until the objective volume was reached.")
      }
      if (HVupCommercial1 < VO){

        inventory <- inventory %>%
          mutate(Condition = ifelse(LoggingStatus == "harvestableUp" | LoggingStatus == "harvestable", TRUE, FALSE)) %>%
          group_by(Condition) %>%
          arrange(desc(TreeHarvestableVolume)) %>%
          mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
          ungroup() %>%
          mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
          select(-Condition)

        HarvestableTable <- inventory %>%
          filter(Selected == "1")
        HVupCommercial1adjust <- sum(HarvestableTable$TreeHarvestableVolume) #49.69643 Harvestable volume, with "1" rank species and upgraded FD individuals

        message("As the harvestable volume (=",paste(HVinit),") was higher (by ",paste(HVinit-VO),") than the objective volume, the Minimum Falling Diameter (MinFD) of the 1st economic rank species were increased with retention of some stems with DBH lower than the UpMinFD to ensure that the objective volume was attained.")
      }
    }

    if (diversification) {

      HarvestableTable <- inventory %>%
        filter(LoggingStatus == "harvestableUp" | LoggingStatus == "harvestable") #the upgraded 1st rank and normal 2nd rank.

      HVupCommercial1 <- sum(HarvestableTable$TreeHarvestableVolume) #173.334 m3

      if (HVupCommercial1 == VO){
        inventory <- inventory %>%

          mutate(Selected = ifelse(LoggingStatus == "harvestableUp" | LoggingStatus == "harvestable",
                                   "1", NA))# if we have our volume, harvestable ind = selected ind
        message("As the harvestable volume (=",paste(HVinit),") was higher (by ",paste(HVinit-VO),") than the objective volume, the Minimum Falling Diameter (MinFD) of the 1st economic rank species were increased. The objective volume has now been reached. It was not necessary to increase the MinFD of the other economic species")
      }

      if (HVupCommercial1 < VO){

        inventory <- inventory %>%
          mutate(Condition = ifelse(LoggingStatus == "harvestableUp" | LoggingStatus == "harvestable", TRUE, FALSE)) %>%
          group_by(Condition) %>%
          arrange(desc(TreeHarvestableVolume)) %>%
          mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
          ungroup() %>%
          mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
          select(-Condition)

        HarvestableTable <- inventory %>%
          filter(Selected == "1")
        HVupCommercial1adjust <- sum(HarvestableTable$TreeHarvestableVolume) #49.69643 Harvestable volume, with "1" rank species and upgraded FD individuals, and 2nd rank no upgraded FD individuals

        message("As the harvestable volume (=",paste(HVinit),") was higher (by ",paste(HVinit-VO),") than the objective volume, the Minimum Falling Diameter (MinFD) of the 1st economic rank species were increased with retention of some stems with DBH lower than the UpMinFD to ensure that the objective volume was attained. It was not necessary to raise the MinFDs of other economic species.")
      }

      if (HVupCommercial1 > VO){

        inventory <- inventory %>%
          mutate(LoggingStatus = ifelse(LoggingStatus == "harvestable" & Commercial == "2" & DBH >= UpMinFD, #designate preferred individuals of 2nd economic rank species too, when the plot is species-rich.
                                        "harvestableUp", LoggingStatus)) %>%

          mutate(Up = ifelse(Commercial == "2", "1", Up))# to inform that the "2" ranks have been FD upgraded. Pas mieux de le faire à l'sp?

        HarvestableTable <- inventory %>%
          filter(LoggingStatus == "harvestableUp")

        HVupCommercial12 <- sum(HarvestableTable$TreeHarvestableVolume) #271.2342 Harvestable volume, with upgraded FD individuals

        if (HVupCommercial12 == VO){
          inventory <- inventory %>%
            mutate(Selected = ifelse(LoggingStatus == "harvestableUp",
                                     "1", NA))# if we have our volume, harvestable ind = selected ind

        }
        if (HVupCommercial12 != VO){

          inventory <- inventory %>%
            mutate(Condition = ifelse(LoggingStatus == "harvestableUp" | LoggingStatus == "harvestable", TRUE, FALSE)) %>%
            group_by(Condition) %>%
            arrange(desc(TreeHarvestableVolume)) %>%
            mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
            ungroup() %>%
            mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
            select(-Condition)

          HarvestableTable <- inventory %>%
            filter(Selected == "1")
          HVupCommercial12adjust <- sum(HarvestableTable$TreeHarvestableVolume) #45.44885 Harvestable volume, with upgraded FD individuals
        }

        message("As the harvestable volume (=",paste(HVinit),") was higher (by ",paste(HVinit-VO),") than the objective volume, it was necessary to increase the Minimum Falling Diameter (MinFD) of all species. The objective volume has now been reached.")

      }
    }
  }

  # Apply S. Schmitt's "Rotten" predictive model to identify "truly" hollow designated trees.
  inventory <- inventory %>%
    mutate(ProbedHollowProba = ifelse(Selected == "1", otherloggingparameters$RottenModel(DBH), NA)) %>%  #Estimates the probability of being probed hollow

    # generate either "1" or "0" randomly for each line, depending on the proba associated with the line:
    rowwise() %>%
    mutate(ProbedHollow = ifelse(!is.na(ProbedHollowProba),
                                 sample(c(1,0), size = 1, replace = F, prob = c(ProbedHollowProba, 1-ProbedHollowProba)), NA)) %>%  # 1 = hollow tree, 0 = not hollow
    mutate(ProbedHollow = factor(as.numeric(ProbedHollow))) %>%
    mutate(Selected = ifelse(ProbedHollow == "1", "deselected", Selected)) %>%  #hollow probed trees are deselected
    # non-upgraded MinFD species:
    mutate(Up = ifelse(is.na(Up) , "0", Up)) %>%
    # No NA in Selected colomn
    mutate(Selected = ifelse(is.na(Selected) , "0", Selected))

  if (any(inventory$Selected == "deselected") && !any(inventory$Selected == "1")) #if there are "deselected" trees and  not of selected = 1
    stop("No trees were selected because they were all probed hollow (",paste(sum(as.numeric(inventory$Selected == "deselected"), na.rm = TRUE))," probed hollow trees). Your objective volume may be too low (the few trees selected were found to be hollow).")

  # Create a POINTS VECTOR with coordinates of the probed hollow trees:
  if (any(inventory$ProbedHollow == "1", na.rm = TRUE)) {
    HollowTreescoord <- inventory %>%
      filter(ProbedHollow == "1") %>%
      select(Xutm, Yutm)

    HollowTreesPoints  <- st_multipoint(x = as.matrix(HollowTreescoord))

    # OUTPUTS list
    selectedOutputs <- list(inventory = inventory,
                            HollowTreesPoints = HollowTreesPoints,
                            EnergywoodTreesPoints = st_multipoint(x = matrix(numeric(0), 0, 2))) # empty multipoint

  } else {
  selectedOutputs <- list(inventory = inventory,
                          HollowTreesPoints = st_multipoint(x = matrix(numeric(0), 0, 2)), # empty multipoint
                          EnergywoodTreesPoints = st_multipoint(x = matrix(numeric(0), 0, 2)))
  }

  if (fuel !="2") {
    HarvestableTable <- inventory %>%
      filter(Selected == "1")
    VolumewithHollowslost <- sum(HarvestableTable$TreeHarvestableVolume) #128.5047. Harvestable volume, with Hollows lost
    VO - VolumewithHollowslost #34 m3 lost: the bonus is therefore generous here (37.5 m3 of bonus).
  }

  if ((fuel =="2") && any(inventory$ProbedHollow == "1", na.rm = TRUE)) {
    # Hollow trees = fuelwood:
    inventory <- inventory %>%
      mutate(DeathCause = ifelse(ProbedHollow == "1", "hollowfuel", NA)) # remplacer NA par DeathCause dans le simulateur ONF

    # Create a POINTS VECTOR with coordinates of the energywood trees:

    EnergywoodTreesPoints <- HollowTreesPoints


    # OUTPUTS list
    selectedOutputs <- list(inventory = inventory,
                            HollowTreesPoints = HollowTreesPoints,
                            EnergywoodTreesPoints = EnergywoodTreesPoints)

  }

  return(selectedOutputs) # return the new inventory and the 2 points vectors (HollowTrees and EnergywoodTrees)

  }

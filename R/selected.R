#' selected
#'
#' @param inventory your inventory (see the inputs formats and metadata in the
#'   \code{\link{vignette}}) (data.frame)
#'
#' @param scenario Logging scenario: "RIL1", "RIL2broken", "RIL2", "RIL3",
#'   "RIL3fuel", "RIL3fuelhollow" or "manual"(character) (see the
#'   \code{\link{vignette}})
#'
#' @param fuel Fuel wood exploitation: no exploitation = "0", damage
#'   exploitation in fuelwood = "1", exploitation of hollow trees and damage in
#'   fuelwood = "2"
#'
#' @param diversification Taking of other species in addition to the main
#'   commercial species (2 levels of commercial species in the
#'   \code{\link{SpeciesCriteria}} table) (logical)
#'
#' @param specieslax Allow diversification if stand is too poor, = FALSE by
#'   default (logical)
#'
#' @param objectivelax Allow exploitation in case of non-achievement of the
#'   objective volume (if stand too poor), = FALSE by default (logical)
#'
#' @param DEM Digital terrain model (DTM) of the inventoried plot (LiDAR or
#'   SRTM) (default: \code{\link{DemParacou}}) (RasterLayer)
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list) MainTrail (multiline)
#'
#' @param VO The objective volume with or without a bonus (if hollow trees
#'   exploitation)(numeric value) (see the \code{\link{loggingparameters}})
#'
#' @param HVinit the harvestable volume in the plot for your criteria
#'   (\code{\link{SpeciesCriteria}}) (numeric value)
#'
#' @return Your inventory with the trees selected for harvesting (depending on
#'   the logging scenario chosen), and 2 sets of spatial points: (HollowTrees
#'   and EnergywoodTrees)
#'
#' @seealso  \code{\link{Paracou6_2016}}, \code{\link{SpeciesCriteria}},
#'   \code{\link{DemParacou}}, \code{\link{loggingparameters}}
#'
#' @export
#'
#'@importFrom dplyr arrange desc ungroup rowwise mutate group_by select filter
#'@importFrom tibble add_column
#'@importFrom sp coordinates proj4string
#'@importFrom sf st_as_sf
#'@importFrom raster crs
#'
#'
#' @examples
#'
#' data(Paracou6_2016)
#' data(DemParacou)
#' data(PlotSlope)
#'
#' inventory <- ONFGuyafortaxojoin(addtreedim(
#' inventorycheckformat(Paracou6_2016)))
#'
#' harvestableOutputs <- harvestable(inventory, diversification = TRUE,
#'  specieslax = FALSE,
#' DEM = DemParacou, plotslope = PlotSlope,
#' advancedloggingparameters = loggingparameters())
#'
#' inventory <- harvestableOutputs$inventory
#' HVinit <- harvestableOutputs$HVinit
#'
#' selecInventory <- selected(inventory, scenario = "manual", fuel = "2",
#' diversification = TRUE, specieslax = FALSE, objectivelax = FALSE,
#' DEM = DemParacou, advancedloggingparameters = loggingparameters(), VO = 30,
#' HVinit = HVinit)$inventory
#'
selected <- function(
  inventory,
  scenario = "manual",
  fuel,
  diversification,
  specieslax = FALSE,
  objectivelax = FALSE,
  DEM,
  advancedloggingparameters = loggingparameters(),
  VO, # objective volume
  HVinit # initial Harvestable Volume
){

  # Arguments check

  if(!inherits(inventory, "data.frame"))
    stop("The 'inventory' argument of the 'selected' function must be a data.frame")

  if(!all(unlist(lapply(list(diversification, specieslax, objectivelax), inherits, "logical"))) && !is.null(diversification))
    stop("The 'diversification', 'specieslax' and 'objectivelax' arguments of the 'selected' function must be logical") # any() don't take a list

  # if (!any(scenario == "RIL1" | scenario == "RIL2broken"| scenario == "RIL2"| scenario == "RIL3"| scenario == "RIL3fuel"|
  #          scenario == "RIL3fuelhollow"| scenario == "manual"))
  #   stop("The 'scenario' argument of the 'selected' function must be 'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  # if (!any(fuel == "0" | fuel == "1"| fuel == "2" | is.null(fuel)))
  #   stop("The 'fuel' argument of the 'selected' function must be '0', '1', or '2'")

  if(!inherits(advancedloggingparameters, "list"))
    stop("The 'advancedloggingparameters' argument of the 'selected' function must be a list")

  if(scenario == "manual" &&
     (is.null(fuel) || is.null(diversification)))
    stop("If you choose the 'manual' mode,
         you must fill in the arguments 'fuel' and 'diversification'")

  if(!all(unlist(lapply(list(VO, HVinit), inherits, "numeric"))))
    stop("The 'VO' and 'HVinit' arguments of the 'selected' function must be numeric")

  # Redefinition of the parameters according to the chosen scenario
  scenariosparameters <- scenariosparameters(scenario = scenario, fuel = fuel, diversification = diversification)

  fuel <- scenariosparameters$fuel
  diversification <- scenariosparameters$diversification

  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <- Commercial <- NULL
  Commercial.genus <- Commercial.species <- Condition <- DBH <- NULL
  DeathCause <- DistCrit <- Family <- ONFName <- NULL
  ForestZoneVolumeParametersTable <- Genus <- Logged <- NULL
  LoggedVolume <- LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  NoHollowLoggedVolume <- ParamCrownDiameterAllometry <- PlotSlope <- NULL
  PlotTopo <- ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- Slope <- SlopeCrit <- Species <- Species.genus <- NULL
  SpeciesCriteria <- Taxo <- Taxo.family <- Taxo.genus <- Taxo.species <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD <- UpMinFD.genus <- NULL
  UpMinFD.species <- VernName.genus <- VernName.genus.genus <- NULL
  VernName.species <- VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL
  alpha <- alpha.family <- alpha.genus <- alpha.species <- bCoef <- NULL
  beta.family <- beta.genus <- beta.species <- geometry <- idTree <- NULL




  # if objective achieved at the first attempt
  inventory <- inventory %>%
    mutate(Selected = ifelse(HVinit == VO & LoggingStatus == "harvestable",
                             "1", NA))# if we have our volume, harvestable sp = selected sp

  inventory <- add_column(inventory, Up = "0") # Create a column to indicate Which sp is FD uped. "0" = no uped, "1"= uped


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


      if (HVlax < VO && objectivelax) message("The exploitable volume (= ",paste(round(HVlax, digits = 1)),"m^3) is still lower (by ",paste(round(VO- HVlax, digits = 1)),"m^3) than your objective volume despite the diversification you have allowed (without the diversification HVinit= ",paste(round(HVinit, digits = 1)),"m^3). In this case, you have chosen to continue harvesting with a volume lower than your objective.")

      if (HVlax < VO && !objectivelax) stop("The harvestable volume = ",paste(round(HVlax, digits = 1)),"m^3) is still lower (by ",paste(round(VO- HVlax, digits = 1)),"m^3) than your objective volume despite the diversification you have allowed (without the diversification HVinit= ",paste(round(HVinit, digits = 1)),"m^3). By default or by your choice, the simulation stops. If you wish to continue the exploitation in spite of an exploitable volume lower than your objective volume, you have the argument 'objectivelax'.")

      if (!HVlax == VO) message("The harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) was lower (by ",paste(round(VO- HVinit, digits = 1)),"m^3) than your objective volume. You have chosen to diversify your species selection in this case. The exploitation was therefore carried out on this diversified selection of species.")
    }

    if (!diversification && !specieslax && objectivelax)
      message("The harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) is less (by ",paste(round(VO- HVinit, digits = 1)),"m^3) than your objective volume. In this case you have chosen to continue logging without diversifying your species.")
    if (diversification && objectivelax)

      message("The harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) is less (by ",paste(round(VO- HVinit, digits = 1)),"m^3) than your objective volume. In this case you have chosen to continue logging.")


    if ((!specieslax & !objectivelax) | (diversification && !objectivelax))
      stop("The harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) is lower (by ",paste(round(VO- HVinit, digits = 1)),"m^3) than your objective volume. By default or by your choice, the simulation stops. If you wish to continue the exploitation in spite of a harvestable volume lower than your objective volume, you can use the argument 'objectivelax' or the diversification of species (if it is not already the case).")
  }


  if (HVinit > VO) {

    inventory <- inventory %>%
      mutate(LoggingStatus = ifelse(LoggingStatus == "harvestable" &
                                      Commercial == "1" & (DBH >= UpMinFD & DBH <= MaxFD), #designate preferred individuals of first economic rank species, when the plot is species-rich.
                                    "harvestableUp", LoggingStatus))

    if (!diversification) {
      HarvestableTable <- inventory %>%
        filter(LoggingStatus == "harvestableUp")

      HVupCommercial1 <- sum(HarvestableTable$TreeHarvestableVolume) #82.42823. compute the harvestable volume with upgraded FD individuals


      if (HVupCommercial1 == VO){

        inventory <- inventory %>%
          mutate(Selected = ifelse(HVupCommercial1 == VO & LoggingStatus == "harvestableUp", "1", NA))# if harvestableUp individuals are sufficient to have our volume, harvestableUp ind = selected ind

        message("As the harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) was higher (by ",paste(round(HVinit-VO, digits = 1)),"m^3) than the objective volume, the Minimum Falling Diameter (MinFD) of 1st economic rank species were increased. The objective volume has now been reached.")
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

        message("The harvestable volume (= ",paste(round(HVupCommercial1, digits = 1)),"m^3)is always higher (by ",paste(round(HVupCommercial1-VO, digits = 1)),"m^3) than your objective volume despite the increase in Minimum Falling Diameter (MinFD) (Initial harvestable volume = ",paste(round(HVinit, digits = 1)),"m^3). In order to reach your objective volume, the trees were selected in decreasing order of volume until the objective volume was reached.")
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

        message("As the harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) was higher (by ",paste(round(HVinit-VO, digits = 1)),"m^3) than the objective volume, the Minimum Falling Diameter (MinFD) of the 1st economic rank species were increased with retention of some stems with DBH lower than the UpMinFD to ensure that the objective volume was attained.")
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
        message("As the harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) was higher (by ",paste(round(HVinit-VO, digits = 1)),"m^3) than the objective volume, the Minimum Falling Diameter (MinFD) of the 1st economic rank species were increased. The objective volume has now been reached. It was not necessary to increase the MinFD of the other economic species")
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

        message("As the harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) was higher (by ",paste(round(HVinit-VO, digits = 1)),"m^3) than the objective volume, the Minimum Falling Diameter (MinFD) of the 1st economic rank species were increased with retention of some stems with DBH lower than the UpMinFD to ensure that the objective volume was attained. It was not necessary to raise the MinFDs of other economic species.")
      }

      if (HVupCommercial1 > VO){

        inventory <- inventory %>%
          mutate(LoggingStatus = ifelse(LoggingStatus == "harvestable" & Commercial == "2" & DBH >= UpMinFD, #designate preferred individuals of 2nd economic rank species too, when the plot is species-rich.
                                        "harvestableUp", LoggingStatus))

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

        message("As the harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) was higher (by ",paste(round(HVinit-VO, digits = 1)),"m^3) than the objective volume, it was necessary to increase the Minimum Falling Diameter (MinFD) of all species. The objective volume has now been reached.")

      }
    }
  }

  # Apply S. Schmitt's "Rotten" predictive model to identify "truly" hollow designated trees.
  inventory <- inventory %>%
    mutate(ProbedHollowProba = ifelse(Selected == "1", advancedloggingparameters$RottenModel(DBH), NA)) %>%  #Estimates the probability of being probed hollow

    # generate either "1" or "0" randomly for each line, depending on the proba associated with the line:
    rowwise() %>%
    mutate(ProbedHollow = ifelse(!is.na(ProbedHollowProba),
                                 sample(c(1,0), size = 1, replace = F, prob = c(ProbedHollowProba, 1-ProbedHollowProba)), NA)) %>%  # 1 = hollow tree, 0 = not hollow
    mutate(ProbedHollow = factor(as.numeric(ProbedHollow))) %>%
    mutate(Selected = ifelse(ProbedHollow == "1", "deselected", Selected)) %>%  #hollow probed trees are deselected

    # Upgraded MinFD species:
    group_by(ONFName) %>%
    mutate(Up = ifelse(any(LoggingStatus == "harvestableUp"), "1", Up)) %>% # to inform for each individual if its species have been FD upgraded
    ungroup() %>%

  # No NA in Selected colomn
    mutate(Selected = ifelse(is.na(Selected) , "0", Selected))

  if (any(inventory$Selected == "deselected") & !any(inventory$Selected == "1")) #if there are "deselected" trees and  not of selected = 1
    stop("No trees were selected because they were all probed hollow (",paste(round(sum(as.numeric(inventory$Selected == "deselected"), na.rm = TRUE),  digits = 1))," probed hollow trees). Your objective volume may be too low (the few trees selected were found to be hollow).")

  # Create a POINTS VECTOR with coordinates of the probed hollow trees:
  if (any(inventory$ProbedHollow == "1", na.rm = TRUE)) {
    HollowTreesPoints <- inventory %>%
      filter(ProbedHollow == "1")

    sp::coordinates(HollowTreesPoints) <- ~ Xutm + Yutm

    sp::proj4string(HollowTreesPoints) <- raster::crs(DEM)

    HollowTreesPoints <- st_as_sf(as(HollowTreesPoints,"SpatialPoints"))

    # OUTPUTS list
    selectedOutputs <- list(inventory = inventory,
                            HollowTreesPoints = HollowTreesPoints,
                            EnergywoodTreesPoints = st_point(x = c(NA_real_, NA_real_))) # empty point

  } else {
    selectedOutputs <- list(inventory = inventory,
                            HollowTreesPoints = st_point(x = c(NA_real_, NA_real_)), # empty point
                            EnergywoodTreesPoints = st_point(x = c(NA_real_, NA_real_)))
  }

  if (fuel !="2") {
    HarvestableTable <- inventory %>%
      filter(Selected == "1")
    VolumewithHollowslost <- sum(HarvestableTable$TreeHarvestableVolume) #128.5047. Harvestable volume, with Hollows lost
    VO - VolumewithHollowslost #34 m3 lost: the bonus is therefore generous here (37.5 m3 of bonus).
  }

  if ((fuel =="2") & any(inventory$ProbedHollow == "1", na.rm = TRUE)) {
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

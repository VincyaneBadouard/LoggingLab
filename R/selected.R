#'Selection of trees to log
#'
#'@description Selection of trees to log among the exploitable trees, according
#'  to the objective volume and the exploitable volume on the plot.
#'
#'@param inventory Input inventory (see the inputs formats and metadata in the
#'  vignette) (data.frame)
#'
#'@param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'  or SRTM) (\code{\link{DTMParacou}}) (RasterLayer **with a crs in UTM**)
#'  We advise you to generate your raster with Qgis rather than with the
#'  'raster' package on R.
#'
#'@param scenario Logging scenario: "RIL1", "RIL2broken", "RIL2", "RIL3",
#'  "RIL3fuel", "RIL3fuelhollow" or "manual"(character) (see the
#'  vignette)
#'
#'@param fuel Fuel wood exploitation: no exploitation = "0", exploitation of
#'  damage and unused part of logged trees for fuel = "1", exploitation of
#'  hollow trees, damage and and unused part of the log for fuel = "2"
#'
#'@param diversification Possibility to log other species in addition to the
#'  main commercial species (species with a value of 2  for commercial in the
#'  \code{\link{SpeciesCriteria}} table) (logical)
#'
#'@param specieslax Allow diversification if the stand is too poor to reach the
#'  objective volume without diversification, = FALSE by default (logical)
#'
#'@param VO Objective volume for the entire area of the plot (m3) (numeric
#'  value)
#'
#'@param HVinit Harvestable volume(m3) in the plot for the chosen scenario
#'  (\code{\link{SpeciesCriteria}}) (numeric value)
#'
#'@param objectivelax Allow exploitation in case of non-achievement of the
#'  objective volume (if stand too poor), = FALSE by default (logical)
#'
#'@param advancedloggingparameters Other parameters of the logging simulator
#'  \code{\link{loggingparameters}} (list)
#'
#'@return Input inventory with:
#'  - The trees selected for harvesting (*Selected*)
#'  - if the Minimum Felling Diameter (*MinFD*) of their species has been raised
#'      (*Up*)
#'  - The probability of a tree being probed hollow (*ProbedHollowProba*) and
#'      the probed hollow trees (*ProbedHollow*).
#'See the outputs metadata in the vignette.
#'
#'2 sets of spatial points: (*HollowTrees* and *EnergywoodTrees*)
#'
#'@details This function allows the selection of trees to be exploited among the
#'  exploitable trees. It also informs about the trees that were finally found
#'  to be hollow during the probe. If fuel = 2, the hollow trees will be
#'  harvested and are therefore included in the objective volume. If fuel = 0 or
#'  1, the hollow trees will not be exploited, so the function looks for other
#'  trees to reach the objective volume (if possible).
#'
#'  If the harvestable volume is higher than the objective volume, and that
#'  diversification was not chosen, *MinFD* of the 1st economic rank species
#'  only is increased.
#'  If the diversification is allowed, *MinFD* of 1st and 2nd economic level
#'  species is increased.
#'  Then, the trees to be harvested are chosen in
#'  decreasing order of volume, until the objective volume is reached.
#'
#'  If the harvestable volume is lower than the objective volume,
#'  diversification can be applied if it was not already applied ('specieslax')
#'  (trees of all commercial ranks are selected in decreasing order of volume
#'  until the objective volume is reached), or harvesting can continue despite
#'  an unreached objective volume, or be abandoned ('objectivelax')
#'
#'@seealso  \code{\link{Paracou6_2016}}, \code{\link{SpeciesCriteria}},
#'  \code{\link{DTMParacou}}, \code{\link{loggingparameters}}
#'
#'@export
#'
#'@importFrom dplyr arrange desc ungroup rowwise mutate group_by select filter
#'@importFrom tibble add_column
#'@importFrom sp coordinates proj4string
#'@importFrom sf st_as_sf
#'@importFrom raster crs
#'
#'
#' @examples
#' data(Paracou6_2016)
#' data(DTMParacou)
#' data(PlotMask)
#' data(HarvestableAreaOutputsCable)
#' data(MainTrails)
#' data(ForestZoneVolumeParametersTable)
#'
#' inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask),
#' volumeparameters = ForestZoneVolumeParametersTable)
#'
#' inventory <- commercialcriteriajoin(inventory, SpeciesCriteria)
#'
#' harvestableOutputs <- harvestable(inventory, topography = DTMParacou,
#' diversification = FALSE, specieslax = TRUE,
#' plotslope = HarvestableAreaOutputsCable$PlotSlope,
#' maintrails = MainTrails,
#' harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons,
#' scenario = "manual", winching = "2",
#' advancedloggingparameters = loggingparameters())
#'
#' inventory <- harvestableOutputs$inventory
#' HVinit <- harvestableOutputs$HVinit
#'
#' selecInventory <- selected(inventory, topography = DTMParacou,
#' scenario = "manual", fuel = "2", diversification = TRUE,
#' VO = 20, HVinit = HVinit, specieslax = TRUE, objectivelax = TRUE,
#' advancedloggingparameters = loggingparameters())$inventory
#'
selected <- function(
  inventory,
  topography,
  scenario,
  fuel = NULL,
  diversification = NULL,
  VO, # objective volume for the entire surface of the plot
  HVinit, # initial Harvestable Volume
  specieslax = FALSE,
  objectivelax = FALSE,
  advancedloggingparameters = loggingparameters()
){

  # Arguments check

  if(!inherits(inventory, "data.frame"))
    stop("Argument 'inventory' of the 'selected' function must be a data.frame")

  if(!all(unlist(lapply(list(diversification, specieslax, objectivelax), inherits, "logical")))
     && !is.null(diversification))
    stop("Arguments 'diversification', 'specieslax' and 'objectivelax'
         of the 'selected' function must be logical")

  # if (!any(scenario == "RIL1" | scenario == "RIL2broken"| scenario == "RIL2"| scenario == "RIL3"| scenario == "RIL3fuel"|
  #          scenario == "RIL3fuelhollow"| scenario == "manual"))
  #   stop("The 'scenario' argument of the 'selected' function must be 'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  # if (!any(fuel == "0" | fuel == "1"| fuel == "2" | is.null(fuel)))
  #   stop("The 'fuel' argument of the 'selected' function must be '0', '1', or '2'")

  if(!inherits(advancedloggingparameters, "list"))
    stop("Argument 'advancedloggingparameters' of the 'selected' function must be a list")

  if(scenario == "manual" &&
     (is.null(fuel) || is.null(diversification)))
    stop("If you choose the 'manual' mode,
         you must fill in the arguments 'fuel' and 'diversification'")

  if(!all(unlist(lapply(list(VO, HVinit), inherits, "numeric"))))
    stop("Arguments 'VO' and 'HVinit' of the 'selected' function must be numeric")


  # initial inventory
  inventory0 <- inventory


  # Redefinition of the parameters according to the chosen scenario
  scenariosparameters <- scenariosparameters(scenario = scenario,
                                             fuel = fuel, diversification = diversification)

  fuel <- scenariosparameters$fuel
  diversification <- scenariosparameters$diversification

  # Global variables
  Accessible <- CircCorr <- CodeAlive <- CommercialLevel <- NULL
  Condition <- DBH <- NULL
  DeathCause <- DistCriteria <- Family <- CommercialName <- Crumbs <- NULL
  ForestZoneVolumeParametersTable <- Genus <- Logged <- NULL
  TimberLoggedVolume <- LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  NoHollowTimberLoggedVolume <- ParamCrownDiameterAllometry <- PlotSlope <- NULL
  ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- Slope <- SlopeCriteria <- Species <- Species.genus <- NULL
  SpeciesCriteria <- Taxo <- Taxo.family <- Taxo.genus <- Taxo.species <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD <- UpMinFD.genus <- NULL
  UpMinFD.species <- NULL
  VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL
  alpha <- alpha.family <- alpha.genus <- alpha.species <- bCoef <- NULL
  beta.family <- beta.genus <- beta.species <- geometry <- idTree <- NULL


  # Apply S. Schmitt's "Rotten" predictive model to identify probed hollow trees.
  inventory <- inventory %>%
    # Estimates the probability of being probed hollow
    mutate(ProbedHollowProba = ifelse(LoggingStatus == "harvestable" | LoggingStatus == "harvestable2nd",
                                      advancedloggingparameters$RottenModel(DBH), NA)) %>%

    # generate either "1" or "0" randomly for each line, depending on the proba associated with the line:
    rowwise() %>%
    mutate(ProbedHollow = ifelse(!is.na(ProbedHollowProba),
                                 sample(c(1,0), size = 1, replace = F, # 1 = hollow tree, 0 = not hollow
                                        prob = c(ProbedHollowProba, 1-ProbedHollowProba)), NA)) %>%
    mutate(ProbedHollow = factor(as.numeric(ProbedHollow)))

  if(fuel != "2"){
    HollowTable <- filter(inventory, ProbedHollow == "1"| is.na(ProbedHollow))
    inventory <- filter(inventory, ProbedHollow == "0") # we continue with just healthy trees in fuel != "2" case
  }


  # if objective achieved at the first attempt
  inventory <- inventory %>%
    mutate(Selected = ifelse(HVinit == VO & LoggingStatus == "harvestable",
                             "1", NA))# if we have our volume, harvestable sp = selected sp

  # Create a column to indicate Which sp is FD uped. "0" = no uped, "1"= uped
  inventory <- add_column(inventory, Up = "0")


  if (HVinit < VO){ #diversification is necessary, designate the secondary-economic-rank species too
    if (!diversification && specieslax){
      inventory <- inventory %>%
        mutate(Condition = ifelse(LoggingStatus == "harvestable2nd"|LoggingStatus == "harvestable",TRUE, FALSE)) %>%
        group_by(Condition) %>%
        arrange(CommercialLevel) %>% # add for reviewer
        arrange(desc(TreeHarvestableVolume)) %>%
        mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
        ungroup() %>%
        mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
        select(-Condition, -VolumeCumSum)

      HarvestableTable <- inventory %>%
        filter(Selected == "1")
      HVlax <- sum(HarvestableTable$TreeHarvestableVolume) # Harvestable volume, with specieslax permission


      if (HVlax < VO && objectivelax)
        message("The exploitable volume (= ",paste(round(HVlax, digits = 1)),"m^3) is still lower
                (by ",paste(round(VO- HVlax, digits = 1)),"m^3) than your objective volume
                despite the diversification
                (without the diversification HVinit= ",paste(round(HVinit, digits = 1)),"m^3).
                You have chosen to continue harvesting with a volume lower than your objective in this case.")

      if (HVlax < VO && !objectivelax)
        stop("The harvestable volume = ",paste(round(HVlax, digits = 1)),"m^3)
             is still lower (by ",paste(round(VO- HVlax, digits = 1)),"m^3)
             than your objective volume despite the diversification
             (without the diversification HVinit= ",paste(round(HVinit, digits = 1)),"m^3).
             By default or because of your choice, the simulation stops.
             If you wish to continue the exploitation with of an exploitable volume lower
             than your objective volume, you can use the argument 'objectivelax'.")

      if (!HVlax == VO)
        message("The harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) was lower
                (by ",paste(round(VO- HVinit, digits = 1)),"m^3) than your objective volume.
                You have chosen to diversify your species selection in this case.
                The exploitation was therefore carried out on this diversified selection of species.")
    } else {

      inventory <- inventory %>%
        mutate(Condition = ifelse(LoggingStatus == "harvestable",TRUE, FALSE)) %>%
        group_by(Condition) %>%
        arrange(CommercialLevel) %>% # add for reviewer
        arrange(desc(TreeHarvestableVolume)) %>%
        mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
        ungroup() %>%
        mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
        select(-Condition, -VolumeCumSum)

    }

    if (!diversification && !specieslax && objectivelax)
      message("The harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) was lower
              (by ",paste(round(VO- HVinit, digits = 1)),"m^3) than your objective volume.
              You have chosen to continue logging without diversifying in this case.")
    if (diversification && objectivelax)

      message("The harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) was lower
              (by ",paste(round(VO- HVinit, digits = 1)),"m^3) than your objective volume.
              You have chosen to continue logging in this case.")


    if ((!specieslax & !objectivelax) | (diversification && !objectivelax))
      stop("The harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) is lower
           (by ",paste(round(VO- HVinit, digits = 1)),"m^3) than your objective volume.
           By default or because of your choice, the simulation stops.
           If you wish to continue the exploitation with of a harvestable volume lower
           than your objective volume, you can use the argument 'objectivelax'
           or the diversification of species (if not already done).")
  }


  if (HVinit > VO) {

    inventory <- inventory %>%
      mutate(LoggingStatus = ifelse(LoggingStatus == "harvestable" &
                                      CommercialLevel == 1 & (DBH >= UpMinFD & DBH <= MaxFD), #designate the bigger individuals, when the plot is species-rich.
                                    "harvestableUp", LoggingStatus))

    if (!diversification) {
      HarvestableTable <- inventory %>%
        filter(LoggingStatus == "harvestableUp")

      HVupCommercial1 <- sum(HarvestableTable$TreeHarvestableVolume) #Compute the harvestable volume with upgraded FD individuals


      if (HVupCommercial1 == VO){

        inventory <- inventory %>%
          mutate(Selected = ifelse(HVupCommercial1 == VO & LoggingStatus == "harvestableUp", "1", NA))# if harvestableUp individuals are sufficient to have our volume, harvestableUp ind = selected ind

        message("As the harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) was higher
                (by ",paste(round(HVinit-VO, digits = 1)),"m^3) than the objective volume,
                the Minimum Falling Diameter (MinFD) of 1st economic rank species were increased.
                The objective volume has now been reached.")
      }

      if (HVupCommercial1 > VO){
        # only individuals with DBH > FD are taken, but not all because their volumes > VO
        inventory <- inventory %>%
          mutate(Condition = ifelse(LoggingStatus == "harvestableUp", TRUE, FALSE)) %>%
          group_by(Condition) %>%
          arrange(CommercialLevel) %>% # add for reviewer
          arrange(desc(TreeHarvestableVolume)) %>%
          mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
          ungroup() %>%
          mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
          select(-Condition, -VolumeCumSum)

        HarvestableTable <- inventory %>%
          filter(Selected == "1")
        HVupCommercial1adjust <- sum(HarvestableTable$TreeHarvestableVolume) #Harvestable volume, with "1" rank species and upgraded FD individuals only

        message("The harvestable volume (= ",paste(round(HVupCommercial1, digits = 1)),"m^3) was higher
                (by ",paste(round(HVupCommercial1-VO, digits = 1)),"m^3) than your objective volume
                despite the increase in Minimum Falling Diameter (MinFD)
                (Initial harvestable volume = ",paste(round(HVinit, digits = 1)),"m^3).
                Trees were selected by decreasing order of volume
                until the objective volume was reached.")
      }
      if (HVupCommercial1 < VO){

        inventory <- inventory %>%
          mutate(Condition = ifelse(LoggingStatus == "harvestableUp"|LoggingStatus == "harvestable",TRUE,FALSE)) %>%
          group_by(Condition) %>%
          arrange(CommercialLevel) %>% # add for reviewer
          arrange(desc(TreeHarvestableVolume)) %>%
          mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
          ungroup() %>%
          mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
          select(-Condition, -VolumeCumSum)

        HarvestableTable <- inventory %>%
          filter(Selected == "1")
        HVupCommercial1adjust <- sum(HarvestableTable$TreeHarvestableVolume) #Harvestable volume, with "1" rank species and upgraded FD individuals

        message("As the harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3)
                was higher (by ",paste(round(HVinit-VO, digits = 1)),"m^3) than the objective volume,
                the Minimum Falling Diameters (MinFD) of the 1st economic rank species were increased to UpMinFD.
                Some trees with DBH lower than the UpMinFD were however selected
                to ensure that the objective volume was reached.")
      }
    }

    if (diversification) {

      # Increase the MinFD of the other economic species
      inventory <- inventory %>%
        mutate(LoggingStatus = ifelse(LoggingStatus == "harvestable" & CommercialLevel >1 &
                                        (DBH >= UpMinFD & DBH <= MaxFD), #designate preferred individuals of 2nd economic rank species too, when the plot is species-rich.
                                      "harvestableUp", LoggingStatus))

      HarvestableTable <- inventory %>%
        filter(LoggingStatus == "harvestableUp") #the upgraded 1st & 2nd ranks

      HVupCommercial12 <- sum(HarvestableTable$TreeHarvestableVolume)

      if (HVupCommercial12 == VO){
        inventory <- inventory %>%

          mutate(Selected = ifelse(LoggingStatus == "harvestableUp",
                                   "1", NA))# if we have our volume, harvestable ind = selected ind
        message("As the harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3) was higher
                (by ",paste(round(HVinit-VO, digits = 1)),"m^3) than the objective volume,
                the Minimum Falling Diameter (MinFD) of the 1st and 2nd economic ranks species were increased to UpMinFD.
                The objective volume has now been reached.")

      } else if (HVupCommercial12 != VO){

        inventory <- inventory %>%
          mutate(Condition = ifelse(LoggingStatus == "harvestableUp"|LoggingStatus == "harvestable",TRUE,FALSE)) %>%
          group_by(Condition) %>%
          arrange(CommercialLevel) %>% # add for reviewer
          arrange(desc(TreeHarvestableVolume)) %>%
          mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
          ungroup() %>%
          mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
          select(-Condition, -VolumeCumSum)

        HarvestableTable <- inventory %>%
          filter(Selected == "1")
        HVupCommercial12adjust <- sum(HarvestableTable$TreeHarvestableVolume) #Harvestable volume, with upgraded FD individuals


        if (HVupCommercial12 < VO){

          message("As the harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3)
                was higher (by ",paste(round(HVinit-VO, digits = 1)),"m^3) than the objective volume,
                the Minimum Falling Diameters (MinFD) of the 1st and 2nd economic ranks species were increased to UpMinFD.
                Some trees with DBH lower than the UpMinFD were however selected to ensure that the objective
                volume was reached.")
        }

        if (HVupCommercial12 > VO){

          message("As the harvestable volume (= ",paste(round(HVinit, digits = 1)),"m^3)
                was higher (by ",paste(round(HVinit-VO, digits = 1)),"m^3)
                than the objective volume, the Minimum Falling Diameter (MinFD) of the 1st and 2nd economic ranks species
                  were increased to UpMinFD. The objective volume has now been reached.")
        }
      } # end : HVupCommercial12 != VO
    }
  }

  # if at this step there are no selected trees
  if(!any(inventory$Selected == "1") & any(inventory$ProbedHollow == "0")){
    inventory <- inventory %>%
      mutate(Condition = ifelse(LoggingStatus == "harvestable", TRUE, FALSE)) %>%
      group_by(Condition) %>%
      arrange(CommercialLevel) %>% # add for reviewer
      arrange(desc(TreeHarvestableVolume)) %>%
      mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
      ungroup() %>%
      mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
      select(-Condition, -VolumeCumSum)
  }

  # No NA in Selected colomn
  inventory <- mutate(inventory, Selected = ifelse(is.na(Selected) , "0", Selected))

  # Complete to reach the objective volume 'with crumbs'
  HarvestableTable <- inventory %>%
    filter(Selected == "1")

  MissingVolume <- VO - sum(HarvestableTable$TreeHarvestableVolume)
  min <- MissingVolume - 1

  inventory <- inventory %>%
    mutate(Crumbs = ifelse((LoggingStatus == "harvestableUp"|LoggingStatus == "harvestable") & Selected != "1" &
                             (TreeHarvestableVolume > min & TreeHarvestableVolume <= MissingVolume),
                           TRUE, FALSE)) %>%
    group_by(Crumbs) %>%
    arrange(CommercialLevel) %>% # add for reviewer
    arrange(desc(TreeHarvestableVolume))

  if(any(inventory$Crumbs)){

    Crumbsdf <- inventory %>%
      filter(Crumbs %in% TRUE)

    MaxCrumbsV <- max(Crumbsdf$TreeHarvestableVolume)  # the most important remaining volume able to reach the objective

    inventory <- inventory %>%
      mutate(Selected = ifelse(Crumbs %in% TRUE & TreeHarvestableVolume == MaxCrumbsV, "1", Selected))

  }

  inventory <- inventory %>%
    ungroup() %>%
    select(-Crumbs)

  # Add the probed hollow trees removed until now in fuel != 2 case
  if(fuel != "2"){
    inventory <- bind_rows(inventory, HollowTable)
  }

  # No NA in Selected colomn
  inventory <- mutate(inventory, Selected = ifelse(is.na(Selected) , "0", Selected))

  # Upgraded MinFD species:
  # to inform for each individual if its species have been FD upgraded
  inventory <- inventory %>%
    group_by(CommercialName) %>%
    mutate(Up = ifelse(any(LoggingStatus == "harvestableUp"), "1", Up)) %>%
    ungroup()

  if (!any(inventory$Selected == "1")) { # no selected trees

  # if there are "deselected" trees and not of 'selected = 1'
  if (any(inventory$ProbedHollow == "1")){ # probed hollow trees and no selected trees
    stop("No trees were selected because they were all probed hollow
         (",paste(round(sum(as.numeric(inventory$ProbedHollow == "1"), na.rm = TRUE),  digits = 1)),
         " probed hollow trees). Your objective volume may be too low (the few trees selected were found to be
         hollow).")

  }else if (all(inventory$LoggingStatus == "non-harvestable")) {
    stop("No trees selected, you have no harvestable trees (according to your criteria (SpeciesCriteria))
         in your inventory.")
    }
  }

  # Create a POINTS VECTOR with coordinates of the probed hollow trees:
  if (any(inventory$ProbedHollow == "1", na.rm = TRUE)) {
    HollowTreesPoints <- inventory %>%
      filter(ProbedHollow == "1")

    sp::coordinates(HollowTreesPoints) <- ~ Xutm + Yutm

    sp::proj4string(HollowTreesPoints) <- raster::crs(topography)

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

  if ((fuel == "2") & any(inventory$ProbedHollow == "1", na.rm = TRUE)) {
    # Hollow trees = fuel wood:
    inventory <- inventory %>%
      mutate(DeathCause = ifelse(Selected == "1" & ProbedHollow == "1", "hollowfuel", NA))# replace "NA" par "DeathCause" in this code line in the ONF simulator

    # Create a POINTS VECTOR with coordinates of the energywood trees:

    EnergywoodTreesPoints <- HollowTreesPoints


    # OUTPUTS list
    selectedOutputs <- list(inventory = inventory,
                            HollowTreesPoints = HollowTreesPoints,
                            EnergywoodTreesPoints = EnergywoodTreesPoints)

  }

  if(nrow(inventory) != nrow(inventory0))
    stop("The number of rows between the input inventory and the output inventory
         of the function selected() is not the same.The function must be corrected.")

  return(selectedOutputs) # return the new inventory and the 2 points vectors (HollowTrees and EnergywoodTrees)

}


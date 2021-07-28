#' treefelling
#'
#' @param inventory (data.frame)
#' @param scenario "RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel",
#'   "RIL3fuelhollow" or "manual"(character)
#' @param fuel no  exploitation = "0", damage exploitation in fuelwood = "1",
#'   exploitation of hollow trees and damage in fuelwood = "2"
#' @param directionalfelling directional felling = "0" (absent), "1" (only to
#'   avoid damage to future and reserve trees), "2" (avoid damage to future and
#'   reserve trees + track orientation)
#' @param advancedloggingparameters (list)
#' @param AccessibleTreesPoints (MULTIPOINT)
#' @param SelectedTreesPoints (MULTIPOINT)
#' @param AvenirTreesPoints (MULTIPOINT)
#' @param ReserveTreesPoints (MULTIPOINT)
#' @param DeadTreesPoints (MULTIPOINT)
#' @param HollowTreesPoints (MULTIPOINT)
#' @param FuelwoodTreesPoints (MULTIPOINT) MainTrail 2ndTrail
#'
#' @return "Shadow" polygons +  "TreefallSuccess", "TreefallFailure",
#'   "DamageTreesPoints", "DeadTreesPoints" vectors
#' @export
#'
#' @examples
#'
#' \dontrun{
#' data(Paracou6_2016)
#'
#' inventory <- addtreedim(inventorycheckformat(Paracou6_2016))
#'
#' inventory <- treeselection(inventory, SpeciesCriteria, scenario = "manual",
#' fuel = "0",objective = 20,
#' diversification = TRUE, specieslax = FALSE, objectivelax = FALSE,
#' advancedloggingparameters = loggingparameters())$inventory
#'
#' treefelling(inventory, scenario = "manual", fuel = "2", directionalfelling = "2",
#'  advancedloggingparameters = loggingparameters())
#'  }
#'
treefelling <- function(
  inventory,
  scenario = "manual",
  fuel,
  directionalfelling,
  advancedloggingparameters = loggingparameters(),
  AccessibleTreesPoints,
  SelectedTreesPoints,
  AvenirTreesPoints,
  ReserveTreesPoints,
  DeadTreesPoints,
  HollowTreesPoints,
  FuelwoodTreesPoints
  # MainTrail
  # 2ndTrail
){

  # Arguments check

  if(!inherits(inventory, "data.frame"))
    stop("The 'inventory'argument of the 'treefelling' function must be data.frame")

  if (!any(scenario == "RIL1" | scenario == "RIL2broken"| scenario == "RIL2"| scenario == "RIL3"| scenario == "RIL3fuel"|
           scenario == "RIL3fuelhollow"| scenario =="manual"))
    stop("The 'scenario' argument of the 'treefelling' function must be 'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  if (!any(fuel == "0" | fuel == "1"| fuel == "2"| is.null(fuel)))
    stop("The 'fuel' argument of the 'treefelling' function must be '0', '1', or '2'")

  if (!any(directionalfelling == "0" | directionalfelling == "1"| directionalfelling == "2"| is.null(directionalfelling)))
    stop("The 'directionalfelling' argument of the 'treefelling' function must be '0', '1', or '2'")

  if(!inherits(advancedloggingparameters, "list"))
    stop("The 'advancedloggingparameters' argument of the 'treefelling' function must be a list")

  #!!!tester les autres arguments!!!

  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <- Commercial <- NULL
  Commercial.genus <- Commercial.species <- Condition <- DBH <- NULL
  DeathCause <- DistCrit <- Family <- NULL
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


  # Redefinition of the parameters according to the chosen scenario
  scenariosparameters <- scenariosparameters(scenario = scenario, fuel = fuel,
                                             directionalfelling = directionalfelling)

  directionalfelling <- scenariosparameters$directionalfelling
  fuel <- scenariosparameters$fuel

  # Succes-failure determination
  ## if no fuelwood exploitation
  if (fuel == "0" && directionalfelling != "1"){
    inventory <- inventory %>%
      mutate(TreeFellingOrientationSuccess = ifelse(Accessible == "1", sample(c(1,0), size = 1, replace = F, prob = c(advancedloggingparameters$TreefallSuccessProportion, 1-advancedloggingparameters$TreefallSuccessProportion)), NA)) # Accessible = linked by 2ndtrails

    ### Tree coordinates
    if (any(inventory$TreeFellingOrientationSuccess == "1", na.rm = TRUE)) {

      TreefallSuccessCoord <- inventory %>%
        filter(TreeFellingOrientationSuccess == "1") %>%
        select(Xutm, Yutm)

      TreefallSuccess  <- st_multipoint(x = as.matrix(TreefallSuccessCoord))   # Create treefelling success
    }

    if (any(inventory$TreeFellingOrientationSuccess == "0", na.rm = TRUE)) {
      TreefallFailCoord <- inventory %>%
        filter(TreeFellingOrientationSuccess == "0") %>%
        select(Xutm, Yutm)

      TreefallFailure  <- st_multipoint(x = as.matrix(TreefallFailCoord))      # Create treefelling fails

    }

  }

  ## if fuelwwod exploitation from damages (no 2ndary trails yet, and no hollow trees exploitation)
  if (fuel =="1") {

    inventory <- inventory %>%
      mutate(TreeFellingOrientationSuccess = ifelse(Selected == "1", sample(c(1,0), size = 1, replace = F, prob = c(advancedloggingparameters$TreefallSuccessProportion, 1-advancedloggingparameters$TreefallSuccessProportion)), NA)) # Selected = not yet linked by 2ndtrails, because 2ndtrails came after

    #Tree coordinates
    if (any(inventory$TreeFellingOrientationSuccess == "1", na.rm = TRUE)) {

      TreefallSuccessCoord <- inventory %>%
        filter(TreeFellingOrientationSuccess == "1") %>%
        select(Xutm, Yutm)

      TreefallSuccess  <- st_multipoint(x = as.matrix(TreefallSuccessCoord))   # Create treefelling success
    }

    if (any(inventory$TreeFellingOrientationSuccess == "0", na.rm = TRUE)) {
      TreefallFailCoord <- inventory %>%
        filter(TreeFellingOrientationSuccess == "0") %>%
        select(Xutm, Yutm)

      TreefallFailure  <- st_multipoint(x = as.matrix(TreefallFailCoord))      # Create treefelling fails

    }
  }

  ## if hollow trees exploitation (no 2ndary trails yet)
  if (fuel =="2") {

    inventory <- inventory %>%
      mutate(TreeFellingOrientationSuccess = ifelse(Selected == "1"| ProbedHollow == "1", sample(c(1,0), size = 1, replace = F, prob = c(advancedloggingparameters$TreefallSuccessProportion, 1-advancedloggingparameters$TreefallSuccessProportion)), NA)) # Selected = not yet linked by 2ndtrails, because 2ndtrails came after

    #Tree coordinates
    if (any(inventory$TreeFellingOrientationSuccess == "1", na.rm = TRUE)) {

      TreefallSuccessCoord <- inventory %>%
        filter(TreeFellingOrientationSuccess == "1") %>%
        select(Xutm, Yutm)

      TreefallSuccess  <- st_multipoint(x = as.matrix(TreefallSuccessCoord))   # Create treefelling success
    }

    if (any(inventory$TreeFellingOrientationSuccess == "0", na.rm = TRUE)) {
      TreefallFailCoord <- inventory %>%
        filter(TreeFellingOrientationSuccess == "0") %>%
        select(Xutm, Yutm)

      TreefallFailure  <- st_multipoint(x = as.matrix(TreefallFailCoord))      # Create treefelling fails

    }
  }

  treefellingOutputs <- list(inventory = inventory,
                             TreefallSuccess = TreefallSuccess,
                             TreefallFailure = TreefallFailure)

  return(treefellingOutputs)

}

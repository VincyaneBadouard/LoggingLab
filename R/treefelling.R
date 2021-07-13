#' treefelling
#'
#' @param inventory (data.frame)
#' @param type "RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel", "RIL3fuelhollow" or "manual"(character)
#' @param fuel no  exploitation = "0", damage exploitation in fuelwood = "1",
#' exploitation of hollow trees and damage in fuelwood = "2"
#' @param directionalfelling directional felling = "0" (absent),
#' "1" (only to avoid damage to future and reserve trees), "2" (avoid damage to future and reserve trees + track orientation)
#' @param otherloggingparameters (list)
#' @param AccessibleTreesPoints (MULTIPOINT)
#' @param SelectedTreesPoints (MULTIPOINT)
#' @param AvenirTreesPoints (MULTIPOINT)
#' @param ReserveTreesPoints (MULTIPOINT)
#' @param DeadTreesPoints (MULTIPOINT)
#' @param HollowTreesPoints (MULTIPOINT)
#' @param FuelwoodTreesPoints (MULTIPOINT) MainTrail 2ndTrail
#'
#' @return "Shadow" polygons +  "TreefallSuccess", "TreefallFailure", "DamageTreesPoints", "DeadTreesPoints" vectors
#' @export
#'
#' @examples
#'
#' inventory <- addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))
#'
#' inventory <- treeselection(inventory, SpeciesCriteria, type = "manual", fuel = "0",objective = 20,
#' diversification = TRUE, specieslax = FALSE, objectivelax = FALSE, otherloggingparameters = loggingparameters())$inventory
#'
#' treefelling(inventory, type = "manual", fuel = "2", directionalfelling = "2", otherloggingparameters = loggingparameters())
#'
treefelling <- function(
  inventory,
  type = "manual",
  fuel,
  directionalfelling,
  otherloggingparameters = loggingparameters(),
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

  if (!any(type == "RIL1" | type == "RIL2broken"| type == "RIL2"| type == "RIL3"| type == "RIL3fuel"|
           type == "RIL3fuelhollow"| type =="manual"))
    stop("The 'type' argument of the 'treefelling' function must be 'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  if (!any(fuel == "0" | fuel == "1"| fuel == "2"| is.null(fuel)))
    stop("The 'fuel' argument of the 'treefelling' function must be '0', '1', or '2'")

  if (!any(directionalfelling == "0" | directionalfelling == "1"| directionalfelling == "2"| is.null(directionalfelling)))
    stop("The 'directionalfelling' argument of the 'treefelling' function must be '0', '1', or '2'")

  if(!inherits(otherloggingparameters, "list"))
    stop("The 'otherloggingparameters' argument of the 'treefelling' function must be a list")

  #!!!tester les autres arguments!!!

  # Redefinition of the parameters according to the chosen scenario
  scenariosparameters <- scenariosparameters(type = type, fuel = fuel,
                                             directionalfelling = directionalfelling)

  directionalfelling <- scenariosparameters$directionalfelling
  fuel <- scenariosparameters$fuel

  # Succes-failure determination
  ## if no fuelwood exploitation
  if (fuel == "0" && directionalfelling != "1"){
    inventory <- inventory %>%
      mutate(TreeFellingOrientationSuccess = ifelse(Accessible == "1", sample(c(1,0), size = 1, replace = F, prob = c(otherloggingparameters$TreefallSuccessProportion, 1-otherloggingparameters$TreefallSuccessProportion)), NA)) # Accessible = linked by 2ndtrails

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
      mutate(TreeFellingOrientationSuccess = ifelse(Selected == "1", sample(c(1,0), size = 1, replace = F, prob = c(otherloggingparameters$TreefallSuccessProportion, 1-otherloggingparameters$TreefallSuccessProportion)), NA)) # Selected = not yet linked by 2ndtrails, because 2ndtrails came after

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
      mutate(TreeFellingOrientationSuccess = ifelse(Selected == "1"| ProbedHollow == "1", sample(c(1,0), size = 1, replace = F, prob = c(otherloggingparameters$TreefallSuccessProportion, 1-otherloggingparameters$TreefallSuccessProportion)), NA)) # Selected = not yet linked by 2ndtrails, because 2ndtrails came after

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

}

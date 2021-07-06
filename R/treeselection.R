#' treeselection
#'
#' @param inventory (data.frame)
#' @param objective Objective volume per hectare (numeric)

#' @param type "RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel", "RIL3fuelhollow" or "manual"(character)
#' @param fuel no  exploitation = "0", damage exploitation in fuelwood = "1",
#' exploitation of hollow trees and damage in fuelwood = "2"
#' @param diversification taking of other species in addition to the main commercial species (logical)
#' @param specieslax Allow diversification if stand is too poor, = FALSE by default (logical)
#' @param objectivelax Allow exploitation in case of non-achievement of the objective volume (if stand too poor), = FALSE by default (logical)
#' @param speciescriteria (data.frame)
#' @param otherloggingparameters (list) MainTrail (multiline)
#'
#' @return
#' @export
#'
#' @examples
#' inventory = addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))
#'
#'treeselection(inventory, SpeciesCriteria,
#'type ="manual", fuel = "0",objective = 20, diversification = TRUE, specieslax = FALSE,
#'objectivelax = FALSE, otherloggingparameters = loggingparameters()) # , MainTrail

treeselection <- function(
  inventory,
  objective,
  type = "manual",
  fuel,
  diversification,
  specieslax = FALSE,
  objectivelax = FALSE,
  speciescriteria = SpeciesCriteria,
  otherloggingparameters = loggingparameters()
  # MainTrail

){

  # Arguments check

  if(!any(unlist(lapply(list(inventory, speciescriteria), inherits, "data.frame"))))
    stop("The 'inventory' and 'speciescriteria' arguments of the 'treeselection' function must be data.frame")

  if(!inherits(objective, "numeric") && !is.null(objective))
    stop("The 'objective' argument of the 'treeselection' function must be numeric")

  if(!all(unlist(lapply(list(diversification, specieslax, objectivelax), inherits, "logical"))) && !is.null(diversification))
    stop("The 'diversification', 'specieslax' and 'objectivelax' arguments of the 'treeselection' function must be logical") # any() don't take a list

  if (!any(type == "RIL1" | type == "RIL2broken"| type == "RIL2"| type == "RIL3"| type == "RIL3fuel"|
           type == "RIL3fuelhollow"| type =="manual"))
    stop("The 'type' argument of the 'treeselection' function must be 'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  if (!any(fuel == "0" | fuel == "1"| fuel == "2"| is.null(fuel)))
    stop("The 'fuel' argument of the 'treeselection' function must be '0', '1', or '2'")

  if(!inherits(otherloggingparameters, "list"))
    stop("The 'otherloggingparameters' argument of the 'treeselection' function must be a list")

  # Redefinition of the parameters according to the chosen scenario
  scenariosparameters <- scenariosparameters(objective = objective, type = type, fuel = fuel,
                                             diversification = diversification)

  objective <- scenariosparameters$objective
  fuel <- scenariosparameters$fuel
  diversification <- scenariosparameters$diversification

  # Function

  if ("DeathCause" %in% names(inventory)) filter(inventory, is.null(DeathCause)) #select only still alived trees (after MainTrails) NULL ou NA

  # inventory <- inventory %>%
  # Visible defect trees detection:
  # mutate(VisibleDefectProba = otherloggingparameters$VisiblyRottenModel(DBH)) %>%
  # mutate(VisibleDefect = sample(c(0,1), size = 1, replace = F, prob = VisibleDefectProba)) # 1 = default tree, 0 = no visible default (là ma fct ne sait pas qd elle doit mettre 0 ou 1)

  # filter(VisibleDefect == "0") %>%

  # Compute the objective volume with or without bonus:
  if (fuel =="2") {VO = objective
  }else{
    VO = objective + otherloggingparameters$ObjectiveBonus # to compensate for the designated hollow wood.

  }


  inventory <- harvestable(                                                      # interne fucntion
    ONFGuyafortaxojoin(inventory, speciescriteria = speciescriteria),            # interne fucntion
    diversification = diversification, specieslax = specieslax)$inventory        # one of the output

  HVinit <- harvestable(                                                         # interne fucntion
    ONFGuyafortaxojoin(inventory, speciescriteria = speciescriteria),            # interne fucntion
    diversification = diversification, specieslax = specieslax)$HVinit           # the other output

  inventory <- selected(
    inventory,
    type = type, fuel = fuel, diversification = diversification,
    specieslax = specieslax, objectivelax = objectivelax,
    otherloggingparameters = otherloggingparameters, VO = VO, HVinit = HVinit)$inventory

  inventory <- futurereserve(inventory)


  # créer les conditions et vecteurs vides dans les listes à retourner
  # Points vector with coordinates of the harvestable trees:
  HarvestableTreescoord <- inventory %>%
    filter(LoggingStatus == "harvestable" |LoggingStatus == "harvestableUp" |LoggingStatus == "harvestable2nd") %>%
    select(Xutm, Yutm)

  HarvestableTreesPoints  <- st_multipoint(x = as.matrix(HarvestableTreescoord))

  # Points vector with coordinates of the selected trees:
  SelectedTreescoord <- inventory %>%
    filter(Selected == "1") %>%
    select(Xutm, Yutm)

  SelectedTreesPoints  <- st_multipoint(x = as.matrix(SelectedTreescoord))

  # Points vector with coordinates of the future trees:
  FutureTreescoord <- inventory %>%
    filter(LoggingStatus == "future") %>%
    select(Xutm, Yutm)

  FutureTreesPoints  <- st_multipoint(x = as.matrix(FutureTreescoord))

  # Points vector with coordinates of the reserve trees:
  ReserveTreescoord <- inventory %>%
    filter(LoggingStatus == "reserve") %>%
    select(Xutm, Yutm)

  ReserveTreesPoints  <- st_multipoint(x = as.matrix(ReserveTreescoord))

  #where specieslax was not necessary, consider eco2s as non-exploitable:
  inventory <- inventory %>%
    mutate(LoggingStatus = ifelse(HVinit > VO & LoggingStatus == "harvestable2nd", "non-harvestable", LoggingStatus))

  # OUTPUTS list
  treeselectionOutputs <- list(inventory = inventory,
                               VO = VO,
                               HarvestableTreesPoints = HarvestableTreesPoints,
                               SelectedTreesPoints = SelectedTreesPoints,
                               FutureTreesPoints = FutureTreesPoints,
                               ReserveTreesPoints = ReserveTreesPoints)



  return(treeselectionOutputs) # return the new inventory, the objective volume,
  # and the 4 points vectors (Harvestable, Selected, Future and Reserve Trees)
#faut rajouter les ouputs des fcts internes
}

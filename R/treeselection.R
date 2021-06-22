#' treeselection
#'
#' @param inventory (data.frame)
#' @param speciescriteria (data.frame)
#' @param type "RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel", "RIL3fuelhollow" or "manual"(character)
#' @param fuel no  exploitation = "0" (default),
#' damage exploitation in fuelwood = "1", exploitation of hollow trees and damage in fuelwood = "2"
#' @param objective Objective volume per hectare (numeric)  diversificationparam specieslaxparam objectivelax
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
#'objectivelax = FALSE, otherloggingparameters = loggingparameters(), MainTrail)

treeselection <- function(
  inventory,
  speciescriteria = SpeciesCriteria,
  type = c("RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel", "RIL3fuelhollow", "manual"),
  fuel = "0",
  objective,
  # diversification,
  # specieslax = FALSE,
  # objectivelax = FALSE,
  otherloggingparameters = loggingparameters()
  # MainTrail

){ # @include .ONFGuyafortaxojoin, .harvestable, .selected, .futurereserve functions

  if ("DeathCause" %in% names(inventory)) filter(inventory, is.null(DeathCause)) #select only still alived trees (after MainTrails) NULL ou NA

  inventory <- inventory %>%
    # Visible defect trees detection:
    # mutate(VisibleDefectProba = otherloggingparameters$VisiblyRottenModel(DBH)) %>%
    # mutate(VisibleDefect = sample(c(0,1), size = 1, replace = F, prob = VisibleDefectProba)) # 1 = default tree, 0 = no visible default (là ma fct ne sait pas qd elle doit mettre 0 ou 1)

    # filter(VisibleDefect == "0") %>%

    # Compute the objective volume with or without bonus:
    if (type == "RIL3fuelhollow"| (type == "manual"& fuel =="2")) {VO = objective
    }else{
      VO = objective + otherloggingparameters$ObjectiveBonus # to compensate for the designated hollow wood.
      left_join(inventory, speciescriteria, by = "ScientificName") #or by VernName

    }



  # Créer un vecteur ("HarvestableTreesPoints") de points de coordonnées des arbres LoggingStatus = "harvestable" ou "harvestableUp" ou "harvestable2nd".
  # Créer un vecteur ("SelectedTreesPoints") de points de coordonnées des arbres Selected = 1.
  # Créer un vecteur ("FutureTreesPoints") de points de coordonnées des arbres LoggingStatus = "future"
  # Créer un vecteur ("ReserveTreesPoints") de points de coordonnées des arbres LoggingStatus = "reserve".

  #where specieslax was not necessary, consider eco2s as non-exploitable:
  inventory <- inventory %>%
    mutate(LoggingStatus = ifelse(HVinit > VO & LoggingStatus == "harvestable2nd", "non-harvestable", LoggingStatus))


}

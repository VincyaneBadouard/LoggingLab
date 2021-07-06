#' scenariosparameters
#'
#' @param objective Objective volume per hectare (numeric)
#' @param type "RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel", "RIL3fuelhollow" or "manual"(character)
#' @param fuel no  exploitation = "0", damage exploitation in fuelwood = "1",
#' exploitation of hollow trees and damage in fuelwood = "2"
#' @param diversification  taking of other species in addition to the main commercial species (logical)
#' @param winching no cable or grapple = "0", only cable = "1", grapple + cable = "2"
#' @param directionalfelling directional felling = "0" (absent),
#' "1" (only to avoid damage to future and reserve trees), "2" (avoid damage to future and reserve trees + track orientation)
#'
#' @return A named list of 5 objects.
#' @export
#'
#' @examples
#'scenariosparameters(objective = NULL, type = "RIL1",
#'fuel = NULL, diversification = NULL, winching = NULL, directionalfelling = NULL)
#'
scenariosparameters <- function(
  objective = NULL,
  type,
  fuel = NULL,
  diversification = NULL,
  winching = NULL,
  directionalfelling = NULL

){
  # Objective volume
  if (is.null(objective)){
    if (type == "RIL1")           objective = 25
    if (type == "RIL2broken")     objective = 25
    if (type == "RIL2")           objective = 25
    if (type == "RIL3")           objective = 30
    if (type == "RIL3fuel")       objective = 30
    if (type == "RIL3fuelhollow") objective = 30

  }
# Fuelwood exploitation
  if (is.null(fuel)){
    if (type == "RIL1")           fuel = "0"
    if (type == "RIL2broken")     fuel = "0"
    if (type == "RIL2")           fuel = "0"
    if (type == "RIL3")           fuel = "0"
    if (type == "RIL3fuel")       fuel = "1"
    if (type == "RIL3fuelhollow") fuel = "2"

  }
#
  if (is.null(diversification)){
    if (type == "RIL1")           diversification = FALSE
    if (type == "RIL2broken")     diversification = FALSE
    if (type == "RIL2")           diversification = FALSE
    if (type == "RIL3")           diversification = TRUE
    if (type == "RIL3fuel")       diversification = TRUE
    if (type == "RIL3fuelhollow") diversification = TRUE

  }

  if (is.null(winching)){
    if (type == "RIL1")           winching = "0"
    if (type == "RIL2broken")     winching = "0"
    if (type == "RIL2")           winching = "1"
    if (type == "RIL3")           winching = "2"
    if (type == "RIL3fuel")       winching = "2"
    if (type == "RIL3fuelhollow") winching = "2"

  }

  if (is.null(directionalfelling)){
    if (type == "RIL1")           directionalfelling = "0"
    if (type == "RIL2broken")     directionalfelling = "1"
    if (type == "RIL2")           directionalfelling = "1"
    if (type == "RIL3")           directionalfelling = "2"
    if (type == "RIL3fuel")       directionalfelling = "2"
    if (type == "RIL3fuelhollow") directionalfelling = "2"

  }

  ScenariosParameters <- list(objective = objective,
                              fuel = fuel,
                              diversification = diversification,
                              winching = winching,
                              directionalfelling = directionalfelling)

  return(ScenariosParameters)

  }

# Type	SpatialDatatype	Winching 	DirectionalFelling 	Objective 	Diversification
# RIL1	           SRTM	    0	           0	            20-25	         FALSE
# RIL2broken	    LIDAR	    0	           1	            20-25	         FALSE
# RIL2	          LIDAR     1	           1	            20-25	         FALSE
# RIL3	          LIDAR	    2	           2	            25-30	          TRUE
# RIL3fuel	      LIDAR	    2	           2	            25-30	          TRUE
# RIL3fuelhollow	LIDAR	    2	           2	            25-30         	TRUE



# Redefinition of the parameters according to the chosen scenario
# scenariosparameters <- scenariosparameters(objective = objective, type = type, fuel = fuel,
#                                            diversification = diversification, winching = winching, directionalfelling = directionalfelling)
#
# objective <- scenariosparameters$objective
# fuel <- scenariosparameters$fuel
# diversification <- scenariosparameters$diversification
# winching <- scenariosparameters$winching
# directionalfelling <- scenariosparameters$directionalfelling

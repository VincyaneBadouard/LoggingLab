#' Parameters of the scenarios
#'
#'@param scenario Logging scenario: "RIL1", "RIL2broken", "RIL2", "RIL3",
#'  "RIL3fuel", "RIL3fuelhollow" or "manual"(character) (see the
#'  vignette)
#'
#'@param objective Objective volume (m^3/ha) (numeric)
#'
#'@param fuel Fuel wood exploitation: no exploitation = "0", exploitation of
#'   damage and unused part of logged trees for fuel = "1", exploitation of
#'   hollow trees, damage and and unused part of the log for fuel = "2"
#'
#'@param diversification Possibility to log other species in addition to the main
#'  commercial species (species with a value of 2  for commercial in the
#'  \code{\link{SpeciesCriteria}} table) (logical)
#'
#'@param winching No cable or grapple = "0", cable only = "1", grapple + cable =
#'  "2"
#'
#'@param directionalfelling Directional felling =
#' "0": only to direct the foot of the tree towards the trail
#' "1": to direct the foot of the tree towards the trail + to avoid damage to
#'         future and reserve trees
#' "2": to avoid damage to future and reserve trees + orientation angle
#'       to the trail
#'
#'@return A named list of 5 objects.
#'@export
#'
#' @examples
#'scenariosparameters(scenario = "RIL1")
#'
scenariosparameters <- function(
  scenario,
  objective = NULL,
  fuel = NULL,
  diversification = NULL,
  winching = NULL,
  directionalfelling = NULL
){

  # check inputs
  if(!(scenario %in% c("RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel", "RIL3fuelhollow", "manual")))
    stop(paste('"scenario" argument should be in "RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel",
               "RIL3fuelhollow" or "manual"; not',
               scenario))

  if(!(class(objective) %in% c("numeric", "NULL")))
    stop("'objective' argument should be numeric or null.")

  if(!all(unlist(lapply(list(fuel, directionalfelling, winching), class)) %in% c("character", "NULL")))
    stop("'fuel', 'winching', and 'directionalfelling' arguments should be character or null.")

  if(!(class(diversification) %in% c("logical", "NULL")))
    stop("'diversification' argument should be logical or null.")


  # Objective volume
  if (is.null(objective))
    objective <- switch(scenario,
                        "RIL1" = 25,
                        "RIL2broken" = 25,
                        "RIL2" = 25,
                        "RIL3" = 30,
                        "RIL3fuel" = 30,
                        "RIL3fuelhollow" = 30
    )

  # Fuel wood exploitation
  if (is.null(fuel))
    fuel <- switch(scenario,
                   "RIL1" = "0",
                   "RIL2broken" = "0",
                   "RIL2" = "0",
                   "RIL3" = "0",
                   "RIL3fuel" = "1",
                   "RIL3fuelhollow" = "2"
    )

  # Diversification
  if (is.null(diversification))
    diversification <- switch(scenario,
                              "RIL1" = FALSE,
                              "RIL2broken" = FALSE,
                              "RIL2" = FALSE,
                              "RIL3" = TRUE,
                              "RIL3fuel" = TRUE,
                              "RIL3fuelhollow" = TRUE
    )

  # Type of winching
  if (is.null(winching))
    winching <- switch(scenario,
                       "RIL1" = "0",
                       "RIL2broken" = "0",
                       "RIL2" = "1",
                       "RIL3" = "2",
                       "RIL3fuel" = "2",
                       "RIL3fuelhollow" = "2"
    )

  # Directionalfelling level
  if (is.null(directionalfelling))
    directionalfelling <- switch(scenario,
                                 "RIL1" = "0",
                                 "RIL2broken" = "0",
                                 "RIL2" = "0",
                                 "RIL3" = "2",
                                 "RIL3fuel" = "2",
                                 "RIL3fuelhollow" = "2"
    )


  ScenariosParameters <- list(objective = objective,
                              fuel = fuel,
                              diversification = diversification,
                              winching = winching,
                              directionalfelling = directionalfelling)

  return(ScenariosParameters)

}

# scenario	SpatialDatatype	Winching 	DirectionalFelling 	Objective 	Diversification
# RIL1	           SRTM	    0	           0	              20-25	         FALSE
# RIL2broken	    LIDAR	    0	           0	              20-25	         FALSE
# RIL2	          LIDAR     1	           0	              20-25	         FALSE
# RIL3	          LIDAR	    2	           2	              25-30	          TRUE
# RIL3fuel	      LIDAR	    2	           2	              25-30	          TRUE
# RIL3fuelhollow	LIDAR	    2	           2	              25-30         	TRUE



# Redefinition of the parameters according to the chosen scenario
# scenariosparameters <- scenariosparameters(objective = objective, scenario = scenario, fuel = fuel,
#                                            diversification = diversification, winching = winching, directionalfelling = directionalfelling)
#
# objective <- scenariosparameters$objective
# fuel <- scenariosparameters$fuel
# diversification <- scenariosparameters$diversification
# winching <- scenariosparameters$winching
# directionalfelling <- scenariosparameters$directionalfelling

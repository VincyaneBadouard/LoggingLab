#' Check & format your inventory data for the package "Maria"
#'
#' @param inventory (dataframe) Your inventory data or the "Paracou6_2016" test data#'
#' @return stop the function if the format is not the one required.
#' @export
#'
#' @examples
#'
#' data(Paracou6_2016)
#' inventorycheckformat(inventory = Paracou6_2016)
#'
inventorycheckformat <- function(
  inventory
){

  GoodData <- TRUE # usefull boolean for later

  GeneralStop <- "" # empty character string to stock all the warnings

  # Variables presence check
  # (Plot, CensusYear, idTree, Family, Genus, Species, CircCorr, CodeAlive, CommercialSp, UTMZone, Lat, Lon, VernName, Xfield, Yfield, Xutm, Yutm)
  if(!("Plot" %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Plot variable is not found.")
  }
  if(!("CensusYear" %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "CensusYear variable is not found.")
  }
  if(!("idTree" %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "idTree variable is not found.")
}
  if(!("Family" %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Family variable is not found.")
}
  if(!("Genus" %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Genus variable is not found.")
}
  if(!("Species" %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Species variable is not found.")
}
  if(!("CircCorr" %in% names(inventory) | "DBH" %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "CircCorr or DBH variable is not found.")
}
  if(!("CodeAlive" %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "CodeAlive variable is not found.")
}
  if(!("CommercialSp" %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "CommercialSp variable is not found.")
}
  if(!("UTMZone" %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "UTMZone variable is not found.")
}
  if(!('Lat' %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Lat variable is not found.")
}
  if(!('Lon' %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Lon variable is not found.")
}
  if(!('VernName' %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "VernName variable is not found.")
}
  if(!('Xfield' %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Xfield variable is not found.")
}
  if(!('Yfield' %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Yfield variable is not found.")
}
  if(!('Xutm' %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Xutm variable is not found.")
}
  if(!('Yutm' %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Yutm variable is not found.")
}
  # Variables class check
  if (!inherits(inventory$idTree, "integer")) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "idTree variable should be an integer.")
}

  if (!inherits(inventory$Plot, "character")) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Plot variable should be a character.")
}

  if (!inherits(inventory$Xfield, "numeric")) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Xfield variable should be numeric.")
}

  if (!inherits(inventory$Yfield, "numeric")) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Yfield variable should be numeric.")
}

  if (!inherits(inventory$Xutm, "numeric")) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Xutm variable should be numeric.")
}

  if (!inherits(inventory$Yutm, "numeric")) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Yutm variable should be numeric.")
}

  if (!inherits(inventory$UTMZone, "integer")) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "UTMZone variable should be an integer.")
}

  if (!inherits(inventory$Lat, "numeric")) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Lat variable should be numeric.")
}

  if (!inherits(inventory$Lon, "numeric")) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Lon variable should be numeric.")
}

  if (!inherits(inventory$Family, "character")) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Family variable should be a character.")
}

  if (!inherits(inventory$Genus, "character")) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Genus variable should be a character.")
}

  if (!inherits(inventory$Species, "character")) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Species variable should be a character.")
}

  if (!inherits(inventory$VernName, "character")) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "VernName variable should be a character.")
}

  if (!inherits(inventory$CommercialSp, "logical")) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "CommercialSp variable should be logical.")
}

  if (!inherits(inventory$CensusYear, "integer")) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "CensusYear variable should be an integer.")
}

  if (!inherits(inventory$CodeAlive, "logical")) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "CodeAlive variable should be logical.")
}

  if(("CircCorr" %in% names(inventory) & !inherits(inventory$CircCorr, "numeric"))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "CircCorr variable should be numeric.")
}

  if(("DBH" %in% names(inventory) & !inherits(inventory$DBH, "numeric"))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "DBH variable should be numeric.")
}

  if (!GoodData)# inverse value of the object
    stop(paste ("Your inventory does not comply.", GeneralStop))

  return(inventory)
}

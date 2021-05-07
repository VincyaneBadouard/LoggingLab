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
  # Variables presence check
  # (Plot, CensusYear, idTree, Family, Genus, Species, CircCorr, CodeAlive, CommercialSp, UTMZone, Lat, Lon, VernName, Xfield, Yfield, Xutm, Yutm)
  if(!("Plot" %in% names(inventory)))
    stop("Plot variable is not found")
  if(!("CensusYear" %in% names(inventory)))
    stop("CensusYear variable is not found")
  if(!("idTree" %in% names(inventory)))
    stop("idTree variable is not found")
  if(!("Family" %in% names(inventory)))
    stop("Family variable is not found")
  if(!("Genus" %in% names(inventory)))
    stop("Genus variable is not found")
  if(!("Species" %in% names(inventory)))
    stop("Species variable is not found")
  if(!("CircCorr" %in% names(inventory) | "DBH" %in% names(inventory)))
    stop("CircCorr or DBH variable is not found")
  if(!("CodeAlive" %in% names(inventory)))
    stop("CodeAlive variable is not found")
  if(!("CommercialSp" %in% names(inventory)))
    stop("CommercialSp variable is not found")
  if(!("UTMZone" %in% names(inventory)))
    stop("UTMZone variable is not found")
  if(!('Lat' %in% names(inventory)))
    stop("Lat variable is not found")
  if(!('Lon' %in% names(inventory)))
    stop("Lon variable is not found")
  if(!('VernName' %in% names(inventory)))
    stop("VernName variable is not found")
  if(!('Xfield' %in% names(inventory)))
    stop("Xfield variable is not found")
  if(!('Yfield' %in% names(inventory)))
    stop("Yfield variable is not found")
  if(!('Xutm' %in% names(inventory)))
    stop("Xutm variable is not found")
  if(!('Yutm' %in% names(inventory)))
    stop("Yutm variable is not found")
  # Variables class check
  if (!inherits(inventory$idTree, "integer"))
    stop("idTree variable should be an integer")

  if (!inherits(inventory$Plot, "character"))
    stop("Plot variable should be a character.")

  if (!inherits(inventory$Xfield, "numeric"))
    stop("Xfield variable should be numeric.")

  if (!inherits(inventory$Yfield, "numeric"))
    stop("Yfield variable should be numeric.")

  if (!inherits(inventory$Xutm, "numeric"))
    stop("Xutm variable should be numeric.")

  if (!inherits(inventory$Yutm, "numeric"))
    stop("Yutm variable should be numeric.")

  if (!inherits(inventory$UTMZone, "integer"))
    stop("UTMZone variable should be an integer.")

  if (!inherits(inventory$Lat, "numeric"))
    stop("Lat variable should be numeric.")

  if (!inherits(inventory$Lon, "numeric"))
    stop("Lon variable should be numeric.")

  if (!inherits(inventory$Family, "character"))
    stop("Family variable should be a character.")

  if (!inherits(inventory$Genus, "character"))
    stop("Genus variable should be a character.")

  if (!inherits(inventory$Species, "character"))
    stop("Species variable should be a character.")

  if (!inherits(inventory$VernName, "character"))
    stop("VernName variable should be a character.")

  if (!inherits(inventory$CommercialSp, "logical"))
    stop("CommercialSp variable should be logical.")

  if (!inherits(inventory$CensusYear, "integer"))
    stop("CensusYear variable should be an integer.")

  if (!inherits(inventory$CodeAlive, "logical"))
    stop("CodeAlive variable should be logical.")

  if(("CircCorr" %in% names(inventory) & !inherits(inventory$CircCorr, "numeric")))
    stop("CircCorr variable should be numeric.")

  if(("DBH" %in% names(inventory) & !inherits(inventory$DBH, "numeric")))
    stop("DBH variable should be numeric.")


}

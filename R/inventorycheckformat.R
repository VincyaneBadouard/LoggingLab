#' Check the input inventory data for the "LoggingLab" package
#'
#' @description Check if the input inventory data is compatible with the
#'   "LoggingLab" package (see 'Required format of the inventory' section of the
#'   vignette vignette)
#'
#' @param inventory Input inventory (see the inputs formats and metadata in the
#'   vignette) (data.frame)
#'
#' @return The inventory if the inventory is in the required format.
#' Stop the function if the format is not the one required.
#' @export
#'
#' @importFrom dplyr mutate
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

  # Argument check
  if (!inherits(inventory, "data.frame"))
    stop("inventory must be a data.frame")

  # Global variables
  Accessible <- CircCorr <- CodeAlive <- NULL
  Condition <- DBH <- NULL
  DeathCause <- DistCriteria <- Family <- NULL
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

  #### Inventory format ####


  #### Variables presence check ####
  # (Plot, CensusYear, idTree, Family, Genus, Species, CircCorr, CodeAlive,
  # UTMZone,  Xutm, Yutm)
  if(!("Plot" %in% names(inventory))) {
    inventory$Plot = "UnknownPlot"
    warning("The 'Plot' variable is not found, we assume that all data come from one plot.")
  }
  if(!("CensusYear" %in% names(inventory))) {
    inventory$CensusYear = NA_integer_
    warning("The 'CensusYear' variable is not found, we assume that all data come from one year.")
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
    possible <- lapply(c("dbh", "diameter", "circ", "circumference"),
                       function(x) names(inventory)[grepl(x, names(inventory), ignore.case = TRUE)])
    possible <- unique(unlist(possible))
    if(all(!unlist(lapply(possible, is.na))))
      GeneralStop <- paste(GeneralStop, "Maybe the columns", paste(possible, collapse = ", "),
                           "can be used to defined CircCorr. As a reminder CircCorr = DBH*pi.")
  }
  if(!("CodeAlive" %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "CodeAlive variable is not found.")
  }
  if(!("UTMZone" %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "UTMZone variable is not found.")
  }
  if(!('Xutm' %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Xutm variable is not found.")
  }
  if(!('Yutm' %in% names(inventory))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Yutm variable is not found.")
  }

  if (!GoodData)# inverse value of the object
    stop(paste ("Input inventory does not comply.", GeneralStop))

  #### Variables class check ####
  if (!inherits(inventory$idTree, "integer")) {
    if(all(as.integer(inventory$idTree) == inventory$idTree))
      inventory$idTree <- as.integer(inventory$idTree)
    if(!all(as.integer(inventory$idTree) == inventory$idTree)) {
      GoodData <- FALSE
      GeneralStop <- paste (GeneralStop, "idTree variable should be an integer")
    }
  }

  if (!inherits(inventory$Plot, "character")) {
    if(all(as.character(inventory$Plot) == inventory$Plot))
      inventory$Plot <- as.character(inventory$Plot)
    if(!all(as.character(inventory$Plot) == inventory$Plot)) {
      GoodData <- FALSE
      GeneralStop <- paste (GeneralStop, "Plot variable should be a character.")
    }
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
    if(all(as.integer(inventory$UTMZone) == inventory$UTMZone))
      inventory$UTMZone <- as.integer(inventory$UTMZone)
    if(!all(as.integer(inventory$UTMZone) == inventory$UTMZone)) {
      GoodData <- FALSE
      GeneralStop <- paste (GeneralStop, "UTMZone variable should be an integer.")
    }
  }

  if (!inherits(inventory$Family, "character")) {
    if(all(as.character(inventory$Family) == inventory$Family))
      inventory$Family <- as.character(inventory$Family)
    if(!all(as.character(inventory$Family) == inventory$Family)) {
      GoodData <- FALSE
      GeneralStop <- paste (GeneralStop, "Family variable should be a character.")
    }
  }

  if (!inherits(inventory$Genus, "character")) {
    if(all(as.character(inventory$Genus) == inventory$Genus))
      inventory$Genus <- as.character(inventory$Genus)
    if(!all(as.character(inventory$Genus) == inventory$Genus)) {
      GoodData <- FALSE
      GeneralStop <- paste (GeneralStop, "Genus variable should be a character.")
    }
  }

  if (!inherits(inventory$Species, "character")) {
    if(all(as.character(inventory$Species) == inventory$Species))
      inventory$Species <- as.character(inventory$Species)
    if(!all(as.character(inventory$Species) == inventory$Species)) {
      GoodData <- FALSE
      GeneralStop <- paste (GeneralStop, "Species variable should be a character.")
    }
  }

  if (!inherits(inventory$CensusYear, "integer")) {
    if(all(as.integer(inventory$CensusYear) == inventory$CensusYear))
      inventory$CensusYear <- as.integer(inventory$CensusYear)
    if(!all(as.integer(inventory$CensusYear) == inventory$CensusYear)) {
      GoodData <- FALSE
      GeneralStop <- paste (GeneralStop, "CensusYear variable should be an integer.")
    }
  }

  if (!inherits(inventory$CodeAlive, "logical")) {
    if(all(as.logical(inventory$CodeAlive) == inventory$CodeAlive))
      inventory$CodeAlive <- as.logical(inventory$CodeAlive)
    if(!all(as.logical(inventory$CodeAlive) == inventory$CodeAlive)) {
      GoodData <- FALSE
      GeneralStop <- paste (GeneralStop, "CodeAlive variable should be logical.")
    }
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
    stop(paste ("Input inventory does not comply.", GeneralStop))

  #if DBH (cm) doesn't exist create it
  if (!("DBH" %in% names(inventory)) && ("CircCorr" %in% names(inventory))) {
    inventory <- mutate(inventory, DBH = CircCorr/pi)
  }

  # test DBH interval is in cm
  if (any(inventory$DBH < 1)) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "You have diameter below 1 cm, please check DBH or CircCorr unit.")
    # stop function if a DBH is below 1 cm
  }
  if (any(inventory$DBH > 1000)) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "You have diameter above 1000 cm, please check DBH or CircCorr unit.")
    # stop function if a DBH is above 1000 cm (10 m)
  }

  # Unique trees
  if (any(duplicated(inventory$idTree))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Tree identifiers (idTree) are not unique.")
    # stop function if the tree identifiers (idTree) are not unique
  }

  # Unique plot
  if (!length(unique(inventory$Plot)) == 1){ #all the Plot values are equal?
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop,"Input inventory concerns different plots
                          (Plot). Our function simulates logging at the plot level.")
  }

  # Unique census year
  if (!length(unique(inventory$CensusYear)) == 1) {#all is the same year?
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop,"Input inventory concerns different years
                          (CensusYear). Our function simulates logging at 1 year scale.")
  }

  if (!GoodData)# inverse value of the object
    stop(paste ("Input inventory does not comply.", GeneralStop))

  return(inventory)
}

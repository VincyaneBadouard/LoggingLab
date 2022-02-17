#' Joins commercial criteria table to the forest inventory
#'
#' @description Joins a commercial criteria table to a forest inventory
#'
#' @param inventory input inventory (see the inputs formats and metadata in the
#'   vignette) (data.frame)
#'
#' @param speciescriteria Table of species exploitability criteria : species
#'   names, economic interest level, minimum and maximum felling diameter, in
#'   the same format as \code{\link{SpeciesCriteria}} (data.frame)
#'
#' @return The forest inventory with economic informations (commercial name, economic interest level,
#'   minimum/maximum felling diameter, increased minimum felling diameter) added for each
#'   individual in the variables CommercialName, CommercialLevel, MinFD, UpMinFD, MaxFD (data.frame)
#'
#' @seealso  \code{\link{Paracou6_2016}}, \code{\link{SpeciesCriteria}}
#'
#' @export
#'
#' @importFrom dplyr filter select left_join mutate rename
#'
#' @examples
#' data(Paracou6_2016)
#' data(SpeciesCriteria)
#' new <- commercialcriteriajoin(Paracou6_2016, SpeciesCriteria)
#'
commercialcriteriajoin <- function(
  inventory,
  speciescriteria
)
{

  # Arguments check

  if(!all(unlist(lapply(list(inventory, speciescriteria), inherits, "data.frame"))))
    stop("The function arguments must be data.frames") # any() don't take a list

  # speciescriteria columns check

  lapply(list("CommercialName", "Genus", "Species", "CommercialLevel", "MinFD", "UpMinFD", "MaxFD"),
         function(element)
           if(!(element %in% names(speciescriteria))) {

             stop("The columns requested in the data frame given in the speciescriteria argument
                  are not found (see the vignette for the format of this dataframe")
           }
  )
  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <- CommercialLevel <- NULL
  CommercialLevel.genus <- CommercialLevel.species <- Condition <- DBH <- NULL
  DeathCause <- DistCriteria <- Family <- CommercialName <- NULL
  ForestZoneVolumeParametersTable <- Genus <- Logged <- NULL
  TimberLoggedVolume <- LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  NoHollowTimberLoggedVolume <- ParamCrownDiameterAllometry <- PlotSlope <- NULL
  ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- Slope <- SlopeCriteria <- Species <- Species.genus <- NULL
  SpeciesCriteria <- Taxo <- Taxo.family <- Taxo.genus <- Taxo.species <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD <- UpMinFD.genus <- NULL
  UpMinFD.species <- CommercialName.genus <- NULL
  CommercialName.species <- VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL
  alpha <- alpha.family <- alpha.genus <- alpha.species <- bCoef <- NULL
  beta.family <- beta.genus <- beta.species <- geometry <- idTree <- NULL
  Aggregative <- Aggregative.species <- Aggregative.genus <- NULL


  # Function
  genuscriteria <- speciescriteria %>% # for economic names that relate to entire genus
    filter(Species == "spp") # Genus spp

  speciescriteria <- speciescriteria %>%
    filter(Species != "spp")

  inventory <- inventory %>%
    left_join(genuscriteria, by = "Genus", suffix = c("", ".genus")) %>%
    dplyr::select(-Species.genus) %>%

    left_join(speciescriteria, by = c("Genus","Species"), suffix = c(".genus", ".species")) %>%

    # Suffixes management (species level first)
    mutate(CommercialName = ifelse(is.na(CommercialName.species), CommercialName.genus, CommercialName.species)) %>%
    dplyr::select(-CommercialName.species, -CommercialName.genus) %>%

    mutate(CommercialLevel.species = as.character(CommercialLevel.species)) %>%
    mutate(CommercialLevel.genus = as.character(CommercialLevel.genus)) %>%

    mutate(CommercialLevel = ifelse(is.na(CommercialLevel.species), CommercialLevel.genus, CommercialLevel.species)) %>%
    mutate(MinFD = ifelse(is.na(MinFD.species), MinFD.genus, MinFD.species)) %>%
    mutate(UpMinFD = ifelse(is.na(UpMinFD.species), UpMinFD.genus, UpMinFD.species)) %>%
    mutate(MaxFD = ifelse(is.na(MaxFD.species), MaxFD.genus, MaxFD.species)) %>%
    mutate(Aggregative = ifelse(is.na(Aggregative.species), Aggregative.genus, Aggregative.species)) %>%


    dplyr::select(-CommercialLevel.species, -CommercialLevel.genus, -MinFD.species, -MinFD.genus,
                  -UpMinFD.species, -UpMinFD.genus, -MaxFD.species, -MaxFD.genus,
                  -Aggregative.species, -Aggregative.genus) %>%
    mutate(CommercialLevel = as.character(CommercialLevel)) %>%

    # Exceptions management (CommercialLevel == "0" in speciescriteria)
    mutate(CommercialName = ifelse(CommercialLevel == "0", NA, CommercialName)) %>%
    mutate(MinFD = ifelse(CommercialLevel == "0", NA, MinFD)) %>%
    mutate(UpMinFD = ifelse(CommercialLevel == "0", NA, UpMinFD)) %>%
    mutate(MaxFD = ifelse(CommercialLevel == "0", NA, MaxFD)) %>%
    mutate(Aggregative = ifelse(CommercialLevel == "0", NA, Aggregative))


  inventory <- inventory %>%
    mutate(CommercialLevel = ifelse(is.na(CommercialLevel), "0", CommercialLevel)) %>%
    mutate(CommercialLevel = factor(as.character(CommercialLevel)))

  return(inventory)
}

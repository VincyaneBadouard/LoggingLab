#' Joins commercial criteria table to the forest inventory
#'
#' @description Joins a commercial criteria table to a forest inventory
#'
#' @param inventory input inventory (see the inputs formats and metadata in the
#'   \code{\link{vignette}}) (data.frame)
#'
#' @param speciescriteria Table of species exploitability criteria : species
#'   names, economic interest level, minimum and maximum felling diameter, in
#'   the same format of \code{\link{SpeciesCriteria}} (data.frame)
#'
#' @return The forest inventory with economic informations (economic name, rank,
#'   mimimum/maximum falling diameter, upped mimimum falling diameter) per
#'   individuals (data.frame)
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
#' ONFGuyafortaxojoin(Paracou6_2016, SpeciesCriteria)
#'
ONFGuyafortaxojoin <- function(
  inventory,
  speciescriteria
)
{

  # Arguments check

  if(!all(unlist(lapply(list(inventory, speciescriteria), inherits, "data.frame"))))
    stop("The function arguments must be data.frames") # any() don't take a list

  # speciescriteria columns check

  lapply(list("VernName", "Genus", "Species", "Commercial", "MinFD", "UpMinFD", "MaxFD"),
         function(element)
           if(!(element %in% names(speciescriteria))) {

             stop("The columns requested in the data frame given in the speciescriteria argument
                  are not found (see the vignette for the format of this dataframe")
           }
  )
  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <- Commercial <- NULL
  Commercial.genus <- Commercial.species <- Condition <- DBH <- NULL
  DeathCause <- DistCrit <- Family <- ONFName <- NULL
  ForestZoneVolumeParametersTable <- Genus <- Logged <- NULL
  TimberLoggedVolume <- LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  NoHollowTimberLoggedVolume <- ParamCrownDiameterAllometry <- PlotSlope <- NULL
  PlotTopo <- ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- Slope <- SlopeCrit <- Species <- Species.genus <- NULL
  SpeciesCriteria <- Taxo <- Taxo.family <- Taxo.genus <- Taxo.species <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD <- UpMinFD.genus <- NULL
  UpMinFD.species <- VernName.genus <- VernName.genus.genus <- NULL
  VernName.species <- VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL
  alpha <- alpha.family <- alpha.genus <- alpha.species <- bCoef <- NULL
  beta.family <- beta.genus <- beta.species <- geometry <- idTree <- NULL

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
    dplyr::rename(VernName = VernName.genus) %>%
    mutate(ONFName = ifelse(is.na(VernName.species), VernName.genus.genus, VernName.species)) %>%
    dplyr::select(-VernName.species, -VernName.genus.genus) %>%

    mutate(Commercial.species = as.character(Commercial.species)) %>%
    mutate(Commercial.genus = as.character(Commercial.genus)) %>%

    mutate(Commercial = ifelse(is.na(Commercial.species), Commercial.genus, Commercial.species)) %>%
    mutate(MinFD = ifelse(is.na(MinFD.species), MinFD.genus, MinFD.species)) %>%
    mutate(UpMinFD = ifelse(is.na(UpMinFD.species), UpMinFD.genus, UpMinFD.species)) %>%
    mutate(MaxFD = ifelse(is.na(MaxFD.species), MaxFD.genus, MaxFD.species)) %>%

    dplyr::select(-Commercial.species, -Commercial.genus, -MinFD.species, -MinFD.genus,
                  -UpMinFD.species, -UpMinFD.genus, -MaxFD.species, -MaxFD.genus) %>%
    mutate(Commercial = as.character(Commercial)) %>%

    # Exceptions management (Commercial == "0" in speciescriteria)
    mutate(ONFName = ifelse(Commercial == "0", NA, ONFName)) %>%
    mutate(MinFD = ifelse(Commercial == "0", NA, MinFD)) %>%
    mutate(UpMinFD = ifelse(Commercial == "0", NA, UpMinFD)) %>%
    mutate(MaxFD = ifelse(Commercial == "0", NA, MaxFD))

  inventory <- inventory %>%
    mutate(Commercial = ifelse(is.na(Commercial), "0", Commercial)) %>%
    mutate(Commercial = factor(as.character(Commercial)))

  return(inventory)
}

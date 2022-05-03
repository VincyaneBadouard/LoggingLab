#'Compute tree dimensions (tree, trunk and crown height, crown diameter,
#'harvestable volume, wood density, and AGB)
#'
#'@param inventory Input inventory (see the inputs formats and metadata in the
#'  vignette) (data.frame)
#'
#'
#'@param volumeparameters Parameters of the tree volume tables (in the same
#'  format of \code{\link{ForestZoneVolumeParametersTable}}) used to compute the
#'  harvestable volume of each tree, depending on its geographic zone if several
#'  locations (data.frame)
#'
#'@param crowndiameterparameters Parameters of crown diameter allometry (in the
#'  same format of \code{\link{ParamCrownDiameterAllometry}}) used to compute
#'  the crown diameter of each tree, depending on its DBH (Diameter at Breast
#'  Height) and its species, genus or family. (data.frame)
#'
#'@param advancedloggingparameters Other parameters of the logging simulator
#'  \code{\link{loggingparameters}} (list)
#'
#'@return The inventory (data.frame) with additional variables: TreeHeight (m),
#'  TreeHarvestableVolume (m^3), TrunkHeight (m), CrownHeight (m), CrownDiameter
#'  (m), estimated 'WoodDensity' (g/cm^3) and tree above-ground biomass ('AGB')
#'  (in Mg)
#'
#'@details 'addtreedim' compute some tree dimensions with the
#'  'advancedloggingparameters' argument:
#'  - Tree height (in m) ('TreeHeightAllometry')
#'  - Tree harvestable volume (m3) ('TreeHarvestableVolumeAllometry')
#'  - Trunk height (in m) ('TrunkHeightAllometry')
#'  - Crown height (in m)
#'  - Crown diameter (in m) ('CrownDiameterAllometry')
#'  - Wood density (g/cm3) (by the BIOMASS package)
#'  - Tree above-ground biomass (AGB) (in ton) (by the BIOMASS package)
#'
#'  'Taxo': the level at which the CrownDiameter was calculated.
#'  'levelWD': the level at which the WoodDensity was assigned by the BIOMASS
#'  package.
#'
#'@export
#'
#'@importFrom dplyr filter select left_join mutate rename
#'@importFrom tidyr unite
#'@importFrom rlang .data
#'@importFrom BIOMASS getWoodDensity computeAGB
#'
#' @examples
#' data(Paracou6_2016)
#' data(ForestZoneVolumeParametersTable) # The volume parameters data in the global environment
#' data(ParamCrownDiameterAllometry)
#'
#' if (!("DBH" %in% names(Paracou6_2016))) {
#' tibble::add_column(Paracou6_2016, DBH = NA) #if DBH doesn't exist create it
#' Paracou6_2016$DBH = Paracou6_2016$CircCorr/pi} # and calculate it
#' Paracou6_2016 <- dplyr::filter(Paracou6_2016, DBH >= 10)
#'
#' addtreedim(Paracou6_2016, volumeparameters = ForestZoneVolumeParametersTable)
#'
addtreedim <- function(
  inventory,
  volumeparameters,
  crowndiameterparameters = ParamCrownDiameterAllometry,
  advancedloggingparameters = loggingparameters()

){

  # Arguments check
  if(!all(unlist(lapply(list(inventory, crowndiameterparameters, volumeparameters), inherits, "data.frame"))))
    stop("The function arguments must be data.frames") # any() don't take a list

  if(!inherits(advancedloggingparameters, "list"))
    stop("The 'advancedloggingparameters' argument of the 'addtreedim' function must be a list")

  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <- NULL
  Condition <- DBH <- NULL
  DeathCause <- DistCriteria <- Family  <- Zone <- NULL
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
  genus <- species <- meanWD <- levelWD <- WoodDensity <- NULL

  # Crown diameter allometry parameters data preparation:

  spParamCrownDiameter <- crowndiameterparameters %>% #parameters at species scale
    filter(Taxo == "sp") %>%
    dplyr::select(-Family)
  genParamCrownDiameter <- crowndiameterparameters %>% #parameters at genus scale
    filter(Taxo == "gen") %>%
    dplyr::select(-Family, -ScientificName, -Species)
  famParamCrownDiameter <- crowndiameterparameters %>% #parameters at family scale
    filter(Taxo == "fam") %>%
    dplyr::select(-Genus, -ScientificName, -Species)


  # Variables computation:

  # test advancedloggingparameters$TreeHarvestableVolumeParameters in names(volumeparameters)

  inventory <- inventory %>%

    # TreeHarvestableVolume (m3)
    left_join(volumeparameters, by = "Forest") %>%
    dplyr::mutate(TreeHarvestableVolume = advancedloggingparameters$TreeHarvestableVolumeAllometry(DBH, aCoef, bCoef)) %>%
    #
    #    #Sylvain's version
    #     mutate(TreeHarvestableVolume = # the variable to compute
    #              advancedloggingparameters$ # the list compute by the fct
    #              TreeHarvestableVolumeAllometry(DBH, # the element of the list
    #                                             pars = c({{advancedloggingparameters$
    #                                                 TreeHarvestableVolumeParameters}}))) %>% # to recover parameters name as column names

    # TrunkHeight (m)
    mutate(TrunkHeight = advancedloggingparameters$TrunkHeightAllometry(DBH, TreeHarvestableVolume)) %>%

    # TreeHeight (m)
    mutate(TreeHeight = advancedloggingparameters$TreeHeightAllometry(DBH)) %>%


    # CrownHeight (m)
    mutate(CrownHeight = TreeHeight - TrunkHeight) %>%


    # CrownDiameter (m)
    # Add crown diameter allometry parameters at the inventory:
    unite(Genus, Species, col = "ScientificName", sep = "_", remove = F) %>% # create ScientificName column in the inventory
    left_join(spParamCrownDiameter, by = c("ScientificName","Genus","Species")) %>% # at species scale
    left_join(genParamCrownDiameter, by = "Genus", suffix = c(".species", ".genus")) %>% # at genus scale
    left_join(famParamCrownDiameter, by = "Family") %>% # at family scale
    dplyr::rename(Taxo.family = Taxo, alpha.family = alpha, beta.family = beta) %>%
    # manage the different scales parameters Species > Genus > Family:
    # alpha:
    mutate(alpha = alpha.species, beta = beta.species, Taxo = Taxo.species) %>% # create the future & unique alpha,beta,taxo columns
    mutate(alpha = ifelse(is.na(alpha), alpha.genus, alpha)) %>% #if species parameters are absent, take genus parameters
    mutate(alpha = ifelse(is.na(alpha), alpha.family, alpha)) %>% #if species&genus parameters are absent, take family parameters
    mutate(alpha = ifelse(is.na(alpha), mean(alpha, na.rm =T), alpha)) %>% #if species,genus&family parameters are absent, take parameters mean
    dplyr::select(-alpha.species, -alpha.genus, -alpha.family) %>% #remove obsolete columns
    # beta:
    mutate(beta = ifelse(is.na(beta), beta.genus, beta)) %>% #if species parameters are absent, take genus parameters
    mutate(beta = ifelse(is.na(beta), beta.family, beta)) %>% #if species&genus parameters are absent, take family parameters
    mutate(beta = ifelse(is.na(beta), mean(beta, na.rm =T), beta)) %>% #if species,genus&family parameters are absent, take parameters mean
    dplyr::select(-beta.species, -beta.genus, -beta.family) %>% #remove obsolete columns
    # scale:
    mutate(Taxo = ifelse(is.na(Taxo), Taxo.genus, Taxo)) %>% #if species parameters are absent, take genus parameters
    mutate(Taxo = ifelse(is.na(Taxo), Taxo.family, Taxo)) %>% #if species&genus parameters are absent, take family parameters
    mutate(Taxo = ifelse(is.na(Taxo), "mean", Taxo)) %>% #if species,genus&family parameters are absent, take parameters mean
    dplyr::select(-Taxo.species, -Taxo.genus, -Taxo.family) %>% #remove obsolete columns
    # compute the crown diameter
    mutate(CrownDiameter = advancedloggingparameters$CrownDiameterAllometry(DBH, TreeHeight, alpha, beta)) %>%
    dplyr::select(-aCoef, -bCoef, -alpha, -beta, -Zone)

  # Estimated wood density (g/cm^3)
  WDDetails <- getWoodDensity(inventory$Genus, inventory$Species, inventory$Family, region = "World", verbose = FALSE)

  #RESULTS :
  # 2 computes at the dataset level (indet.indet)
  # 0 computes at the family level
  # 997 computes at the genus level
  # 2493 computes at the species level
  # 629 based only on 1 individual
  # 462 based only on 2 individuals

  WDDetails <- WDDetails %>%
    dplyr::select(genus, species, meanWD, levelWD) %>%
    dplyr::rename(Genus = genus) %>%
    dplyr::rename(Species = species) %>%
    dplyr::rename(WoodDensity = meanWD) %>%
    unique()


  inventory <- inventory %>%
    left_join(WDDetails, by = c("Genus","Species")) %>%

    # Tree Above-Ground Biomass (AGB) (in ton!) (with DBH in cm, WoodDensity in in g/cm3, TreeHeight in m)
    mutate(AGB = computeAGB(DBH, WoodDensity, TreeHeight))

  if(all(is.na(inventory$TreeHarvestableVolume)))
    stop("No harvestable volume per tree could be calculated.
         Check that the name of your 'Forest' is in the table in 'volumeparameters'")

  return(inventory)
}

#TreeHeight
#TreeHarvestableVolume
#TrunkHeight
#CrownHeight
#CrownDiameter
#WoodDensity
#AGB


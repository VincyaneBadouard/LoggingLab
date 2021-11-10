#'Tree selection (trees to be exploited, future and reserve trees)
#'
#'@param inventory input inventory (see the inputs formats and metadata in the
#'  \code{\link{vignette}}) (data.frame)
#'
#'@param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'  or SRTM) (\code{\link{DTMParacou}}) (RasterLayer)
#'
#'@param plotslope Slopes (in radians) of the inventoried plot (with a
#'  neighbourhood of 8 cells) (default: \code{\link{PlotSlope}}) (RasterLayer)
#'
#'@param speciescriteria Table of species exploitability criteria : species
#'  names, economic interest level, minimum and maximum felling diameter, in the
#'  same format as \code{\link{SpeciesCriteria}} (data.frame)
#'
#'@param scenario Logging scenario: "RIL1", "RIL2broken", "RIL2", "RIL3",
#'  "RIL3fuel", "RIL3fuelhollow" or "manual"(character) (see the
#'  \code{\link{vignette}})
#'
#'@param objective Objective volume per hectare (numeric)
#'
#'@param fuel Fuel wood exploitation: no exploitation = "0", exploitation of
#'   damage and unused part of logged trees for fuel = "1", exploitation of
#'   hollow trees, damage and and unused part of the log for fuel = "2"
#'
#'@param diversification Possibility to log other species in addition to the
#' main commercial species (species with a value of 2 for commercial in the
#' \code{\link{SpeciesCriteria}} table) (logical)
#'
#'@param specieslax Allow diversification if the stand is too poor to reach the
#' objective volume without diversification, = FALSE by
#'  default (logical)
#'
#'@param objectivelax Allow exploitation in case of non-achievement of the
#'  objective volume (if stand too poor), = FALSE by default (logical)
#'
#'@param advancedloggingparameters Other parameters of the logging simulator
#'  \code{\link{loggingparameters}} (list)
#'
#'@return A list with:
#'  - input inventory with new columns:
#'     - The probability of a tree having visible defects ("VisibleDefectProba")
#'         and the visible defect trees ("VisibleDefect").
#'     - The exploitability criteria ("DistCrit", "Slope", "SlopeCrit"), and if
#'        they are validated for each of the trees ("LoggingStatus").
#'     - The trees selected for harvesting ("Selected"), if the Minimum
#'        Felling Diameter (MinFD) of their species has been raised ("Up").
#'        The cumulative harvestable volume of harvestable trees("VolumeCumSum").
#'     - The probability of a tree being probed hollow ("ProbedHollowProba")
#'         and the probed hollow trees ("ProbedHollow").
#'     - Future and reserve trees (LoggingStatus = "future"/"reserve")
#'    (see the outputs metadata in the \code{\link{vignette}})
#'  - the objective volume with or without a bonus (if
#'  hollow trees exploitation) (VO) for the entire plot
#'  - the harvestable volume with the initial
#'  criteria (HVinit) for the entire plot
#'  - 6 layers of spatial points: harvestable, selected, future and
#'  reserve, hollow and fuel wood trees
#'
#'@details Trees with visible defects are identified ('VisiblyDefectModel' in
#'  'advancedloggingparameters' argument) and therefore not designated.
#'  (Objective volume: If the user has chosen not to harvest hollow probed trees
#'  for energy ('RottenModel' in 'advancedloggingparameters' argument), 20-30%
#'  will be added to the objective volume in order to compensate for these
#'  designated hollow trees.)
#'
#'  Trees will be designated as "**harvestable**" if they:
#'  - belong to species of 1st economic level (if no diversification) or 1st and 2nd level if (diversification)
#'  - have a DBH between MinFD and MaxFD.
#'  - are not isolated (> 100m ('IsolateTreeMinDistance') from other individuals of
#'   the same species)
#'  - are on slopes < 22% ('TreeMaxSlope')
#'  - are off the main tracks.
#'
#'  If the harvestable volume is higher than the objective volume, MinFD of
#'  the 1st economic rank species is increased. If this is not enough and if diversification is allowed,
#'  MinFD of 2nd economic level species is increased.
#'  Then, the trees to be harvested are chosen in decreasing order
#'  of volume, until the objective volume is reached.
#'
#'  If the harvestable volume is lower than the objective volume, diversification
#'  can be applied if it
#'  was not already applied ('specieslax') (trees of all commercial ranks are
#'  selected in decreasing order of volume until the objective volume is
#'  reached), or harvesting can continue despite an unreached objective volume,
#'  or be abandoned ('objectivelax')
#'
#'  **Future** trees are all trees satisfying the following conditions:
#'  - species of 1st economic rank
#'  - DBH between 35cm ('FutureTreesMinDiameter') and the species MinFD
#'  or UpMinFD if it has been raised for its species.
#'
#'  **Reserve** trees are randomly chosen among future trees so that
#'  the number of reserve trees is equal to the number of harvested trees.
#'
#'@seealso  \code{\link{Paracou6_2016}}, \code{\link{SpeciesCriteria}},
#'  \code{\link{DTMParacou}}, \code{\link{PlotSlope}},
#'  \code{\link{loggingparameters}}
#'
#'
#'@export
#'
#'@importFrom dplyr filter mutate select left_join bind_rows
#'@importFrom sf st_as_sf st_point
#'@importFrom raster crs extract
#'@importFrom methods as
#'
#'
#' @examples
#' data(Paracou6_2016)
#' data(DTMParacou)
#' data(PlotSlope)
#'
#' inventory <- addtreedim(inventorycheckformat(Paracou6_2016),
#' volumeparameters = ForestZoneVolumeParametersTable)
#'
#' treeselectionoutputs <- treeselection(inventory,
#' topography = DTMParacou, plotslope = PlotSlope,
#' speciescriteria = SpeciesCriteria, objective = 20,
#' scenario ="manual", fuel = "2", diversification = TRUE, specieslax = FALSE,
#' objectivelax = TRUE,
#' advancedloggingparameters = loggingparameters())
#'
#' NewInventory <- treeselectionoutputs$inventory
#'
#' NonHarvestable <- sf::st_as_sf(
#' dplyr::filter(NewInventory, LoggingStatus == "non-harvestable"),
#' coords = c("Xutm", "Yutm"))
#'
#' Harvestable <- sf::st_as_sf(
#' dplyr::filter(NewInventory, LoggingStatus == "harvestable"),
#' coords = c("Xutm", "Yutm"))
#'
#' HarvestableUp <- sf::st_as_sf(
#' dplyr::filter(NewInventory, LoggingStatus == "harvestableUp"),
#' coords = c("Xutm", "Yutm"))
#'
#' Selected <- sf::st_as_sf(
#' dplyr::filter(NewInventory, Selected == "1"), coords = c("Xutm", "Yutm"))
#'
#' Reserve <- sf::st_as_sf(
#' dplyr::filter(NewInventory, LoggingStatus == "reserve"),
#' coords = c("Xutm", "Yutm"))
#'
#' Future <- sf::st_as_sf(
#' dplyr::filter(NewInventory, LoggingStatus == "future"),
#' coords = c("Xutm", "Yutm"))
#'
#' ProbedHollow <- sf::st_as_sf(
#' dplyr::filter(NewInventory, ProbedHollow == "1"), coords = c("Xutm", "Yutm"))
#'
#' VisibleDefect <- sf::st_as_sf(
#' dplyr::filter(NewInventory, VisibleDefect == "1"), coords = c("Xutm", "Yutm"))
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = NonHarvestable,
#'   aes(colour = "Non-harvestable"), show.legend = "point") +
#'   geom_sf(data = VisibleDefect,
#'   aes(colour = "Visible defect"), show.legend = "point") +
#'   geom_sf(data = Future,
#'   aes(colour = "Future"), show.legend = "point", size = 4) +
#'   geom_sf(data = Reserve,
#'   aes(colour = "Reserve"), show.legend = "point", size = 4) +
#'   geom_sf(data = Harvestable,
#'   aes(colour = "Harvestable"), show.legend = "point", size = 4) +
#'   geom_sf(data = HarvestableUp,
#'   aes(colour = "HarvestableUp"), show.legend = "point", size = 4) +
#'   geom_sf(data = Selected,
#'   aes(colour = "Selected"), show.legend = "point") +
#'   geom_sf(data = ProbedHollow,
#'   aes(colour = "Probed hollow"), show.legend = "point") +
#'
#'   scale_colour_manual(values = c("Non-harvestable" = "grey",
#'   "Visible defect" = "pink", "Harvestable" = "skyblue",
#'   "HarvestableUp" = "blue", "Selected" = "red", "Future" = "orange",
#'   "Reserve" = "purple", "Probed hollow" = "forestgreen")) +
#'   labs(color = "Logging status")
#'
treeselection <- function(
  inventory,
  topography,
  plotslope,
  speciescriteria,
  scenario,
  objective = NULL,
  fuel = NULL,
  diversification = NULL,
  specieslax = FALSE,
  objectivelax = FALSE,
  advancedloggingparameters = loggingparameters()
  # MainTrail

){

  # Arguments check

  if(!all(unlist(lapply(list(inventory, speciescriteria), inherits, "data.frame"))))
    stop("The 'inventory' and 'speciescriteria' arguments of the 'treeselection' function must be data.frames")

  if(!any(inherits(objective, "numeric") || is.null(objective)))
    stop("The 'objective' argument of the 'treeselection' function must be numeric or NULL")

  if(!any(inherits(diversification, "logical") || is.null(diversification)))
    stop("The 'diversification' argument of the 'treeselection' function must be logical or NULL")

  if(!all(unlist(lapply(list(specieslax, objectivelax), inherits, "logical"))))
    stop("The 'specieslax' and 'objectivelax' arguments of the 'treeselection' function must be logicals")

  if (!any(scenario == "RIL1" || scenario == "RIL2broken"|| scenario == "RIL2"||
           scenario == "RIL3"|| scenario == "RIL3fuel"|| scenario == "RIL3fuelhollow"|| scenario =="manual"))
    stop("The 'scenario' argument of the 'treeselection' function must be
         'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  if (!any(fuel == "0" || fuel == "1"|| fuel == "2"|| is.null(fuel)))
    stop("The 'fuel' argument of the 'treeselection' function must be '0', '1', or '2'")

  if(!all(unlist(lapply(list(topography, plotslope), inherits, "RasterLayer"))))
    stop("The 'topography' and 'plotslope' arguments of the 'treeselection' function must be RasterLayer")

  if(!inherits(advancedloggingparameters, "list"))
    stop("The 'advancedloggingparameters' argument of the 'treeselection' function must be a list")

  if(scenario == "manual" &&
     (is.null(objective) || is.null(fuel) || is.null(diversification)))
    stop("If you choose the 'manual' mode,
         you must fill in the arguments 'objective', 'fuel' and 'diversification'")

  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <- Commercial <- NULL
  Commercial.genus <- Commercial.species <- Condition <- DBH <- NULL
  DeathCause <- DistCrit <- Family <- VisibleDefect <- LogDBH <- NULL
  ForestZoneVolumeParametersTable <- Genus <- Logged <- VisibleDefectProba <- NULL
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

  # Redefinition of the parameters according to the chosen scenario
  scenariosparameters <- scenariosparameters(scenario = scenario, objective = objective, fuel = fuel,
                                             diversification = diversification)

  objective <- scenariosparameters$objective
  fuel <- scenariosparameters$fuel
  diversification <- scenariosparameters$diversification

  # Function

  if ("DeathCause" %in% names(inventory)) filter(inventory, is.null(DeathCause)) #select only still alived trees (after MainTrails) NULL ou NA

  inventory <- inventory %>%
    # Visible defect trees detection:
    mutate(LogDBH = log(DBH)) %>% # DBH is logged in the model

    mutate(VisibleDefectProba = advancedloggingparameters$VisiblyDefectModel(LogDBH)) %>%  # Proba to have defects for each tree

    # generate either "1" or "0" randomly for each line, depending on the proba associated with the line:
    rowwise() %>%
    mutate(VisibleDefect = sample(c(1,0), size = 1, replace = F,
                                  prob = c(VisibleDefectProba, 1-VisibleDefectProba))) %>% # 1 = visible defects tree, 0 = no visible defects
    ungroup() %>%
    mutate(VisibleDefect = as.factor(VisibleDefect))

  # Compute the objective volume for the entire surface of the plot
  VO <- objective * unique(inventory$PlotArea)

  # Joins commercial criteria to the inventory
  inventory <- ONFGuyafortaxojoin(inventory, speciescriteria = speciescriteria)

  VisibleDefectTable <- filter(inventory, VisibleDefect == "1")
  inventory <- filter(inventory, VisibleDefect == "0") # we continue with just visibly healthy trees

  # Harvestable trees identification
  harvestableOutputs <- harvestable(inventory,
                                    topography = topography, plotslope = plotslope,
                                    diversification = diversification, specieslax = specieslax,
                                    advancedloggingparameters = advancedloggingparameters)

  inventory <- harvestableOutputs$inventory       # new inventory

  HVinit <- harvestableOutputs$HVinit             # initial harvestable volume

  # Trees to be exploited selection
  selectedOutputs <- selected(inventory,
                              topography = topography,
                              scenario = scenario, fuel = fuel, diversification = diversification,
                              VO = VO, HVinit = HVinit,
                              specieslax = specieslax, objectivelax = objectivelax,
                              advancedloggingparameters = advancedloggingparameters)

  inventory <- selectedOutputs$inventory                          # extract inventory of the selected outputs

  HollowTreesPoints <- selectedOutputs$HollowTreesPoints          # extract a pts vector of the selected outputs
  EnergywoodTreesPoints <- selectedOutputs$EnergywoodTreesPoints  # extract a pts vector of the selected outputs

  # Future & reserve trees designation
  inventory <- futurereserve(inventory,
                             speciescriteria = speciescriteria,
                             advancedloggingparameters = advancedloggingparameters)


  # Points vector with coordinates of the harvestable trees:
  HarvestableTreesPoints <- inventory %>%
    filter(LoggingStatus == "harvestable"|LoggingStatus == "harvestableUp"|LoggingStatus == "harvestable2nd") # harvestableUp = DBH > MinFD individuals, harvestable2nd = eco2 individuals is specieslax

  if (dim(HarvestableTreesPoints)[1] != 0) {

    HarvestableTreesPoints <- st_as_sf(HarvestableTreesPoints, coords = c("Xutm", "Yutm")) # sp::coordinates(HarvestableTreesPoints) <- ~ Xutm + Yutm

    # sp::proj4string(HarvestableTreesPoints) <- raster::crs(topography)

  } else {HarvestableTreesPoints = st_point(x = c(NA_real_, NA_real_))} # empty

  # Points vector with coordinates of the selected trees:
  SelectedTreesPoints <- inventory %>%
    filter(Selected == "1")

  if (dim(SelectedTreesPoints)[1] != 0) {

    st_as_sf(SelectedTreesPoints, coords = c("Xutm", "Yutm")) # sp::coordinates(SelectedTreesPoints) <- ~ Xutm + Yutm

    # sp::proj4string(SelectedTreesPoints) <- raster::crs(topography)

  } else {SelectedTreesPoints = st_point(x = c(NA_real_, NA_real_))} # empty

  # Points vector with coordinates of the future trees:
  FutureTreesPoints <- inventory %>%
    filter(LoggingStatus == "future")

  if (dim(FutureTreesPoints)[1] != 0) {

    st_as_sf(FutureTreesPoints, coords = c("Xutm", "Yutm")) # sp::coordinates(FutureTreesPoints) <- ~ Xutm + Yutm

    # sp::proj4string(FutureTreesPoints) <- raster::crs(topography)

  } else {FutureTreesPoints = st_point(x = c(NA_real_, NA_real_))} # empty

  # Points vector with coordinates of the reserve trees:
  ReserveTreesPoints <- inventory %>%
    filter(LoggingStatus == "reserve")

  if (dim(ReserveTreesPoints)[1] != 0) {

    st_as_sf(ReserveTreesPoints, coords = c("Xutm", "Yutm")) # sp::coordinates(ReserveTreesPoints) <- ~ Xutm + Yutm

    # sp::proj4string(ReserveTreesPoints) <- raster::crs(topography)

  } else {ReserveTreesPoints = st_point(x = c(NA_real_, NA_real_))} # empty

  # where specieslax was not necessary, consider eco2s as non-exploitable:
  inventory <- inventory %>%
    mutate(LoggingStatus = ifelse(HVinit > VO & LoggingStatus == "harvestable2nd", "non-harvestable",
                                  LoggingStatus)) %>%

    # Add the visible defects trees removed until now
    bind_rows(VisibleDefectTable) %>%
    mutate(LoggingStatus = ifelse(VisibleDefect == "1", "non-harvestable",
                                  LoggingStatus))
  # OUTPUTS list
  treeselectionOutputs <- list(inventory = inventory,
                               VO = VO,
                               HVinit = HVinit,
                               HarvestableTreesPoints = HarvestableTreesPoints,
                               SelectedTreesPoints = SelectedTreesPoints,
                               FutureTreesPoints = FutureTreesPoints,
                               ReserveTreesPoints = ReserveTreesPoints,
                               HollowTreesPoints = HollowTreesPoints,
                               EnergywoodTreesPoints = EnergywoodTreesPoints)



  return(treeselectionOutputs) # return the new inventory, the objective volume,
  # and the 4 points vectors (Harvestable, Selected, Future and Reserve,
  # Hollow and Energywood Trees)
}

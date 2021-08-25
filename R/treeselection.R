#'Tree selection
#'
#'@param inventory your inventory (see the inputs formats and metadata in the
#'  \code{\link{vignette}}) (data.frame)
#'
#'@param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'  or SRTM) (default: \code{\link{DTMParacou}}) (RasterLayer)
#'
#'@param speciescriteria Table of species exploitability criteria : species
#'  names, economic interest level, minimum and maximum felling diameter, in the
#'  same format of \code{\link{SpeciesCriteria}} (data.frame)
#'
#'@param scenario Logging scenario: "RIL1", "RIL2broken", "RIL2", "RIL3",
#'  "RIL3fuel", "RIL3fuelhollow" or "manual"(character) (see the
#'  \code{\link{vignette}})
#'
#'@param objective Objective volume per hectare (numeric)
#'
#' @param fuel Fuel wood exploitation: no exploitation = "0", damages and purge
#'   exploitation in fuelwood = "1", exploitation of hollow trees, damages and purge in
#'   fuelwood = "2"
#'
#'@param diversification Taking of other species in addition to the main
#'  commercial species (2 levels of commercial species in the
#'  \code{\link{SpeciesCriteria}} table) (logical)
#'
#'@param specieslax Allow diversification if stand is too poor, = FALSE by
#'  default (logical)
#'
#'@param objectivelax Allow exploitation in case of non-achievement of the
#'  objective volume (if stand too poor), = FALSE by default (logical)
#'
#'@param plotslope Slopes (in radians) of the inventoried plot (with a
#'  neighbourhood of 8 cells) (default: \code{\link{PlotSlope}}) (RasterLayer)
#'
#'
#'@param advancedloggingparameters Other parameters of the logging simulator
#'  \code{\link{loggingparameters}} (list) MainTrail (multiline)
#'
#'@return A list with: - your inventory with: "DistCrit", "Slope", "SlopeCrit",
#'  "LoggingStatus", "Selected", "Up", "VolumeCumSum", "ProbedHollowProba",
#'  "ProbedHollow" new columns (see the outputs metadata in the
#'  \code{\link{vignette}}). - your objective volume with or without a bonus (if
#'  hollow trees exploitation) - the harvestable volume with your initial
#'  criteria - 6 sets of spatial points: harvestable, selected, future and
#'  reserve, hollow and energy wood trees
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
#'@importFrom topoDistance topoDist
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
#' scenario ="manual", fuel = "2", diversification = FALSE, specieslax = FALSE,
#' objectivelax = TRUE,
#' advancedloggingparameters = loggingparameters()) #  MainTrail
#'
#'
treeselection <- function(
  inventory,
  topography,
  speciescriteria,
  scenario,
  objective,
  fuel,
  diversification,
  specieslax = FALSE,
  objectivelax = FALSE,
  plotslope,
  advancedloggingparameters = loggingparameters()
  # MainTrail

){

  # Arguments check

  if(!all(unlist(lapply(list(inventory, speciescriteria), inherits, "data.frame"))))
    stop("The 'inventory' and 'speciescriteria' arguments of the 'treeselection' function must be data.frame")

  if(!any(inherits(objective, "numeric") || is.null(objective)))
    stop("The 'objective' argument of the 'treeselection' function must be numeric or NULL")

  if(!any(inherits(diversification, "logical") || is.null(diversification)))
    stop("The 'diversification' argument of the 'treeselection' function must be logical or NULL")

  if(!all(unlist(lapply(list(specieslax, objectivelax), inherits, "logical"))))
    stop("The 'specieslax' and 'objectivelax' arguments of the 'treeselection' function must be logicals")

  if (!any(scenario == "RIL1" || scenario == "RIL2broken"|| scenario == "RIL2"|| scenario == "RIL3"|| scenario == "RIL3fuel"||
           scenario == "RIL3fuelhollow"|| scenario =="manual"))
    stop("The 'scenario' argument of the 'treeselection' function must be 'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

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
    mutate(VisibleDefect = sample(c(1,0), size = 1, replace = F, prob = c(VisibleDefectProba, 1-VisibleDefectProba))) %>%    # 1 = visible defects tree, 0 = no visible defects
    ungroup() %>%
    mutate(VisibleDefect = as.factor(VisibleDefect))

  # Compute the objective volume with or without bonus:
  if (fuel =="2") {VO = objective
  }else{
    VO = objective + advancedloggingparameters$ObjectiveBonus # to compensate for the designated hollow wood.

  }

  inventory <- ONFGuyafortaxojoin(inventory, speciescriteria = speciescriteria) # Joins commercial criteria to the inventory

  VisibleDefectTable <- filter(inventory, VisibleDefect == "1")
  inventory <- filter(inventory, VisibleDefect == "0") # we continue with just visibly healthy trees

  harvestableOutputs <- harvestable(inventory,                                               # harvestable function
    diversification = diversification, specieslax = specieslax,
    topography = topography, plotslope = plotslope, advancedloggingparameters = advancedloggingparameters)

  inventory <- harvestableOutputs$inventory       # one of the output

  HVinit <- harvestableOutputs$HVinit             # the other output: initial harvestable volume

  selectedOutputs <- selected(                    # outputs of the selected function
    inventory,
    scenario = scenario, fuel = fuel, diversification = diversification,
    specieslax = specieslax, objectivelax = objectivelax, topography = topography,
    advancedloggingparameters = advancedloggingparameters, VO = VO, HVinit = HVinit)

  inventory <- selectedOutputs$inventory                                           # extract inventory of the selected outputs

  HollowTreesPoints <- selectedOutputs$HollowTreesPoints                           # extract a pts vector of the selected outputs
  EnergywoodTreesPoints <- selectedOutputs$EnergywoodTreesPoints                   # extract a pts vector of the selected outputs


  inventory <- futurereserve(inventory)


  # créer les conditions et vecteurs vides dans les listes à retourner
  # Points vector with coordinates of the harvestable trees:
  HarvestableTreesPoints <- inventory %>%
    filter(LoggingStatus == "harvestable" |LoggingStatus == "harvestableUp" |LoggingStatus == "harvestable2nd") # harvestableUp = DBH > MinFD individuals, harvestable2nd = eco2 individuals is specieslax

  if (dim(HarvestableTreesPoints)[1] != 0) {

    HarvestableTreesPoints <- st_as_sf(HarvestableTreesPoints, coords = c("Xutm", "Yutm")) # sp::coordinates(HarvestableTreesPoints) <- ~ Xutm + Yutm

    # sp::proj4string(HarvestableTreesPoints) <- raster::crs(topography)

  } else {HarvestableTreesPoints = st_point(x = c(NA_real_, NA_real_))}

  # Points vector with coordinates of the selected trees:
  SelectedTreesPoints <- inventory %>%
    filter(Selected == "1")

  if (dim(SelectedTreesPoints)[1] != 0) {

    st_as_sf(SelectedTreesPoints, coords = c("Xutm", "Yutm")) # sp::coordinates(SelectedTreesPoints) <- ~ Xutm + Yutm

    # sp::proj4string(SelectedTreesPoints) <- raster::crs(topography)

  } else {SelectedTreesPoints = st_point(x = c(NA_real_, NA_real_))}

  # Points vector with coordinates of the future trees:
  FutureTreesPoints <- inventory %>%
    filter(LoggingStatus == "future")

  if (dim(FutureTreesPoints)[1] != 0) {

    st_as_sf(FutureTreesPoints, coords = c("Xutm", "Yutm")) # sp::coordinates(FutureTreesPoints) <- ~ Xutm + Yutm

    # sp::proj4string(FutureTreesPoints) <- raster::crs(topography)

  } else {FutureTreesPoints = st_point(x = c(NA_real_, NA_real_))}

  # Points vector with coordinates of the reserve trees:
  ReserveTreesPoints <- inventory %>%
    filter(LoggingStatus == "reserve")

  if (dim(ReserveTreesPoints)[1] != 0) {

    st_as_sf(ReserveTreesPoints, coords = c("Xutm", "Yutm")) # sp::coordinates(ReserveTreesPoints) <- ~ Xutm + Yutm

    # sp::proj4string(ReserveTreesPoints) <- raster::crs(topography)

  } else {ReserveTreesPoints = st_point(x = c(NA_real_, NA_real_))}

  #where specieslax was not necessary, consider eco2s as non-exploitable:
  inventory <- inventory %>%
    mutate(LoggingStatus = ifelse(HVinit > VO & LoggingStatus == "harvestable2nd", "non-harvestable", LoggingStatus)) %>%

    # add the visible defects trees removed until now
    bind_rows(VisibleDefectTable)

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

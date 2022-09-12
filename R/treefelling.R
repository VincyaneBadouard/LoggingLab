#'Tree felling
#'
#'@description Simulates the tree felling, with the success or failure of the
#'  direction of the tree fall, foot to the trail, with an angle to the trail
#'  and avoiding the trees to protect, as desired. If FWE (Fuel Wood
#'  Exploitation), the tree will be directed with its crown towards the trail
#'  (if the orientation is successful) if it can be retrieved with a grapple.
#'
#'@param inventory Input inventory (see the inputs formats and metadata in the
#'  vignette) (data.frame)
#'
#'@param scenario Logging scenario among: "RIL1", "RIL2broken", "RIL2", "RIL3",
#'  "RIL3fuel", "RIL3fuelhollow" or "manual"(character) (see the
#'  vignette)
#'
#'@param fuel Fuel wood exploitation: no exploitation = "0", exploitation of
#'   damage and unused part of logged trees for fuel = "1", exploitation of
#'   hollow trees, damage and and unused part of the log for fuel = "2"
#'   If fuel wood exploitation (fuel = "1" or "2") the tree will be recovered
#'   from the crown with a grapple if possible (respected grapple conditions).
#'   If not, recovery at the foot with a cable at an angle to the trail.
#'   Avoid future/reserve trees if chosen.
#'
#'@param winching
#' "0": no cable or grapple (trail to tree foot)
#' "1": only cable (default = 40 m)
#' "2": grapple (default = 6 m) + cable (grapple priority)
#' If grapple + cable (winching = "2") without fuel wood (fuel = "0")
#'  recovery of the tree foot with grapple if possible (respected grapple
#'  conditions) otherwise with cable with angle to the trail.
#'  Avoidance of future/reserves if chosen.

#'
#'@param directionalfelling Directional felling =
#' "0": only to direct the foot of the tree towards the trail
#' "1": to direct the foot of the tree towards the trail + to avoid damage to
#'         future and reserve trees if possible
#' "2": to avoid damage to future and reserve trees if possible
#'       + orientation angle to the trail. Among the 2 possible angle positions,
#'       the position that favours the return to the main trail should be chosen.
#'       The angle to the trail is favoured to avoid future/reserve trees.
#' If the avoidance of future/reserve trees could not be performed,
#' a message is returned.
#'
#'@param maintrailsaccess Access point of main trail for each harvestable zone (sf or sfc)
#'@param scndtrail Secondary trails (sf)
#'
#'@param advancedloggingparameters Other parameters of the logging simulator
#'  \code{\link{loggingparameters}} (list)
#'
#'
#'@return Input inventory with new columns:
#'- The tree felling success or fail("*TreeFellingOrientationSuccess*")
#'- The fallen trees ("*TreePolygon*"): a MULTIPOLYGON of the tree oriented
#'   according to the chosen scenario
#'- The dead trees under felled trees (*DeathCause* = "*treefall2nd*")
#'
#'@details The felling of the tree creates a tree (including crown) on the
#' ground, with dimensions calculated with specific allometries
#' ('advancedloggingparameters').
#'
#'The crowns (fuel wood exploitation case) can only be retrieved with a grapple.
#'
#'RIL1/RIL2broken/RIL2:
#' - at 40%: random fall
#' - at 60% ('TreefallSuccessProportion'):
#' *base of the tree towards the nearest trail* (main or secondary)
#'
#'RIL3/RIL3 timber + fuel wood:
#' - at 40%: random fall
#' - at 60% ('TreefallSuccessProportion'):
#'   * if trees < 6 m from the trail and slope <20% (grapple use):
#'     - no particular angle to orientate to the trail,
#'        only to orient the tree *base*(*crown* if fuel wood exploitation)
#'        as close as possible to the trail
#'     - priority 1: avoid futures and reserves,
#'     - priority 2: conformation allowing skidding back to the main trail
#'
#'   * otherwise (trees > 6 m from the trail and/or slope >20%)(cable use):
#'     - 30-45◦ (default) orientation
#'        ('MinTreefallOrientation'; 'MaxTreefallOrientation')
#'     - *base* to nearest trail
#'     - conformation allowing skidding back to the main trail
#'     - avoid futures and reserves if possible
#'
#'Damage:
#'Secondary windfall: Not all trees under the felled tree (timber or energy)
#'will be considered dead. The probability of a tree dying under a felled tree
#'is estimated by the model 'Treefall2ndDeathModel', according to the DBH of the
#'tree whose probability of dying is estimated.
#'
#'@export
#'
#'@importFrom sf st_as_sf st_as_text st_geometry st_intersection st_make_valid
#'  st_buffer st_union
#'@importFrom dplyr filter group_by do left_join mutate select
#'@importFrom tibble add_column
#'@importFrom tidyr unnest
#'@importFrom stats runif
#'
#' @examples
#' set.seed(1)
#' data(SecondaryTrails)
#'
#'
#' scenario <- "manual"
#' winching <- "2"
#' fuel <- "2"
#' directionalfelling <- "2"
#'
#'
#' NewInventory <- treefelling(SecondaryTrails$inventory,
#'   scenario = scenario, fuel = fuel,
#'   winching = winching,
#'   directionalfelling = directionalfelling,
#'   maintrailsaccess = SecondaryTrails$MainTrailsAccess,
#'   scndtrail = SecondaryTrails$SmoothedTrails,
#'   advancedloggingparameters = loggingparameters())
#'
#' TreePolygon <- NewInventory %>%
#' getgeometry(TreePolygon) %>%
#' sf::st_set_crs(sf::st_crs(MainTrails)) # set a crs
#'
#' Inventory_crs <- sf::st_as_sf(NewInventory, coords = c("Xutm", "Yutm")) # as sf
#' Inventory_crs <- sf::st_set_crs(Inventory_crs, sf::st_crs(MainTrails)) # set a crs
#'
#' Treefall <- sf::st_as_sf(
#' dplyr::filter(NewInventory, DeathCause == "treefall2nd"),
#' coords = c("Xutm", "Yutm")) %>%
#' sf::st_set_crs(sf::st_crs(MainTrails)) # set a crs
#'
#' NonHarvestable <- sf::st_as_sf(
#' dplyr::filter(Inventory_crs, LoggingStatus == "non-harvestable"),
#' coords = c("Xutm", "Yutm"))
#'
#' Harvestable <- sf::st_as_sf(
#' dplyr::filter(Inventory_crs, LoggingStatus == "harvestable"),
#' coords = c("Xutm", "Yutm"))
#'
#' HarvestableUp <- sf::st_as_sf(
#' dplyr::filter(Inventory_crs, LoggingStatus == "harvestableUp"),
#' coords = c("Xutm", "Yutm"))
#'
#' Selected <- sf::st_as_sf(
#' dplyr::filter(Inventory_crs, Selected == "1"), coords = c("Xutm", "Yutm"))
#'
#' Reserve <- sf::st_as_sf(
#' dplyr::filter(Inventory_crs, LoggingStatus == "reserve"),
#' coords = c("Xutm", "Yutm"))
#'
#' Future <- sf::st_as_sf(
#' dplyr::filter(Inventory_crs, LoggingStatus == "future"),
#' coords = c("Xutm", "Yutm"))
#'
#' ProbedHollow <- sf::st_as_sf(
#' dplyr::filter(Inventory_crs, ProbedHollow == "1"), coords = c("Xutm", "Yutm"))
#'
#' VisibleDefect <- sf::st_as_sf(
#' dplyr::filter(Inventory_crs, VisibleDefect == "1"), coords = c("Xutm", "Yutm"))
#'
#' Treefall2nd <- sf::st_as_sf(
#' dplyr::filter(Inventory_crs, DeathCause == "treefall2nd"), coords = c("Xutm", "Yutm"))
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = Inventory_crs) +
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
#'   geom_sf(data = TreePolygon, # cuted trees
#'   alpha = 0.5, fill = "red") +
#'   geom_sf(data = Selected, aes(colour = "Selected"), show.legend = "point") +
#'   geom_sf(data = ProbedHollow,
#'   aes(colour = "Probed hollow"), show.legend = "point") +
#'   geom_sf(data = Treefall2nd,
#'   aes(colour = "Treefall2nd"), show.legend = "point") +
#'   geom_sf(data = SecondaryTrails$maintrailsaccess,
#'   alpha = 0.5, fill = "black") +
#'   geom_sf(data = SecondaryTrails$SmoothedTrails,
#'   alpha = 0.5, fill = "black") +
#'
#'   scale_colour_manual(values = c("Non-harvestable" = "grey",
#'   "Visible defect" = "pink", "Harvestable" = "skyblue",
#'   "HarvestableUp" = "blue", "Selected" = "red", "Future" = "orange",
#'   "Reserve" = "purple", "Probed hollow" = "forestgreen",
#'   "Treefall2nd" = "chocolate4")) +
#'   labs(color = "Logging status")
#'
#'\dontrun{
#' # The trees under the fallen trees
#' suppressWarnings(sf::st_intersection( # trees under the fallen trees
#'   getgeometry (NewInventory, TreePolygon),
#'   sf::st_as_sf(NewInventory, coords = c("Xutm", "Yutm"))
#' )) %>%
#'   ggplot() +
#'   geom_sf()
#'}
#'
treefelling <- function(
  inventory,
  scenario,
  fuel = NULL,
  winching = NULL,
  directionalfelling = NULL,
  maintrailsaccess,
  scndtrail,
  advancedloggingparameters = loggingparameters()
){

  # Arguments check

  if(!inherits(inventory, "data.frame"))
    stop("The 'inventory'argument of the 'treefelling' function must be data.frame")

  if (!any(scenario == "RIL1" || scenario == "RIL2broken" || scenario == "RIL2"
           || scenario == "RIL3" || scenario == "RIL3fuel"||
           scenario == "RIL3fuelhollow" || scenario =="manual"))
    stop("The 'scenario' argument of the 'treefelling' function must be
         'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  if (!any(fuel == "0" || fuel == "1"|| fuel == "2"|| is.null(fuel)))
    stop("The 'fuel' argument of the 'treefelling' function must be '0', '1', '2' or NULL")

  if (!any(directionalfelling == "0" || directionalfelling == "1" || directionalfelling == "2" || is.null(directionalfelling)))
    stop("The 'directionalfelling' argument of the 'treefelling' function must be '0', '1', '2' or NULL")

  if(!all(unlist(lapply(list(maintrailsaccess, scndtrail), inherits, c("sf", "sfc")))))
    stop("The 'maintrailsaccess' and 'scndtrail'arguments of the 'treefelling' function must be sf or sfc")

  if(!inherits(advancedloggingparameters, "list"))
    stop("The 'advancedloggingparameters' argument of the 'treefelling' function must be a list")

  if(scenario == "manual" &&
     (is.null(fuel) || is.null(directionalfelling)))
    stop("If you choose the 'manual' mode,
         you must fill in the arguments 'fuel' and 'directionalfelling'")

  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <-NULL
  Condition <- DBH <- MinFD <- Taxo <- alpha <- bCoef <- NULL
  DeathCause <- DistCriteria <- Family <- CrownHeight <- CrownDiameter <- NULL
  Genus <- Logged <- TreePolygon <- NULL
  LoggingStatus <- MaxFD <- Crowns <- NULL
  ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- SlopeCriteria <- Species <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD <- NULL
  VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL
  geometry <- idTree <- . <- Treefall2ndDeath <- Treefall2ndDeathProba <-  NULL


  # Redefinition of the parameters according to the chosen scenario
  scenariosparameters <- scenariosparameters(scenario = scenario, fuel = fuel, winching = winching,
                                             directionalfelling = directionalfelling)

  directionalfelling <- scenariosparameters$directionalfelling
  fuel <- scenariosparameters$fuel
  winching <- scenariosparameters$winching

  # Compute treefelling success and fails
  inventory <- directionalfellingsuccessdef(
    inventory,
    fuel = fuel,
    directionalfelling = directionalfelling,
    advancedloggingparameters = advancedloggingparameters)


  # Future/reserve crowns
  FutureReserveCrowns <- inventory %>% # create an object with future/reserve crowns only
    dplyr::filter(LoggingStatus == "future" | LoggingStatus == "reserve") %>%
    createcanopy() %>% # create all inventory crowns in the 'Crowns' column
    getgeometry(Crowns)


  # Main trails accesses with a width
  MainTrailsAccessBuff <- maintrailsaccess %>%
    sf::st_buffer(dist = runif(1,
                           advancedloggingparameters$MinMainTrailWidth,
                           advancedloggingparameters$MaxMainTrailWidth)) %>%
    sf::st_union() %>% sf::st_as_sf()

  # Treefelling
  felttrees <- inventory %>%
    filter(!is.na(TreeFellingOrientationSuccess)) %>%
    dplyr::group_by(idTree) %>% # for each tree
    dplyr::do(TreePolygon = # inform geometry. # Filling a column from a function whose input is a table
                felling1tree(.,
                             fuel = fuel, winching = winching, directionalfelling = directionalfelling,
                             maintrailsaccess = MainTrailsAccessBuff, scndtrail = scndtrail,
                             FutureReserveCrowns = FutureReserveCrowns,
                             advancedloggingparameters = advancedloggingparameters)$FallenTree %>%
                sf::st_as_text()) %>% # as text to easy join with a non spacial table
    tidyr::unnest(TreePolygon) # here to pass from list to character

  inventory <- left_join(inventory, felttrees, by = "idTree") # join spatial filtered inventory and non spatial complete inventory



  # Mortality

  # Records the felled trees
  if (!("DeathCause" %in% names(inventory))){
    inventory <- inventory %>%
      add_column(DeathCause = NA) # if "DeathCause" column doesnt exist create it
  }

  inventory <- inventory %>%
    mutate(DeathCause = ifelse(is.na(DeathCause) & !is.na(TreePolygon) & ProbedHollow == "0",
                               "cutted", DeathCause)) %>% # timber exploitation
    mutate(DeathCause = ifelse(is.na(DeathCause) & !is.na(TreePolygon) & ProbedHollow == "1",
                               "hollowfuel", DeathCause)) # fuel wood exploitation

  # Trees under the fallen trees
  felttrees <- felttrees %>% dplyr::select(-idTree) # remove pol infos to keep the information of the points

  DeadTrees <- suppressWarnings(sf::st_intersection(
    sf::st_make_valid(st_as_sf(inventory, coords = c("Xutm", "Yutm"))),
    sf::st_make_valid(getgeometry(felttrees, TreePolygon)) # "make valid" to avoid self-intersection
  )) %>%
    add_column(DeadTrees = "1") %>%
    dplyr::select(idTree, DeadTrees)
  sf::st_geometry(DeadTrees) <- NULL # remove TreePolygon column (sf to data.frame)
  DeadTrees <- unique(DeadTrees)

  inventory <- inventory %>%
    left_join(DeadTrees, by = "idTree") %>%
    mutate(DeathCause = ifelse(is.na(DeathCause) & is.na(TreePolygon) & DeadTrees == "1",
                               "treefall2nd", DeathCause)) %>%  # Damage trees
    dplyr::select(-DeadTrees)

  # length(which(inventory$DeathCause == "treefall2nd"))

  # Not all trees die under the felled tree:
  inventory <- inventory %>%

    mutate(Treefall2ndDeathProba = ifelse(DeathCause == "treefall2nd",
                                       advancedloggingparameters$Treefall2ndDeathModel(DBH), NA)) %>%  # Proba to be dead under a felled tree


    # generate either "1" or "0" randomly for each line, depending on the proba associated with the line:
    rowwise() %>%
    mutate(Treefall2ndDeath = ifelse(!is.na(Treefall2ndDeathProba),
                                  sample(c(1,0), size = 1, replace = F,
                                         prob = c(Treefall2ndDeathProba, 1-Treefall2ndDeathProba)), NA)) %>% # 1 = dead tree, 0 = alive tree
    ungroup() %>%
    mutate(Treefall2ndDeath = as.factor(Treefall2ndDeath)) %>% # alive trees ("0") -> DeathCause = NA
    mutate(DeathCause = ifelse(Treefall2ndDeath %in% "0", NA, # alive trees ("0") -> DeathCause = NA
                               DeathCause))

  # length(which(inventory$DeathCause == "treefall2nd"))


  return(inventory)

}

#'Directional felling success definition
#'
#'@description Defines whether the directed fall of the tree is successful or
#'  not by drawing in a Bernoulli distribution where the probability of success
#'  is by default 60%, and can be changed with the
#'  \code{advancedloggingparameters} argument.
#'
#'@param inventory input inventory (see the inputs formats and metadata in the
#'  vignette) (data.frame)
#'
#'@param fuel Fuel wood exploitation: no exploitation = "0", exploitation of
#'   damage and unused part of logged trees for fuel = "1", exploitation of
#'   hollow trees, damage and and unused part of the log for fuel = "2"
#'
#'@param directionalfelling Directional felling =
#' "0": only to direct the foot of the tree towards the trail
#' "1": to direct the foot of the tree towards the trail + to avoid damage to
#'         future and reserve trees
#' "2": to avoid damage to future and reserve trees + orientation angle
#'       to the trail
#'
#'@param advancedloggingparameters Other parameters of the logging simulator
#'  \code{\link{loggingparameters}} (list)
#'
#'@return Input inventory with the "TreeFellingOrientationSuccess" new column (see
#'  the outputs metadata in the vignette).
#'@export
#'
#'@importFrom dplyr mutate rowwise
#'
#' @examples
#'
#' data(Paracou6_2016)
#' data(DTMParacou)
#' data(MainTrails)
#' data(HarvestableAreaOutputsCable)
#'
#' inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask),
#' volumeparameters = ForestZoneVolumeParametersTable)
#'
#' inventory <- treeselection(inventory,
#' topography = DTMParacou,
#' speciescriteria = SpeciesCriteria,
#' scenario = "manual", objective = 10, fuel = "2", diversification = TRUE,
#' winching = "2", specieslax = FALSE, objectivelax = TRUE,
#' harvestablearea = HarvestableAreaOutputsCable$HarvestableArea,
#' plotslope = HarvestableAreaOutputsCable$PlotSlope,
#' maintrails = MainTrails,
#' harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons,
#' advancedloggingparameters = loggingparameters())$inventory
#'
#' new <- directionalfellingsuccessdef(inventory, fuel = "2",
#' directionalfelling = "2",
#' advancedloggingparameters = loggingparameters())
#'
directionalfellingsuccessdef <- function(
  inventory,
  fuel,
  directionalfelling,
  advancedloggingparameters = loggingparameters()) {


  # Arguments check

  if(!inherits(inventory, "data.frame"))
    stop("The 'inventory'argument of the 'directionalfellingsuccessdef' function must be data.frame")

  if (!any(fuel == "0" || fuel == "1"|| fuel == "2"|| is.null(fuel)))
    stop("The 'fuel' argument of the 'directionalfellingsuccessdef' function must be '0', '1', '2' or NULL")

  if (!any(directionalfelling == "0" || directionalfelling == "1"||
           directionalfelling == "2"|| is.null(directionalfelling)))
    stop("The 'directionalfelling' argument of the 'directionalfellingsuccessdef' function must be '0', '1', '2' or NULL")

  if(!inherits(advancedloggingparameters, "list"))
    stop("The 'advancedloggingparameters' argument of the 'directionalfellingsuccessdef' function must be a list")

  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <- NULL
  Condition <- DBH <- NULL
  DeathCause <- DistCriteria <- Family <- CrownHeight <- CrownDiameter <- NULL
  ForestZoneVolumeParametersTable <- Genus <- Logged <- TreePolygon <- NULL
  LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  ParamCrownDiameterAllometry <- NULL
  ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- SlopeCriteria <- Species <- Species.genus <- NULL
  Taxo <- Taxo.family <- Taxo.genus <- Taxo.species <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD <- UpMinFD.genus <- NULL
  UpMinFD.species <- VernName.genus <- VernName.genus.genus <- NULL
  VernName.species <- VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL
  alpha <- alpha.family <- alpha.genus <- alpha.species <- bCoef <- NULL
  beta.family <- beta.genus <- beta.species <- geometry <- idTree <- NULL


  # Totally random
  # if (totally random case){ # No directional felling
  #   inventory <- inventory %>%
  #     mutate(TreeFellingOrientationSuccess = ifelse(Selected == "1", 0, NA)) # always fail -> totally random
  # }

  # No hollow trees exploitation
  if (fuel !="2"){
    inventory <- inventory %>%
      rowwise() %>%
      mutate(TreeFellingOrientationSuccess =
               ifelse(Selected == "1",
                      sample(c(1,0), size = 1, replace = F,
                             prob = c(advancedloggingparameters$TreefallSuccessProportion,
                                      1-advancedloggingparameters$TreefallSuccessProportion)), NA))
    # Accessible <- Selected !!!!!!!!!!!! pas oublier pour if (fuel == "0")

  }

  # Hollow trees exploitation too
  if (fuel =="2") {

    inventory <- inventory %>%
      rowwise() %>%
      mutate(TreeFellingOrientationSuccess =
               ifelse(Selected == "1"| (Selected == "1" & ProbedHollow == "1"), # Selected = not yet linked by 2ndtrails, because 2ndtrails came after
                      sample(c(1,0), size = 1, replace = F,
                             prob = c(advancedloggingparameters$TreefallSuccessProportion,
                                      1-advancedloggingparameters$TreefallSuccessProportion)), NA))

  }

  inventory$TreeFellingOrientationSuccess <- as.factor(inventory$TreeFellingOrientationSuccess)


  return(inventory)

}


#' rotatepolygon
#'
#' @description Rotate the input polygon with a given angle and around a fix point.
#' Function adapted from Jeffrey Evans' 'rotate.polygon' function:
#' https://github.com/jeffreyevans/spatialEco/blob/master/R/rotate.polygon.R
#'
#' @param p Polygon (POLYGON or sfc_POLYGON)
#' @param angle Angle in degrees in the clockwise direction (numeric)
#' @param fixed Fix point around which the polygon will be rotated (POINT)
#'
#' @return The polygon (sfc_POLYGON) rotated.
#' @export
#' @importFrom nngeo st_ellipse
#' @importFrom sf st_polygon st_coordinates st_sfc st_difference st_union
#'
#' @examples
#' data(Paracou6_2016)
#' data(DTMParacou)
#' data(SpeciesCriteria)
#' data(MainTrails)
#'
#' inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask),
#' volumeparameters = ForestZoneVolumeParametersTable)
#'
#' inventory <- suppressMessages(treeselection(inventory,
#' topography = DTMParacou,
#' speciescriteria = SpeciesCriteria,
#' scenario = "manual", objective = 10, fuel = "2", diversification = TRUE,
#' winching = "0", specieslax = FALSE, objectivelax = TRUE,
#' plotslope = HarvestableAreaOutputsCable$PlotSlope,
#' maintrails = MainTrails,
#' harvestablearea = HarvestableAreaOutputsCable$HarvestableArea,
#' harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons,
#' advancedloggingparameters = loggingparameters())$inventory)
#'
#' inventory <- inventory %>%
#'      dplyr::filter(Selected == "1") %>%
#'      dplyr::select(idTree,DBH,TrunkHeight,TreeHeight,CrownHeight,
#'      CrownDiameter,Selected, Xutm, Yutm)
#' dat <- inventory[1,]
#'
#' library(sf)
#' library(nngeo)
#' library(dplyr)
#' Foot <- st_point(c(dat$Xutm,dat$Yutm)) # tree foot point
#' Crown <- dat %>%
#' dplyr::mutate(xCrown = Xutm,
#'        yCrown = Yutm + TrunkHeight + CrownHeight/2,
#'        exCrown = CrownDiameter/2,
#'        eyCrown = CrownHeight/2) %>%
#' sf::st_as_sf(coords = c("xCrown", "yCrown")) # ellipse centroid coordinates
#' Crown <- st_ellipse(Crown, Crown$exCrown, Crown$eyCrown) # create the ellipse
#' Trunk <- with(dat, # and the trunk
#'               st_polygon(list(matrix(c(Xutm-(DBH/100)/2, Yutm,
#'                                       Xutm-(DBH/100)/2, Yutm + TrunkHeight,
#'                                       Xutm+(DBH/100)/2, Yutm + TrunkHeight,
#'                                       Xutm+(DBH/100)/2, Yutm,
#'                                       Xutm-(DBH/100)/2, Yutm) # the return
#'                                     ,ncol=2, byrow=TRUE))))
#'  RandomAngle <- as.numeric(sample(c(0:359), size = 1))
#'
#'  TreeP <- st_difference(st_union(
#'  rotatepolygon(Trunk, angle = RandomAngle, fixed = Foot), # turned trunk
#'  rotatepolygon(Crown, angle = RandomAngle, fixed = Foot) # turned crown
#'  ))
#'
#' library(ggplot2)
#'  ggplot() +
#'   geom_sf(data = st_union(Trunk, Crown), colour = "red") +
#'   geom_sf(data = TreeP, colour = "green")
#'
rotatepolygon <- function(
  p,
  angle,
  fixed
){

  # Arguments check
  if(!inherits(p, c("sf", "sfc_POLYGON", "POLYGON")))
    stop("The 'p' argument of the 'rotatepolygon' function must be a sf, a sfc_POLYGON or a POLYGON")

  if(!inherits(angle, "numeric"))
    stop("The 'angle' argument of the 'rotatepolygon' function must be numeric")

  if(!inherits(fixed, "POINT"))
    stop("The 'fixed' argument of the 'rotatepolygon' function must be a POINT")

  p.coords <- st_coordinates(p)[,1:2] # Polygon coordinates extraction
  p.center <- st_coordinates(fixed)

  center = c(p.center[1],
             p.center[2])

  co <- cos(-angle * pi / 180)
  si <- sin(-angle * pi / 180)
  adj <- matrix(rep(center, nrow(p.coords)), ncol = 2, byrow = TRUE) # matrix with fixed point coordinates
  p.coords <- p.coords-adj
  p.rotate <- cbind(co * p.coords[,1] - si * p.coords[,2],si * p.coords[,1] + co * p.coords[,2]) + adj

  Turned <- st_sfc(st_polygon(list(p.rotate))) # create the turned polygon

  return(Turned)
}

#'felling1tree
#'
#'@description Simulates the tree (multipolygon) falling towards the trail or
#'  not, at a given angle. If it has been decided to exploit fuel wood, the tree
#'  crowns will be directed towards the trail if they can be accessed with a
#'  grapple (see the \code{GrappleLength} argument of the
#'  \code{\link{loggingparameters}} function). In other cases, the fall will be
#'  made from the base of the tree towards the trail. The orientation of the
#'  fall succeeds or fails according to a Bernoulli law where the probability of
#'  success is by default 60%, and can be changed with the
#'  \code{advancedloggingparameters} argument.
#'
#'@param dat 1 row data.frame with columns: Xutm, Yutm, CrownDiameter,
#'  CrownHeight, DBH, TrunkHeight, TreeHeight, TreeFellingOrientationSuccess
#'
#'@param fuel Fuel wood exploitation: no exploitation = "0", exploitation of
#'   damage and unused part of logged trees for fuel = "1", exploitation of
#'   hollow trees, damage and and unused part of the log for fuel = "2"
#'   If fuel wood exploitation (fuel = "1" or "2") the tree will be recovered
#'   from the crown with a grapple if possible (respected grapple conditions).
#'   If not, recovery at the foot with a cable at an angle to the trail.
#'   Avoid future/reserve trees if chosen.
#'
#'@param winching
#' "0": no cable or grapple (trail to tree foot)
#' "1": only cable (default = 40m)
#' "2": grapple (default = 6m) + cable (grapple priority)
#' If grapple + cable (winching = "2") without fuel wood (fuel = "0")
#'  recovery of the tree foot with grapple if possible (respected grapple
#'  conditions) otherwise with cable with angle to the trail.
#'  Avoidance of future/reserves if chosen.
#'
#'@param directionalfelling Directional felling =
#' "0": only to direct the foot of the tree towards the trail
#' "1": to direct the foot of the tree towards the trail + to avoid damage to
#'         future and reserve trees if possible
#' "2": to avoid damage to future and reserve trees if possible
#'       + orientation angle to the trail. Among the 2 possible angle positions,
#'       the position that favours the return to the main trail should be chosen.
#'       The angle to the trail is favoured to avoid future/reserve trees.
#'
#'@param maintrailsaccess Main trails access points (sf)
#'@param scndtrail Secondary trails (sf)
#'
#'@param FutureReserveCrowns Future/reserve trees crown (sf)
#'
#'@param advancedloggingparameters Other parameters of the logging simulator
#'  \code{\link{loggingparameters}} (list)
#'
#'@return A list with: FallenTree: a sfc_MULTIPOLYGON of the tree oriented
#'  according to the chosen scenario. Foot: a POINT for the base of the tree
#'  (the rotation fixed point). NearestPoints: a sfc_LINESTRING for the shortest
#'  path from the base of the tree to the nearest trail, Trail: the union of the
#'  main and the secondary trails. TrailPt: the POINT on the Trail closest
#'  to the location of the tree.
#'
#'@seealso \code{\link{loggingparameters}}
#'
#'@export
#'
#'@importFrom dplyr mutate filter rowwise ungroup bind_rows summarise sample_n
#'@importFrom sf st_point st_multipolygon st_as_sf st_nearest_points st_distance
#'  st_cast st_intersects st_buffer st_sfc st_combine st_set_crs st_crs
#'@importFrom nngeo st_ellipse
#'@importFrom matlib angle
#'
#' @examples
#' data(SecondaryTrails)
#'
#' # create an object with future/reserve crowns only
#' FutureReserveCrowns <- SecondaryTrails$inventory %>%
#'  dplyr::filter(LoggingStatus == "future" | LoggingStatus == "reserve") %>%
#'  createcanopy() %>% # create all inventory crowns in the 'Crowns' column
#'  getgeometry(Crowns)
#'
#' dat <- SecondaryTrails$inventory %>%
#'   dplyr::filter(Selected == "1") %>%
#'   dplyr::select(idTree,DBH,TrunkHeight,TreeHeight,CrownHeight,
#'                 CrownDiameter,Selected, Xutm, Yutm) %>%
#'   dplyr::sample_n(1) %>% # just 1 row (1 tree)
#'   # force the orientation success for the exemple
#'   tibble::add_column(TreeFellingOrientationSuccess = "1")
#'
#' rslt <- felling1tree(dat,
#'  fuel = "0", winching = "0", directionalfelling = "0",
#'  maintrailsaccess = SecondaryTrails$MainTrailsAccess,
#'  scndtrail = SecondaryTrails$SmoothedTrails,
#'  FutureReserveCrowns = FutureReserveCrowns,
#'  advancedloggingparameters = loggingparameters())
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = sf::st_set_crs(sf::st_sfc(rslt$Foot),
#'                                 sf::st_crs(SecondaryTrails$MainTrailsAccess))) +
#'   geom_sf(data = sf::st_set_crs(sf::st_as_sf(rslt$Trail),
#'                                 sf::st_crs(SecondaryTrails$MainTrailsAccess))) +
#'   geom_sf(data = rslt$NearestPoints) +
#'   geom_sf(data = sf::st_set_crs(sf::st_sfc(rslt$TrailPt),
#'                                 sf::st_crs(SecondaryTrails$MainTrailsAccess))) +
#'   geom_sf(data = sf::st_set_crs(rslt$FallenTree,
#'                                 sf::st_crs(SecondaryTrails$MainTrailsAccess))) +
#'   geom_sf(data = sf::st_set_crs(FutureReserveCrowns,
#'                                 sf::st_crs(SecondaryTrails$MainTrailsAccess))) # set a crs
#'
felling1tree <- function(
  dat,
  fuel,
  winching,
  directionalfelling,
  maintrailsaccess,
  scndtrail,
  FutureReserveCrowns,
  advancedloggingparameters = loggingparameters()
){

  # Arguments check

  if(!inherits(dat, "data.frame"))
    stop("The 'dat' argument of the 'felling1tree' function must be data.frame")

  if(nrow(dat)!=1)
    stop("the data.frame given in the 'dat' argument
         of the 'felling1tree' function must contain only 1 row")

  if (!any(fuel == "0" || fuel == "1"|| fuel == "2"|| is.null(fuel)))
    stop("The 'fuel' argument of the 'felling1tree' function must be '0', '1', '2' or NULL")

  if (!any(directionalfelling == "0" || directionalfelling == "1"
           || directionalfelling == "2" || is.null(directionalfelling)))
    stop("The 'directionalfelling' argument of the 'felling1tree' function must be '0', '1', '2' or NULL")

  if(!all(unlist(lapply(list(maintrailsaccess, scndtrail), inherits, c("sf", "sfc")))))
    stop("The 'maintrailsaccess' and 'scndtrail'arguments of the 'felling1tree' function must be sf or sfc")

  if(!inherits(advancedloggingparameters, "list"))
    stop("The 'advancedloggingparameters' argument of the 'felling1tree' function must be a list")

  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <- NULL
  Condition <- DBH <- NULL
  DeathCause <- DistCriteria <- Family <- CrownHeight <- CrownDiameter <- NULL
  Genus <- Logged <- TreePolygon <- Crowns <- NULL
  LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- SlopeCriteria <- Species <- Species.genus <- NULL
  Taxo <- Taxo.family <- Taxo.genus <- Taxo.species <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD <- UpMinFD.genus <- NULL
  UpMinFD.species <- VernName.genus <- VernName.genus.genus <- NULL
  VernName.species <- VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL
  alpha <- alpha.family <- alpha.genus <- alpha.species <- bCoef <- NULL
  beta.family <- beta.genus <- beta.species <- geometry <- idTree <- NULL

  #### Individual geometry ####
  # The foot
  Foot <- st_point(c(dat$Xutm,dat$Yutm))
  Foot_crs <- st_sfc(Foot) # as sfc
  Foot_crs <- st_set_crs(Foot_crs, st_crs(maintrailsaccess)) # set a crs

  # The crown
  Crown <- dat %>%
    mutate(xCrown = Xutm,
           yCrown = Yutm + TrunkHeight + CrownHeight/2,
           exCrown = CrownDiameter/2,
           eyCrown = CrownHeight/2) %>%
    st_as_sf(coords = c("xCrown", "yCrown")) # ellipse centroid coordinates
  Crown <- st_ellipse(Crown, Crown$exCrown, Crown$eyCrown) # create the ellipse

  # The trunk
  Trunk <- with(dat,
                st_polygon(list(matrix(c(Xutm-(DBH/100)/2, Yutm,
                                         Xutm-(DBH/100)/2, Yutm + TrunkHeight,
                                         Xutm+(DBH/100)/2, Yutm + TrunkHeight,
                                         Xutm+(DBH/100)/2, Yutm,
                                         Xutm-(DBH/100)/2, Yutm) # the return
                                       ,ncol=2, byrow=TRUE))))

  #### Tree-Trail link ####
  # Find the point (TrailPt) on the Trail closest to the location of the tree (Foot)

  Trail <- st_union(maintrailsaccess, scndtrail) # Our trail will be maintrailsaccess or scndtrail
  #Trail <- scndtrail # only scndtrail

  NearestPoints <- st_nearest_points(Foot_crs, Trail) # from the Foot of the tree to the Trail (linestring)

  NearestPoint <- st_cast(NearestPoints, "POINT") # to have start (Foot) and end (TrailPt) points
  TrailPt <- NearestPoint[[2]] # the point (TrailPt) on the Trail closest to the location of the tree (Foot)

  #### Angles ####
  # Compute the angle between the tree default position and the shortest way from the foot to the trail

  ## Right-hand trail
  if(TrailPt[1] >= Foot[1]){ # x trail > x foot

    theta <- as.numeric(matlib::angle(c(Foot[1] - Foot[1], dat$TreeHeight),
                                      c(TrailPt[1] - Foot[1], TrailPt[2] - Foot[2]), degree = TRUE))

    ## Left-hand trail
  }else if(TrailPt[1] < Foot[1]){ # x trail < x foot

    theta <- 180 - as.numeric(matlib::angle(c(Foot[1] - Foot[1], dat$TreeHeight),
                                            c(TrailPt[1] - Foot[1], TrailPt[2] - Foot[2]), degree = TRUE))
  }

  ## when the tree is on the trail (no angle)
  if(is.na(theta)) theta <- 0L

  # Felling angle
  TreefallOrientation <- as.numeric(c(1:179)) # angle limits
  OppAng <- 180-(90 + TreefallOrientation) # Compute the third angle of the right-angled triangle (see vignette figure)

  # For cable (default = [30;45°]) if directionalfelling == "2" or fuel != "0"
  if(directionalfelling == "2" || winching == "2" || fuel != "0"){
    CableTreefallOrientation <- as.numeric(c(advancedloggingparameters$MinTreefallOrientation:
                                               advancedloggingparameters$MaxTreefallOrientation))

    CableOppAng <- 180-(90 + CableTreefallOrientation) # the angle between the closest position to the trail (90°) and the desired position (desired angle to the trail)
  }

  ## Angle to the trail

  # if no angle to the trail or fuel wood exploitation
  if (directionalfelling != "2" && fuel == "0") {

    ### Right-hand trail
    if(TrailPt[1] >= Foot[1]){ # x trail > x foot
      Aangle <- round(as.numeric(180 + OppAng + theta), digits = 0) # Foot oriented
      ### Left-hand trail
    }else if(TrailPt[1] < Foot[1]){ # x trail < x foot
      Aangle <- round(as.numeric(360 - OppAng + theta), digits = 0) # Foot oriented
    }
  }

  # if winching by grapple and cable without fuel wood exploitation
  if(winching == "2" && fuel == "0"){

    # Angle depending on whether the trail is to the right/left of the default position of the tree
    ## Right-hand trail
    if(TrailPt[1] >= Foot[1]){ # x trail > x foot
      # CABLE
      CableAangle <- round(as.numeric(180 + CableOppAng + theta), digits = 0)
      CableBangle <- round(as.numeric(180 - CableOppAng + theta), digits = 0)
      # GRAPPLE
      Aangle <- round(as.numeric(180 + OppAng + theta), digits = 0) # Foot oriented

      ## Left-hand trail
    }else if(TrailPt[1] < Foot[1]){ # x trail < x foot
      # CABLE
      CableAangle <- round(as.numeric(360 - CableOppAng + theta), digits = 0)
      CableBangle <- round(as.numeric(theta + CableOppAng), digits = 0)
      # GRAPPLE
      Aangle <- round(as.numeric(360 - OppAng + theta), digits = 0) # Foot oriented
    }
  }

  # if angle to the trail or fuel wood exploitation
  if ((directionalfelling == "2" && winching != "2") || (fuel =="1" || fuel =="2")) {

    # Angle depending on whether the trail is to the right/left of the default position of the tree
    ## Right-hand trail
    if(TrailPt[1] >= Foot[1]){ # x trail > x foot
      # Foot oriented
      Aangle <- round(as.numeric(180 + CableOppAng + theta), digits = 0) # CABLE
      Bangle <- round(as.numeric(180 - CableOppAng + theta), digits = 0) # CABLE
      # Crown oriented
      CrownAangle <- as.numeric(theta + OppAng) # GRAPPLE

      ## Left-hand trail
    }else if(TrailPt[1] < Foot[1]){ # x trail < x foot
      # Foot oriented
      Aangle <- round(as.numeric(360 - CableOppAng + theta), digits = 0) # CABLE
      Bangle <- round(as.numeric(theta + CableOppAng), digits = 0) # CABLE
      # Crown oriented
      CrownAangle <- as.numeric(180 + OppAng + theta) # GRAPPLE
    }
  }


  #### Felling function ####
  felling0angle <- function(Angl){

    Trunksf <- st_as_sf(rotatepolygon(Trunk, angle = Angl, fixed = Foot))
    Crownsf <- st_as_sf(rotatepolygon(Crown, angle = Angl, fixed = Foot))

    Trunk_Crowns <- Trunksf %>% rbind(Crownsf)

    FallenTree <- st_sfc(do.call(c, st_geometry(Trunk_Crowns)))

    # Previous version
    # FallenTree <- st_difference(st_union(
    #   rotatepolygon(Trunk, angle = Angl, fixed = Foot), # turned trunk
    #   rotatepolygon(Crown, angle = Angl, fixed = Foot) # turned crown
    # ))

    return(FallenTree)
  }

  #### Scenarios ####

  #### For a totally random direction felling ####
  # if(totally random)){
  #   RandomAngle <- as.numeric(sample(c(0:359), size = 1))
  #   FallenTree <- felling0angle(RandomAngle)
  # }

  #### No other orientation than to direct the foot of the tree towards the trail (0-180°) ####
  if (directionalfelling == "0" && fuel == "0" && winching != "2") {
    if(dat$TreeFellingOrientationSuccess == "1"){

      Aangle <- as.numeric(sample(Aangle, size = 1))
      FallenTree <- felling0angle(Aangle)

    }else if(dat$TreeFellingOrientationSuccess == "0"){ # else random felling

      RandomAngle <- as.numeric(sample(c(0:359), size = 1))

      FallenTree <- felling0angle(RandomAngle)
    }
  }

  #### To direct to !avoid damage to future and reserve trees!, and foot directed to the trail
  if (directionalfelling == "1" && fuel == "0" && winching != "2") {
    if(dat$TreeFellingOrientationSuccess == "1"){

      # Available felling positions
      AvailableFellingPositions <- lapply(as.list(Aangle), felling0angle) %>%
        lapply(function(x) data.frame(Tree = x)) %>%
        bind_rows()

      # Overlaps with future/reserve trees
      overlaps <- st_intersects(AvailableFellingPositions$geometry,
                                summarise(FutureReserveCrowns, Crowns = st_combine(Crowns))$Crowns) %>%
        lengths()

      ## if there are any position without intersection
      if(any(overlaps == 0)){
        EmptyFellingPositions <- AvailableFellingPositions %>%
          mutate(overlaps = overlaps) %>%
          filter(overlaps == 0) # only positions without intersection

        ## if there are no position without intersection
      }else{
        EmptyFellingPositions <- AvailableFellingPositions # take all the positions
        message("It was not possible to avoid future/reserve trees in the felling of a tree")
      }

      # Sample only one position
      FallenTree <- sample_n(EmptyFellingPositions, 1)$geometry

    }else if(dat$TreeFellingOrientationSuccess == "0"){ # else random felling

      RandomAngle <- as.numeric(sample(c(0:359), size = 1))

      FallenTree <- felling0angle(RandomAngle)
    }
  }


  #### Scenarios with orientation angle to the trail or fuel wood exploitation ####

  #### To direct to avoid damage to future and reserve trees + orientation angle to the trail (no fuel wood exploitation) ####
  if(winching == "1" && directionalfelling == "2" && fuel == "0"){ # = CABLE only
    if(dat$TreeFellingOrientationSuccess == "1"){

      # Calculate the two possible crown configurations
      ACrown <- lapply(as.list(Aangle), function(element) rotatepolygon(Crown, angle = element, fixed = Foot)) # turned crowns
      BCrown <- lapply(as.list(Bangle), function(element) rotatepolygon(Crown, angle = element, fixed = Foot)) # turned crowns

      ACrown <- lapply(ACrown, function(element) st_set_crs(element, st_crs(maintrailsaccess))) # set a crs
      BCrown <- lapply(BCrown, function(element) st_set_crs(element, st_crs(maintrailsaccess))) # set a crs

      # Test the best to pull the tree back to the main trail (farthest crown from the main trail)
      ADist <- unlist(lapply(as.list(ACrown), function(element) st_distance(element, maintrailsaccess)[1,1])) #matrix to value
      BDist <- unlist(lapply(as.list(BCrown), function(element) st_distance(element, maintrailsaccess)[1,1])) #matrix to value

      if(max(ADist, BDist) == max(BDist)){
        Aangle <- Bangle # B configuration
      }

      # Available felling positions
      AvailableFellingPositions <- lapply(as.list(Aangle), felling0angle) %>%
        lapply(function(x) data.frame(Tree = x)) %>%
        bind_rows()

      # Overlaps with future/reserve trees
      overlaps <- st_intersects(AvailableFellingPositions$geometry,
                                summarise(FutureReserveCrowns, Crowns = st_combine(Crowns))$Crowns) %>%
        lengths()

      ## if there are any position without intersection
      if(any(overlaps == 0)){
        EmptyFellingPositions <- AvailableFellingPositions %>%
          mutate(overlaps = overlaps) %>%
          filter(overlaps == 0) # only positions without intesection

        ## if there are no position without intersection
      }else{
        EmptyFellingPositions <- AvailableFellingPositions # take all the positions
        message("It was not possible to avoid future/reserve trees in the felling of a tree")

      }

      # Sample only one position
      FallenTree <- sample_n(EmptyFellingPositions, 1)$geometry

    }else if(dat$TreeFellingOrientationSuccess == "0"){ # else random felling
      RandomAngle <- as.numeric(sample(c(0:359), size = 1))

      FallenTree <- felling0angle(RandomAngle)
    }
  }

  #### Grapple + cable without fuel wood exploitation (always foot towards the trail) ####
  if(winching == "2" & fuel == "0"){

    # TrailDist <- st_distance(Foot, TrailPt) # distance between the tree foot and the Trail closest point

    if(dat$TreeFellingOrientationSuccess == "1"){

      if(dat$WinchingMachine %in% "Grpl"){ # accessible by grapple (slope and length) -> winching by grapple

        if(directionalfelling == "0"){
          Aangle <- as.numeric(sample(Aangle, size = 1))
          FallenTree <- felling0angle(Aangle)

          # Fut/res tree avoidance
        }else{
          # Available felling positions
          AvailableFellingPositions <- lapply(as.list(Aangle), felling0angle) %>%
            lapply(function(x) data.frame(Tree = x)) %>%
            bind_rows()

          # Overlaps with future/reserve trees
          overlaps <- st_intersects(AvailableFellingPositions$geometry,
                                    summarise(FutureReserveCrowns, Crowns = st_combine(Crowns))$Crowns) %>%
            lengths()

          ## if there are any position without intersection
          if(any(overlaps == 0)){
            EmptyFellingPositions <- AvailableFellingPositions %>%
              mutate(overlaps = overlaps) %>%
              filter(overlaps == 0) # only positions without intesection

            ## if there are no position without intersection
          }else{
            EmptyFellingPositions <- AvailableFellingPositions # take all the positions
            message("It was not possible to avoid future/reserve trees in the felling of a tree")

          }

          # Sample only one position
          FallenTree <- sample_n(EmptyFellingPositions, 1)$geometry
        } # end Fut/res tree avoidance


      }else{ # > 6m -> winching by CABLE

        # Calculate the two possible crown configurations
        ACrown <- lapply(as.list(CableAangle), function(element) rotatepolygon(Crown, angle = element, fixed = Foot)) # turned crowns
        BCrown <- lapply(as.list(CableBangle), function(element) rotatepolygon(Crown, angle = element, fixed = Foot)) # turned crowns

        ACrown <- lapply(ACrown, function(element) st_set_crs(element, st_crs(maintrailsaccess))) # set a crs
        BCrown <- lapply(BCrown, function(element) st_set_crs(element, st_crs(maintrailsaccess))) # set a crs

        # Test the best to pull the tree back to the main trail (farthest crown from the main trail)
        ADist <- unlist(lapply(as.list(ACrown), function(element) st_distance(element, maintrailsaccess)[1,1])) #matrix to value
        BDist <- unlist(lapply(as.list(BCrown), function(element) st_distance(element, maintrailsaccess)[1,1])) #matrix to value

        if(max(ADist, BDist) == max(BDist)){
          CableAangle <- CableBangle # B configuration
        }

        # No fut/res tree avoidance
        if(directionalfelling == "0"){

          CableAangle <- as.numeric(sample(CableAangle, size = 1))
          FallenTree <- felling0angle(CableAangle)

          # Fut/res tree avoidance
        }else{
          # Available felling positions
          AvailableFellingPositions <- lapply(as.list(CableAangle), felling0angle) %>%
            lapply(function(x) data.frame(Tree = x)) %>%
            bind_rows()

          # Overlaps with future/reserve trees
          overlaps <- st_intersects(AvailableFellingPositions$geometry,
                                    summarise(FutureReserveCrowns, Crowns = st_combine(Crowns))$Crowns) %>%
            lengths()

          ## if there are any position without intersection
          if(any(overlaps == 0)){
            EmptyFellingPositions <- AvailableFellingPositions %>%
              mutate(overlaps = overlaps) %>%
              filter(overlaps == 0) # only positions without intersection

            ## if there are no position without intersection
          }else{
            EmptyFellingPositions <- AvailableFellingPositions # take all the positions
            message("It was not possible to avoid future/reserve trees in the felling of a tree")

          }

          # Sample only one position
          FallenTree <- sample_n(EmptyFellingPositions, 1)$geometry
        }
      }

    }else if(dat$TreeFellingOrientationSuccess == "0"){ # else random felling
      RandomAngle <- as.numeric(sample(c(0:359), size = 1))
      FallenTree <- felling0angle(RandomAngle)
    }
  }

  #### Fuel wood exploitation (including to avoid damage to future and reserve trees + trail orientation (if cable)) ####
  if(fuel =="1" || fuel =="2"){

    # TrailDist <- st_distance(Foot, TrailPt) # distance between the tree foot and the Trail closest point

    if(dat$TreeFellingOrientationSuccess == "1"){

      if(dat$WinchingMachine %in% "Grpl"){ # accessible by grapple (slope and length) -> winching by grapple

        # No fut/res tree avoidance
        if(directionalfelling == "0"){
          CrownAangle <- as.numeric(sample(CrownAangle, size = 1))
          FallenTree <- felling0angle(CrownAangle)

          # Fut/res tree avoidance
        }else{
          # Available felling positions
          AvailableFellingPositions <- lapply(as.list(CrownAangle), felling0angle) %>%
            lapply(function(x) data.frame(Tree = x)) %>%
            bind_rows()

          # Overlaps with future/reserve trees
          overlaps <- st_intersects(AvailableFellingPositions$geometry,
                                    summarise(FutureReserveCrowns, Crowns = st_combine(Crowns))$Crowns) %>%
            lengths()

          ## if there are any position without intersection
          if(any(overlaps == 0)){
            EmptyFellingPositions <- AvailableFellingPositions %>%
              mutate(overlaps = overlaps) %>%
              filter(overlaps == 0) # only positions without intesection

            ## if there are no position without intersection
          }else{
            EmptyFellingPositions <- AvailableFellingPositions # take all the positions
            message("It was not possible to avoid future/reserve trees in the felling of a tree")

          }

          # Sample only one position
          FallenTree <- sample_n(EmptyFellingPositions, 1)$geometry
        } # end Fut/res tree avoidance

      }else{ # > 6m -> winching by CABLE -> foot to trail

        # Calculate the two possible crown configurations
        ACrown <- lapply(as.list(Aangle), function(element) rotatepolygon(Crown, angle = element, fixed = Foot)) # turned crowns
        BCrown <- lapply(as.list(Bangle), function(element) rotatepolygon(Crown, angle = element, fixed = Foot)) # turned crowns

        ACrown <- lapply(ACrown, function(element) st_set_crs(element, st_crs(maintrailsaccess))) # set a crs
        BCrown <- lapply(BCrown, function(element) st_set_crs(element, st_crs(maintrailsaccess))) # set a crs

        # Test the best to pull the tree back to the main trail (farthest crown from the main trail)
        ADist <- unlist(lapply(as.list(ACrown), function(element) st_distance(element, maintrailsaccess)[1,1])) #matrix to value
        BDist <- unlist(lapply(as.list(BCrown), function(element) st_distance(element, maintrailsaccess)[1,1])) #matrix to value

        if(max(ADist, BDist) == max(BDist)){
          Aangle <- Bangle # B configuration
        }

        # No fut/res tree avoidance
        if(directionalfelling == "0"){

          Aangle <- as.numeric(sample(Aangle, size = 1))
          FallenTree <- felling0angle(Aangle)

          # Fut/res tree avoidance
        }else{
          # Available felling positions
          AvailableFellingPositions <- lapply(as.list(Aangle), felling0angle) %>%
            lapply(function(x) data.frame(Tree = x)) %>%
            bind_rows()

          # Overlaps with future/reserve trees
          overlaps <- st_intersects(AvailableFellingPositions$geometry,
                                    summarise(FutureReserveCrowns, Crowns = st_combine(Crowns))$Crowns) %>%
            lengths()

          ## if there are any position without intersection
          if(any(overlaps == 0)){
            EmptyFellingPositions <- AvailableFellingPositions %>%
              mutate(overlaps = overlaps) %>%
              filter(overlaps == 0) # only positions without intesection

            ## if there are no position without intersection
          }else{
            EmptyFellingPositions <- AvailableFellingPositions # take all the positions
            message("It was not possible to avoid future/reserve trees in the felling of a tree")

          }

          # Sample only one position
          FallenTree <- sample_n(EmptyFellingPositions, 1)$geometry
        }
      }

    }else if(dat$TreeFellingOrientationSuccess == "0"){ # else random felling
      RandomAngle <- as.numeric(sample(c(0:359), size = 1))
      FallenTree <- felling0angle(RandomAngle)
    }
  }

  FellingOuputs <- list(Foot = Foot, # Foot_crs
                        NearestPoints = NearestPoints,
                        TrailPt = TrailPt, # set a crs : st_set_crs(st_sfc(TrailPt), st_crs(maintrailsaccess))
                        Trail = Trail, # set a crs : st_set_crs(st_as_sf(Trail), st_crs(maintrailsaccess))
                        FallenTree = FallenTree # set a crs : st_set_crs(st_sfc(FallenTree), st_crs(maintrailsaccess))
  )


  return(FellingOuputs)

}

#'Tree felling
#'
#'@param inventory Input inventory (see the inputs formats and metadata in the
#'  \code{\link{vignette}}) (data.frame)
#'
#'@param scenario Logging scenario: "RIL1", "RIL2broken", "RIL2", "RIL3",
#'  "RIL3fuel", "RIL3fuelhollow" or "manual"(character) (see the
#'  \code{\link{vignette}})
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
#'@param MainTrail Main trail (sfg)
#'@param ScndTrail Secondary trails (sfg)
#'
#'@param advancedloggingparameters Other parameters of the logging simulator
#'  \code{\link{loggingparameters}} (list)
#'
#'
#'@return Input inventory with new columns:
#'- The tree felling success or fail("TreeFellingOrientationSuccess")
#'- The crowns of all the trees in the inventory  (Polygon)
#'- The fallen trees ("TreePolygon"): a MULTIPOLYGON of the tree oriented
#'   according to the chosen scenario
#'- The dead trees under felled trees (DeathCause = "treefall2nd")
#'
#'@details The felling of the tree creates a tree (including crown) on the
#' ground, with dimensions calculated with specific allometries
#' ('advancedloggingparameters').
#'
#'RIL1/RIL2broken/RIL2:
#' - at 40%: random fall
#' - at 60% ('TreefallSuccessProportion'):
#' *base of the tree towards the nearest trail* (main or 2ndary)
#'
#'RIL3/RIL3 timber + fuel wood:
#' - at 40%: random fall
#' - at 60% ('TreefallSuccessProportion'):
#'   * if RIL3 + fuel & trees < 6 m from the trail and slope <20% (grapple use):
#'     - no particular angle to orientate to the trail,
#'     only to orient the tree *crown* as close as possible to the trail
#'     - priority 1: avoid futures and reserves,
#'     - priority 2: conformation allowing skidding back to the main trail
#'
#'   * otherwise (RIL3, RIL3 + fuel &
#'      trees > 6 m from the trail and/or slope >20%)(cable use):
#'     - 30-45◦ orientation ('MinTreefallOrientation'; 'MaxTreefallOrientation')
#'     - *base* to nearest trail
#'     - priority 1: avoid futures and reserves
#'     - priority 2: conformation allowing skidding back to the main trail
#'
#'Damage:
#'Secondary windfall: *all trees under the felled tree (timber or energy)
#'will be considered dead*.
#'
#'@export
#'
#'@importFrom dplyr group_by do left_join mutate select
#'@importFrom tibble add_column
#'@importFrom sf st_as_sf st_as_text st_geometry st_intersection st_make_valid
#'@importFrom tidyr unnest
#'
#' @examples
#' \dontrun{
#' MainTrail <- sf::st_linestring(matrix(c(286400, 583130,
#'                                         286400, 583250,
#'                                         286655, 583250,
#'                                         286655, 583130,
#'                                         286400, 583130) # the return
#'                                      ,ncol=2, byrow=TRUE))
#'
#' pol1 <- list(matrix(c(286503, 583134,
#'                       286503, 583240,
#'                       286507, 583240,
#'                       286507, 583134,
#'                       286503, 583134) # the return
#'                    ,ncol=2, byrow=TRUE))
#' pol2 <- list(matrix(c(286650, 583134,
#'                       286650, 583240,
#'                       286654, 583240,
#'                       286654, 583134,
#'                       286650, 583134) # the return
#'                    ,ncol=2, byrow=TRUE))
#'
#' PolList = list(pol1,pol2) #list of lists of numeric matrices
#' ScndTrail <- sf::st_multipolygon(PolList)
#'
#' inventory <- addtreedim(inventorycheckformat(Paracou6_2016),
#' volumeparameters = ForestZoneVolumeParametersTable)
#'
#' inventory <- suppressMessages(treeselection(inventory, objective = 20,
#'  scenario ="manual", fuel = "2", diversification = TRUE, specieslax = FALSE,
#'  objectivelax = TRUE, topography = DTMParacou, plotslope = PlotSlope,
#'  speciescriteria = SpeciesCriteria,
#'  advancedloggingparameters = loggingparameters())$inventory)
#'
#' NewInventory <- treefelling(inventory, scenario = "manual", fuel = "0",
#' directionalfelling = "2", MainTrail = MainTrail, ScndTrail = ScndTrail,
#' advancedloggingparameters = loggingparameters())
#'
#' Treefall <- NewInventory %>%
#'    dplyr::filter(DeathCause == "treefall2nd")
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
#'   geom_sf(data = sf::st_as_sf(NewInventory, coords = c("Xutm", "Yutm"))) +
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
#'   geom_sf(data = getgeometry (NewInventory, TreePolygon), # cuted trees
#'   alpha = 0.5, fill = "red") +
#'   geom_sf(data = Selected, aes(colour = "Selected"), show.legend = "point") +
#'   geom_sf(data = ProbedHollow,
#'   aes(colour = "Probed hollow"), show.legend = "point") +
#'
#'   scale_colour_manual(values = c("Non-harvestable" = "grey",
#'   "Visible defect" = "pink", "Harvestable" = "skyblue",
#'   "HarvestableUp" = "blue", "Selected" = "red", "Future" = "orange",
#'   "Reserve" = "purple", "Probed hollow" = "forestgreen")) +
#'   labs(color = "Logging status")
#'
#'
#' sf::st_intersection( # trees under the fallen trees
#'   getgeometry (inventory, TreePolygon),
#'   sf::st_as_sf(inventory, coords = c("Xutm", "Yutm"))
#' ) %>%
#'   ggplot() +
#'   geom_sf()
#'}
#'
treefelling <- function(
  inventory,
  scenario,
  fuel = NULL,
  directionalfelling = NULL,
  MainTrail,
  ScndTrail,
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

  if(!all(unlist(lapply(list(MainTrail, ScndTrail), inherits, "sfg"))))
    stop("The 'MainTrail' and 'ScndTrail' arguments of the 'treefelling' function must be sfg")


  if(!inherits(advancedloggingparameters, "list"))
    stop("The 'advancedloggingparameters' argument of the 'treefelling' function must be a list")

  if(scenario == "manual" &&
     (is.null(fuel) || is.null(directionalfelling)))
    stop("If you choose the 'manual' mode,
         you must fill in the arguments 'fuel' and 'directionalfelling'")

  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <- Commercial <- NULL
  Condition <- DBH <- MinFD <- Taxo <- alpha <- bCoef <- NULL
  DeathCause <- DistCrit <- Family <- CrownHeight <- CrownDiameter <- NULL
  Genus <- Logged <- TreePolygon <- NULL
  LoggingStatus <- MaxFD <- Crowns <- NULL
  ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- SlopeCrit <- Species <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD <- NULL
  VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL
  geometry <- idTree <- . <- NULL


  # Redefinition of the parameters according to the chosen scenario
  scenariosparameters <- scenariosparameters(scenario = scenario, fuel = fuel,
                                             directionalfelling = directionalfelling)

  directionalfelling <- scenariosparameters$directionalfelling
  fuel <- scenariosparameters$fuel

  # Compute treefelling success and fails
  inventory <- directionalfellingsuccessdef(
    inventory,
    fuel = fuel,
    directionalfelling = directionalfelling,
    advancedloggingparameters = advancedloggingparameters)


  # Future/reserve trees to avoid
  inventory <- createcanopy(inventory) # create all inventory crowns in the 'Crowns' column

  FutureReserveCrowns <- inventory %>% # create an object with future/reserve crowns only
    filter(LoggingStatus == "future" | LoggingStatus == "reserve") %>%
    getgeometry(Crowns)


  # Treefelling
  felttrees <- inventory %>%
    filter(!is.na(TreeFellingOrientationSuccess)) %>%
    group_by(idTree) %>% # for each tree
    do(TreePolygon = # inform geometry. # Filling a column from a function whose input is a table
         felling1tree(.,
                      fuel = fuel, directionalfelling = directionalfelling,
                      MainTrail = MainTrail, ScndTrail = ScndTrail,
                      FutureReserveCrowns = FutureReserveCrowns,
                      advancedloggingparameters = advancedloggingparameters)$FallenTree %>%
         st_as_text()) %>% # as text to easy join with a non spacial table
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
  felttrees <- select(felttrees, -idTree) # remove pol infos to keep the information of the points

  DeadTrees <- suppressWarnings(sf::st_intersection(
    st_as_sf(inventory, coords = c("Xutm", "Yutm")),
    sf::st_make_valid(getgeometry(felttrees, TreePolygon)) # "make valid" to avoid self-intersection
  )) %>%
    add_column(DeadTrees = "1") %>%
    select(idTree, DeadTrees)
  sf::st_geometry(DeadTrees) <- NULL # remove TreePolygon column (sf to data.frame)
  DeadTrees <- unique(DeadTrees)

  inventory <- inventory %>%
    left_join(DeadTrees, by = "idTree") %>%
    mutate(DeathCause = ifelse(is.na(DeathCause) & is.na(TreePolygon) & DeadTrees == "1",
                               "treefall2nd", DeathCause)) %>%  # Damage trees
    select(-DeadTrees)


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
#'  \code{\link{vignette}}) (data.frame)
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
#'  the outputs metadata in the \code{\link{vignette}}).
#'@export
#'
#'@importFrom dplyr mutate rowwise
#'
#' @examples
#' inventory <- addtreedim(inventorycheckformat(Paracou6_2016),
#' volumeparameters = ForestZoneVolumeParametersTable)
#'
#' inventory <- treeselection(inventory, objective = 20, scenario ="manual",
#'  fuel = "2", diversification = TRUE, specieslax = FALSE,
#'  objectivelax = TRUE, topography = DTMParacou, plotslope = PlotSlope,
#'  speciescriteria = SpeciesCriteria,
#'  advancedloggingparameters = loggingparameters())$inventory
#'
#' new <- directionalfellingsuccessdef(inventory,fuel = "2", directionalfelling = "2",
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
  Accessible <- Circ <- CircCorr <- CodeAlive <- Commercial <- NULL
  Commercial.genus <- Commercial.species <- Condition <- DBH <- NULL
  DeathCause <- DistCrit <- Family <- CrownHeight <- CrownDiameter <- NULL
  ForestZoneVolumeParametersTable <- Genus <- Logged <- TreePolygon <- NULL
  LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  ParamCrownDiameterAllometry <- NULL
  ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- SlopeCrit <- Species <- Species.genus <- NULL
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
#' data(PlotSlope)
#' data(SpeciesCriteria)
#'
#' inventory <- addtreedim(inventorycheckformat(Paracou6_2016),
#' volumeparameters = ForestZoneVolumeParametersTable)
#'
#' inventory <- suppressMessages(treeselection(inventory, objective = 20, scenario ="manual",
#'  fuel = "2", diversification = TRUE, specieslax = FALSE,
#'  objectivelax = TRUE, topography = DTMParacou, plotslope = PlotSlope,
#'  speciescriteria = SpeciesCriteria,
#'  advancedloggingparameters = loggingparameters())$inventory)
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
#'  RandomAngle <- as.numeric(sample(c(0:359.9), size = 1))
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
  if(!inherits(p, c("POLYGON", "sfc_POLYGON")))
    stop("The 'p' argument of the 'rotatepolygon' function must be a POLYGON or a sfc_POLYGON")

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
  adj <- matrix(rep(center, nrow(p.coords)), ncol=2, byrow=TRUE) # matrix with fixed point coordinates
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
#'
#'@param directionalfelling Directional felling =
#' "0": only to direct the foot of the tree towards the trail
#' "1": to direct the foot of the tree towards the trail + to avoid damage to
#'         future and reserve trees
#' "2": to avoid damage to future and reserve trees + orientation angle
#'       to the trail
#'
#'@param MainTrail (sfg)
#'@param ScndTrail (sfg)
#'
#'@param FutureReserveCrowns Future/reserve trees crown (sf)
#'
#'@param advancedloggingparameters Other parameters of the logging simulator
#'  \code{\link{loggingparameters}} (list)
#'
#'@return A list with: FallenTree: a MULTIPOLYGON of the tree oriented according
#'  to the chosen scenario. Foot: a point for the base of the tree (the rotation
#'  fixed point). NearestPoints: a linestring for the shortest path from the
#'  base of the tree to the nearest trail, Trail: the union of the main and the
#'  secondary trails. TrailPt: the point on the Trail closest to the location of
#'  the tree.
#'
#'@seealso \code{\link{loggingparameters}}
#'
#'@export
#'
#'@importFrom dplyr mutate
#'@importFrom sf st_point st_multipolygon st_as_sf st_nearest_points st_cast
#'  st_intersects
#'@importFrom nngeo st_ellipse
#'@importFrom matlib angle
#'
#' @examples
#' MainTrail <- sf::st_linestring(matrix(c(286400, 582945,
#'                                         286400, 583250,
#'                                         286700, 583250,
#'                                         286700, 582945,
#'                                         286400, 582945) # the return
#'                                      ,ncol=2, byrow=TRUE))
#'
#' pol1 <- list(matrix(c(286503, 583134,
#'                       286503, 583240,
#'                       286507, 583240,
#'                       286507, 583134,
#'                       286503, 583134) # the return
#'                    ,ncol=2, byrow=TRUE))
#' pol2 <- list(matrix(c(286650, 583134,
#'                       286650, 583240,
#'                       286654, 583240,
#'                       286654, 583134,
#'                       286650, 583134) # the return
#'                    ,ncol=2, byrow=TRUE))
#'
#' PolList = list(pol1,pol2) #list of lists of numeric matrices
#' ScndTrail <- sf::st_multipolygon(PolList)
#'
#' inventory <- addtreedim(inventorycheckformat(Paracou6_2016),
#' volumeparameters = ForestZoneVolumeParametersTable)
#'
#' inventory <- suppressMessages(treeselection(inventory, objective = 20, scenario ="manual",
#'  fuel = "2", diversification = TRUE, specieslax = FALSE,
#'  objectivelax = TRUE, topography = DTMParacou, plotslope = PlotSlope,
#'  speciescriteria = SpeciesCriteria,
#'  advancedloggingparameters = loggingparameters())$inventory)
#'
#' FutureReserveCrowns <- inventory %>% # create an object with future/reserve crowns only
#'  dplyr::filter(LoggingStatus == "future" | LoggingStatus == "reserve") %>%
#'  createcanopy() %>% # create all inventory crowns in the 'Crowns' column
#'  getgeometry(Crowns)
#'
#' inventory <- inventory %>%
#'      dplyr::filter(Selected == "1") %>%
#'      dplyr::select(idTree,DBH,TrunkHeight,TreeHeight,CrownHeight,
#'      CrownDiameter,Selected, Xutm, Yutm)
#'
#' dat <- inventory[1,] %>% # just 1 row (1 tree)
#' # force the orientation success for the exemple
#' tibble::add_column(TreeFellingOrientationSuccess = "1")
#'
#' rslt <- felling1tree(dat,
#'  fuel = "0", directionalfelling = "2",
#'  MainTrail = MainTrail, ScndTrail = ScndTrail,
#'  FutureReserveCrowns = FutureReserveCrowns,
#'  advancedloggingparameters = loggingparameters())
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = rslt$Foot) +
#'   geom_sf(data = rslt$Trail) +
#'   geom_sf(data = rslt$NearestPoints) +
#'   geom_sf(data = rslt$TrailPt) +
#'   geom_sf(data = rslt$FallenTree) +
#'   geom_sf(data = FutureReserveCrowns)
felling1tree <- function(
  dat,
  fuel,
  directionalfelling,
  MainTrail,
  ScndTrail,
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

  if(!all(unlist(lapply(list(MainTrail, ScndTrail), inherits, "sfg"))))
    stop("The 'MainTrail' and 'ScndTrail' arguments of the 'felling1tree' function must be sfg")

  if(!inherits(advancedloggingparameters, "list"))
    stop("The 'advancedloggingparameters' argument of the 'felling1tree' function must be a list")

  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <- Commercial <- NULL
  Commercial.genus <- Commercial.species <- Condition <- DBH <- NULL
  DeathCause <- DistCrit <- Family <- CrownHeight <- CrownDiameter <- NULL
  Genus <- Logged <- TreePolygon <- NULL
  LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- SlopeCrit <- Species <- Species.genus <- NULL
  Taxo <- Taxo.family <- Taxo.genus <- Taxo.species <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD <- UpMinFD.genus <- NULL
  UpMinFD.species <- VernName.genus <- VernName.genus.genus <- NULL
  VernName.species <- VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL
  alpha <- alpha.family <- alpha.genus <- alpha.species <- bCoef <- NULL
  beta.family <- beta.genus <- beta.species <- geometry <- idTree <- NULL

  # Function
  Trail <- st_union(MainTrail, ScndTrail) # Our trail will be MainTrail or ScndTrail

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

  # Find the point (TrailPt) on the Trail closest to the location of the tree (Foot)
  Foot <- st_point(c(dat$Xutm,dat$Yutm)) # tree foot point

  NearestPoints <- st_nearest_points(Foot, Trail) # from the Foot of the tree to the Trail (linestring)

  NearestPoint <- st_cast(NearestPoints, "POINT") # to have start (Foot) and end (TrailPt) points
  TrailPt <- NearestPoint[[2]] # the point (TrailPt) on the Trail closest to the location of the tree (Foot)

  # Compute the angle between the tree default position and the shortest way from the foot to the trail
  theta <- as.numeric(matlib::angle(c(Foot[1] - Foot[1], dat$TreeHeight),
                                    c(TrailPt[1] - Foot[1], TrailPt[2] - Foot[2]), degree = TRUE))

  ## when the tree is on the trail (no angle)
  if(is.na(theta)) theta <- 0


  # Scenarios
  ## For a totally random direction felling
  # if(totally random)){
  #   RandomAngle <- as.numeric(sample(c(0:359.9), size = 1))
  #   FallenTree <- st_difference(st_union( # Crown and Trunk together
  #     rotatepolygon(Trunk, angle = RandomAngle, fixed = Foot), # turned trunk
  #     rotatepolygon(Crown, angle = RandomAngle, fixed = Foot) # turned crown
  #   ))
  # }

  ## No other orientation than to direct the foot of the tree towards the trail (0-180°)
  if (directionalfelling == "0" && (fuel !="1" || fuel !="2")) {
    if(dat$TreeFellingOrientationSuccess == "1"){

      TreefallOrientation <- as.numeric(sample(c(0.1: 179.9), size = 1)) # no particular angle to orientate to the trail, only to orientate the tree foot towards the trail
      OppAng <- 180-(90 + TreefallOrientation) # the angle between the closest position to the trail (90°) and the desired position (desired angle to the trail)

      ## Right-hand trail
      if(TrailPt[1] >= Foot[1]){ # x trail > x foot

        Aangle <- round(as.numeric(180 + OppAng + theta), digits = 0) # Foot oriented

        ## Left-hand trail
      }else if(TrailPt[1] < Foot[1]){ # x trail < x foot

        Aangle <- round(as.numeric(360 - OppAng + theta), digits = 0) # Foot oriented
      }

      # Calculate the crown and trunk position
      FallenTree <- st_difference(st_union( # A configuration (90° to the trail (shortest))
        rotatepolygon(Trunk, angle = Aangle, fixed = Foot), # turned trunk
        rotatepolygon(Crown, angle = Aangle, fixed = Foot) # turned crown
      ))

    }else{ # else random felling
      RandomAngle <- as.numeric(sample(c(0:359.9), size = 1))

      FallenTree <- st_difference(st_union(
        rotatepolygon(Trunk, angle = RandomAngle, fixed = Foot), # turned trunk
        rotatepolygon(Crown, angle = RandomAngle, fixed = Foot) # turned crown
      ))
    }

  }

  ## To direct to !avoid damage to future and reserve trees!, and foot directed to the trail
  if (directionalfelling == "1"&& (fuel !="1" || fuel !="2")) {

    RandomAngle <- as.numeric(sample(c(0:359.9), size = 4)) # I leave 4 chances to avoid fut/res trees

    if(dat$TreeFellingOrientationSuccess == "1"){

      for(i in 1:3){
        FallenTree <- st_difference(st_union(
          rotatepolygon(Trunk, angle = RandomAngle[i], fixed = Foot), # turned trunk
          rotatepolygon(Crown, angle = RandomAngle[i], fixed = Foot) # turned crown
        ))


        FRintersect <- sf::st_intersects(FallenTree, FutureReserveCrowns)
        if(lengths(FRintersect) > 0) { # if there is an intersection
          # New chance
          FallenTree <- st_difference(st_union(
            rotatepolygon(Trunk, angle = RandomAngle[i+1], fixed = Foot), # turned trunk
            rotatepolygon(Crown, angle = RandomAngle[i+1], fixed = Foot) # turned crown
          ))

        }
      }

    }else{ # else random felling
      FallenTree <- st_difference(st_union(
        rotatepolygon(Trunk, angle = RandomAngle[1], fixed = Foot), # turned trunk
        rotatepolygon(Crown, angle = RandomAngle[1], fixed = Foot) # turned crown
      ))
    }

  }


  ## Scenarios with trail orientation:
  # Compute the third angle of the right-angled triangle (see vignette figure)
  # TreefallOrientation is between the minimum (30°) and the maximum (45°) angle

  # Orientation for cable
  TreefallOrientation <- as.numeric(sample(c(advancedloggingparameters$MinTreefallOrientation:
                                               advancedloggingparameters$MaxTreefallOrientation), size = 1))
  # Orientation for grapple
  CrownTreefallOrientation <- as.numeric(sample(c(0.1: 179.9), size = 1)) # no particular angle to orientate to the trail, only to orientate the tree crown towards the trail

  OppAng <- 180-(90 + TreefallOrientation) # the angle between the closest position to the trail (90°) and the desired position (desired angle to the trail)
  CrownOppAng <- 180-(90 + CrownTreefallOrientation) # for grapple case

  # Angle depending on whether the trail is to the right/left of the default position of the tree
  ## Right-hand trail
  if(TrailPt[1] >= Foot[1]){ # x trail > x foot
    # Foot oriented
    Aangle <- round(as.numeric(180 + OppAng + theta), digits = 0)
    Bangle <- round(as.numeric(180 - OppAng + theta), digits = 0)
    # Crown oriented
    CrownAangle <- as.numeric(theta) # angle between tree default position and the shortest way from the foot to the trail (to be at 90° to the trail)
    CrownBangle <- as.numeric(theta + CrownOppAng) # ]0;180[ to the trail


    ## Left-hand trail
  }else if(TrailPt[1] < Foot[1]){ # x trail < x foot
    # Foot oriented
    Aangle <- round(as.numeric(360 - OppAng + theta), digits = 0)
    Bangle <- round(as.numeric(theta + OppAng), digits = 0)
    # Crown oriented
    CrownAangle <- as.numeric(180 + theta) # angle between tree default position and the shortest way from the foot to the trail (to be at 90° to the trail)
    CrownBangle <- as.numeric(180 + CrownOppAng + theta) # ]0;180[ to the trail

  }


  ### To direct to avoid damage to future and reserve trees + trail orientation (angle).
  # Without fuel wood exploitation -> Winching: Foot before.
  if(directionalfelling == "2" && (fuel !="1" || fuel !="2")){
    if(dat$TreeFellingOrientationSuccess == "1"){

      # Calculate the two possible crown configurations
      ACrown <- rotatepolygon(Crown, angle = Aangle, fixed = Foot) # turned crown
      BCrown <- rotatepolygon(Crown, angle = Bangle, fixed = Foot) # turned crown

      # Test the best to pull the tree back to the main trail (farthest crown from the main trail)
      ADist <- st_distance(ACrown, MainTrail)[1,1] #matrix to value
      BDist <- st_distance(BCrown, MainTrail)[1,1]

      if(max(ADist, BDist) == ADist){

        FallenTree <- AFallenTree <- st_difference(st_union( # A configuration
          rotatepolygon(Trunk, angle = Aangle, fixed = Foot), # turned trunk
          ACrown # turned crown
        ))
        BFallenTree <- NULL

      }else{

        FallenTree <-  BFallenTree <- st_difference(st_union( # B configuration
          rotatepolygon(Trunk, angle = Bangle, fixed = Foot), # turned trunk
          BCrown # turned crown
        ))
        AFallenTree <- st_sfc(st_point(c(0,0))) # "null" sfc object to compare after

      }

      # Check intersection with future/reserve trees
      FRintersect <- sf::st_intersects(FallenTree, FutureReserveCrowns)

      if(lengths(FRintersect) > 0) { # if there is an intersection
        if(FallenTree == AFallenTree){ # if it was A configuration

          FallenTree <- BFallenTree <- st_difference(st_union( # B configuration
            rotatepolygon(Trunk, angle = Bangle, fixed = Foot), # turned trunk
            BCrown # turned crown
          ))

          # check intersection for this configuration
          FRintersect <- sf::st_intersects(BFallenTree, FutureReserveCrowns)
          if(lengths(FRintersect) > 0) { # if there is an intersection. if not FallenTree stay BFallenTree
            FallenTree <- AFallenTree # we prefer 1st configuration (the best for winching)
          }

        }else if(FallenTree == BFallenTree){ # if it was B configuration

          FallenTree <- AFallenTree <- st_difference(st_union( # A configuration
            rotatepolygon(Trunk, angle = Aangle, fixed = Foot), # turned trunk
            ACrown # turned crown
          ))

          # check intersection for this configuration
          FRintersect <- sf::st_intersects(AFallenTree, FutureReserveCrowns)
          if(lengths(FRintersect) > 0) { # if there is an intersection. if not FallenTree stay AFallenTree
            FallenTree <- BFallenTree # we prefer 1st configuration (the best for winching)
          }
        }
      }


    }else{ # else random felling
      RandomAngle <- as.numeric(sample(c(0:359.9), size = 1))

      FallenTree <- st_difference(st_union(
        rotatepolygon(Trunk, angle = RandomAngle, fixed = Foot), # turned trunk
        rotatepolygon(Crown, angle = RandomAngle, fixed = Foot) # turned crown
      ))
    }
  }

  # Fuel wood exploitation in the crowns + to avoid damage to future and reserve trees + trail orientation
  if(fuel =="1" || fuel =="2"){

    TrailDist <- st_distance(Foot, TrailPt) # distance between the tree foot and the Trail closest point

    # ADD SLOPE CRITERIA !!!

    if(dat$TreeFellingOrientationSuccess == "1"){

      if(TrailDist <= advancedloggingparameters$GrappleLength){ # <= 6m (= grapple length) -> winching by grapple -> crown to trail

        # Calculate the two possible crown configurations
        ACrown <- rotatepolygon(Crown, angle = CrownAangle, fixed = Foot) # turned crown
        BCrown <- rotatepolygon(Crown, angle = CrownBangle, fixed = Foot) # turned crown

        FallenTree <- AFallenTree <- st_difference(st_union( # A configuration (90° to the trail (shortest))
          rotatepolygon(Trunk, angle = CrownAangle, fixed = Foot), # turned trunk
          ACrown # turned crown
        ))

        # No pull towards Maintrail for the grapple

        # Check intersection with future/reserve trees
        FRintersect <- sf::st_intersects(FallenTree, FutureReserveCrowns)

        if(lengths(FRintersect) > 0) { # if there is an intersection

          FallenTree <- BFallenTree <- st_difference(st_union( # B configuration (]0;180[)
            rotatepolygon(Trunk, angle = CrownBangle, fixed = Foot), # turned trunk
            BCrown # turned crown
          ))

          # check intersection for this new configuration
          FRintersect <- sf::st_intersects(BFallenTree, FutureReserveCrowns)
          if(lengths(FRintersect) > 0) { # if there is an intersection. If not FallenTree stay BFallenTree
            FallenTree <- AFallenTree # we prefer 1st configuration (the best for winching)
          }
        }


      } else { # > 6m -> winching by cable -> foot to trail

        # Calculate the two possible crown configurations
        ACrown <- rotatepolygon(Crown, angle = Aangle, fixed = Foot) # turned crown
        BCrown <- rotatepolygon(Crown, angle = Bangle, fixed = Foot) # turned crown

        # Test the best to pull the tree back to the main trail (farthest crown from the main trail)
        ADist <- st_distance(ACrown, MainTrail)[1,1] #matrix to value
        BDist <- st_distance(BCrown, MainTrail)[1,1]

        if(max(ADist, BDist) == ADist){

          FallenTree <- AFallenTree <- st_difference(st_union( # A configuration
            rotatepolygon(Trunk, angle = Aangle, fixed = Foot), # turned trunk
            ACrown # turned crown
          ))
          BFallenTree <- st_sfc(st_point(c(0,0))) # "null" sfc object to compare after

        }else{

          FallenTree <-  BFallenTree <- st_difference(st_union( # B configuration
            rotatepolygon(Trunk, angle = Bangle, fixed = Foot), # turned trunk
            BCrown # turned crown
          ))
          AFallenTree <- st_sfc(st_point(c(0,0))) # "null" sfc object to compare after

        }

        # Check intersection with future/reserve trees
        FRintersect <- sf::st_intersects(FallenTree, FutureReserveCrowns)

        if(lengths(FRintersect) > 0) { # if there is an intersection
          if(FallenTree == AFallenTree){ # if it was A configuration

            FallenTree <- BFallenTree <- st_difference(st_union( # B configuration
              rotatepolygon(Trunk, angle = Bangle, fixed = Foot), # turned trunk
              BCrown # turned crown
            ))

            # check intersection for this configuration
            FRintersect <- sf::st_intersects(BFallenTree, FutureReserveCrowns)
            if(lengths(FRintersect) > 0) { # if there is an intersection. if not FallenTree stay BFallenTree
              FallenTree <- AFallenTree # we prefer 1st configuration (the best for winching)
            }

          }else if(FallenTree == BFallenTree){ # if it was B configuration

            FallenTree <- AFallenTree <- st_difference(st_union( # A configuration
              rotatepolygon(Trunk, angle = Aangle, fixed = Foot), # turned trunk
              ACrown # turned crown
            ))

            # check intersection for this configuration
            FRintersect <- sf::st_intersects(AFallenTree, FutureReserveCrowns)
            if(lengths(FRintersect) > 0) { # if there is an intersection. if not FallenTree stay AFallenTree
              FallenTree <- BFallenTree # we prefer 1st configuration (the best for winching)
            }
          }
        }
      }
    }else{ # else random felling
      RandomAngle <- as.numeric(sample(c(0:359.9), size = 1))
      FallenTree <- st_difference(st_union(
        rotatepolygon(Trunk, angle = RandomAngle, fixed = Foot), # turned trunk
        rotatepolygon(Crown, angle = RandomAngle, fixed = Foot) # turned crown
      ))
    }

  }

  FellingOuputs <- list(Foot = Foot,
                        NearestPoints = NearestPoints,
                        TrailPt = TrailPt,
                        Trail = Trail,
                        FallenTree = FallenTree)


  return(FellingOuputs)

}

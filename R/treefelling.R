#' treefelling
#'
#' @param inventory (data.frame)
#' @param scenario "RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel",
#'   "RIL3fuelhollow" or "manual"(character)
#' @param fuel no  exploitation = "0", damage exploitation in fuelwood = "1",
#'   exploitation of hollow trees and damage in fuelwood = "2"
#' @param directionalfelling directional felling = "0" (absent), "1" (only to
#'   avoid damage to future and reserve trees), "2" (avoid damage to future and
#'   reserve trees + track orientation)
#' @param advancedloggingparameters (list)
#'
#' @param MainTrail (sfg)
#' @param ScndTrail (sfg)
#'
#' @return "Shadow" polygons +  "TreefallSuccess", "TreefallFailure",
#'   "DamageTreesPoints", "DeadTreesPoints" vectors
#' @export
#'
#' @importFrom dplyr group_by do left_join
#' @importFrom sf st_as_text
#' @importFrom tidyr unnest
#'
#' @examples
#'
#' \dontrun{
#'
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
#' inventory <- addtreedim(inventorycheckformat(Paracou6_2016))
#' inventory <- suppressMessages(treeselection(inventory, objective = 30, scenario ="manual",
#'  fuel = "2", diversification = TRUE, specieslax = FALSE,
#'  objectivelax = FALSE, DEM = DemParacou, plotslope = PlotSlope,
#'  speciescriteria = SpeciesCriteria,
#'  advancedloggingparameters = loggingparameters())$inventory)
#'
#' inventory <- treefelling(inventory, scenario = "manual", fuel = "2",
#' directionalfelling = "2", MainTrail = MainTrail, ScndTrail = ScndTrail,
#' advancedloggingparameters = loggingparameters())
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = st_as_sf(inventory, coords = c("Xutm", "Yutm"))) +
#'   geom_sf(data = getgeometry (inventory, TreePolygon), fill = "red") # trees polygons
#'
#' st_intersection( # trees under the fallen trees
#'   getgeometry (inventory, TreePolygon),
#'   st_as_sf(inventory, coords = c("Xutm", "Yutm"))
#' ) %>%
#'   ggplot() +
#'   geom_sf()
#'  }
#'
treefelling <- function(
  inventory,
  scenario = "manual",
  fuel,
  directionalfelling,
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

  if(!any(unlist(lapply(list(MainTrail, ScndTrail), inherits, "sfg"))))
    stop("The 'MainTrail' and 'ScndTrail' arguments of the 'treefelling' function must be sfg")


  if(!inherits(advancedloggingparameters, "list"))
    stop("The 'advancedloggingparameters' argument of the 'treefelling' function must be a list")

  if(scenario == "manual" &&
     (is.null(fuel) || is.null(directionalfelling)))
    stop("If you choose the 'manual' mode,
         you must fill in the arguments 'fuel' and 'directionalfelling'")

  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <- Commercial <- NULL
  Commercial.genus <- Commercial.species <- Condition <- DBH <- NULL
  DeathCause <- DistCrit <- Family <- CrownHeight <- CrownDiameter <- NULL
  ForestZoneVolumeParametersTable <- Genus <- Logged <- TreePolygon <- NULL
  LoggedVolume <- LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  NoHollowLoggedVolume <- ParamCrownDiameterAllometry <- PlotSlope <- NULL
  PlotTopo <- ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- Slope <- SlopeCrit <- Species <- Species.genus <- NULL
  SpeciesCriteria <- Taxo <- Taxo.family <- Taxo.genus <- Taxo.species <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD <- UpMinFD.genus <- NULL
  UpMinFD.species <- VernName.genus <- VernName.genus.genus <- NULL
  VernName.species <- VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL
  alpha <- alpha.family <- alpha.genus <- alpha.species <- bCoef <- NULL
  beta.family <- beta.genus <- beta.species <- geometry <- idTree <- . <- NULL


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
    advancedloggingparameters = loggingparameters())

  # Treefelling
  felttrees <- inventory %>%
    filter(!is.na(TreeFellingOrientationSuccess)) %>%
    group_by(idTree) %>% # for each tree
    do(TreePolygon = # inform geometry. # Filling a column from a function whose input is a table
         st_tree(.,
                 fuel = fuel, directionalfelling = directionalfelling,
                 MainTrail = MainTrail, ScndTrail = ScndTrail,
                 advancedloggingparameters = loggingparameters())$A %>%
         st_as_text()) %>% # as text to easy join with a non spacial table
    tidyr::unnest(TreePolygon) # here to pass from list to character

  inventory <- left_join(inventory, felttrees, by = "idTree") # join spatial filtered inventory and non spatial complete inventory

  return(inventory)



}

#' directionalfellingsuccessdef
#'
#' @description Defines whether the directed fall of the tree is successful or
#'   not by drawing in a Bernoulli distribution where the probability of success
#'   is by default 60%, and can be changed with the
#'   \code{advancedloggingparameters} argument.
#'
#' @param inventory your inventory (see the inputs formats and metadata in the
#'   \code{\link{vignette}}) (data.frame)
#'
#' @param fuel Fuel wood exploitation: no exploitation = "0", damage
#'   exploitation in fuelwood = "1", exploitation of hollow trees and damage in
#'   fuelwood = "2"
#'
#' @param directionalfelling Directional felling = "0" (absent), "1" (only to
#'   avoid damage to future and reserve trees), "2" (avoid damage to future and
#'   reserve trees + track orientation)
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @return Your inventory with: "TreeFellingOrientationSuccess" new column (see
#'   the outputs metadata in the \code{\link{vignette}}).
#' @export
#'
#' @importFrom dplyr mutate
#'
#' @examples
#' inventory <- addtreedim(inventorycheckformat(Paracou6_2016))
#' inventory <- treeselection(inventory, objective = 20, scenario ="manual",
#'  fuel = "2", diversification = TRUE, specieslax = FALSE,
#'  objectivelax = FALSE, DEM = DemParacou, plotslope = PlotSlope,
#'  speciescriteria = SpeciesCriteria,
#'  advancedloggingparameters = loggingparameters())$inventory
#'
#' directionalfellingsuccessdef(inventory,fuel = "2",directionalfelling = "2",
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
  LoggedVolume <- LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  NoHollowLoggedVolume <- ParamCrownDiameterAllometry <- PlotSlope <- NULL
  PlotTopo <- ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- Slope <- SlopeCrit <- Species <- Species.genus <- NULL
  SpeciesCriteria <- Taxo <- Taxo.family <- Taxo.genus <- Taxo.species <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD <- UpMinFD.genus <- NULL
  UpMinFD.species <- VernName.genus <- VernName.genus.genus <- NULL
  VernName.species <- VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL
  alpha <- alpha.family <- alpha.genus <- alpha.species <- bCoef <- NULL
  beta.family <- beta.genus <- beta.species <- geometry <- idTree <- NULL


  if (fuel == "0" && directionalfelling == "0"){
    inventory <- inventory %>%
      mutate(TreeFellingOrientationSuccess = 0)
  }

  if (fuel == "0" && directionalfelling != "0"){
    inventory <- inventory %>%
      mutate(TreeFellingOrientationSuccess =
               ifelse(Selected == "1",
                      sample(c(1,0), size = 1, replace = F,
                             prob = c(advancedloggingparameters$TreefallSuccessProportion,
                                      1-advancedloggingparameters$TreefallSuccessProportion)), NA)) # Accessible = linked by 2ndtrails
    # Accessible <- Selected !!!!!!!!!!!! pas oublier pour if (fuel == "0" && directionalfelling != "0")

  }

  if (fuel =="1") {

    inventory <- inventory %>%
      mutate(TreeFellingOrientationSuccess =
               ifelse(Selected == "1",  # Selected = not yet linked by 2ndtrails, because 2ndtrails came after
                      sample(c(1,0), size = 1, replace = F,
                             prob = c(advancedloggingparameters$TreefallSuccessProportion,
                                      1-advancedloggingparameters$TreefallSuccessProportion)), NA))

  }

  # Abattre les creux :
  if (fuel =="2") {

    inventory <- inventory %>%
      mutate(TreeFellingOrientationSuccess =
               ifelse(Selected == "1"| ProbedHollow == "1", # Selected = not yet linked by 2ndtrails, because 2ndtrails came after
                      sample(c(1,0), size = 1, replace = F,
                             prob = c(advancedloggingparameters$TreefallSuccessProportion,
                                      1-advancedloggingparameters$TreefallSuccessProportion)), NA))

  }

  inventory$TreeFellingOrientationSuccess <- as.factor(inventory$TreeFellingOrientationSuccess)


  return(inventory)

}


#' rotatepolygon
#'
#' @description Orient your polygon to a given angle and fixed point.
#'
#' @param p Polygon (POLYGON or sfc_POLYGON)
#' @param angle Angle in degrees in the clockwise direction (numeric)
#' @param fixed Fixed point on which the polygon will rotate (POINT)
#'
#' @return Your polygon (sfc_POLYGON) oriented at the given angle
#' @export
#' @importFrom nngeo st_ellipse
#' @importFrom sf st_polygon
#' @importFrom sf st_coordinates
#' @importFrom sf st_sfc
#' @importFrom sf st_polygon
#' @importFrom sf st_difference
#' @importFrom sf st_union
#'
#' @examples
#' data(Paracou6_2016)
#' data(DemParacou)
#' data(PlotSlope)
#' data(SpeciesCriteria)
#'
#' inventory <- addtreedim(inventorycheckformat(Paracou6_2016))
#' inventory <- suppressMessages(treeselection(inventory, objective = 20, scenario ="manual",
#'  fuel = "2", diversification = TRUE, specieslax = FALSE,
#'  objectivelax = FALSE, DEM = DemParacou, plotslope = PlotSlope,
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
#' st_as_sf(coords = c("xCrown", "yCrown")) # ellipse centroid coordinates
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

#' st_tree
#'
#' @description Simulates the tree (multipolygon) falling towards the trail or
#'   not, at a given angle. If it has been decided to exploit fuelwood, the tree
#'   crowns will be directed towards the trail if they can be accessed with a
#'   grapple (see the \code{GrappleLength} argument of the
#'   \code{\link{loggingparameters}} function). In other cases, the fall will be
#'   made from the base of the tree towards the trail. The orientation of the
#'   fall succeeds or fails according to a Bernoulli law where the probability
#'   of success is by default 60%, and can be changed with the
#'   \code{advancedloggingparameters} argument.
#'
#' @param dat 1 row data.frame with columns: Xutm, Yutm, CrownDiameter,
#'   CrownHeight, DBH, TrunkHeight, TreeHeight, TreeFellingOrientationSuccess
#'
#' @param MainTrail (sfg)
#' @param ScndTrail (sfg)
#'
#' @param fuel Fuel wood exploitation: no exploitation = "0", damage
#'   exploitation in fuelwood = "1", exploitation of hollow trees and damage in
#'   fuelwood = "2"
#'
#' @param directionalfelling Directional felling = "0" (absent), "1" (only to
#'   avoid damage to future and reserve trees), "2" (avoid damage to future and
#'   reserve trees + track orientation)
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @return
#'
#' @seealso \code{\link{loggingparameters}}
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom sf st_point
#' @importFrom sf st_multipolygon
#' @importFrom sf st_as_sf
#' @importFrom sf st_nearest_points
#' @importFrom sf st_cast
#' @importFrom nngeo st_ellipse
#' @importFrom matlib angle
#'
#' @examples
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
#' inventory <- addtreedim(inventorycheckformat(Paracou6_2016))
#' inventory <- treeselection(inventory, objective = 20, scenario ="manual",
#'  fuel = "2", diversification = TRUE, specieslax = FALSE,
#'  objectivelax = FALSE, DEM = DemParacou, plotslope = PlotSlope,
#'  speciescriteria = SpeciesCriteria,
#'  advancedloggingparameters = loggingparameters())$inventory
#'
#' inventory <- directionalfellingsuccessdef(inventory,fuel = "0",directionalfelling = "2",
#' advancedloggingparameters = loggingparameters())
#'
#' inventory <- inventory %>%
#'      dplyr::filter(Selected == "1") %>%
#'      dplyr::select(idTree,DBH,TrunkHeight,TreeHeight,CrownHeight,
#'      CrownDiameter,Selected, Xutm, Yutm, TreeFellingOrientationSuccess)
#' dat <- inventory[1,]
#'
#' rslt <- st_tree(dat,
#'  fuel = "0", directionalfelling = "2",
#'  MainTrail = MainTrail, ScndTrail = ScndTrail,
#'  advancedloggingparameters = loggingparameters())
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = rslt$Foot) +
#'   geom_sf(data = rslt$Trail) +
#'   geom_sf(data = rslt$NearestPoints) +
#'   geom_sf(data = rslt$TrailPt) +
#'   geom_sf(data = rslt$A)
st_tree <- function(
  dat,
  fuel,
  directionalfelling,
  MainTrail = MainTrail,
  ScndTrail = ScndTrail,
  advancedloggingparameters = loggingparameters()
){

  # Arguments check

  if(!inherits(dat, "data.frame"))
    stop("The 'dat' argument of the 'st_tree' function must be data.frame")

  if(nrow(dat)!=1)
    stop("the data.frame given in the 'dat' argument
         of the 'st_tree' function must contain only 1 row")

  if (!any(fuel == "0" || fuel == "1"|| fuel == "2"|| is.null(fuel)))
    stop("The 'fuel' argument of the 'st_tree' function must be '0', '1', '2' or NULL")

  if (!any(directionalfelling == "0" || directionalfelling == "1"
           || directionalfelling == "2" || is.null(directionalfelling)))
    stop("The 'directionalfelling' argument of the 'st_tree' function must be '0', '1', '2' or NULL")

  if(!any(unlist(lapply(list(MainTrail, ScndTrail), inherits, "sfg"))))
    stop("The 'MainTrail' and 'ScndTrail' arguments of the 'st_tree' function must be sfg")

  if(!inherits(advancedloggingparameters, "list"))
    stop("The 'advancedloggingparameters' argument of the 'st_tree' function must be a list")

  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <- Commercial <- NULL
  Commercial.genus <- Commercial.species <- Condition <- DBH <- NULL
  DeathCause <- DistCrit <- Family <- CrownHeight <- CrownDiameter <- NULL
  ForestZoneVolumeParametersTable <- Genus <- Logged <- TreePolygon <- NULL
  LoggedVolume <- LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  NoHollowLoggedVolume <- ParamCrownDiameterAllometry <- PlotSlope <- NULL
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


  # Scenarios
  # For a random direction felling
  if(directionalfelling == "0" && (fuel !="1" || fuel !="2")){
    RandomAngle <- as.numeric(sample(c(0:359.9), size = 1))
    A <- st_difference(st_union( # Crown and Trunk together
      rotatepolygon(Trunk, angle = RandomAngle, fixed = Foot), # turned trunk
      rotatepolygon(Crown, angle = RandomAngle, fixed = Foot) # turned crown
    ))
  }


  # # To direct only to avoid damage to future and reserve trees. Winching: Foot before.
  # if (directionalfelling == "1"&& (fuel !="1" || fuel !="2")) {
  #   if(dat$TreeFellingOrientationSuccess == "1"){
  #
  # }else{ # else random felling
  # RandomAngle <- as.numeric(sample(c(0:359.9), size = 1))
  #   A <- st_difference(st_union(
  #   rotatepolygon(Trunk, angle = RandomAngle, fixed = Foot), # turned trunk
  #   rotatepolygon(Crown, angle = RandomAngle, fixed = Foot) # turned crown
  #   ))
  # }
  #
  # }


  # Scenarios with track orientation:
  # Compute the last angle of the right-angled triangle (see vignette figure)
  # TreefallOrientation is between the mimimun (30°) and the maximum (45°) angle
  TreefallOrientation <- as.numeric(sample(c(advancedloggingparameters$MinTreefallOrientation:
                                             advancedloggingparameters$MaxTreefallOrientation), size = 1))
  OppAng <- 180-(90 + TreefallOrientation)


  # To direct !!to avoid damage to future and reserve trees!! + track orientation. Winching: Foot before.
  if(directionalfelling == "2"&& (fuel !="1" || fuel !="2")){
    if(dat$TreeFellingOrientationSuccess == "1"){
      A <- st_difference(st_union(
        rotatepolygon(Trunk, angle = 180 + OppAng + theta, fixed = Foot), # turned trunk
        rotatepolygon(Crown, angle = 180 + OppAng + theta, fixed = Foot) # turned crown
      ))
      B <- st_difference(st_union(
        rotatepolygon(Trunk, angle = 180 - OppAng + theta, fixed = Foot), # turned trunk
        rotatepolygon(Crown, angle = 180 - OppAng + theta, fixed = Foot) # turned crown
      ))
    }else{ # else random felling
      RandomAngle <- as.numeric(sample(c(0:359.9), size = 1))

      A <- st_difference(st_union(
        rotatepolygon(Trunk, angle = RandomAngle, fixed = Foot), # turned trunk
        rotatepolygon(Crown, angle = RandomAngle, fixed = Foot) # turned crown
      ))
    }

  }

  # Fuelwood exploitation in the crowns !!to avoid damage to future and reserve trees!!
  if(fuel =="1" || fuel =="2"){

    TrailDist <- st_distance(Foot, TrailPt) # distance between the tree foot and the Trail closest point

    # ADD SLOPE CRITERIA !!!

    if(dat$TreeFellingOrientationSuccess == "1"){

      if(TrailDist <= advancedloggingparameters$GrappleLength){ # <= 6m (= grapple length) -> winching by grapple -> crown to trail
        A <- st_difference(st_union(
          rotatepolygon(Trunk, angle = theta + OppAng , fixed = Foot), # turned trunk
          rotatepolygon(Crown, angle = theta + OppAng , fixed = Foot) # turned crown
        ))
        B <- st_difference(st_union(
          rotatepolygon(Trunk, angle = 360 - OppAng + theta, fixed = Foot), # turned trunk
          rotatepolygon(Crown, angle = 360 - OppAng + theta, fixed = Foot) # turned crown
        ))
      } else { # > 6m -> winching by cable -> foot to trail
        A <- st_difference(st_union(
          rotatepolygon(Trunk, angle = 180 + OppAng + theta, fixed = Foot), # turned trunk
          rotatepolygon(Crown, angle = 180 + OppAng + theta, fixed = Foot) # turned crown
        ))
        B <- st_difference(st_union(
          rotatepolygon(Trunk, angle = 180 - OppAng + theta, fixed = Foot), # turned trunk
          rotatepolygon(Crown, angle = 180 - OppAng + theta, fixed = Foot) # turned crown
        ))
      }
    }else{ # else random felling
      RandomAngle <- as.numeric(sample(c(0:359.9), size = 1))
      A <- st_difference(st_union(
        rotatepolygon(Trunk, angle = RandomAngle, fixed = Foot), # turned trunk
        rotatepolygon(Crown, angle = RandomAngle, fixed = Foot) # turned crown
      ))
    }

  }

  FellingOuputs <- list(Foot = Foot,
                        NearestPoints = NearestPoints,
                        TrailPt = TrailPt,
                        Trail = Trail,
                        A = A)


  return(FellingOuputs)

}

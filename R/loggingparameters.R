#'Advanced parameters of the logging simulator
#'
#'@param MinDBHValue Minimum DBH for inclusion in the forest inventory.
#'   Default = 10, in cm (double)
#'
#'@param MaxTrailCenterlineSlope Maximum trail centerline slope.
#'  Default = 22, in % (double)
#'
#'@param MaxTrailCrossSlope Maximum trail cross slope. Default = 4,
#'  in % (double)
#'
#'@param GrappleMaxslope Maximum slope accessible by the grapple. Default
#'  = 20, in % (double)
#'
#'@param CableTreesMaxSlope Maximum slope around the tree to access it with
#'  cable. Default = 35, in % (double)
#'
#'@param PlateauMaxSlope Maximum slope to define an area as a plateau.
#'   Default = 5, in % (double)
#'
#'@param SlopeDistance Distance over which the slope is calculated.
#'  Default = 3, in m (3m each side) (integer)
#'
#'@param WaterSourcesBufferZone Buffer zone based on relative horizontal
#'  distance to the nearest water source. Default = 30, in m (double)
#'
#'@param WaterSourcesRelativeHeight Buffer zone based on relative elevation to
#'  the nearest water source. Default = 2, in m (double)
#'
#'@param MinMainTrailWidth Minimum main trail width. Default = 5, in m (double)
#'
#'@param MaxMainTrailWidth Maximum main trail width. Default = 6, in m (double)
#'
#'@param ScndTrailWidth Secondary trail width. Default = 4, in m (double)
#'
#'@param BigTrees Minimum DBH of trees to be avoided by trails. Default =
#'  50, in cm (double)
#'
#'@param SmoothingFact Secondary trails smoothing factor. Default =
#'  10 (unitless) (double)
#'
#'@param ResamplDistDTM Distance of DTM resampling to erase microtopographic
#'  variation. Default = 5, in m (integer).
#'
#'@param CableLength Cable length. Default = 40, in m (double)
#'
#'@param GrappleLength Grapple length. Default = 6, in m (double)
#'
#'@param IsolateTreeMinDistance Minimum distance to consider a tree "isolated"
#'  from other trees of its species, in the aggregative species case
#'  (\code{\link{SpeciesCriteria}}, 'Aggregative' column).
#'  Default = 100, in m (double)
#'
#'@param FutureTreesMinDiameter Future trees minimum diameter. Default = 35, in
#'  cm (future trees are only commercial species of the 1st economic level)
#'  (double)
#'
#'@param TreefallSuccessProportion Proportion of successful directional felling
#'  events. Default = 0.6 (double)
#'
#'@param MinTreefallOrientation Minimum orientation of the tree fall to the
#'  trail. Default = 30, in degree (double)
#'
#'@param MaxTreefallOrientation Maximum orientation of the tree fall to the
#'  trail. Default = 45, in degree (double)
#'
#'@param TreeHollowPartForFuel Proportion of hollow trees used as fuel wood.
#'  Default = 1/3 (double)
#'
#'@param CrownPartForFuel Proportion of the tree crown biomass used as fuel wood.
#'  Default = 2/3 (double) (Branches diameter >= 5 cm) (Eleotério et al. 2019)
#'
#'@param Purge Part of the harvested log not used for timber, can be used for fuel
#'  wood. Default = 0.14, in m3 of purge/m3 of volume of timber harvested.
#'  (double)
#'
#'@param MaxTrailDensity Maximum trail density. Default = 200, in m/ha (double)
#'  (has no impact on the simulation. A message will be sent to inform if
#'  this threshold has been exceeded)
#'
#'@param MaxLandingArea Maximum landing area. Default = 1500) in m2 (double)
#'  (has no impact on the simulation. A message will be sent to inform if this
#'  threshold has been exceeded)
#'
#'@param CostMatrix Cost matrix for optimized trail layout (list of 2 lists).
#'  Gives an increasing cost according to a slope gradient (1st sub-list), and
#'  different costs on certain cases (2nd sub-list):
#'    - "Initial" (default = 1000)
#'    - "Access" (default = Inf)
#'    - "BigTrees" (default = 500)
#'    - "Reserves" (default = 500)
#'    - "Futures" (default = 50)
#'    - "MainTrails" (default = 1E-4)
#'    - "SecondTrails" (default = 0.1)
#'
#'@param TreeHarvestableVolumeAllometry By default, allometry of tree
#'  harvestable volume, French Guiana ONF formula: aCoef + bCoef * (DBH/100)^2.
#'  With aCoef and bCoef depending on the forest location, stored in
#'  \code{\link{ForestZoneVolumeParametersTable}}, DBH in cm. (function)
#'
#'@param TrunkHeightAllometry Allometry of trunk height, based on the cylinder
#'  volume formula: CylinderVolume = pi ((DBH/100)/2)^2 * H, with the height (H)
#'  in m and the DBH in cm (function)
#'
#'@param TreeHeightAllometry By default, allometry parameters estimated from
#'  Guyanese data with the BIOMASS package: ln(H) = 0.07359191 + 1.34241216 *
#'  ln(DBH) -0.12282344 * ln(DBH)^2, with the height (H) in m and the DBH in
#'  cm (function)
#'
#'@param CrownDiameterAllometry ln(DBH) = 𝜶 +𝜷 ln(H*CD) + 𝜺, with 𝜺~N(0,σ^2)
#'  and mean σ^2 = 0.0295966977 with the crown diameter (CD), the tree height
#'  (H) in m, and the DBH in cm. (Aubry-Kientz et al.2019)(function)
#'
#'@param RottenModel Estimates the tree probability of being probed hollow
#'  (default: 1 / (1 + exp(-(-5.151 + 0.042 * DBH))) with DBH in cm (developed
#'  by S.Schmitt)) (function)
#'
#'@param VisiblyDefectModel Estimates the commercial tree probability to have
#'  visible defects. Default: 1 / (1 + exp(-(-3.392 + 0.357 * ln(DBH)))) with
#'  DBH in cm (developed by V.Badouard) (function)
#'
#'@param Treefall2ndDeathModel Estimates the probability of a tree dying when it
#'  is in the area disturbed by the felling of a tree, according to the DBH of
#'  the tree whose probability of dying is estimated. Default: 1 / (1 +
#'  exp(-(-0.47323 + -0.02564 * DBH))) with DBH in cm (developed by M.Rojat)
#'  (function)
#'
#'@references Aubry-Kientz, Mélaine, et al. "A comparative assessment of the
#'   performance of individual tree crowns delineation algorithms from ALS data
#'   in tropical forests." Remote Sensing 11.9 (2019): 1086.
#' Eleotério, Jackson Roberto, et al. "Aboveground biomass quantification and
#'   tree-level prediction models for the Brazilian subtropical Atlantic Forest."
#'   Southern Forests: a Journal of Forest Science 81.3 (2019): 261-271.
#'
#'@return A named list of 35 objects.
#'
#'@export
#'
#'@importFrom stats rnorm
#'
#' @examples
#' loggingparameters(MinDBHValue = 5)
#'
loggingparameters <- function(
  MinDBHValue = 10, #in cm
  MaxTrailCenterlineSlope = 22, #in %
  MaxTrailCrossSlope = 4, #in %
  GrappleMaxslope = 20, #in %
  CableTreesMaxSlope = 35, #in %
  PlateauMaxSlope = 5, #in %
  SlopeDistance = 3L, #in m (for each side)
  WaterSourcesBufferZone = 30, #in m
  WaterSourcesRelativeHeight = 2, #in m
  MinMainTrailWidth = 5, #in m
  MaxMainTrailWidth = 6, #in m
  ScndTrailWidth = 4, #in m
  BigTrees = 50, #in cm
  ResamplDistDTM = 5L, #in m
  SmoothingFact = 10 , #unitless
  # ObjectiveBonus = 30, #in % [20;30%]
  CableLength = 40, #in m
  GrappleLength = 6, #in m
  IsolateTreeMinDistance = 100, #in m
  FutureTreesMinDiameter = 35, #in cm
  TreefallSuccessProportion = 0.6,
  MinTreefallOrientation = 30, #in degree
  MaxTreefallOrientation = 45, #in degree
  TreeHollowPartForFuel = 1/3, #Part taken from hollow trees for fuel exploitation
  CrownPartForFuel = 2/3, # Proportion of the tree crown biomass used as fuel wood
  Purge = 0.14, # in m^3 of fuel wood/m^3 of logged trees
  MaxTrailDensity = 200, #in m/ha
  MaxLandingArea = 1500, #in square meters

  CostMatrix = list(list(list(Slope = 3, Cost = 3),
                         list(Slope = 5, Cost = 5),
                         list(Slope = 12, Cost = 20),
                         list(Slope = 20, Cost = 60),
                         list(Slope = 35, Cost = 1000),
                         list(Slope = Inf, Cost = Inf)),
                    list(list(CostType = "Initial", CostValue = 1000),
                         list(CostType = "Access", CostValue = Inf),
                         list(CostType = "BigTrees", CostValue = 500),
                         list(CostType = "Reserves", CostValue = 500),
                         list(CostType = "Futures", CostValue = 50),
                         list(CostType = "MainTrails", CostValue = 1E-4),
                         list(CostType = "SecondTrails", CostValue = 0.1))),

  ### Models (peut-etre mettre les modèles dans un fichier .R chacun ac leure propre doc)

  # my too much specific version
  TreeHarvestableVolumeAllometry = function(DBH, aCoef, bCoef)
    aCoef + bCoef * (DBH/100)^2, # a and b depend on the forest location)(DBH in cm, in m in the formula)

  # #Sylvain's version
  # TreeHarvestableVolumeParameters = c("aCoef", "bCoeff"), # create a characters vectoc with the coef names
  #
  # TreeHarvestableVolumeAllometry = function(DBH, pars){
  #   # pars1 and pars2 depend on the forest location)
  #   if(length(pars) != 2) # check the parameters number
  #     stop("You should have 2 parameters for the tree harvestable volume allometry")
  #   return(pars[1] + pars[2] * (DBH/100)^2) #(DBH in cm, in m in the formula)
  # },

  TrunkHeightAllometry = function(DBH, TreeHarvestableVolume) # compute the trunk height
    TreeHarvestableVolume/(pi*(((DBH/100)/2)^2)),  # CylinderVolume = pi(DBH/2)^2 * H.
  # DBH in cm, in m in the formula.

  TreeHeightAllometry = function(DBH) exp(0.07359191 + 1.34241216*log(DBH) + -0.12282344*log(DBH)^2),

  CrownDiameterAllometry = function(DBH, TreeHeight, alpha, beta)
    exp(((log(DBH)- alpha - rnorm(length(DBH), 0, 0.0295966977))/beta))/TreeHeight,
  # compute the crown diameter (CD) (ln(D) = alpha + beta ln(H*CD) + error, with error~N(0,sigma^2) and meansigma^2 = 0.0295966977. (Melaine's allometries))

  RottenModel = function(DBH) 1 / (1 + exp(-(-5.151 + 0.042 * DBH))), # Hollow trees identification

  VisiblyDefectModel = function(LogDBH) 1 / (1 + exp(-(-3.392 + 0.357 * LogDBH))), #  Visible defects trees identification

  Treefall2ndDeathModel = function(DBH) 1 / (1 + exp(-(-0.47323 + -0.02564 * DBH))) # Treefall2nd death
){

  # Arguments check
  lapply(c(
    MinDBHValue,
    MaxTrailCenterlineSlope,
    MaxTrailCrossSlope,
    GrappleMaxslope,
    CableTreesMaxSlope,
    PlateauMaxSlope,
    WaterSourcesBufferZone,
    MinMainTrailWidth,
    MaxMainTrailWidth,
    ScndTrailWidth,
    BigTrees,
    SmoothingFact,
    # ObjectiveBonus,
    CableLength,
    GrappleLength,
    IsolateTreeMinDistance,
    FutureTreesMinDiameter,
    TreefallSuccessProportion,
    MinTreefallOrientation,
    MaxTreefallOrientation,
    TreeHollowPartForFuel,
    CrownPartForFuel,
    Purge,
    MaxTrailDensity,
    MaxLandingArea), function(element)
      if(!inherits(element, "numeric"))
        stop("You have assigned a non-numerical value to one of the arguments of the 'loggingparameters' function
         expects a numerical value. Look at the help page for the 'loggingparameters' function (?loggingparameters)"))

  if(!all(unlist(lapply(list(ResamplDistDTM, SlopeDistance), inherits, "integer"))))
    stop("You have assigned a non-integer value to 'ResamplDistDTM' and/or 'SlopeDistance' parameter(s)
    of the 'loggingparameters' function expects an integer value.
         Look at the help page for the 'loggingparameters' function (?loggingparameters)")

  if(!inherits(CostMatrix, "list"))
    stop("You must assign a list to the 'CostMatrix' argument of the 'loggingparameters' function.
             Look at the help page for the 'loggingparameters' function (?loggingparameters)")
  lapply(c(
    TreeHarvestableVolumeAllometry,
    TrunkHeightAllometry,
    TreeHeightAllometry,
    CrownDiameterAllometry,
    RottenModel), function(element)
      if(!inherits(element, "function"))
        stop("One of the arguments of the 'loggingparameters' function you want to fill in expects an object of function class.
             Look at the help page for the 'loggingparameters' function (?loggingparameters)"))



  # Function
  list(
    ### Values
    MinDBHValue = MinDBHValue,
    MaxTrailCenterlineSlope = MaxTrailCenterlineSlope,
    MaxTrailCrossSlope = MaxTrailCrossSlope,
    GrappleMaxslope = GrappleMaxslope,
    CableTreesMaxSlope = CableTreesMaxSlope,
    PlateauMaxSlope = PlateauMaxSlope,
    SlopeDistance = SlopeDistance,
    WaterSourcesBufferZone = WaterSourcesBufferZone,
    WaterSourcesRelativeHeight = WaterSourcesRelativeHeight,
    MinMainTrailWidth = MinMainTrailWidth,
    MaxMainTrailWidth = MaxMainTrailWidth,
    ScndTrailWidth = ScndTrailWidth,
    BigTrees = BigTrees,
    ResamplDistDTM = ResamplDistDTM,
    SmoothingFact = SmoothingFact,
    # ObjectiveBonus = ObjectiveBonus,
    CableLength = CableLength,
    GrappleLength = GrappleLength,
    IsolateTreeMinDistance = IsolateTreeMinDistance,
    FutureTreesMinDiameter = FutureTreesMinDiameter,
    TreefallSuccessProportion = TreefallSuccessProportion,
    MinTreefallOrientation = MinTreefallOrientation,
    MaxTreefallOrientation = MaxTreefallOrientation,
    TreeHollowPartForFuel = TreeHollowPartForFuel,
    CrownPartForFuel = CrownPartForFuel,
    Purge = Purge,
    MaxTrailDensity = MaxTrailDensity,
    MaxLandingArea = MaxLandingArea,

    ### Cost matrix
    CostMatrix = CostMatrix,

    ### Models
    TreeHarvestableVolumeAllometry = TreeHarvestableVolumeAllometry,
    TrunkHeightAllometry = TrunkHeightAllometry,
    TreeHeightAllometry = TreeHeightAllometry,
    CrownDiameterAllometry = CrownDiameterAllometry,
    RottenModel = RottenModel,
    VisiblyDefectModel = VisiblyDefectModel,
    Treefall2ndDeathModel = Treefall2ndDeathModel
  )
}

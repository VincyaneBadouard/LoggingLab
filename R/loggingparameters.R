#'Advanced parameters of the logging simulator
#'
#'@param MinDBHValue Minimum DBH for inclusion in the forest inventory. Default = 10, in cm
#'  (double)
#'@param MaxAreaSlope Maximum area slope for inclusion in a logging unit.
#'  Default = 27, in % (double)
#'@param MaxTrailCenterlineSlope Maximum main & 2nd trail centerline slope.
#'  Default = 22, in % (double)
#'@param MaxTrailCrossSlope Maximum main & 2nd trail cross slope. Default = 4,
#'  in % (double)
#'@param GrappleMaxslope Maximum slope accessible by the grapple. Default
#'  = 20, in % (double)
#'@param TreeMaxSlope Maximum slope around the tree to access it. Default = 22,
#'  in % (double)
#'@param PlateauMaxSlope Maximum slope to define an area as a plateau. Default = 5,
#'  in % (double)
#'@param SlopeDistance Distance over which the slope is calculated. Default = 6, in m (3m each
#'  side) (double)
#'@param WaterSourcesBufferZone Buffer zone around the water sources. Default =
#'  30, in m (double)
#'@param MinMainTrailWidth Minimum main trail width. Default = 5, in m (double)
#'@param MaxMainTrailWidth Maximum main trail width. Default = 6, in m (double)
#'@param ScndTrailWidth 2nd trail width. Default = 4, in m (double)
#'@param BigTrees Minimum DBH of trees to be avoided by trails. Default =
#'  50, in cm (double)
#'@param ObjectiveBonus Objective bonus. Default = 30, in % (double)
#'@param CableLength Cable length. Default = 40, in m (double)
#'@param GrappleLength Grapple length. Default = 6, in m (double)
#'@param IsolateTreeMinDistance Minimum distance to consider a tree "isolated"
#'  from other trees of its species. Default = 100, in m (double)
#'@param FutureTreesMinDiameter Future trees minimum diameter. Default = 35, in
#'  cm (double)
#'@param TreefallSuccessProportion Proportion of successful directional felling
#'  events. Default = 0.6 (double)
#'@param MinTreefallOrientation Minimum orientation of the tree fall to the
#'  trail. Default = 30, in degree (double)
#'@param MaxTreefallOrientation Maximum orientation of the tree fall to the
#'  trail. Default = 45, in degree (double)
#'@param TreeHollowPartForFuel Proportion of hollow trees used as fuelwood
#'  . Default = 1/3 (double)
#'@param Purge Part of the log no used for timber, can be used for fuelwood. Default = 0.14, in m^3 of purge/m^3 of
#'  volume of timber harvested. (double)
#'@param MaxTrailDensity Maximum trail density. Default = 200, in m/ha (double)
#'  (has no impact on the simulation. A message will be sent to inform if
#'  this threshold has been exceeded)
#'@param MaxLandingArea Maximum landing area. Default = 1500) in square meters
#'  (double) (has no impact on the simulation. A message will be sent to inform if
#'  this threshold has been exceeded)
#'
#'@param TreeHarvestableVolumeAllometry By default, allometry of tree
#'  harvestable volume, French Guiana ONF formula: aCoef + bCoef x (DBH/100)^2,
#'  aCoef and bCoef depend on the forest location, stored in
#'  \code{\link{ForestZoneVolumeParametersTable}}, DBH in cm. (function)
#'
#'@param TrunkHeightAllometry Allometry of trunk height, based on the cylinder
#'  volume formula: CylinderVolume = pi x ((DBH/100)/2)^2 x H, with the height (H)
#'  in m and the DBH in cm (function)
#'
#'@param TreeHeightAllometry By default, allometry parameters estimated from
#'  Guyanese data with the BIOMASS package: ln(H) = 0.07359191 + 1.34241216 x
#'  ln(DBH) -0.12282344 x ln(DBH)^2, with the height (H) in m and the DBH in
#'  cm (function)
#'
#'@param CrownDiameterAllometry ln(DBH) = 𝜶+ 𝜷 x ln(H*CD) + 𝜺, with 𝜺~N(0,σ^2)
#'  and mean σ^2 = 0.0295966977 with the crown diameter (CD), the tree height
#'  (H) in m, and the DBH in cm. (ref)(function)
#'
#'@param RottenModel Estimates the tree probability of being probed hollow
#'  (default: 1 / (1 + exp(-(-5.151 + 0.042 DBH))) with DBH in cm) (function)
#'
#'@param VisiblyDefectModel Estimates the tree probability to have visible
#'  defects. Default: 1 / (1 + exp(-(-3.392 + 0.357 * Log(DBH)))) with DBH in cm
#'  (function)
#'
#'@return A named list of 31 objects.
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
  MaxAreaSlope = 27, #in %
  MaxTrailCenterlineSlope = 22, #in %
  MaxTrailCrossSlope = 4, #in %
  GrappleMaxslope = 20, #in %
  TreeMaxSlope = 22, #in %
  PlateauMaxSlope = 5, #in %
  SlopeDistance = 6, #in m (3m for each side)
  WaterSourcesBufferZone = 30, #in m
  MinMainTrailWidth = 5, #in m
  MaxMainTrailWidth = 6, #in m
  ScndTrailWidth = 4, #in m
  BigTrees = 50, #in cm
  ObjectiveBonus = 30, #in % [20;30%]
  CableLength = 40, #in m
  GrappleLength = 6, #in m
  IsolateTreeMinDistance = 100, #in m
  FutureTreesMinDiameter = 35, #in cm
  TreefallSuccessProportion = 0.6,
  MinTreefallOrientation = 30, #in degree
  MaxTreefallOrientation = 45, #in degree
  TreeHollowPartForFuel = 1/3, #Part taken from hollow trees for fuel exploitation
  Purge = 0.14, # in m^3 of fuelwood/m^3 of logged trees
  MaxTrailDensity = 200, #in m/ha
  MaxLandingArea = 1500, #in square meters

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
    TreeHarvestableVolume/(pi*(((DBH/100)/2)^2)),  # CylinderVolume = pi(DBH/2)^2 x H.
  # DBH in cm, in m in the formula.

  TreeHeightAllometry = function(DBH) exp(0.07359191 + 1.34241216*log(DBH) + -0.12282344*log(DBH)^2),

  CrownDiameterAllometry = function(DBH, TreeHeight, alpha, beta)
    exp(((log(DBH)- alpha - rnorm(length(DBH), 0, 0.0295966977))/beta))/TreeHeight,
  # compute the crown diameter (CD) (ln(D) = alpha + beta ln(H*CD) + error, with error~N(0,sigma^2) and meansigma^2 = 0.0295966977. (Melaine's allometries))

  RottenModel = function(DBH) 1 / (1 + exp(-(-5.151 + 0.042 * DBH))), # Hollow trees identification

  VisiblyDefectModel = function(LogDBH) 1 / (1 + exp(-(-3.392 + 0.357 * LogDBH))) #  Visible defects trees identification
){

  # Arguments check
  lapply(c(
    MinDBHValue,
    MaxAreaSlope,
    MaxTrailCenterlineSlope,
    MaxTrailCrossSlope,
    GrappleMaxslope,
    TreeMaxSlope,
    PlateauMaxSlope,
    SlopeDistance,
    WaterSourcesBufferZone,
    MinMainTrailWidth,
    MaxMainTrailWidth,
    ScndTrailWidth,
    BigTrees,
    ObjectiveBonus,
    CableLength,
    GrappleLength,
    IsolateTreeMinDistance,
    FutureTreesMinDiameter,
    TreefallSuccessProportion,
    MinTreefallOrientation,
    MaxTreefallOrientation,
    TreeHollowPartForFuel,
    Purge,
    MaxTrailDensity,
    MaxLandingArea), function(element)
      if(!inherits(element, "numeric"))
        stop("You have assigned a non-numerical value to one of the arguments of the 'loggingparameters' function
         expects a numerical value. Look at the help page for the 'loggingparameters' function (?loggingparameters)"))

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
    MaxAreaSlope = MaxAreaSlope,
    MaxTrailCenterlineSlope = MaxTrailCenterlineSlope,
    MaxTrailCrossSlope = MaxTrailCrossSlope,
    GrappleMaxslope = GrappleMaxslope,
    TreeMaxSlope = TreeMaxSlope,
    PlateauMaxSlope = PlateauMaxSlope,
    SlopeDistance = SlopeDistance,
    WaterSourcesBufferZone = WaterSourcesBufferZone,
    MinMainTrailWidth = MinMainTrailWidth,
    MaxMainTrailWidth = MaxMainTrailWidth,
    ScndTrailWidth = ScndTrailWidth,
    BigTrees = BigTrees,
    ObjectiveBonus = ObjectiveBonus,
    CableLength = CableLength,
    GrappleLength = GrappleLength,
    IsolateTreeMinDistance = IsolateTreeMinDistance,
    FutureTreesMinDiameter = FutureTreesMinDiameter,
    TreefallSuccessProportion = TreefallSuccessProportion,
    MinTreefallOrientation = MinTreefallOrientation,
    MaxTreefallOrientation = MaxTreefallOrientation,
    TreeHollowPartForFuel = TreeHollowPartForFuel,
    Purge = Purge,
    MaxTrailDensity = MaxTrailDensity,
    MaxLandingArea = MaxLandingArea,

    ### Models
    TreeHarvestableVolumeAllometry = TreeHarvestableVolumeAllometry,
    TrunkHeightAllometry = TrunkHeightAllometry,
    TreeHeightAllometry = TreeHeightAllometry,
    CrownDiameterAllometry = CrownDiameterAllometry,
    RottenModel = RottenModel,
    VisiblyDefectModel = VisiblyDefectModel
  )
}

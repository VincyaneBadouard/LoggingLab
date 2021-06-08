#' Other parameters of the logging simulator
#'
#' @param MinDBHValue Mimimum DBH value. Default = 10, in cm (double)
#' @param MaxAreaSlope Maximum area slope. Default = 27, in % (double)
#' @param MaxTrailCenterlineSlope Maximum main&2nd trail centerline slope. Default = 22, in % (double)
#' @param MaxTrailCrossSlope Maximum main&2nd trail cross slope. Default = 4, in % (double)
#' @param GrappleMaxslope Grapple maximum slope. Default = 20, in % (double)
#' @param TreeMaxSlope Trees slope. Default = 22, in % (double)
#' @param PlateauMaxSlope Plateau maximum slope. Default = 5, in % (double)
#' @param SlopeDistance Distance to compute slope. Default = 6, in m (3m for each side) (double)
#' @param WaterSourcesBufferZone Water sources buffer zone. Default = 30, in m (double)
#' @param MinMainTrailWidth Minimum main trail width. Default = 5, in m (double)
#' @param MaxMainTrailWidth Maximum main trail width. Default = 6, in m (double)
#' @param ScndTrailWidth 2nd trail width. Default = 4, in m (double)
#' @param BigTrees Big trees. Default = 50, in cm (double)
#' @param ObjectiveBonus Objective bonus. Default = 30, in % (20;30%) (double)
#' @param CableLength Cable. Default = 40, in m (double)
#' @param GrappleLength Grapple. Default = 6, in m (double)
#' @param IsolateTreeMinDistance Isolate tree minimum distance. Default = 100, in m (double)
#' @param FutureTreesMinDiameter Future trees minimum diameter. Default = 35, in cm (double)
#' @param TreefallSuccessProportion TreefallSuccess proportion. Default = 0.6, (double)
#' @param MinTreefallOrientation Minimum treefall orientation. Default = 30, in degree (double)
#' @param MaxTreefallOrientation Maximum treefall orientation. Default = 45, in degree (double)
#' @param TreeHollowPartForFuel Part taken from hollow trees for fuel exploitation. Default = 1/3 (double)
#' @param Purge Purge. Default = 0.14, in m^3 of fuelwood/m^3 of logged trees (double)
#' @param MaxTrailDensity Maximum TrailDensity. Default = 200, in m/ha (double)
#' @param MaxLandingArea Maximum landing area. Default = 1500) in square meters (double)
#' @param TreeHarvestableVolumeAllometry (function)
#' @param TrunkHeightAllometry (function)
#' @param TreeHeightAllometry log(H) = 0.07359191 + 1.34241216 log(DBH) + -0.12282344 log(DBH)^2 (function)
#' @param CrownDiameterAllometry ln(D) = 𝜶+ 𝜷 ln(H*CD) + 𝜺, with 𝜺~N(0,σ^2) and meanσ^2 = 0.0295966977 (ref)(function)
#'
#'
#' @return A named list of 30 objects.

#' @export
#' @importFrom stats rnorm
#'
#' @examples
#' loggingparameters(MinDBHValue = 5)
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
  TreeHollowPartForFuel = 1/3, #(Hollow) Part taken from hollow trees for fuel exploitation
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
    exp(((log(DBH)- alpha - rnorm(length(DBH), 0, 0.0295966977))/beta))/TreeHeight
  # compute the crown diameter (CD) (ln(D) = alpha + beta ln(H*CD) + error, with error~N(0,sigma^2) and meansigma^2 = 0.0295966977. (Melaine's allometries))

  # RottenModel = function() # Hollow trees identification
){
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
    CrownDiameterAllometry = CrownDiameterAllometry
    # RottenModel = RottenModel
  )
}

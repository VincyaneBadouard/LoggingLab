# Maria package global options
NULL

.PkgEnv <- new.env()
.onAttach <- function(libname, pkgname){
  packageStartupMessage('Welcome to the ForestLogging package.')
}


.onLoad <- function(libname, pkgname) {
  op <- options() #Get the current options from the envmt and store them in op
  op.Maria <- list( # creation of objects (future options)
    # Package options
    Maria.path = "~/R-dev",
    Maria.install.args = "",
    Maria.name = "Vincyane Badouard, Guillaume Salzet, Thomas Gaquiere, Géraldine Derroire and Sylvain Schmitt",
    Maria.desc.author = c('person("Vincyane","Badouard",
         role = c("aut", "cre"),
          "vincyane.badouard@gmail.com"),
  person("Guillaume","Salzet",
         role = "aut",
          "Guillaume.Salzet@ecofog.gf"),
  person("Thomas","Gaquiere",
         role = "aut",
          "gaquiere.thomas@yahoo.com "),
  person("Géraldine","Derroire",
         role = "ctb",
          "geraldine.derroire@cirad.fr"),
  person("Sylvain","Schmitt",
         role = "ctb",
          "sylvain.m.schmitt@gmail.com "'),
    Maria.desc.license = "GPL-3",

    # Simulator options
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
    "2ndTrailWidth" = 4, #in m
    BigTrees = 50, #in cm
    ObjectiveBonus = 30, #in % [20;30%]
    CableLength = 40, #in m
    GrappleLength = 6, #in m
    IsolateTreeMinDistance = 100, #in m
    FutureTreesMinDiameter = 35, #in cm
    TreefallSuccessProportion = 0.6,
    MinTreefallOrientation = 30, #in degree (◦)
    MaxTreefallOrientation = 45, #in degree (◦)
    TreeHollowPartForFuel = 1/3, #(Hollow) Part taken from hollow trees for fuel exploitation
    Purge = 0.14, # in m^3 of fuelwood/m^3 of logged trees
    MaxTrailDensity = 200, #in m/ha
    MaxLandingArea = 1500) #in square meters

  toset <- !(names(op.Maria) %in% names(op)) #if these objects do not exist in the envmt, put them there -> no conflict,
  if(any(toset)) options(op.Maria[toset]) #but the existing envmt takes precedence -> my homonymous objects are not taken.


  invisible() # send no msg to the user.
}

# pour les options qui sont des formules il est nécessaire de les inscrire dans des fcts (truc qui rentrent et qui sortent):

# TrunkHeightAllometry = function(DBH, TreeHarvestableVolume) TreeHarvestableVolume/(pi*(((DBH/100)/2)^2)) # compute the trunk height (cylinderVolume= π(DBH/2)² x H) DBH in cm, in m in the formule.

# CrownDiameterAllometry = function(DBH, TreeHeight, alpha, beta, sigma) exp(((log(DBH)-alpha-rnorm(length(DBH), 0, sigma))/beta))/TreeHeight #compute the crown diameter (CD) (ln(D) = 𝜶+ 𝜷 ln(H*CD) + 𝜺, with 𝜺~N(0,σ^2) and meanσ^2 = 0.0295966977. (Mélaine's allometries))

# RottenModel = function() # Hollow trees identification


## CrownDiameterAllometry(c(10, 15), c(20, 25), 2, 4, 0.0295966977)# application
#
# myFun <- function(
#   arg1,
#   arg2,
#   allometry = function(DBH, TreeHeight, alpha, beta, sigma) {exp(((log(DBH)-alpha-rnorm(length(DBH), 0, sigma))/beta))/TreeHeight}
# ){
#   NA
# }
#
# myAllo <- y = ax+b
# myFun(arg1, arg2, myAllo)


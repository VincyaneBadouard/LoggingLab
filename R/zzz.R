# Maria package global options
NULL

.PkgEnv <- new.env()
.onAttach <- function(libname, pkgname){
  packageStartupMessage('Welcome to the ForestLogging package.')
}

# .onLoad <- function(libname, pkgname) {
#   op <- options()
#   op.Maria <- list(
#     Maria.path = "~/R-dev",
#     Maria.install.args = "",
#     Maria.name = "Vincyane Badouard, Guillaume Salzet, Thomas Gaquiere, Géraldine Derroire and Sylvain Schmitt",
#     Maria.desc.author = c('person("Vincyane","Badouard",
#          role = c("aut", "cre"),
#           "vincyane.badouard@gmail.com"),
#   person("Guillaume","Salzet",
#          role = "aut",
#           "Guillaume.Salzet@ecofog.gf"),
#   person("Thomas","Gaquiere",
#          role = "aut",
#           "gaquiere.thomas@yahoo.com "),
#   person("Géraldine","Derroire",
#          role = "ctb",
#           "geraldine.derroire@cirad.fr"),
#   person("Sylvain","Schmitt",
#          role = "ctb",
#           "sylvain.m.schmitt@gmail.com "'))
#     Maria.desc.license = "GPL-3"
# }
#
# pars <- list(MinDBHValue = 10, #in cm
#              MaxAreaSlope = 27, #in %
#              MaxTrailCenterlineSlope = 22, #in %
#              MaxTrailCrossSlope = 4, #in %
#              GrappleMaxslope = 20, #in %
#              TreeMaxSlope = 22, #in %
#              PlateauMaxSlope = 5, #in %
#              SlopeDistance = 6, #in m (3m for each side)
#              WaterSourcesBufferZone = 30, #in m
#              MainTrailWidth = 5-6, #in m
#              "2ndTrailWidth" = 4, #in m
#              BigTrees = 50, #in cm
#              ObjectiveBonus = 20-30, #in % (ObjectiveBonus >= 20 && ObjectiveBonus =< 30 ou c(20:30))
#              CableLength = 40, #in m
#              GrappleLength = 6, #in m
#              IsolateTreeMinDistance = 100, #in m
#              FutureTreesMinDiameter = 35, #in cm
#              TreefallSuccessProportion = 0.6,
#              TreefallOrientation = 30-45, #in degree (◦)
#              TreeHollowPartForFuel = 1/3, #(Hollow) Part taken from hollow trees for fuel exploitation
#              Purge = 0.14, # in m^3 of fuelwood/m^3 of logged trees
#              MaxTrailDensity = 200, #in m/ha
#              MaxLandingArea = 1500, #in square meters
#              TrunkHeightAllometry = TreeHarvestableVolume/(pi*(((DBH/100)/2)^2)),
#              CrownDiameterAllometry = exp(((ln(DBH)-alpha-error)/beta))/TreeHeight,
#              RottenModel = )


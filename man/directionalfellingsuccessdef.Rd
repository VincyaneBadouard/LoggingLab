% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/treefelling.R
\name{directionalfellingsuccessdef}
\alias{directionalfellingsuccessdef}
\title{Directional felling success definition}
\usage{
directionalfellingsuccessdef(
  inventory,
  fuel,
  directionalfelling,
  advancedloggingparameters = loggingparameters()
)
}
\arguments{
\item{inventory}{input inventory (see the inputs formats and metadata in the
vignette) (data.frame)}

\item{fuel}{Fuel wood exploitation: no exploitation = "0", exploitation of
damage and unused part of logged trees for fuel = "1", exploitation of
hollow trees, damage and and unused part of the log for fuel = "2"}

\item{directionalfelling}{Directional felling =
"0": only to direct the foot of the tree towards the trail
"1": to direct the foot of the tree towards the trail + to avoid damage to
future and reserve trees
"2": to avoid damage to future and reserve trees + orientation angle
to the trail}

\item{advancedloggingparameters}{Other parameters of the logging simulator
\code{\link{loggingparameters}} (list)}
}
\value{
Input inventory with the "TreeFellingOrientationSuccess" new column (see
the outputs metadata in the vignette).
}
\description{
Defines whether the directed fall of the tree is successful or
not by drawing in a Bernoulli distribution where the probability of success
is by default 60\%, and can be changed with the
\code{advancedloggingparameters} argument.
}
\examples{

data(Paracou6_2016)
data(DTMParacou)
data(MainTrails)
data(HarvestableAreaOutputsCable)

inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask),
volumeparameters = ForestZoneVolumeParametersTable)

inventory <- treeselection(inventory,
topography = DTMParacou,
speciescriteria = SpeciesCriteria,
scenario = "manual", objective = 10, fuel = "2", diversification = TRUE,
winching = "2", specieslax = FALSE, objectivelax = TRUE,
harvestablearea = HarvestableAreaOutputsCable$HarvestableArea,
plotslope = HarvestableAreaOutputsCable$PlotSlope,
maintrails = MainTrails,
harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons,
advancedloggingparameters = loggingparameters())$inventory

new <- directionalfellingsuccessdef(inventory, fuel = "2",
directionalfelling = "2",
advancedloggingparameters = loggingparameters())

}

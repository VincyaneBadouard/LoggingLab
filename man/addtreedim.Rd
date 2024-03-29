% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addtreedim.R
\name{addtreedim}
\alias{addtreedim}
\title{Compute tree dimensions (tree, trunk and crown height, crown diameter,
harvestable volume, wood density, and AGB)}
\usage{
addtreedim(
  inventory,
  volumeparameters,
  crowndiameterparameters = ParamCrownDiameterAllometry,
  advancedloggingparameters = loggingparameters()
)
}
\arguments{
\item{inventory}{Input inventory (see the inputs formats and metadata in the
vignette) (data.frame)}

\item{volumeparameters}{Parameters of the tree volume tables (in the same
format of \code{\link{ForestZoneVolumeParametersTable}}) used to compute the
harvestable volume of each tree, depending on its geographic zone if several
locations (data.frame)}

\item{crowndiameterparameters}{Parameters of crown diameter allometry (in the
same format of \code{\link{ParamCrownDiameterAllometry}}) used to compute
the crown diameter of each tree, depending on its DBH (Diameter at Breast
Height) and its species, genus or family. (data.frame)}

\item{advancedloggingparameters}{Other parameters of the logging simulator
\code{\link{loggingparameters}} (list)}
}
\value{
The inventory (data.frame) with additional variables: TreeHeight (m),
TreeHarvestableVolume (m^3), TrunkHeight (m), CrownHeight (m), CrownDiameter
(m), estimated 'WoodDensity' (g/cm^3) and tree above-ground biomass ('AGB')
(in Mg)
}
\description{
Compute tree dimensions (tree, trunk and crown height, crown
diameter, harvestable volume, wood density, and AGB) from its Diameter at
Breast Height (DBH)
}
\details{
'addtreedim' compute some tree dimensions with the
'advancedloggingparameters' argument:
\itemize{
\item Tree height (in m) ('TreeHeightAllometry')
\item Tree harvestable volume (m3) ('TreeHarvestableVolumeAllometry')(French Guiana ONF)
\item Trunk height (in m) ('TrunkHeightAllometry')
\item Crown height (in m)
\item Crown diameter (in m) ('CrownDiameterAllometry') (Aubry-Kientz et al.2019)
\item Wood density (g/cm3) (by the BIOMASS package)
\item Tree above-ground biomass (AGB) (in ton) (by the BIOMASS package)
}

'Taxo': the level at which the CrownDiameter was calculated.
'levelWD': the level at which the WoodDensity was assigned by the BIOMASS
package.
}
\examples{
data(Paracou6_2016)
data(ForestZoneVolumeParametersTable) # The volume parameters data in the global environment
data(ParamCrownDiameterAllometry)

if (!("DBH" \%in\% names(Paracou6_2016))) {
tibble::add_column(Paracou6_2016, DBH = NA) #if DBH doesn't exist create it
Paracou6_2016$DBH = Paracou6_2016$CircCorr/pi} # and calculate it
Paracou6_2016 <- dplyr::filter(Paracou6_2016, DBH >= 10)

addtreedim(Paracou6_2016, volumeparameters = ForestZoneVolumeParametersTable)

}

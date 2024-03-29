% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createcanopy.R
\name{treefromthesky}
\alias{treefromthesky}
\title{treefromthesky}
\usage{
treefromthesky(dat)
}
\arguments{
\item{dat}{1 row data.frame with columns:
"Xutm", "Yutm" (Tree coordinates) and "CrownDiameter".}
}
\value{
A circle (sfc_POLYGON) representing the tree crown, with its diameter
filled in, representing the tree from the sky.
}
\description{
treefromthesky
}
\examples{
data(Paracou6_2016)
data(ForestZoneVolumeParametersTable)

inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask),
volumeparameters = ForestZoneVolumeParametersTable)

dat <- inventory \%>\%
  dplyr::filter(CrownDiameter == max(CrownDiameter))

Crown <- treefromthesky(dat)

library(ggplot2)
ggplot() +
 geom_sf(data = sf::st_as_sf(inventory, coords = c("Xutm", "Yutm"))) +
 geom_sf(data = Crown, fill = "forestgreen") # trees polygons

}

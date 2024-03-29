% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SecondaryTrails.R
\docType{data}
\name{SecondaryTrails}
\alias{SecondaryTrails}
\title{Secondary trails of Paracou plot 6}
\format{
list
}
\usage{
SecondaryTrails
}
\description{
A list with :
}
\details{
\itemize{
\item inventory: Updated inventory
\item SmoothedTrails: Smoothed secondary trails polygons
(sfc_MULTIPOLYGON with crs)
\item TrailsDensity: Second trails density (in m/ha)
\item TrailsIdentity: information on sections of the trails (matrix) with:
\itemize{
\item LineID:
\item LoggedTrees: idTree of trees reached by the trails
\item TypeExpl: type of winching
}
\item MainTrailsAccess : Random access point of main trail for each harvestable
zone (sfc_POINT with crs)
\item RawSecondTrails : non-smoothed secondary trails (SpatialLines with crs)
\item CostRasterAgg: The cost raster (RasterLayer  with crs)
}
}
\keyword{datasets}

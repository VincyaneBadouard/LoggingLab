% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getgeometry.R
\name{getgeometry}
\alias{getgeometry}
\title{Get geometry}
\usage{
getgeometry(inventory, var)
}
\arguments{
\item{inventory}{(data.frame) with a column of characters, which contains the
WKT encoded geometries.}

\item{var}{Name of the character column that holds WKT encoded geometries, as a data/env-variable.}
}
\value{
An sf object
}
\description{
Converts a dataframe with a column of characters containing
the WKT encoded geometries into an sf object. The column is converted to sfc.
}
\examples{
data(Paracou6_2016)

felttrees <- Paracou6_2016 \%>\%
dplyr::filter(VernName == "wacapou") \%>\%
  dplyr::group_by(idTree) \%>\%
  dplyr::do(LocPts = # inform geometry.
       sf::st_point(c(.$Xutm,.$Yutm)) \%>\%
       sf::st_as_text()) \%>\% # as text to easy join with a non spacial table
  tidyr::unnest(LocPts) # here to pass from list to character

inventory <- dplyr::left_join(Paracou6_2016, felttrees, by = "idTree")

Rslt <- getgeometry (inventory, LocPts)

library(ggplot2)
ggplot() +
  geom_sf(data = Rslt)

}

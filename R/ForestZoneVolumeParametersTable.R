#' Table of tree volume parameters by forest zone in French Guiana
#'
#' Parameters of the tree volume table used to compute the harvestable volume
#'  of each tree, depending on its geographic zone
#'  (Source: ONF ('Office national des forêts') of French Guiana)
#' @format A tibble with 12 rows and 4 variables:
#' \describe{
#'   \item{Forest}{Guyafor site name (character)}
#'   \item{Zone}{Geographic zone (character)}
#'   \item{aCoef}{coefficient of the equation: Volume = aCoef + bCoef * DBH^2 (numeric)}
#'   \item{bCoef}{coefficient of the equation: Volume = aCoef + bCoef * DBH^2 (numeric)}

#'   ...
#' }
#'
"ForestZoneVolumeParametersTable"

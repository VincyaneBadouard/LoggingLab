#' ForestZoneVolumeParametersTable
#'
#' Volume parameters table to compute the harvestable volume of each tree, depend to its geographic zone
#' @format A tibble with 12 rows and 4 variables:
#' \describe{
#'   \item{Forest}{Site name (character)}
#'   \item{Zone}{Geographic zone (character)}
#'   \item{aCoef}{coefficient of the equation: Volume = aCoef + bCoef * DBH^2 (numeric)}
#'   \item{bCoef}{coefficient of the equation: Volume = aCoef + bCoef * DBH^2 (numeric)}

#'   ...
#' }
#'
"ForestZoneVolumeParametersTable"

#' ForestZoneVolumeParametersTable
#'
#' Parameters of the tree volume tables used to compute the harvestable volume of each tree, depending on its geographic zone
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

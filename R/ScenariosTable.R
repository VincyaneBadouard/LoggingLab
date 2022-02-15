#' Scenarios Table
#'
#' Table showing the 6 logging scenarios proposed by the LoggingLab package.
#'
#' @format A tibble with 6 rows and 6 variables:
#' \describe{
#'   \item{Type}{Species vernacular/economic name (character)}
#'
#'   \item{SpatialDataType}{Type of spatial data (character)}
#'
#'   \item{Objective}{Objective volume per hectare (character)}
#'
#'   \item{Diversification}{Taking of other species in addition to the main
#'   commercial species (logical)}
#'
#'   \item{Winching}{Winching method (No cable or grapple = "0", only cable = "1", grapple + cable
#'   = "2") (factor)}
#'
#'   \item{DirectionalFelling}{Directional felling = "0" (absent), "1" (only to
#'   avoid damage to future and reserve trees), "2" (avoid damage to future and
#'   reserve trees + trail orientation) (factor)}
#'   ...
#' }
#'
"ScenariosTable"

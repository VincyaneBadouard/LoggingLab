#' Filter access to harvestable zone
#'
#' @param harvestablepolygons Accessible area of the inventoried plot (default:
#'   \code{\link{HarvestableAreaDefinition}}) (sf polygons data.frame)
#'
#' @param MainTrails Main trails defined at the entire harvestable area (sf
#'   polylines)
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @return Accessible area of the inventoried plot according to selected
#'   logging scenario (sf polygons data.frame)
#'
#' @export
#'
#' @importFrom sf st_union st_buffer st_intersects
#' @importFrom dplyr mutate filter
#'
#' @examples
#' \dontrun{
#' data(MainTrails)
#' data(HarvestablePolygons)
#'
#' AccessPolygones <- FilterAccesExplArea(
#'  harvestablepolygons = HarvestablePolygons,
#'  MainTrails = MainTrails,
#'  advancedloggingparameters = loggingparameters())
#'  }
#'
FilterAccesExplArea <- function(
  harvestablepolygons,
  MainTrails,
  advancedloggingparameters = loggingparameters()
){

  # Global Variables
  Harvestable <- Access <- NULL

  # Arguments check


  #function

  MainTrails <- MainTrails %>%
    sf::st_buffer(dist = advancedloggingparameters$MaxMainTrailWidth)

  accesspolygones <- harvestablepolygons %>% filter(Harvestable == 1) %>%
    sf::st_buffer(dist = -advancedloggingparameters$ScndTrailWidth) %>%
    sf::st_union() %>% sf::st_cast("POLYGON") %>% sf::st_as_sf()

  accesspolygones <- accesspolygones %>%
    mutate("Access" = sf::st_intersects(accesspolygones, MainTrails, sparse = FALSE))

  accesspolygonesMain <- accesspolygones %>% filter(Access == TRUE)  %>%
    sf::st_buffer(dist = advancedloggingparameters$ScndTrailWidth) %>%
    sf::st_union()

  # accesspolygonesCable <- accesspolygonesMain  %>%
  #   sf::st_union()  %>%
  #   sf::st_buffer(dist = advancedloggingparameters$CableLength)
  #
  # if (winching == "0") {
  #   accesspolygones <- accesspolygonesMain
  # } else {
  #   accesspolygones <- accesspolygonesCable
  # }

  return(accesspolygonesMain)
}

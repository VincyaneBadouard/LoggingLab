#' FilterAccesExplArea
#'
#' @param exploitpolygones Accessible area of the inventoried plot
#'  (default: \code{\link{HarvestableAreaDefinition}}) (sf polygons data.frame)
#'
#' @param maintrails Main trails defined at the entire harvestable area (sf polylines)
#'
#' @param winching no cable or grapple = "0", only cable = "1", grapple + cable =
#'  "2"
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#' \code{\link{loggingparameters}} (list) MainTrail (multiline)
#'
#' @return Accessible area  of the inventoried plot according to selected logging scenario (sf polygons data.frame)
#'
#' @export
#'
#' @import sf
#'
#' @examples
#'
#'  AccessPolygones <-  FilterAccesExplArea (exploitpolygones = ExploitPolygones,
#'  plot = Plots,
#'  maintrails = MainTrails,
#'  scenario = scenario,
#'  fuel = fuel,
#'  diversification = diversification,
#'  advancedloggingparameters = loggingparameters())
#'
FilterAccesExplArea <- function (exploitpolygones = ExploitPolygones,
                                 maintrails = MainTrails,
                                 winching =  winching,
                                 advancedloggingparameters = loggingparameters()) {

  # Arguments check


  #function

  maintrails <- maintrails %>%
    sf::st_buffer(dist = advancedloggingparameters$MaxMainTrailWidth)

  accesspolygones <- exploitpolygones %>% filter(Exploit == 1) %>%
    sf::st_buffer(dist = -advancedloggingparameters$ScndTrailWidth) %>%
    sf::st_union() %>% sf::st_cast("POLYGON") %>% sf::st_as_sf()

  accesspolygones <- accesspolygones %>%
    mutate("Access" = sf::st_intersects(accesspolygones,maintrails, sparse = FALSE))

  accesspolygonesMain <- accesspolygones %>% filter(Access == TRUE)  %>%
    sf::st_buffer(dist = advancedloggingparameters$ScndTrailWidth) %>%
    sf::st_union()

  accesspolygonesCable <- accesspolygonesMain  %>%
    sf::st_union()  %>%
    sf::st_buffer(dist = advancedloggingparameters$CableLength)

  if (winching == "0") {
    accesspolygones <- accesspolygonesMain
  } else {
    accesspolygones <- accesspolygonesCable
  }

  return(accesspolygones)
}

#' Filter access to harvestable zone
#'
#' @param harvestablepolygons Accessible area of the inventoried plot (default:
#'   \code{\link{harvestableareadefinition}}) (sf polygons data.frame)
#'
#' @param scenario Logging scenario: "RIL1", "RIL2broken", "RIL2", "RIL3",
#'  "RIL3fuel", "RIL3fuelhollow" or "manual"(character) (see the
#'  \code{\link{vignette}})
#'
#' @param winching
#' "0": no cable or grapple (trail to tree foot)
#' "1": only cable (default = 40m)
#' "2": grapple (default = 6m) + cable (grapple priority)
#' If grapple + cable (winching = "2") without fuel wood (fuel = "0")
#'  recovery of the tree foot with grapple if possible (respected grapple
#'  conditions) otherwise with cable with angle to the trail.
#'  Avoidance of future/reserves if chosen.
#'
#' @param MainTrails Main trails defined at the entire harvestable area (sf
#'   polylines)
#'
#' @param resolution Initial résolution of DTM raster (numeric)
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
#' data(MainTrails)
#' data(HarvestablePolygons)
#'
#' AccessPolygones <- filteraccesexplarea(
#'  harvestablepolygons = HarvestablePolygons,
#'  MainTrails = MainTrails,
#'  advancedloggingparameters = loggingparameters())
#'
#'
filteraccesexplarea <- function(
  harvestablepolygons,
  scenario,
  winching = NULL,
  MainTrails,
  resolution,
  advancedloggingparameters = loggingparameters()
){

  # Global Variables
  Harvestable <- Access <- NULL

  # Arguments check

  #### Redefinition of the parameters according to the chosen scenario ####
  scenariosparameters <- scenariosparameters(scenario = scenario, winching = winching)
  winching <- scenariosparameters$winching

  #function

  MainTrails <- MainTrails %>%
    st_buffer(dist = advancedloggingparameters$MaxMainTrailWidth)

  accesspolygones <- harvestablepolygons %>% filter(Harvestable == 1) %>%
    st_union() %>% st_cast("POLYGON") %>% st_as_sf() %>%

  accesspolygones <- accesspolygones %>% filter(as.numeric(st_area(accesspolygones)) > 9 * resolution )

  accesspolygonesMain <- accesspolygones %>%
    st_buffer(dist =  -advancedloggingparameters$ScndTrailWidth/2) %>%
    mutate("Access" = st_intersects(accesspolygones, MainTrails, sparse = FALSE)) %>%
    filter(Access == TRUE) %>%
    st_union() %>% st_buffer(dist =  advancedloggingparameters$ScndTrailWidth/2)




  if (winching != "0") {
    accesspolygonesCable <- accesspolygones  %>%
      st_union()  %>% st_intersection(accesspolygonesMain %>%
                                            st_union() %>%
                                            st_buffer(dist = advancedloggingparameters$CableLength))
    accesspolygones <- accesspolygonesCable
  }

  return(accesspolygones)
}

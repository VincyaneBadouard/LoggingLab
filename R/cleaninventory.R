#' Clean inventory
#'
#' @param inventory Input inventory (see the inputs formats and metadata in the
#'   vignette) (data.frame)
#'
#' @param plotmask Inventoried plot mask (SpatialPolygonsDataFrame)
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'  \code{\link{loggingparameters}} (list)
#'
#' @return The inventory (data.frame) with only alive trees within the inventoried plot.
#'
#' @export
#'
#' @importFrom dplyr filter mutate select left_join
#' @importFrom tibble add_column
#' @importFrom sf st_as_sf st_geometry st_intersection st_set_crs st_crs st_make_valid
#'
#' @examples
#' data(Paracou6_2016) # inventory
#' data(PlotMask) # the inventoried plot mask
#' new <- cleaninventory(Paracou6_2016, PlotMask, loggingparameters(MinDBHValue = 5))
#'
cleaninventory <- function(
  inventory,
  plotmask,
  advancedloggingparameters = loggingparameters()
){

  # Argument check
  if (!inherits(inventory, "data.frame"))
    stop("The 'inventory' argument must be a data.frame")

  if (!inherits(plotmask, "SpatialPolygonsDataFrame"))
    stop("The 'plotmask' argument must be a SpatialPolygonsDataFrame")

  if(!inherits(advancedloggingparameters, "list"))
    stop("The 'advancedloggingparameters' argument must be a list")


  # Global variables
  CodeAlive <- DBH <- Circ <- CircCorr <- idTree <- Xutm <- Yutm <- NULL

  #if DBH (cm) doesn't exist create it
  if (!("DBH" %in% names(inventory)) && ("CircCorr" %in% names(inventory))) {
    inventory <- mutate(inventory, DBH = ifelse(is.na(CircCorr), Circ/pi, CircCorr/pi))
  }

  #### Alive trees and with good minimal DBH ####
  inventory <- inventory %>%
    filter(CodeAlive == "TRUE") %>%
    filter(DBH >= advancedloggingparameters$MinDBHValue) # Default: DBH >= 10, Circ = perimeter of the circle? =! diameter !

  #### Remove trees out of the plot ####

  plotmask <- st_as_sf(plotmask) # sp to sf (sfc_POLYGON)

  inventory_coordinates <- inventory %>%
    select(idTree, Xutm, Yutm)

  inventory <- st_as_sf(inventory, coords = c("Xutm", "Yutm")) %>% # as sf
    st_set_crs(st_crs(plotmask)) # set crs

  TreesIn <- suppressWarnings(st_intersection(sf::st_make_valid(inventory),
                                              sf::st_make_valid(plotmask) # "make valid" to avoid self-intersection
  )) %>%
    tibble::add_column(TreesIn = "1") %>% # trees in the plot
    dplyr::select(idTree, TreesIn)

  sf::st_geometry(TreesIn) <- NULL # sf to data.frame
  TreesIn <- unique(TreesIn) # to be sure

  inventory <- inventory %>%
    dplyr::left_join(TreesIn, by = "idTree") %>%
    dplyr::mutate(TreesIn = ifelse(is.na(TreesIn), "0", TreesIn)) %>%  # and the trees out
    dplyr::filter(TreesIn == "1")

  sf::st_geometry(inventory) <- NULL # sf to data.frame

  inventory <- inventory %>%
    left_join(inventory_coordinates, by = "idTree") # to recover the coordinates lost with the geometry

  return(inventory)

}

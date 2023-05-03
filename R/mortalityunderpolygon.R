#' Trees removed under polygons
#'
#' @param inventory Input inventory (see the inputs formats and metadata in the
#'   vignette) (data.frame) with at least the following columns: - "idTree":
#'   unique ID of the tree - "Xutm": X coordinate of the tree - "Yutm": Y
#'   coordinate of the tree - "Selected": == "1" for the trees selected for
#'   logging.
#'
#' @param polygons Polygon(s) (trails, landings) under which trees are removed.
#'   (sfc_POLYGON or sfc_MULTIPOLYGON **with a crs in UTM**)
#'
#' @param deathcause The cause of the tree death under the polygon(s) among:
#'   "maintrail", "2ndtrail", "treefall2nd", "landing" (character)
#'
#' @return Input inventory (data.frame) with a DeathCause column filled with the
#'   given deathcause.
#'
#' @importFrom sf st_as_sf st_intersection st_make_valid st_union st_crs
#' @importFrom dplyr left_join mutate select
#' @importFrom tibble add_column
#'
#' @export
#'
#' @examples
#'
#' # inventory
#' data(Paracou6_2016)
#' data(DTMParacou)
#' data(SpeciesCriteria)
#' data(MainTrails)
#' data(HarvestableAreaOutputsCable)
#'
#' Paracou6_2016 <- tibble::add_column(Paracou6_2016, DBH= NA) # add a DBH column
#' Paracou6_2016$DBH = Paracou6_2016$CircCorr/pi # and calculate it
#' Paracou6_2016 <- dplyr::filter(Paracou6_2016, DBH >= 10)
#'
#' inventory <- addtreedim(Paracou6_2016,
#'                         volumeparameters = ForestZoneVolumeParametersTable)
#'
#' inventory <- suppressMessages(treeselection(inventory,
#'   topography = DTMParacou,
#'   speciescriteria = SpeciesCriteria,
#'   scenario = "RIL2",
#'   specieslax = FALSE, objectivelax = TRUE,
#'   harvestablearea = HarvestableAreaOutputsCable$HarvestableArea,
#'   plotslope = HarvestableAreaOutputsCable$PlotSlope,
#'   maintrails = MainTrails,
#'   harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons
#'   )$inventory)
#'
#' # polygon
#' data(SecondaryTrails)
#' polygons <- SecondaryTrails$SmoothedTrails
#' deathcause <- "2ndtrail"
#'
#' Rslt <- mortalityunderpolygon(inventory,
#'                               polygons,
#'                               deathcause)
#' library(ggplot2)
#' library(sf)
#' inventory <- st_set_crs(st_as_sf(Rslt, coords = c("Xutm", "Yutm")),
#'                                  st_crs(polygons))
#'
#' DeadTrees <- sf::st_as_sf(
#' dplyr::filter(inventory, DeathCause == deathcause),
#'               coords = c("Xutm", "Yutm"))
#' ggplot() +
#'   geom_sf(data = inventory) +
#'   geom_sf(data = DeadTrees,
#'   aes(colour = "DeadTrees"), show.legend = "point") +
#'   geom_sf(data = polygons,
#'   alpha = 0.5, fill = "red") +
#'   scale_colour_manual(values = c("DeadTrees" = "red")) +
#'   labs(color = "Status")
#'
mortalityunderpolygon <- function(
    inventory,
    polygons,
    deathcause # "maintrail", "2ndtrail", "treefall2nd", "landing"
){

  # Arguments check
  if(!inherits(inventory, "data.frame"))
    stop("The 'inventory' argument of the 'mortalityunderpolygon' function must be data.frame")

  if((!all(c("idTree", "Xutm", "Yutm", "Selected") %in% names(inventory))) |
     (all(is.na(inventory$idTree)) &  all(is.na(inventory$Xutm))
      & all(is.na(inventory$Yutm)) & all(is.na(inventory$Selected)) ) )
    stop("The 'idTree', 'Xutm', 'Yutm' or 'Selected' column is missing or empty in your inventory")

  if(!any(inherits(polygons, "sfc_MULTIPOLYGON") || inherits(polygons, "sfc_POLYGON")))
    stop("The 'polygons' argument of the 'mortalityunderpolygon' function must be a sfc_POLYGON or sfc_MULTIPOLYGON")

  if(is.na(sf::st_crs(polygons)))
    stop("The 'polygons' argument of the 'mortalityunderpolygon' function must have a crs")

  if(!any(deathcause == "maintrail" || deathcause == "2ndtrail" ||
          deathcause == "treefall2nd"|| deathcause == "landing"))
    stop("The 'deathcause' argument of the 'mortalityunderpolygon' function must be
         'maintrail', '2ndtrail', 'treefall2nd', 'landing'")

  # Global variables
  DeathCause <- idTree <- Xutm <- Yutm <- Selected <- NULL

  # Prepare the DeathCause column
  if (!("DeathCause" %in% names(inventory))){
    inventory <- inventory %>%
      tibble::add_column(DeathCause = NA) # if "DeathCause" column doesnt exist create it
  }


  # Trees under the polygons

  DeadTrees <- suppressWarnings(sf::st_intersection(
    sf::st_make_valid(st_set_crs(st_as_sf(inventory, coords = c("Xutm", "Yutm")),
                                 st_crs(polygons))),
    sf::st_make_valid(st_as_sf(polygons) %>% st_union()) # "make valid" to avoid self-intersection
  )) %>%
    tibble::add_column(DeadTrees = "1") %>%
    dplyr::select(idTree, DeadTrees) %>% sf::st_drop_geometry() # remove geometry column if there is (sf to data.frame)
  # sf::st_geometry(DeadTrees) <- NULL # remove geometry column if there is (sf to data.frame)
  DeadTrees <- unique(DeadTrees)

  inventory <- inventory %>%
    dplyr::left_join(DeadTrees, by = "idTree") %>%
    dplyr::mutate(DeathCause = ifelse(is.na(DeathCause) & Selected != "1" & DeadTrees == "1",
                                      deathcause, DeathCause)) %>%  # Damage trees
    dplyr::select(-DeadTrees)

  # length(which(inventory$DeathCause == deathcause))

  return(inventory)

}

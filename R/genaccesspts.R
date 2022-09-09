#' Generate access points for machine
#'
#' @description Accessory function to generate access point according to operating-machine area.
#'
#'
#' @param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'  or SRTM) (\code{\link{DTMParacou}}) (RasterLayer **with a crs in UTM**)
#'
#' @param machinepolygons Accessible zones for machines of the inventoried plot
#'  (default: \code{\link{harvestableareadefinition}}) (sf polygons data.frame)
#'
#' @param maintrails Main trails defined at the entire harvestable area (sf
#'   linestring **with a crs in UTM**)
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @return list of 2 elements :
#'  - *PartMainTrails* available area to draw access points of maintrail for each PU (prospection
#'    unit) (sf or sfc)
#'  - *AccessPointAll* a draw of access points of maintrail for each PU (prospection
#'    unit) (sf or sfc)
#'
#' @importFrom sf st_cast st_as_sf st_as_sfc st_intersection st_union st_sample st_join
#'   st_buffer as_Spatial st_centroid st_set_precision st_make_valid st_set_agr
#'   st_geometry sf_use_s2 st_geometry<-
#'   st_drop_geometry
#'
#' @importFrom dplyr mutate row_number select filter
#'   arrange desc
#'
#' @importFrom raster res
#'
#' @examples
#'
#' \dontrun{
#' set.seed(1)
#' data(MainTrails)
#' data(HarvestableAreaOutputsCable)
#' data(DTMParacou)
#'
#' accesspts <- genaccesspts(topography = DTMParacou,
#' machinepolygons = HarvestableAreaOutputsCable$MachinePolygons,
#' maintrails = MainTrails,
#' advancedloggingparameters = loggingparameters())
#'}
#'
genaccesspts <- function(topography,
                         machinepolygons,
                         maintrails,
                         advancedloggingparameters = loggingparameters()){

  # Arguments check

  if(!inherits(topography, "RasterLayer"))
    stop("The 'topography' argument of the 'genaccesspts' function must
         be RasterLayer")

  ##################################

  suppressMessages(sf_use_s2(FALSE)) # to deal with actual unresolved s2 issues in sf ("Spherical geometry (s2) switched off")

  factagg <-  floor(advancedloggingparameters$SlopeDistance/res(topography)[1])

  ##################################

  AccessPolygons <- machinepolygons

  maintrailsRed <- maintrails %>% st_as_sfc() %>%
    st_cast("POLYGON") %>% st_buffer(-2*factagg) %>%
    st_cast("LINESTRING") %>% st_buffer(1)


  AccessMainTrails <- AccessPolygons  %>% st_union() %>%
    st_cast("POLYGON", warn = FALSE) %>%
    st_as_sf() %>%
    mutate(ID = paste0("ID_",row_number()))


  # Generate intersections between accessible area and maintrails (ID = accessible area index)
  PartMainTrails <- st_intersection(st_geometry(maintrailsRed),
                                    st_geometry(AccessMainTrails)) %>%
    st_union(by_feature = T) %>%
    st_cast("MULTIPOLYGON", warn = FALSE)  %>% # "In st_cast.MULTIPOLYGON(X[[i]], ...) : polygon from first part only"
    st_as_sf() %>%
    st_set_agr(value = "constant") %>%
    st_join(AccessMainTrails)%>%
    st_make_valid()

  PartMainTrails <- PartMainTrails %>% arrange(desc(st_area(PartMainTrails))) %>%
    filter(duplicated(PartMainTrails$ID) == FALSE) %>%
    st_make_valid()


  # Generate point access in the intersections between accessible area and maintrails (ID = accessible area index)
  AccessPointAll <- PartMainTrails %>%
    st_sample(rep(1,dim(PartMainTrails)[1]) ,type = "random", by_polygon=TRUE) %>% as_Spatial() %>%
    st_as_sf() %>%
    mutate(idTree = NA) %>% st_join(PartMainTrails) %>%
    st_make_valid()

  return(list("PartMainTrails" = PartMainTrails,
              "AccessPointAll" = AccessPointAll))
}

#' Trails smoothing
#'
#'@param paths Raw secondary trails polygons (sp polylines)
#'
#'@param plotmask Inventoried plot mask (SpatialPolygonsDataFrame)
#'
#'@param verbose return message on second trails density criteria (boolean)
#'
#'
#'@param advancedloggingparameters Other parameters of the logging simulator
#'  \code{\link{loggingparameters}} (list)
#'
#'
#'@importFrom  sf st_as_sf st_buffer st_union st_geometry st_area st_difference
#'  st_combine st_make_valid
#'@importFrom smoothr smooth
#'
#'@return A list with :
#'   1) SmoothedTrails: Smoothed trails polygons;
#'   2) Smoothed second trails polygons;
#'   3) Smoothed main trails polygons;
#'   4) smoothed trail density.
#'
#'
#'
#' @examples
#' \dontrun{
#' data(SecondaryTrails)
#' SecondTrailsSmth <- smoothtrails(paths = SecondaryTrails$RawSecondTrails,
#'                                  plotmask = PlotMask,
#'                                    )
#'         }
#'
smoothtrails <- function(
  paths,
  plotmask,
  verbose = FALSE,
  advancedloggingparameters = loggingparameters()
){

  # Global Variables
  n.overlaps <- NULL

  if ( class(paths) == "SpatialLines") {
  RawSecondTrails <- paths %>%
    st_as_sf() %>% st_make_valid() %>% filter(!st_is_empty(paths %>%
                                        st_as_sf()))

  SmoothedSecondTrails <- RawSecondTrails %>% st_intersection() %>% filter(n.overlaps < 20) %>% st_make_valid()%>%
      st_union() %>% st_cast("LINESTRING") %>% st_make_valid() %>%
      smoothr::smooth(method = "ksmooth", smoothness = advancedloggingparameters$SmoothingFact) %>%
      st_buffer(dist = advancedloggingparameters$ScndTrailWidth/2)  %>% st_make_valid() %>%
      st_union()

    RawMainTrails <- RawSecondTrails %>% st_intersection() %>% filter(
      st_geometry_type(.)
      %in% c("POLYGON", "LINESTRING") ) %>% filter(n.overlaps >= 20) %>% st_make_valid()

    if (dim(RawMainTrails %>% st_as_sf() %>% filter(!st_is_empty(RawMainTrails %>% st_as_sf())))[1]>0) {
      RawMainTrails <- RawSecondTrails %>% st_intersection() %>% filter(n.overlaps >= 20) %>%
        st_union()  %>%  st_cast("LINESTRING") %>%
        st_union() %>% as_Spatial()

      SmoothedMainTrails <- RawMainTrails %>% st_as_sf()  %>% st_make_valid()%>%
        st_union() %>% st_cast("LINESTRING") %>% st_make_valid() %>%
        smoothr::smooth(method = "ksmooth", smoothness = advancedloggingparameters$SmoothingFact) %>%
        st_buffer(dist = advancedloggingparameters$MaxMainTrailWidth/2)  %>% st_make_valid() %>%
        st_union()

      SmoothedTrails <- st_union(SmoothedSecondTrails,SmoothedMainTrails)
    }else{
      SmoothedMainTrails <- NULL

      SmoothedTrails <- SmoothedSecondTrails
    }


    TrailsDensity <- (SmoothedTrails  %>% st_intersection(plotmask %>% st_as_sf()) %>% st_area / advancedloggingparameters$ScndTrailWidth)/(plotmask %>% st_as_sf() %>% st_area() /10000)
    if (verbose) {
      if (as.numeric(TrailsDensity) <= 200) {
        message(paste0("The second trails density criteria is validated (", round(TrailsDensity, digits = 1)," m/ha <= 200 m/ha)"))
      }else{
        message(paste0("The second trails density criteria is NOT validated (", round(TrailsDensity, digits = 1)," m/ha > 200 m/ha)"))
      }
    }



    smoothedtrails <- list("SmoothedTrails" = SmoothedTrails,
                           "SmoothedSecondTrails" = SmoothedSecondTrails,
                           "SmoothedMainTrails" = SmoothedMainTrails,
                           "TrailsDensity" = TrailsDensity)

    return(smoothedtrails)
  }else{

    smoothedtrails <- list("SmoothedTrails" = SmoothedSecondTrails %>% st_buffer(dist = advancedloggingparameters$MaxMainTrailWidth/2),
                           "SmoothedSecondTrails" = SmoothedSecondTrails %>% st_buffer(dist = advancedloggingparameters$MaxMainTrailWidth/2),
                           "SmoothedMainTrails" = NULL,
                           "TrailsDensity" = 0)

    return(smoothedtrails)
  }






}

#' Second trails adjusted
#'
#' @param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'  or SRTM) (\code{\link{DTMParacou}}) (RasterLayer)
#'
#' @param plotmask Inventoried plot mask (SpatialPolygonsDataFrame)
#'
#' @param maintrails Main trails defined at the entire harvestable area (sf
#'   linestring)
#'
#' @param inventory Input inventory with new columns:
#'- The tree felling success or fail("TreeFellingOrientationSuccess")
#'- The crowns of the future/reserve trees (Polygon)
#'- The fallen trees ("TreePolygon"): a MULTIPOLYGON of the tree oriented
#'   according to the chosen scenario
#'- The dead trees under felled trees (DeathCause = "treefall2nd")
#'
#' @param harvestablepolygons Accessible area of the inventoried plot
#'  (default: \code{\link{harvestableareadefinition}}) (sfc_MULTIPOLYGON)
#'
#' @param machinepolygons Accessible for machine area of the inventoried plot
#'  (default: \code{\link{harvestableareadefinition}}) (sf polygons data.frame)
#'
#' @param accesspts Random access point of maintrail for each PU
#'   (sfc_MULTIPOLYGON)
#'
#' @param plotslope Slopes (in radians) of the inventoried plot (with a
#'  neighbourhood of 8 cells) (default:
#'  \code{\link{HarvestableAreaOutputsCable}}) (RasterLayer)
#'
#'@param scenario Logging scenario: "RIL1", "RIL2broken", "RIL2", "RIL3",
#'  "RIL3fuel", "RIL3fuelhollow" or "manual"(character) (see the
#'  \code{\link{vignette}})
#'
#'@param winching
#' "0": no cable or grapple (trail to tree foot)
#' "1": only cable (default = 40m)
#' "2": grapple (default = 6m) + cable (grapple priority)
#' If grapple + cable (winching = "2") without fuel wood (fuel = "0")
#'  recovery of the tree foot with grapple if possible (respected grapple
#'  conditions) otherwise with cable with angle to the trail.
#'  Avoidance of future/reserves if chosen.
#'
#'@param verbose Allow to provide messages from internal functions (boolean)
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @return A list with :
#' - inventory: Updated inventory
#' - SmoothedTrails: Smoothed main and secondary trails polygons
#' (sfc_MULTIPOLYGON)
#' - TrailsDensity: Second trails density (in m/ha)
#' - TrailsIdentity: information on sections of the trails (matrix) with:
#'     - LineID:
#'     - LoggedTrees: idTree of trees reached by the trails
#'     - TypeExpl: type of winching
#' - MainTrailsAccess : Random access point of maintrail for each PU
#' (sfc_MULTIPOLYGON)
#' - RawSecondTrails : non-smoothed secondary trails (SpatialLines)
#' - CostRasterAgg: A cost Raster (RasterLayer)
#'
#' @importFrom sf st_cast st_as_sf st_intersection st_union st_sample st_join
#'   st_buffer as_Spatial st_centroid st_set_precision st_make_valid st_set_agr
#'   st_geometry st_area st_is_empty st_set_crs st_crs sf_use_s2 st_geometry<-
#'   st_as_sfc
#' @importFrom dplyr mutate row_number select as_tibble left_join if_else filter
#'   arrange desc
#' @importFrom raster raster extend extent focal res crs mask crop rasterize
#'   rasterToPoints rasterToPolygons rasterFromXYZ aggregate values ncell
#'   values<-
#' @importFrom sp proj4string<- coordinates<-
#'
#' @importFrom lwgeom  st_snap_to_grid
#'
#' @importFrom smoothr smooth
#'
#'@importFrom gdistance transition geoCorrection
#'@importFrom raster adjacent aggregate resample ncol ncell
#'@importFrom utils txtProgressBar setTxtProgressBar
#'@importFrom stats na.exclude
#'
#' @export
#'
#' @examples
#' data(DTMParacou)
#' data(PlotMask)
#' data(HarvestableAreaOutputsCable)
#' data(SecondaryTrails)
#'
#' scenario <- "manual"
#' winching <- "2"
#' fuel <- "0"
#' directionalfelling <- "0"
#'
#' PostLogInventory <- treefelling(SecondaryTrails$inventory, scenario = "manual",
#' fuel = fuel,
#' winching = winching,
#' directionalfelling = directionalfelling,
#' maintrailsaccess = SecondaryTrails$MainTrailsAccess,
#' scndtrail = SecondaryTrails$SmoothedTrails,
#' advancedloggingparameters = loggingparameters())
#'
#' ScdTrailsAdj <- secondtrailsadjusted(
#'   topography = DTMParacou,
#'   plotmask = PlotMask,
#'   maintrails = MainTrails,
#'   plotslope = HarvestableAreaOutputsCable$PlotSlope,
#'   harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons,
#'   machinepolygons = HarvestableAreaOutputsCable$MachinePolygons,
#'   inventory = PostLogInventory,
#'   accesspts = SecondaryTrails$MainTrailsAccess,
#'   scenario = "manual",
#'   winching = winching,
#'   advancedloggingparameters = loggingparameters())
#'
#'
#' library(ggplot2)
#' library(sf)
#'
#' NewInventory <- PostLogInventory
#' NewInventory_crs <- PostLogInventory %>%
#' getgeometry(TreePolygon) %>%
#' sf::st_set_crs(sf::st_crs(MainTrails)) # set a crs
#'
#'
#' Harvestable <- sf::st_as_sf(
#' dplyr::filter(NewInventory, LoggingStatus == "harvestable"),
#' coords = c("Xutm", "Yutm")) %>%
#' st_set_crs(st_crs(MainTrails))
#'
#' HarvestableUp <- sf::st_as_sf(
#' dplyr::filter(NewInventory, LoggingStatus == "harvestableUp"),
#' coords = c("Xutm", "Yutm")) %>%
#' st_set_crs(st_crs(MainTrails))
#'
#' Selected <- sf::st_as_sf(
#' dplyr::filter(NewInventory, Selected == "1"), coords = c("Xutm", "Yutm")) %>%
#' st_set_crs(st_crs(MainTrails))
#'
#' Reserve <- sf::st_as_sf(
#' dplyr::filter(NewInventory, LoggingStatus == "reserve"),
#' coords = c("Xutm", "Yutm")) %>%
#' st_set_crs(st_crs(MainTrails))
#'
#' Future <- sf::st_as_sf(
#' dplyr::filter(NewInventory, LoggingStatus == "future"),
#' coords = c("Xutm", "Yutm")) %>%
#' st_set_crs(st_crs(MainTrails))
#'
#' ProbedHollow <- sf::st_as_sf(
#' dplyr::filter(NewInventory, ProbedHollow == "1"),
#' coords = c("Xutm", "Yutm")) %>%
#' st_set_crs(st_crs(MainTrails))
#'
#' VisibleDefect <- sf::st_as_sf(
#' dplyr::filter(NewInventory, VisibleDefect == "1"),
#' coords = c("Xutm", "Yutm")) %>%
#' st_set_crs(st_crs(MainTrails))
#'
#'
#' ggplot() +
#'   # Harvestable zones
#'   geom_sf(data = HarvestableAreaOutputsCable$HarvestablePolygons,
#'         fill = "olivedrab", alpha = 0.1) +
#'    geom_sf(data = HarvestableAreaOutputsCable$MachinePolygons,
#'         fill = "olivedrab", alpha = 0.5) +
#'   labs(alpha = "Harvestable") +
#'   labs(title = "P6 zones exploitables") +
#'
#'   geom_sf(data = VisibleDefect,
#'   aes(colour = "Visible defect"), show.legend = "point") +
#'   geom_sf(data = Future,
#'   aes(colour = "Future"), show.legend = "point", size = 4) +
#'   geom_sf(data = Reserve,
#'   aes(colour = "Reserve"), show.legend = "point", size = 4) +
#'   geom_sf(data = Harvestable,
#'   aes(colour = "Harvestable"), show.legend = "point", size = 4) +
#'   geom_sf(data = HarvestableUp,
#'   aes(colour = "HarvestableUp"), show.legend = "point", size = 4) +
#'   geom_sf(data = Selected,
#'   aes(colour = "Selected"), show.legend = "point") +
#'   geom_sf(data = ProbedHollow,
#'   aes(colour = "Probed hollow"), show.legend = "point") +
#'
#'     geom_sf(data = NewInventory_crs, # cuted trees
#'     alpha = 0.5, fill = "red") +
#'
#'   # 2ndary trails
#'     geom_sf(data = st_as_sf(SecondaryTrails$SmoothedTrails),
#'     aes(color = "Initial-trails"),alpha = 0.5) +
#'     geom_sf(data = st_as_sf(SecondaryTrails$RawSecondTrails),
#'     color = "green",alpha = 0.5) +
#'
#'   # 2ndary trails adjusted
#'     geom_sf(data = st_as_sf(ScdTrailsAdj$SmoothedTrails),
#'     aes(color = "Adjusted-trails"),alpha = 0.5) +
#'     geom_sf(data = st_as_sf(ScdTrailsAdj$RawSecondTrails),
#'     color = "red",alpha = 0.5) +
#'
#'     scale_colour_manual(values = c("Visible defect" = "pink",
#'     "Harvestable" = "skyblue",
#'   "HarvestableUp" = "blue", "Selected" = "red", "Future" = "orange",
#'   "Reserve" = "purple", "Probed hollow" = "forestgreen",
#'    "Harvestable area" = "olivedrab", "Initial-trails" = "darkgreen" ,
#'    "Adjusted-trails" = "darkred"))
#'
#' ScdTrailsAdj$TrailsIdentity
#'
secondtrailsadjusted <- function(
  topography,
  plotmask,
  maintrails,
  plotslope,
  harvestablepolygons,
  machinepolygons,
  inventory, # à mettre en 1er arg
  accesspts = NULL,
  scenario,
  winching = NULL,
  verbose = FALSE,
  advancedloggingparameters = loggingparameters()
){

  # Arguments check

  if(!inherits(inventory, "data.frame"))
    stop("The 'inventory'argument of the 'secondtrailsadjusted' function must be data.frame")

  if(!inherits(plotmask, "SpatialPolygonsDataFrame"))
    stop("The 'plotmask' argument of the 'secondtrailsadjusted' function must be
         SpatialPolygonsDataFrame")

  # if(!any(unlist(lapply(list(maintrails), inherits, "sf" ))))
  #   stop("The 'maintrails' argument of the 'secondtrailsopening' function must be sf polygon")

  if(!inherits(topography, "RasterLayer"))
    stop("The 'topography' argument of the 'secondtrailsadjusted' function must
         be RasterLayer")

  # Options
  options("rgdal_show_exportToProj4_warnings"="none") # to avoid gdal warnings


  # Global Variables
  slope <- x <- y <- Harvestable <- idTree <- ID <- type <- ptAcc  <- NULL
  EstCost <- n.overlaps <- TypeAcc <- IDpts <- Logged <- AccessPolygons <- NULL
  Selected <- DeathCause <- ID_Acc <- isEmpty <- gprlAcc <- cblAcc <- NULL
  LoggingStatus <- TreePolygon <- DBH <- ID.y <- IdPU <- NULL
  IdPU.y <- IdPU.x <- NULL


  #### Redefinition of the parameters according to the chosen scenario ####
  scenariosparameters <- scenariosparameters(scenario = scenario, winching = winching)
  WinchingInit <- scenariosparameters$winching
  winching <- WinchingInit

  ##################################

  sf_use_s2(FALSE) # to deal with actual unresolved s2 issues in sf

  CostMatrix <- advancedloggingparameters$CostMatrix

  factagg <-  floor(advancedloggingparameters$SlopeDistance/res(topography)[1])

  # Transformation of the DTM so that the maintrails are outside the plot


  DTMExtExtended <- raster::extend(topography, c(factagg,factagg)) # extend the extent

  fill.boundaries <- function(x) {
    center = 0.5 + (factagg*factagg/2)
    if( is.na(x)[center] ) {
      return( round(mean(x, na.rm=T),5) )
    } else {
      return( x[center] )
    }
  }

  DTMExtended <- raster::focal(DTMExtExtended,
                               matrix(1,factagg,factagg),
                               fun=fill.boundaries,
                               na.rm=F, pad=T)

  # Generate accessible area from HarvestablePolygones and winching choice

  AccessPolygons <- machinepolygons

  # Generate accessible area from HarvestablePolygones and winching == "0"

  if (!is.null(accesspts)) {
    AccessMainTrails <- AccessPolygons  %>% st_union() %>%
      st_cast("POLYGON") %>%
      st_as_sf() %>% st_join(accesspts) %>%
      select(ID)

    PartMainTrails <- st_intersection(st_geometry(maintrails %>%
                                                    st_buffer(dist = 2*factagg)),
                                      st_geometry(AccessMainTrails %>%
                                                    st_buffer(dist = -0.5*factagg))) %>%
      st_union(by_feature = T) %>%
      st_buffer(dist = 0.5) %>%
      st_intersection(st_as_sf(plotmask) %>% st_union()) %>%
      st_cast("MULTIPOLYGON")  %>%
      st_as_sf() %>%
      st_set_agr(value = "constant") %>%
      st_join(AccessMainTrails)
    PartMainTrails <- PartMainTrails %>% arrange(desc(st_area(PartMainTrails))) %>%
      filter(duplicated(PartMainTrails$ID) == FALSE)

    AccessPointAll <- accesspts
  }else{

    AccessMainTrails <- AccessPolygons  %>% st_union() %>%
      st_cast("POLYGON") %>%
      st_as_sf() %>%
      mutate(ID = paste0("ID_",row_number()))


    # Generate intersections between accessible area and maintrails (ID = accessible area index)
    PartMainTrails <- st_intersection(st_geometry(maintrails %>%
                                                    st_buffer(dist = 2*factagg)),
                                      st_geometry(AccessMainTrails %>%
                                                    st_buffer(dist = -0.5*factagg))) %>%
      st_union(by_feature = T) %>%
      st_buffer(dist = 0.5) %>%
      st_intersection(st_as_sf(plotmask) %>% st_union()) %>%
      st_cast("MULTIPOLYGON")  %>%
      st_as_sf() %>%
      st_set_agr(value = "constant") %>%
      st_join(AccessMainTrails)
    PartMainTrails <- PartMainTrails %>% arrange(desc(st_area(PartMainTrails))) %>%
      filter(duplicated(PartMainTrails$ID) == FALSE)


    # Generate point access in the intersections between accessible area and maintrails (ID = accessible area index)
    AccessPointAll <- PartMainTrails %>%
      st_sample(rep(1,dim(PartMainTrails)[1]) ,type = "random", by_polygon=TRUE) %>% as_Spatial() %>%
      st_as_sf() %>%
      mutate(idTree = NA) %>% st_join(PartMainTrails)
  }












  # Generate spatial objects from inventory

  # Points vector with coordinates of the harvestable trees:
  HarvestableTreesPoints <- inventory %>%
    filter(LoggingStatus == "harvestable"|LoggingStatus == "harvestableUp"|LoggingStatus == "harvestable2nd") # harvestableUp = DBH > MinFD individuals, harvestable2nd = eco2 individuals is specieslax



  if (dim(HarvestableTreesPoints)[1] != 0) {

    HarvestableTreesPoints <- st_as_sf(HarvestableTreesPoints, coords = c("Xutm", "Yutm"))%>%
      sf::st_set_crs(sf::st_crs(topography)) # sp::coordinates(HarvestableTreesPoints) <- ~ Xutm + Yutm

    # sp::proj4string(HarvestableTreesPoints) <- raster::crs(topography)

  } else {HarvestableTreesPoints = st_point(x = c(NA_real_, NA_real_))} # empty

  # Points vector with coordinates of the selected trees:
  SelectedTreesPoints <- inventory %>%
    filter(Selected == "1")

  if (dim(SelectedTreesPoints)[1] != 0) {

    SelectedTreesPoints <- st_as_sf(SelectedTreesPoints, coords = c("Xutm", "Yutm"))%>%
      sf::st_set_crs(sf::st_crs(topography)) # sp::coordinates(SelectedTreesPoints) <- ~ Xutm + Yutm

    # sp::proj4string(SelectedTreesPoints) <- raster::crs(topography)

  } else {SelectedTreesPoints = st_point(x = c(NA_real_, NA_real_))} # empty

  inventory_sf <- inventory %>%
    filter(Selected == "1") %>% st_as_sf(coords = c("Xutm", "Yutm")) %>%
    sf::st_set_crs(sf::st_crs(topography)) %>% # set a crs
    mutate("CrownGeom" = st_point(x = c(NA_real_, NA_real_)) %>% st_as_text())
  inventory_Tr <- inventory %>%  getgeometry(TreePolygon)

  Trunks <- st_cast(inventory_Tr$TreePolygon , "POLYGON")[seq(1, by = 2, len = nrow(inventory))]


  Trunks <- Trunks %>% sf::st_set_crs(sf::st_crs(topography))
  inventory_Tr <- inventory_sf

  for (h in 1:nrow(inventory)) {
    if (!st_is_empty(Trunks[h])) {
      inventory_Tr$CrownGeom[h] <- st_difference(Trunks[h],inventory_sf[h,] %>%
                                                              st_buffer(dist = (inventory_sf$TrunkHeight[h]-0.05))) %>%
        st_centroid() %>% st_union()%>% st_as_text()
    }

  }

  Trunks <- Trunks %>%
    sf::st_set_crs(sf::st_crs(topography))%>%
    st_buffer(dist = 0.1) %>% st_as_sf()%>% st_join(SelectedTreesPoints) %>% dplyr::select(idTree)


  # Points vector with coordinates of the selected trees:
  SelectedCrownsPoints <- st_as_sfc(inventory_Tr$CrownGeom) %>%
    sf::st_set_crs(sf::st_crs(topography)) %>%  st_as_sf() %>% st_join(Trunks)

  SelectedCrownsPoints <- SelectedCrownsPoints %>% filter(!st_is_empty(SelectedCrownsPoints))


  # Points vector with coordinates of the future trees:
  FutureTreesPoints <- inventory %>%
    filter(LoggingStatus == "future")

  if (dim(FutureTreesPoints)[1] != 0) {

    FutureTreesPoints <-  st_as_sf(FutureTreesPoints, coords = c("Xutm", "Yutm"))%>%
      sf::st_set_crs(sf::st_crs(topography)) # sp::coordinates(FutureTreesPoints) <- ~ Xutm + Yutm

    # sp::proj4string(FutureTreesPoints) <- raster::crs(topography)

  } else {FutureTreesPoints = st_point(x = c(NA_real_, NA_real_))} # empty

  # Points vector with coordinates of the reserve trees:
  ReserveTreesPoints <- inventory %>%
    filter(LoggingStatus == "reserve")

  if (dim(ReserveTreesPoints)[1] != 0) {

    ReserveTreesPoints <- st_as_sf(ReserveTreesPoints, coords = c("Xutm", "Yutm"))%>%
      sf::st_set_crs(sf::st_crs(topography)) # sp::coordinates(ReserveTreesPoints) <- ~ Xutm + Yutm

    # sp::proj4string(ReserveTreesPoints) <- raster::crs(topography)

  } else {ReserveTreesPoints = st_point(x = c(NA_real_, NA_real_))} # empty

  # Points vector with coordinates of the big trees (DBH >= 50 cm):
  BigTreesPoints <- inventory %>%
    filter(DBH >= advancedloggingparameters$BigTrees) #  & (Selected != "1" & LoggingStatus != "harvestable") & LoggingStatus != "harvestableUp" & LoggingStatus != "harvestable2nd"

  if (dim(BigTreesPoints)[1] != 0) {


    sp::coordinates(BigTreesPoints) <- ~ Xutm + Yutm

    sp::proj4string(BigTreesPoints) <- crs(topography)

    BigTreesPoints <- BigTreesPoints %>% st_as_sf()

  } else {BigTreesPoints = st_point(x = c(NA_real_, NA_real_))}

  treeselectionoutputs  <- list(inventory = inventory ,
                                HarvestableTreesPoints = HarvestableTreesPoints,
                                SelectedTreesPoints = SelectedTreesPoints,
                                SelectedCrownsPoints = SelectedCrownsPoints,
                                FutureTreesPoints = FutureTreesPoints,
                                ReserveTreesPoints = ReserveTreesPoints,
                                BigTreesPoints = BigTreesPoints)


  # Generate Cost raster --> cf CostMatrix
  CostRaster <- raster(extent(DTMExtended),resolution = res(DTMExtended), crs = crs(DTMExtended))
  values(CostRaster) <- CostMatrix[[2]][[1]]$CostValue
  CostRaster <- mask(CostRaster, plotmask)
  CostRaster <- crop(CostRaster, plotmask)




  #Generate weight according to slope conditions
  # RastersToPoints

  plotslopePoint <- as_tibble(rasterToPoints(plotslope))

  CostRasterPoint <- as_tibble(rasterToPoints(CostRaster))

  # left join par x et y
  plotTib <-
    left_join(plotslopePoint, CostRasterPoint, by = c('x', 'y'))

  CostSlopeRaster <- plotTib %>%
    mutate(Harvestable = if_else(
      slope <= atan(CostMatrix[[1]][[1]]$Slope/100),
      true = CostMatrix[[1]][[1]]$Cost,
      false = if_else(
        slope <= atan(CostMatrix[[1]][[2]]$Slope/100),
        true = CostMatrix[[1]][[2]]$Cost,
        false = if_else(
          slope <= atan(CostMatrix[[1]][[3]]$Slope/100),
          true = CostMatrix[[1]][[3]]$Cost,
          false = if_else(
            slope <= atan(CostMatrix[[1]][[4]]$Slope/100),
            true = CostMatrix[[1]][[4]]$Cost,
            false = if_else(
              slope <= atan(CostMatrix[[1]][[5]]$Slope/100),
              true = CostMatrix[[1]][[5]]$Cost,
              false = CostMatrix[[1]][[6]]$Cost)
          )
        )
      )
    )) %>% dplyr::select(x,y,Harvestable)

  CostRaster <- rasterFromXYZ(CostSlopeRaster, crs = crs(DTMExtended)) # Update weights from plotTib tibble

  #Aggregation each raster to selected resolution
  CostRasterMean <- aggregate(CostRaster, fact=factagg, fun=mean)
  CostRasterMean <- crop(CostRasterMean,  CostRaster)
  CostRasterMean <- mask(CostRasterMean, plotmask)

  DTMExtended <- crop(DTMExtended,  CostRasterMean)
  DTMExtended <- mask(DTMExtended, plotmask)

  DTMmean <- aggregate(DTMExtended, fact=factagg, fun=mean)
  DTMmean <- crop(DTMmean,  CostRasterMean)
  DTMmean <- mask(DTMmean, plotmask)

  #Generate protection buffer for Big Trees (dist : ScndTrailWidth/2 + 2 m)



  BigTreesRaster <- raster(extent(DTMmean),resolution = res(DTMmean), crs = crs(DTMmean))
  values(BigTreesRaster) <- 0
  BigTreesRaster <- raster::rasterize(x = as_Spatial(treeselectionoutputs$BigTreesPoints %>%
                                                       st_buffer(dist = advancedloggingparameters$ScndTrailWidth/2 + 2) ),
                                      y = BigTreesRaster,
                                      field = CostMatrix[[2]][[3]]$CostValue,
                                      update = TRUE)
  BigTreesRaster <- crop(BigTreesRaster, CostRasterMean)

  #Generate protection buffer for Reserve Trees (dist : ScndTrailWidth/2 + 2 m)

  ReserveRaster <- raster(extent(DTMmean),resolution = res(DTMmean), crs = crs(DTMmean))
  values(ReserveRaster) <- 0
  ReserveRaster <- raster::rasterize(x = as_Spatial(treeselectionoutputs$HarvestableTreesPoints %>%
                                                      st_buffer(dist = advancedloggingparameters$ScndTrailWidth/2 + 2) ),
                                     y = ReserveRaster,
                                     field = CostMatrix[[2]][[4]]$CostValue,
                                     update = TRUE)
  ReserveRaster <- crop(ReserveRaster, CostRasterMean)

  #Generate protection buffer for Futures Trees (dist : ScndTrailWidth/2 + 2 m)
  FutureRaster <- raster(extent(DTMmean),resolution = res(DTMmean), crs = crs(DTMmean))
  values(FutureRaster ) <- 0
  FutureRaster  <- raster::rasterize(x = as_Spatial(treeselectionoutputs$HarvestableTreesPoints %>%
                                                      st_buffer(dist =  advancedloggingparameters$ScndTrailWidth/2 +2) ),
                                     y = FutureRaster  ,
                                     field = CostMatrix[[2]][[5]]$CostValue,
                                     update = TRUE)
  FutureRaster  <- crop(FutureRaster, CostRasterMean)

  #Update Cost Raster with protection buffers

  #Generate protection buffer for selected Trees (dist : ScndTrailWidth/2 + 2 m)
  SelectedRaster <- raster(extent(DTMmean),resolution = res(DTMmean), crs = crs(DTMmean))
  values(SelectedRaster ) <- 0
  SelectedRaster  <- raster::rasterize(x = as_Spatial(treeselectionoutputs$SelectedTreesPoints %>%
                                                        st_buffer(dist = 2) ),
                                       y = SelectedRaster,
                                       field = CostMatrix[[2]][[3]]$CostValue/2,
                                       update = TRUE)
  SelectedRaster <- crop(SelectedRaster, CostRasterMean)

  #Update Cost Raster with protection buffers

  CostRasterMean <- CostRasterMean + BigTreesRaster + ReserveRaster + FutureRaster + SelectedRaster



  #Generate maintrail intersection cost raster
  CostRasterMean <- raster::rasterize(x = as_Spatial(AccessPointAll %>% st_buffer(dist = advancedloggingparameters$ScndTrailWidth+2)),
                                      y = CostRasterMean ,
                                      field = CostMatrix[[2]][[6]]$CostValue,
                                      update = TRUE)

  #Generate Slope accessibility for grapple machine
  if (winching == "2") {

    plotslopePointGrpl <- as_tibble(rasterToPoints(plotslope))

    CostRasterPointGrpl <- as_tibble(rasterToPoints(CostRaster))

    # left join par x et y
    plotTibGrpl <-
      left_join(plotslopePointGrpl, CostRasterPointGrpl, by = c('x', 'y'))

    CostSlopeRasterGrpl <- plotTibGrpl %>%
      mutate(Harvestable = if_else(
        slope <= atan(advancedloggingparameters$GrappleMaxslope/100),
        true = 0,
        false = Inf
      )) %>% dplyr::select(x,y,Harvestable)

    CostRasterGrpl <- rasterFromXYZ(CostSlopeRasterGrpl, crs = crs(CostRasterMean))

    PolygonGrpl <- rasterToPolygons(x = CostRasterGrpl,
                                    n = 8,
                                    dissolve = TRUE)

    CostRasterMeanGrpl <- raster::rasterize(x = PolygonGrpl[PolygonGrpl$Harvestable == Inf,],
                                            y = CostRasterMean ,
                                            field = Inf,
                                            update = TRUE)

    CostRasterMeanGrpl <- raster::rasterize(x = as_Spatial(AccessPointAll %>% st_buffer(dist = advancedloggingparameters$ScndTrailWidth+2)),
                                            y = CostRasterMeanGrpl ,
                                            field = CostMatrix[[2]][[6]]$CostValue,
                                            update = TRUE)


    #Generate accessible weights raster
    AccessRaster <- raster(extent(CostRasterMeanGrpl),resolution = res(CostRasterMeanGrpl), crs = crs(CostRasterMeanGrpl))
    values(AccessRaster) <- CostMatrix[[2]][[2]]$CostValue

    AccessRaster <- raster::rasterize(x = as_Spatial(machinepolygons),
                                      y = AccessRaster ,
                                      field = 0,
                                      update = TRUE)
    AccessRaster <- crop(AccessRaster,  CostRaster)
    AccessRaster <- mask(AccessRaster, plotmask)


    #Update Cost Raster with accessible weights raster

    CostRasterMeanGrpl <- CostRasterMeanGrpl + AccessRaster


  }

  #Generate accessible weights raster
  AccessRaster <- raster(extent(CostRasterMean),resolution = res(CostRasterMean), crs = crs(CostRasterMean))
  values(AccessRaster) <- CostMatrix[[2]][[2]]$CostValue

  AccessRaster <- raster::rasterize(x = as_Spatial(machinepolygons),
                                    y = AccessRaster ,
                                    field = 0,
                                    update = TRUE)
  AccessRaster <- crop(AccessRaster,  CostRaster)
  AccessRaster <- mask(AccessRaster, plotmask)


  #Update Cost Raster with accessible weights raster

  CostRasterMean <- CostRasterMean + AccessRaster


  #Compute conductance raster
  CondSurf <- 1/CostRasterMean


  pathLines <- list() #Initialize storage pathlines
  Lines <- list() #Initialize storage logged trees
  k <- 1 #Initialize pathlines index
  l <- 1 #Initialize lines index

  #Generate appropriate selected trees points format


  ptsAllinit <- treeselectionoutputs$SelectedTreesPoints %>%
    dplyr::select(idTree) %>%
    st_cast("POINT") %>%
    mutate(ID = NA) %>%
    mutate(type = "Tree") %>%
    st_set_crs(st_crs(AccessPointAll)) %>%
    st_join(AccessMainTrails %>% st_buffer(dist =  max((advancedloggingparameters$ScndTrailWidth/2),factagg)) %>%
              mutate(ID_Acc = ID) %>% dplyr::select(-ID)) %>%
    dplyr::select(ID,ID_Acc,type,idTree)

  ptsAllNA <- ptsAllinit %>% filter(is.na(ID_Acc)) %>%
    mutate(ID_Acc = AccessMainTrails$ID[c(max.col(-st_distance(ptsAllinit %>% filter(is.na(ID_Acc)),AccessMainTrails)))])

  ptsAllNotNA <- ptsAllinit %>% filter(!is.na(ID_Acc))

  ptsAll <- rbind(ptsAllNotNA,ptsAllNA)

  ptsCrAllinit <- treeselectionoutputs$SelectedCrownsPoints %>%
    dplyr::select(idTree) %>%
    st_cast("POINT") %>%
    mutate(ID = NA) %>%
    mutate(type = "Crown") %>%
    st_set_crs(st_crs(AccessPointAll))
  ptsCrAll <- ptsCrAllinit %>% filter(ptsCrAllinit %>%st_intersects(AccessMainTrails %>% st_union(),sparse = FALSE)) %>%
    st_join(AccessMainTrails %>% st_buffer(dist =  max((advancedloggingparameters$ScndTrailWidth/2),factagg)) %>%
              mutate(ID_Acc = ID) %>% dplyr::select(-ID)) %>%
    dplyr::select(ID,ID_Acc,type,idTree)  %>% filter(!is.na(ID_Acc))

  # ptsCrAllNA <- ptsCrAllinit %>% filter(is.na(ID_Acc)) %>%
  #   mutate(ID_Acc = AccessMainTrails$ID[c(max.col(-st_distance(ptsCrAllinit %>% filter(is.na(ID_Acc)),AccessMainTrails)))])
  #
  # ptsCrAllNotNA <- ptsCrAllinit
  #
  # ptsCrAll <- rbind(ptsCrAllNA,ptsCrAllNotNA)
  #



  # Reassign Selected Tree values (= BigTrees) to the aggregated Cost raster
  CostRasterMean <- raster::rasterize(x = as_Spatial(ptsAll %>% st_buffer(dist = factagg)),
                                      y = CostRasterMean ,
                                      field = CostMatrix[[2]][[3]]$CostValue,
                                      update = TRUE)
  if (winching == "2") {
    # Reassign Selected Tree values (= BigTrees) to the aggregated Cost raster (Grpl)
    CostRasterMeanGrpl <- raster::rasterize(x = as_Spatial(ptsAll %>% st_buffer(dist = factagg)),
                                            y = CostRasterMeanGrpl ,
                                            field = CostMatrix[[2]][[3]]$CostValue,
                                            update = TRUE)

    CostRasterMeanGrpl <- crop(CostRasterMeanGrpl,  CostRaster)
    CostRasterMeanGrpl <- mask(CostRasterMeanGrpl, plotmask)
  }

  #Generate maintrail intersection cost raster
  CostRasterMean <- raster::rasterize(x = as_Spatial(AccessPointAll %>% st_buffer(dist = 3*factagg)),
                                      y = CostRasterMean ,
                                      field = CostMatrix[[2]][[6]]$CostValue,
                                      update = TRUE)

  CostRasterMean <- crop(CostRasterMean,  CostRaster)
  CostRasterMean <- mask(CostRasterMean, plotmask)

  ########### Compute slope criteria transition graph ###############


  if (winching == "2") {
    #Compute adjacent transition layer according to slope conditions (winching = "2")
    SlopeCondGrpl <- sloperdcond(topography = DTMmean,
                                 advancedloggingparameters = advancedloggingparameters,
                                 grapple = TRUE)
  }
  #Compute adjacent transition layer according to slope conditions (winching = "1")
  SlopeCond <- sloperdcond(topography = DTMmean,advancedloggingparameters = advancedloggingparameters)

  ########### Compute LCP algorithm ###############

  for (ID_Access in unique(ptsAll$ID_Acc)) {

    print(ID_Access)

    winching <- WinchingInit
    pts <- ptsAll %>% filter(ID_Acc == ID_Access) %>%  dplyr::select(-ID_Acc)
    ptsCr <- ptsCrAll %>% filter(ID_Acc == ID_Access) %>%  dplyr::select(-ID_Acc)
    AccessPoint <- AccessPointAll %>% filter(ID == ID_Access)

    pathLinesWIP <- c()

    ki <- 1



    if (winching == "0") {
      # winching 0 #########

      pts <- st_set_crs(pts, st_crs(AccessPoint))# set crs

      pts <- rbind(AccessPoint %>%
                     mutate(type = "Access") %>%
                     mutate(IPpts = paste0("A.",row_number())),
                   pts %>%
                     mutate(IPpts = paste0("T.",idTree)))



      PointAcc <- pts %>%
        filter(type == "Access") %>%
        mutate(EstCost = NA)



      #Compute Cost between points and Access points
      CostDistEst <- adjtopolcp(costSurface = CondSurf,
                                topography = DTMmean ,
                                pts = pts %>%
                                  as_Spatial(),
                                slopeRdCond = SlopeCond,
                                paths = FALSE) [,1:dim(AccessPoint)[1]]
      CostDistEst <- CostDistEst[(length(PointAcc$ID)+1):length(CostDistEst)[1]]

      CostDistEst <- matrix(CostDistEst,ncol = length(PointAcc$ID) )

      #Attribute a least cost point access to each points
      PointTree <- pts %>% filter(type == "Tree") %>%
        mutate(ptAcc = max.col(-CostDistEst))
      PointTree$ID <- as.factor(PointTree$ptAcc)
      levels(PointTree$ID) <- as.character(AccessPoint$ID)

      pts <- rbind(AccessPoint %>%
                     mutate(type = "Access") %>%
                     mutate(ptAcc = row_number()) %>%
                     mutate(IPpts = paste0("A.",row_number())),
                   PointTree %>%
                     mutate(IPpts = paste0("T.",idTree)))


      PointAcc <- pts %>%
        filter(type == "Access") %>%
        mutate(EstCost = NA)

      #Attribute a least cost point access to each points
      PointTree <- pts %>% filter(type == "Tree") %>%
        mutate(ptAcc = max.col(-CostDistEst))
      if (length(PointAcc$ID) == 1) {
        PointTree <- PointTree %>% mutate(EstCost = CostDistEst)
      }else{PointTree <- PointTree %>% mutate(EstCost = apply(CostDistEst,1, min))}
      selectedPoints <- rbind(PointAcc,PointTree)

      accessPtId <- 1

      TmpSelectedPts <- selectedPoints %>%
        filter(ptAcc == accessPtId)

      TmpAccPts <- TmpSelectedPts %>%
        filter(type == "Access")

      #Select Costliest selected tree
      TmpTreePts <- TmpSelectedPts %>%
        filter(type == "Tree") %>%
        arrange(desc(EstCost))

      if (dim(TmpTreePts)[1] != 0) {
        for (TreeId in 1:dim(TmpTreePts)[1]){

          #Compute Least cost path polygons to the WIP selected tree
          TmpPtsWIP <- rbind(TmpAccPts,TmpTreePts[TreeId,])
          TmpPathWIP <-  adjtopolcp(costSurface = CondSurf,topography = DTMmean , pts = TmpPtsWIP %>% as_Spatial(),
                                    slopeRdCond = SlopeCond,paths = TRUE)
          if (TmpPathWIP[[1]][1,2] == 0) {
            #Update Cost raster with LCP second trail
            CostRasterMean  <- raster::rasterize(x = TmpTreePts[TreeId,] ,
                                                 y = CostRasterMean  ,
                                                 field = CostMatrix[[2]][[7]]$CostValue,update =  TRUE)
          }else{
            #Update Cost raster with LCP second trail
            CostRasterMean  <- raster::rasterize(x = TmpPathWIP[[2]] ,
                                                 y = CostRasterMean  ,
                                                 field = CostMatrix[[2]][[7]]$CostValue,update =  TRUE)
          }

          CondSurf <- 1/CostRasterMean

          #Store pathline
          pathLines[[k]] <- TmpPathWIP[[2]]
          pathLines[[k]]@lines[[1]]@ID <- paste("Path", TmpPtsWIP$idTree[2], sep = ".")

          Lines[[k]] <- list("LineID" = k,"LoggedTrees" = TmpPtsWIP$idTree[2],"TypeExpl" = "FoT")

          k <- k +1

        }
      }


    }else{
      # winching 1/2 #########

      # Select intersection points from buffer polygons


      PointAcc <- AccessPoint %>% #def Access Point
        mutate(type = "Access") %>%
        mutate(IDpts = paste0("A.",row_number())) %>%
        mutate(n.overlaps = NA, origins = NA) %>%
        dplyr::select(-ID)

      TreePts <- pts %>% filter(type == "Tree")
      CrownPts <- ptsCr %>% filter(type == "Crown")

      if (winching == "2") {

        if (dim(CrownPts)[1] == 0 ) {

          Crown2FoT <- TRUE


          ptsGrpl <- TreePts %>% #def Grpl point
            st_buffer(dist = advancedloggingparameters$GrappleLength) %>%
            st_snap_to_grid(size = .2) %>% # avoid GEOS error (st-intersection issue)
            #st_set_precision(1) %>%
            st_temporaryintersection(topography = topography,
                                     plotmask = plotmask,
                                     advancedloggingparameters = advancedloggingparameters) %>%
            st_make_valid() %>%
            mutate(type = "InterTr") %>%
            dplyr::select(-ID)




          for (OriginI in 1:length(ptsGrpl$origins)) {
            for (OriginJ in 1:length(ptsGrpl$origins[[OriginI]])) {
              IndexPts <- ptsGrpl$origins[[OriginI]][OriginJ]
              ptsGrpl$origins[[OriginI]][OriginJ] <- TreePts$idTree[IndexPts]
            }
          }

          ptsGrpl$isEmpty <- FALSE


          for (h in 1:dim(ptsGrpl)[1]) {
            if (ptsGrpl[h,] %>% st_intersects(AccessPolygons,sparse = FALSE) == FALSE) {
              ptsGrpl$isEmpty[h] <- TRUE
            }else{
              suppressWarnings(st_geometry(ptsGrpl[h,]) <- st_geometry(ptsGrpl[h,] %>% st_intersection(AccessPolygons)))
            }

          }

          #Filter polygons which intersect accessible area to second trails

          ptsGrpl<- st_set_crs(ptsGrpl,st_crs(AccessPolygons)) # set crs from AccessPolygons

          ptsGrpl <- ptsGrpl %>% filter(isEmpty == FALSE) %>% dplyr::select(-isEmpty) %>%
            mutate(IDpts = paste0("I.", row_number()))

          ptsWIP <- ptsGrpl %>% #def Grpl point as WIP point
            st_set_agr("constant") %>%
            st_centroid()
        }else{

          Crown2FoT <- FALSE


          ptsGrpl <- CrownPts %>% #def Grpl point
            st_buffer(dist = advancedloggingparameters$GrappleLength) %>%
            st_snap_to_grid(size = .2) %>% # avoid GEOS error (st-intersection issue)
            #st_set_precision(1) %>%
            st_temporaryintersection(topography = topography,
                                     plotmask = plotmask,
                                     advancedloggingparameters = advancedloggingparameters) %>%
            st_make_valid() %>%
            mutate(type = "InterCr") %>%
            dplyr::select(-ID)




          for (OriginI in 1:length(ptsGrpl$origins)) {
            for (OriginJ in 1:length(ptsGrpl$origins[[OriginI]])) {
              IndexPts <- ptsGrpl$origins[[OriginI]][OriginJ]
              ptsGrpl$origins[[OriginI]][OriginJ] <- CrownPts$idTree[IndexPts]
            }
          }

          ptsGrpl$isEmpty <- FALSE


          for (h in 1:dim(ptsGrpl)[1]) {
            if (ptsGrpl[h,] %>% st_intersects(AccessPolygons,sparse = FALSE) == FALSE) {
              ptsGrpl$isEmpty[h] <- TRUE
            }else{
              suppressWarnings(st_geometry(ptsGrpl[h,]) <- st_geometry(ptsGrpl[h,] %>% st_intersection(AccessPolygons)))
            }

          }

          #Filter polygons which intersect accessible area to second trails

          ptsGrpl<- st_set_crs(ptsGrpl,st_crs(AccessPolygons)) # set crs from AccessPolygons

          ptsGrpl <- ptsGrpl %>% filter(isEmpty == FALSE) %>% dplyr::select(-isEmpty) %>%
            mutate(IDpts = paste0("I.", row_number()))

          ptsWIP <- ptsGrpl %>% #def Grpl point as WIP point
            st_set_agr("constant") %>%
            st_centroid()

        }


        ptsCbl <- TreePts %>% #def cbl polygons
          st_buffer(dist = advancedloggingparameters$CableLength) %>%
          st_snap_to_grid(size = 1) %>%# avoid GEOS error (st-intersection issue)
          st_set_precision(1) %>%
          st_temporaryintersection(topography = topography,
                                   plotmask = plotmask,
                                   advancedloggingparameters = advancedloggingparameters) %>%
          st_make_valid() %>%
          mutate(type = "InterTr")%>%
          dplyr::select(-ID)



        for (OriginI in 1:length(ptsCbl$origins)) {
          for (OriginJ in 1:length(ptsCbl$origins[[OriginI]])) {
            IndexPts <- ptsCbl$origins[[OriginI]][OriginJ]
            ptsCbl$origins[[OriginI]][OriginJ] <- TreePts$idTree[IndexPts]
          }
        }


        ptsCbl$isEmpty <- FALSE


        for (h in 1:dim(ptsCbl)[1]) {
          if (ptsCbl[h,] %>% st_intersects(AccessPolygons,sparse = FALSE) == FALSE) {
            ptsCbl$isEmpty[h] <- TRUE
          }else{
            suppressWarnings(st_geometry(ptsCbl[h,]) <- st_geometry(ptsCbl[h,] %>% st_intersection(AccessPolygons)))
          }

        }


        ptsCbl <- st_set_crs(ptsCbl,st_crs(AccessPolygons)) #set crs from AccessPolygons

        ptsCbl <- ptsCbl %>% filter(isEmpty == FALSE) %>% dplyr::select(-isEmpty) %>%
          mutate(IDpts = paste0("I.",row_number())) #Filter polygons which intersect accessible area to second trails



        ptsWIPCbl <- ptsCbl %>%#Convert polygons to centroid
          st_set_agr("constant") %>%
          st_centroid()


        ptsWIP <- ptsWIP %>%
          arrange(desc(n.overlaps))

        RemainTree <- dim(TreePts)[1]


        #Select adjacent grpl graph
        SlopeCondRd <- SlopeCondGrpl

      }else{

        Crown2FoT <- TRUE


        ptsCbl <- TreePts %>% #def cbl point
          st_buffer(dist = advancedloggingparameters$CableLength) %>%
          st_snap_to_grid(size = 1) %>% # avoid GEOS error (st-intersection issue)
          st_set_precision(1) %>%
          st_temporaryintersection(topography = topography,
                                   plotmask = plotmask,
                                   advancedloggingparameters = advancedloggingparameters) %>%
          st_make_valid() %>%
          mutate(IDpts = paste0("I.",row_number())) %>%
          mutate(type =  "InterTr") %>% dplyr::select(-ID)


        ptsCbl <- st_set_crs(ptsCbl,st_crs(AccessPolygons)) #set crs from AccessPolygons

        for (OriginI in 1:length(ptsCbl$origins)) {
          for (OriginJ in 1:length(ptsCbl$origins[[OriginI]])) {
            IndexPts <- ptsCbl$origins[[OriginI]][OriginJ]
            ptsCbl$origins[[OriginI]][OriginJ] <- TreePts$idTree[IndexPts]
          }
        }

        ptsCbl$isEmpty <- FALSE


        for (h in 1:dim(ptsCbl)[1]) {
          if (st_geometry(ptsCbl[h,]) %>% st_intersects(AccessPolygons,sparse = FALSE) == FALSE) {
            ptsCbl$isEmpty[h] <- TRUE
          }else{
            suppressWarnings(st_geometry(ptsCbl[h,]) <- st_geometry(ptsCbl[h,] %>% st_intersection(AccessPolygons)))
          }

        }



        ptsWIP <-  ptsCbl %>% filter(isEmpty == FALSE) %>% dplyr::select(-isEmpty) %>%#def cbl point as WIP point
          st_set_agr("constant") %>%
          st_centroid()

        ptsWIP <- ptsWIP %>%  #filter cbl intersection centroid point out plot
          filter(st_intersects(st_geometry(ptsWIP),
                               st_geometry(plotmask %>%
                                             st_as_sf()),
                               sparse = FALSE)) %>%
          arrange(desc(n.overlaps))

        RemainTree <- dim(TreePts)[1]

        #Select adjacent cbl graph
        SlopeCondRd <- SlopeCond

      }


      #Define a switch from grapple accessible tree exploitation to cable exploitation

      if (winching == "2") {
        WinchingSwitch <- FALSE
      }else{
        WinchingSwitch <- TRUE
      }






      #Loop over possible intersection
      while (RemainTree !=0L) {

        if (verbose) {
          print(RemainTree)
          print(paste0("Crown2FoT : ",Crown2FoT))
        }



        #Switch from grpl to cbl exploitation when grapple accessible tree != 0
        Grpl2CblFlag <- FALSE
        stoplog <- FALSE


        if (length(unique(unlist(ptsGrpl$origins))) == 0 & WinchingSwitch == FALSE & winching == "2" & Crown2FoT) {
          winching <- "1"
          ptsWIP <- ptsWIPCbl
          WinchingSwitch <- TRUE
        }

        if (!Crown2FoT & dim(CrownPts)[1] == 0) {
          Crown2FoT <- TRUE
          stoplog <- TRUE
        }



        if ((dim(ptsWIP)[1] != 0 & !Crown2FoT)| (dim(TreePts)[1] != 0 & Crown2FoT & !stoplog) | (dim(TreePts)[1] != 0 & (winching=="1")) ) {





          ptsWIPmax <- rbind(PointAcc,ptsWIP %>%
                               filter(n.overlaps == max(ptsWIP$n.overlaps))) %>%
            mutate(TypeAcc = NA) %>%
            mutate(EstCost = NA)  %>%
            mutate(ptsAcc = NA)

          if (!Crown2FoT) {
            ptsTreeWIP <-  rbind(PointAcc,CrownPts %>%
                                   mutate(n.overlaps = NA, origins = idTree,IDpts = NA) %>%
                                   dplyr::select(-ID)) %>%
              mutate(TypeAcc = NA) %>%
              mutate(EstCost = NA)  %>%
              mutate(ptsAcc = NA)
          }else{
            ptsTreeWIP <-  rbind(PointAcc,TreePts %>%
                                   mutate(n.overlaps = NA, origins = idTree,IDpts = NA) %>%
                                   dplyr::select(-ID)) %>%
              mutate(TypeAcc = NA) %>%
              mutate(EstCost = NA)  %>%
              mutate(ptsAcc = NA)
          }



          if (k == 1 ) {
            if (winching == "2") {
              ptsDirAcc <- ptsTreeWIP %>%  mutate(gprlAcc  = c(FALSE,as.numeric(st_distance(ptsTreeWIP)[2:dim(ptsTreeWIP)[1],1]) < advancedloggingparameters$GrappleLength)) %>%
                filter(gprlAcc == TRUE | type == "Access") %>% dplyr::select(-gprlAcc)
              TmpTypeAcc <- "Grpl"

            }else{
              ptsDirAcc <- ptsTreeWIP %>%  mutate(cblAcc  = c(FALSE,as.numeric(st_distance(ptsTreeWIP)[2:dim(ptsTreeWIP)[1],1]) < advancedloggingparameters$CableLength)) %>%
                filter(cblAcc == TRUE | type == "Access") %>% dplyr::select(-cblAcc)
              TmpTypeAcc <- "Cbl"
            }
          }else{


            if (winching == "2") {
              ptsDirAcc <- ptsTreeWIP %>%  mutate(gprlAcc  = c(FALSE,as.numeric(st_distance(ptsTreeWIP,st_union(pathsWIP,PointAcc))[2:dim(ptsTreeWIP)[1],1]) < advancedloggingparameters$GrappleLength)) %>%
                filter(gprlAcc == TRUE | type == "Access") %>% dplyr::select(-gprlAcc)

              TmpTypeAcc <- "Grpl"

            }else{
              ptsDirAcc <- ptsTreeWIP %>%  mutate(cblAcc  = c(FALSE,as.numeric(st_distance(ptsTreeWIP,st_union(pathsWIP,PointAcc))[2:dim(ptsTreeWIP)[1],1]) < advancedloggingparameters$CableLength)) %>%
                filter(cblAcc == TRUE | type == "Access") %>% dplyr::select(-cblAcc)

              TmpTypeAcc <- "Cbl"
            }
          }



          if (dim(ptsDirAcc)[1] > 1) {

            PointTreeWIP <- ptsDirAcc %>% filter(type == "Tree"| type == "Crown")

            print(paste0("LoggedTrees : ",PointTreeWIP$origins[[1]]))

            Lines[[l]] <- list("LineID" = NA,"LoggedTrees" = PointTreeWIP$origins,"Crownpts" = !Crown2FoT,"TypeExpl" = TmpTypeAcc,"IdPU" = ID_Access)


            l <- l+1

          }else{



            #Compute Cost between points and Access points in cbl exploitation
            CostDistEstCbl <- adjtopolcp(costSurface = CondSurf,
                                         topography = DTMmean ,
                                         pts = ptsWIPmax %>%
                                           as_Spatial(),
                                         slopeRdCond = SlopeCond,
                                         paths = FALSE) [,1]

            CostDistEstCbl <- CostDistEstCbl[(length(PointAcc$ID)+1):length(CostDistEstCbl)[1]]

            CostDistEstCbl <- matrix(CostDistEstCbl,ncol = length(PointAcc$ID) )

            #Attribute a least cost point access to each points
            PointTree <- ptsWIPmax %>% filter(type != "Access" ) %>%
              mutate(ptAccCbl = max.col(- CostDistEstCbl,ties.method = "first")) %>%
              mutate(EstCostCbl = CostDistEstCbl)


            if (winching == "2") {

              CondSurfGrpl <- 1/CostRasterMeanGrpl

              #Compute Cost between points and Access points in grpl exploitation
              CostDistEstGrpl <- adjtopolcp(costSurface = CondSurfGrpl,
                                            topography = DTMmean ,
                                            pts = ptsWIPmax %>%
                                              as_Spatial(),
                                            slopeRdCond = SlopeCondGrpl,
                                            paths = FALSE) [,1:length(PointAcc$ID)]

              CostDistEstGrpl <- CostDistEstGrpl[(length(PointAcc$ID)+1):length(CostDistEstGrpl)[1]]
              CostDistEstGrpl <- matrix(CostDistEstGrpl,ncol = length(PointAcc$ID) )


              #Attribute a least cost point access to each points
              PointTree <- PointTree %>%
                mutate(ptAccGpl = max.col(-matrix(CostDistEstGrpl,ncol = length(PointAcc$ID) ),ties.method = "first")) %>%
                mutate(EstCostGrpl = CostDistEstGrpl)

              #Prioritize grpl exploitation if possible
              CostDistEstGrpl[CostDistEstGrpl != Inf] <- 0
              CostDistEstGrpl[CostDistEstGrpl == Inf] <- 1

              for (j in 1:length(CostDistEstGrpl)[1]) {
                PointTree[j,"TypeAcc"] <-  as.character(prod(CostDistEstGrpl[j,]))
              }
              PointTree$TypeAcc[PointTree$TypeAcc == "0"] <- "Grpl"
              PointTree$TypeAcc[PointTree$TypeAcc == "1"] <- "Cbl"

              PointTree$ptsAcc[PointTree$TypeAcc == "Grpl"] <- PointTree$ptAccGpl[PointTree$TypeAcc == "Grpl"]
              PointTree$ptsAcc[PointTree$TypeAcc != "Grpl"] <- PointTree$ptAccCbl[PointTree$TypeAcc != "Grpl"]

              PointTree$EstCost[PointTree$TypeAcc == "Grpl"] <- PointTree$EstCostGrpl[PointTree$TypeAcc == "Grpl"]
              PointTree$EstCost[PointTree$TypeAcc != "Grpl"] <- PointTree$EstCostCbl[PointTree$TypeAcc != "Grpl"]


              PointTree <- PointTree %>% arrange(desc(TypeAcc),EstCost)


              if (PointTree$TypeAcc[1] == "Grpl" & !Crown2FoT) {

                TmpPtsWIP <- ptsGrpl %>%
                  filter(IDpts == PointTree$IDpts[1])  %>%
                  st_union() %>%
                  st_cast("POINT") %>%
                  st_as_sf() %>%
                  mutate(type = "Overlay") %>%
                  mutate(ptsAcc = PointTree$ptsAcc[1]) %>%
                  mutate(IDpts = PointTree$IDpts[1]) %>%
                  mutate(origins = PointTree$origins[1])%>%
                  mutate(n.overlaps = PointTree$n.overlaps[1])

                TmpTypeAcc <- "Grpl"

                #Select adjacent grpl graph
                SlopeCondRd <- SlopeCondGrpl


              }else{

                if (PointTree$TypeAcc[1] == "Grpl" & Crown2FoT) {
                  #Reconstruct access points + selected tree in grpl exploitation
                  TmpPtsWIP <- ptsGrpl %>%
                    filter(IDpts == PointTree$IDpts[1])  %>%
                    st_union() %>%
                    st_cast("POINT") %>%
                    st_as_sf() %>%
                    mutate(type = "Overlay") %>%
                    mutate(ptsAcc = PointTree$ptsAcc[1]) %>%
                    mutate(IDpts = PointTree$IDpts[1]) %>%
                    mutate(origins = PointTree$origins[1])%>%
                    mutate(n.overlaps = PointTree$n.overlaps[1])

                  TmpTypeAcc <- "Grpl"

                  #Select adjacent grpl graph
                  SlopeCondRd <- SlopeCondGrpl

                }else{

                  if (!Crown2FoT) {
                    stoplog <- TRUE

                    ptsWIP <- ptsWIP %>% filter(n.overlaps < max(ptsWIPmax$n.overlaps,na.rm = TRUE))

                  }else{
                    #if the selected point is not a grpl accessible point BUT not all grpl accessible point are done

                    #Shift to cbl exploitation
                    Grpl2CblFlag <- TRUE


                    #Reconstruct access points + selected tree in cbl exploitation
                    TmpPtsWIP <- ptsCbl %>%
                      filter(IDpts == PointTree$IDpts[1])  %>%
                      st_union() %>%
                      st_cast("POINT") %>%
                      st_as_sf() %>%
                      mutate(type = "Overlay") %>%
                      mutate(ptsAcc = PointTree$ptsAcc[1]) %>%
                      mutate(IDpts = PointTree$IDpts[1]) %>%
                      mutate(origins = PointTree$origins[1])%>%
                      mutate(n.overlaps = PointTree$n.overlaps[1])

                    #Select adjacent cbl graph
                    SlopeCondRd <- SlopeCond

                    TmpTypeAcc <- "Cbl"
                  }



                }
              }

            }else{

              #Reconstruct access points + selected tree in cbl exploitation
              TmpPtsWIP <- ptsCbl %>%
                filter(IDpts == PointTree$IDpts[1])  %>%
                st_union() %>%
                st_cast("POINT") %>%
                st_as_sf() %>%
                mutate(type = "Overlay") %>%
                mutate(ptsAcc = PointTree$ptsAcc[1]) %>%
                mutate(IDpts = PointTree$IDpts[1]) %>%
                mutate(origins = PointTree$origins[1]) %>%
                mutate(n.overlaps = PointTree$n.overlaps[1])



              #Select adjacent cbl graph
              SlopeCondRd <- SlopeCond

              TmpTypeAcc <- "Cbl"

            }


            if (!stoplog) {


              #filter WIP points in accessible scd trail area
              # TmpPtsWIP <- st_set_crs(TmpPtsWIP,st_crs(ptsCbl)) # set crs from ptsCbl

              TmpPtsWIP <- TmpPtsWIP %>%
                filter(st_intersects(st_geometry(TmpPtsWIP),st_geometry(AccessPolygons %>% st_union()),sparse = FALSE))


              #Reconstruct access points + selected points
              TmpPtsWIP <- rbind( PointAcc %>%
                                    st_union() %>%
                                    st_cast("POINT")%>%
                                    st_as_sf() %>%
                                    mutate(type = "Access") %>%
                                    mutate(ptsAcc = NA ) %>%
                                    mutate(IDpts = NA) %>%
                                    mutate(origins = NA)%>%
                                    mutate(n.overlaps = NA),TmpPtsWIP)


              #Compute Cost between all points and Access points

              CostDistEstWIP <-  adjtopolcp(costSurface = CondSurf,topography = DTMmean , pts = TmpPtsWIP %>% as_Spatial(),
                                            slopeRdCond = SlopeCondRd,paths = FALSE)[,1]


              CostDistEstWIP <- CostDistEstWIP[(length(PointAcc$ID)+1):length(CostDistEstWIP)[1]]

              CostDistEstWIP <- matrix(CostDistEstWIP, ncol = length(PointAcc$ID))

              PointTreeWIP <- TmpPtsWIP %>%
                filter(type == "Overlay") %>%
                mutate(EstCost = CostDistEstWIP) %>%
                arrange(EstCost)




              TmpPtsWIP <- rbind(TmpPtsWIP %>% filter(type == "Access") %>% mutate(EstCost = NA),PointTreeWIP[1,])

              TmpPathWIP <- adjtopolcp(costSurface = CondSurf,topography = DTMmean , pts = TmpPtsWIP %>% as_Spatial(),
                                       slopeRdCond = SlopeCondRd,paths = FALSE)

              TmpPathWIPCost <- TmpPathWIP[1:length(PointAcc$ID),length(PointAcc$ID)+1]

              LCPathWIP <- max.col(t(-TmpPathWIPCost))

              TmpPtsWIP <- rbind(TmpPtsWIP[LCPathWIP,],PointTreeWIP[1,])

              TmpPathWIP <- adjtopolcp(costSurface = CondSurf,topography = DTMmean , pts = TmpPtsWIP %>% as_Spatial(),
                                       slopeRdCond = SlopeCondRd,paths = TRUE)

              if (TmpPathWIP[[1]][2,1] != 0) {

                if (TmpTypeAcc == "Grpl") {
                  CostRasterMean  <- raster::rasterize(x = TmpPathWIP[[2]] ,
                                                       y = CostRasterMean  ,
                                                       field = CostMatrix[[2]][[7]]$CostValue,update =  TRUE)
                  CostRasterMeanGrpl  <- raster::rasterize(x = TmpPathWIP[[2]] ,
                                                           y = CostRasterMeanGrpl  ,
                                                           field = CostMatrix[[2]][[7]]$CostValue,update =  TRUE)
                }else{
                  CostRasterMean  <- raster::rasterize(x = TmpPathWIP[[2]] ,
                                                       y = CostRasterMean  ,
                                                       field = CostMatrix[[2]][[7]]$CostValue,update =  TRUE)
                }

              }

              pathLines[[k]] <- TmpPathWIP[[2]]
              pathLines[[k]]@lines[[1]]@ID <- paste("Path",
                                                    "A",
                                                    LCPathWIP,
                                                    "NTree",
                                                    length(PointTreeWIP$origins[[1]]),
                                                    "T",
                                                    paste(as.character(unlist(PointTreeWIP$origins[[1]])),
                                                          collapse='-'),
                                                    sep = ".")

              if (verbose) {
                print(paste0("LoggedTrees : ",PointTreeWIP$origins[[1]]))
              }


              Lines[[l]] <- list("LineID" = k,"LoggedTrees" = PointTreeWIP$origins[[1]],"Crownpts" = !Crown2FoT,"TypeExpl" = TmpTypeAcc,"IdPU" = ID_Access)

              k <- k +1
              l <- l+1



              pathLinesWIP[[ki]] <- TmpPathWIP[[2]]

              pathLinesWIP[[ki]]@lines[[1]]@ID <- paste("Path",
                                                        "A",
                                                        LCPathWIP,
                                                        "NTree",
                                                        length(PointTreeWIP$origins[[1]]),
                                                        "T",
                                                        paste(as.character(unlist(PointTreeWIP$origins[[1]])),
                                                              collapse='-'),
                                                        sep = ".")

              ki <- ki +1

              pathsWIP <- do.call(rbind, pathLinesWIP)

              if(dim( pathsWIP %>%
                      st_as_sf() %>% filter(!st_is_empty(pathsWIP %>%
                                                         st_as_sf())))[1]> 1){
                pathsWIP <-  smoothtrails(paths = pathsWIP,
                                          plotmask = plotmask,
                                          verbose = FALSE,
                                          advancedloggingparameters = advancedloggingparameters)$SmoothedTrails %>%
                  st_union()

              }else{

                pathsWIP <- pathsWIP %>% st_as_sf() %>%
                  st_union()
              }




              ###################

            }

          }

          if (!stoplog) {



            TreePts$Logged<- FALSE



            for (j in 1:dim(TreePts)[1]) {
              TreePts$Logged[j] <- any(PointTreeWIP$origins[[1]] %in% TreePts$idTree[j])
            }


            TreePts <- TreePts %>%
              filter(Logged == FALSE)%>%
              dplyr::select(-Logged)

            if (dim(TreePts)[1]> 0) {



              if (winching == "2") {
                ptsGrpl <- TreePts %>% #def Grpl point
                  st_buffer(dist = advancedloggingparameters$GrappleLength) %>%
                  st_snap_to_grid(size = .2) %>% # avoid GEOS error (st-intersection issue)
                  #st_set_precision(1) %>%
                  st_temporaryintersection(topography = topography,
                                           plotmask = plotmask,
                                           advancedloggingparameters = advancedloggingparameters) %>%
                  st_make_valid() %>%
                  mutate(type = "InterTr") %>%
                  dplyr::select(-ID)


                for (OriginI in 1:length(ptsGrpl$origins)) {
                  for (OriginJ in 1:length(ptsGrpl$origins[[OriginI]])) {
                    IndexPts <- ptsGrpl$origins[[OriginI]][OriginJ]
                    ptsGrpl$origins[[OriginI]][OriginJ] <- TreePts$idTree[IndexPts]
                  }
                }

                ptsGrpl$isEmpty <- FALSE


                for (h in 1:dim(ptsGrpl)[1]) {
                  if (ptsGrpl[h,] %>% st_intersects(AccessPolygons,sparse = FALSE) == FALSE) {
                    ptsGrpl$isEmpty[h] <- TRUE
                  }else{
                    suppressWarnings(st_geometry(ptsGrpl[h,]) <- st_geometry(ptsGrpl[h,] %>% st_intersection(AccessPolygons)))
                  }

                }

                #Filter polygons which intersect accessible area to second trails

                ptsGrpl<- st_set_crs(ptsGrpl,st_crs(AccessPolygons)) # set crs from AccessPolygons

                ptsGrpl <- ptsGrpl %>% filter(isEmpty == FALSE) %>% dplyr::select(-isEmpty) %>%
                  mutate(IDpts = paste0("I.", row_number()))

                ptsWIP <- ptsGrpl %>% #def Grpl point as WIP point
                  st_set_agr("constant") %>%
                  st_centroid()


                ptsCbl <- TreePts %>% #def cbl polygons
                  st_buffer(dist = advancedloggingparameters$CableLength) %>%
                  st_snap_to_grid(size = 1) %>%# avoid GEOS error (st-intersection issue)
                  st_set_precision(1) %>%
                  st_temporaryintersection(topography = topography,
                                           plotmask = plotmask,
                                           advancedloggingparameters = advancedloggingparameters) %>%
                  st_make_valid() %>%
                  mutate(type = "Inter")%>%
                  dplyr::select(-ID)



                for (OriginI in 1:length(ptsCbl$origins)) {
                  for (OriginJ in 1:length(ptsCbl$origins[[OriginI]])) {
                    IndexPts <- ptsCbl$origins[[OriginI]][OriginJ]
                    ptsCbl$origins[[OriginI]][OriginJ] <- TreePts$idTree[IndexPts]
                  }
                }


                ptsCbl$isEmpty <- FALSE


                for (h in 1:dim(ptsCbl)[1]) {
                  if (ptsCbl[h,] %>% st_intersects(AccessPolygons,sparse = FALSE) == FALSE) {
                    ptsCbl$isEmpty[h] <- TRUE
                  }else{
                    suppressWarnings(st_geometry(ptsCbl[h,]) <- st_geometry(ptsCbl[h,] %>% st_intersection(AccessPolygons)))
                  }

                }


                ptsCbl <- st_set_crs(ptsCbl,st_crs(AccessPolygons)) #set crs from AccessPolygons

                ptsCbl <- ptsCbl %>% filter(isEmpty == FALSE) %>% dplyr::select(-isEmpty) %>%
                  mutate(IDpts = paste0("I.",row_number())) #Filter polygons which intersect accessible area to second trails



                ptsWIPCbl <- ptsCbl %>%#Convert polygons to centroid
                  st_set_agr("constant") %>%
                  st_centroid()


                ptsWIP <- ptsWIP %>%
                  arrange(desc(n.overlaps))

                #Select adjacent grpl graph
                SlopeCondRd <- SlopeCondGrpl

              }else{



                ptsCbl <- TreePts %>% #def cbl point
                  st_buffer(dist = advancedloggingparameters$CableLength) %>%
                  st_snap_to_grid(size = 1) %>% # avoid GEOS error (st-intersection issue)
                  st_set_precision(1) %>%
                  st_temporaryintersection(topography = topography,
                                           plotmask = plotmask,
                                           advancedloggingparameters = advancedloggingparameters) %>%
                  st_make_valid() %>%
                  mutate(IDpts = paste0("I.",row_number())) %>%
                  mutate(type =  "InterTr") %>% dplyr::select(-ID)


                ptsCbl <- st_set_crs(ptsCbl,st_crs(AccessPolygons)) #set crs from AccessPolygons

                for (OriginI in 1:length(ptsCbl$origins)) {
                  for (OriginJ in 1:length(ptsCbl$origins[[OriginI]])) {
                    IndexPts <- ptsCbl$origins[[OriginI]][OriginJ]
                    ptsCbl$origins[[OriginI]][OriginJ] <- TreePts$idTree[IndexPts]
                  }
                }

                ptsCbl$isEmpty <- FALSE


                for (h in 1:dim(ptsCbl)[1]) {
                  if (ptsCbl[h,] %>% st_intersects(AccessPolygons,sparse = FALSE) == FALSE) {
                    ptsCbl$isEmpty[h] <- TRUE
                  }else{
                    suppressWarnings(st_geometry(ptsCbl[h,]) <- st_geometry(ptsCbl[h,] %>% st_intersection(AccessPolygons)))
                  }

                }



                ptsWIP <-  ptsCbl %>% filter(isEmpty == FALSE) %>% dplyr::select(-isEmpty) %>%#def cbl point as WIP point
                  st_set_agr("constant") %>%
                  st_centroid()

                ptsWIP <- ptsWIP %>%  #filter cbl intersection centroid point out plot
                  filter(st_intersects(st_geometry(ptsWIP),
                                       st_geometry(plotmask %>%
                                                     st_as_sf()),
                                       sparse = FALSE)) %>%
                  arrange(desc(n.overlaps))



                #Select adjacent cbl graph
                SlopeCondRd <- SlopeCond

              }

            }


            if (!Crown2FoT & winching == "2") {
              CrownPts$Logged<- FALSE



              for (j in 1:dim(CrownPts)[1]) {
                CrownPts$Logged[j] <- any(PointTreeWIP$origins[[1]] %in% CrownPts$idTree[j])
              }


              CrownPts <- CrownPts %>%
                filter(Logged == FALSE)%>%
                dplyr::select(-Logged)

              if (dim(CrownPts)[1]> 0) {

                ptsGrpl <- CrownPts %>% #def Grpl point
                  st_buffer(dist = advancedloggingparameters$GrappleLength) %>%
                  st_snap_to_grid(size = .2) %>% # avoid GEOS error (st-intersection issue)
                  #st_set_precision(1) %>%
                  st_temporaryintersection(topography = topography,
                                           plotmask = plotmask,
                                           advancedloggingparameters = advancedloggingparameters) %>%
                  st_make_valid() %>%
                  mutate(type = "InterCr") %>%
                  dplyr::select(-ID)


                for (OriginI in 1:length(ptsGrpl$origins)) {
                  for (OriginJ in 1:length(ptsGrpl$origins[[OriginI]])) {
                    IndexPts <- ptsGrpl$origins[[OriginI]][OriginJ]
                    ptsGrpl$origins[[OriginI]][OriginJ] <- CrownPts$idTree[IndexPts]
                  }
                }

                ptsGrpl$isEmpty <- FALSE


                for (h in 1:dim(ptsGrpl)[1]) {
                  if (ptsGrpl[h,] %>% st_intersects(AccessPolygons,sparse = FALSE) == FALSE) {
                    ptsGrpl$isEmpty[h] <- TRUE
                  }else{
                    suppressWarnings(st_geometry(ptsGrpl[h,]) <- st_geometry(ptsGrpl[h,] %>% st_intersection(AccessPolygons)))
                  }

                }

                #Filter polygons which intersect accessible area to second trails

                ptsGrpl<- st_set_crs(ptsGrpl,st_crs(AccessPolygons)) # set crs from AccessPolygons

                ptsGrpl <- ptsGrpl %>% filter(isEmpty == FALSE) %>% dplyr::select(-isEmpty) %>%
                  mutate(IDpts = paste0("I.", row_number()))

                ptsWIP <- ptsGrpl %>% #def Grpl point as WIP point
                  st_set_agr("constant") %>%
                  st_centroid()



              }


              RemainTree <- dim(TreePts)[1]



            }


          }

          RemainTree <- dim(TreePts)[1]


        }else{

          Crown2FoT <- TRUE


          ptsGrpl <- TreePts %>% #def Grpl point
            st_buffer(dist = advancedloggingparameters$GrappleLength) %>%
            st_snap_to_grid(size = .2) %>% # avoid GEOS error (st-intersection issue)
            #st_set_precision(1) %>%
            st_temporaryintersection(topography = topography,
                                     plotmask = plotmask,
                                     advancedloggingparameters = advancedloggingparameters) %>%
            st_make_valid() %>%
            mutate(type = "Inter") %>%
            dplyr::select(-ID)




          for (OriginI in 1:length(ptsGrpl$origins)) {
            for (OriginJ in 1:length(ptsGrpl$origins[[OriginI]])) {
              IndexPts <- ptsGrpl$origins[[OriginI]][OriginJ]
              ptsGrpl$origins[[OriginI]][OriginJ] <- TreePts$idTree[IndexPts]
            }
          }

          ptsGrpl$isEmpty <- FALSE


          for (h in 1:dim(ptsGrpl)[1]) {
            if (ptsGrpl[h,] %>% st_intersects(AccessPolygons,sparse = FALSE) == FALSE) {
              ptsGrpl$isEmpty[h] <- TRUE
            }else{
              suppressWarnings(st_geometry(ptsGrpl[h,]) <- st_geometry(ptsGrpl[h,] %>% st_intersection(AccessPolygons)))
            }

          }

          #Filter polygons which intersect accessible area to second trails

          ptsGrpl<- st_set_crs(ptsGrpl,st_crs(AccessPolygons)) # set crs from AccessPolygons

          ptsGrpl <- ptsGrpl %>% filter(isEmpty == FALSE) %>% dplyr::select(-isEmpty) %>%
            mutate(IDpts = paste0("I.", row_number()))

          ptsWIP <- ptsGrpl %>% #def Grpl point as WIP point
            st_set_agr("constant") %>%
            st_centroid()


          ptsCbl <- TreePts %>% #def cbl polygons
            st_buffer(dist = advancedloggingparameters$CableLength) %>%
            st_snap_to_grid(size = 1) %>%# avoid GEOS error (st-intersection issue)
            st_set_precision(1) %>%
            st_temporaryintersection(topography = topography,
                                     plotmask = plotmask,
                                     advancedloggingparameters = advancedloggingparameters) %>%
            st_make_valid() %>%
            mutate(type = "InterTr")%>%
            dplyr::select(-ID)



          for (OriginI in 1:length(ptsCbl$origins)) {
            for (OriginJ in 1:length(ptsCbl$origins[[OriginI]])) {
              IndexPts <- ptsCbl$origins[[OriginI]][OriginJ]
              ptsCbl$origins[[OriginI]][OriginJ] <- TreePts$idTree[IndexPts]
            }
          }


          ptsCbl$isEmpty <- FALSE


          for (h in 1:dim(ptsCbl)[1]) {
            if (ptsCbl[h,] %>% st_intersects(AccessPolygons,sparse = FALSE) == FALSE) {
              ptsCbl$isEmpty[h] <- TRUE
            }else{
              suppressWarnings(st_geometry(ptsCbl[h,]) <- st_geometry(ptsCbl[h,] %>% st_intersection(AccessPolygons)))
            }

          }


          ptsCbl <- st_set_crs(ptsCbl,st_crs(AccessPolygons)) #set crs from AccessPolygons

          ptsCbl <- ptsCbl %>% filter(isEmpty == FALSE) %>% dplyr::select(-isEmpty) %>%
            mutate(IDpts = paste0("I.",row_number())) #Filter polygons which intersect accessible area to second trails



          ptsWIPCbl <- ptsCbl %>%#Convert polygons to centroid
            st_set_agr("constant") %>%
            st_centroid()


          ptsWIP <- ptsWIP %>%
            arrange(desc(n.overlaps))

          RemainTree <- dim(TreePts)[1]



        }




      }


      ###############END LOOP################"
    }
  }



  paths <- do.call(rbind, pathLines)

  MainTrailsAccess <- AccessPointAll %>%
    st_set_crs(st_crs(topography))

  inventory <- treeselectionoutputs$inventory

  if (!("IdPU" %in% names(inventory))){
    inventory <- inventory %>%
      add_column(IdPU = NA) # if "IdPU" column doesnt exist create it
  }

  PuInventory <- suppressWarnings(sf::st_intersection(
    st_set_crs(st_as_sf(inventory, coords = c("Xutm", "Yutm")), st_crs(topography)),
    sf::st_make_valid(st_as_sf(AccessMainTrails)) # "make valid" to avoid self-intersection
  )) %>% st_join(AccessMainTrails)  %>% mutate(IdPU =  ID.y) %>%
    dplyr::select(idTree, IdPU)
  sf::st_geometry(PuInventory) <- NULL # remove geometry column (sf to data.frame)
  PuInventory <- unique(PuInventory)

  inventory <- inventory %>%
    left_join(PuInventory, by = "idTree") %>%
    mutate(IdPU = IdPU.y) %>% dplyr::select(-IdPU.x,-IdPU.y)




  if (length(paths)> 0) {

    lines <- do.call(rbind, Lines)

    secondtrails <- smoothtrails(paths,
                                 plotmask,
                                 verbose = verbose,
                                 advancedloggingparameters = advancedloggingparameters)

    SmoothedTrails <- secondtrails$SmoothedTrails %>% st_set_crs(st_crs(topography))

    TrailsDensity <- secondtrails$TrailsDensity
    #
    #
    # # Records the dead trees


    DeadTrees <- suppressWarnings(sf::st_intersection(
      st_set_crs(st_as_sf(inventory, coords = c("Xutm", "Yutm")), st_crs(topography)),
      sf::st_make_valid(st_as_sf(SmoothedTrails)) # "make valid" to avoid self-intersection
    )) %>%
      add_column(DeadTrees = "1") %>%
      dplyr::select(idTree, DeadTrees)
    sf::st_geometry(DeadTrees) <- NULL # remove geometry column (sf to data.frame)
    DeadTrees <- unique(DeadTrees)

    inventory <- inventory %>%
      left_join(DeadTrees, by = "idTree") %>%
      mutate(DeathCause = ifelse(Selected != "1" & DeadTrees == "1",
                                 "2ndTrail", DeathCause)) %>%  # Damage trees
      dplyr::select(-DeadTrees)

  }else{
    paths <- NULL
    lines <- do.call(rbind, Lines)
    SmoothedTrails <- MainTrailsAccess %>% st_union() %>% st_set_crs(st_crs(topography))
    TrailsDensity <- 0

    DeadTrees <- suppressWarnings(sf::st_intersection(
      st_set_crs(st_as_sf(inventory, coords = c("Xutm", "Yutm")), st_crs(topography)),
      sf::st_make_valid(st_as_sf(SmoothedTrails)) # "make valid" to avoid self-intersection
    )) %>%
      add_column(DeadTrees = "1") %>%
      dplyr::select(idTree, DeadTrees)
    sf::st_geometry(DeadTrees) <- NULL # remove geometry column (sf to data.frame)
    DeadTrees <- unique(DeadTrees)

    inventory <- inventory %>%
      left_join(DeadTrees, by = "idTree") %>%
      mutate(DeathCause = ifelse( Selected != "1" & DeadTrees == "1",
                                  "2ndTrail", DeathCause)) %>%  # Damage trees
      dplyr::select(-DeadTrees)
  }

  #

  if (WinchingInit == "2") {
    secondtrails <- list("inventory" =  inventory,
                         "SmoothedTrails" =  SmoothedTrails,
                         "TrailsDensity" =  TrailsDensity,
                         "TrailsIdentity" = lines,        # "LineID","LoggedTrees", "TypeExpl"
                         "MainTrailsAccess" = MainTrailsAccess,
                         "RawSecondTrails" = paths,
                         "CostRasterAgg" = list("CostRasterMean" = CostRasterMean,"CostRasterMeanGrpl" = CostRasterMeanGrpl))
  }else{
    secondtrails <- list("inventory" =  inventory,
                         "SmoothedTrails" =  SmoothedTrails,
                         "TrailsDensity" =  TrailsDensity,
                         "TrailsIdentity" = lines,        # "LineID","LoggedTrees", "TypeExpl"
                         "MainTrailsAccess" = MainTrailsAccess,
                         "RawSecondTrails" = paths,
                         "CostRasterAgg" = list("CostRasterMean" = CostRasterMean,"CostRasterMeanGrpl" = NULL))
  }



  return(secondtrails)

}

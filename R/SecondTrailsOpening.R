#' Second trails opening
#'
#' @param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'  or SRTM) (\code{\link{DTMParacou}}) (RasterLayer)
#'
#' @param plotmask Inventoried plot mask (SpatialPolygonsDataFrame)
#'
#' @param treeselectionoutputs A list with:
#' - your inventory with: "DistCriteria", "Slope", "SlopeCriteria", "LoggingStatus",
#' "Selected", "Up", "VolumeCumSum", "ProbedHollowProba", "ProbedHollow"
#' new columns (see the outputs metadata in the \code{\link{vignette}}).
#'
#' - the objective volume with or without a bonus (if hollow trees exploitation)
#'
#' - 6 sets of spatial points:
#'   harvestable, selected, future and reserve, hollow and fuel wood trees
#'
#' @param creekdistances Relative distances (vertical (distvert) and horizontal
#' (distHorz)) from nearest channel network (list of 2 large RasterLayers)
#'
#' @param CostMatrix List of list defining conditional weight over binned slopes
#'   values
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
#' @param fact Aggregation factor of cost raster resolution to initial DTM
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @return A list with :
#' - RawSecondTrails : non-smoothed secondary trails
#' - TrailsIdentity: information on sections of the trails with:
#'     - LineID:
#'     - LoggedTrees: idTree of trees reached by the trails
#'     - TypeExpl: type of winching
#' - SmoothedSecondTrails: Smoothed secondary trails polygons (sfc_MULTIPOLYGON)
#' - TrailsDensity: Second trails density (in m/ha)
#' - inventory: Updated inventory
#' - CostRasterAgg: A cost Raster (RasterLayer)
#'
#' @importFrom sf st_cast st_as_sf st_intersection st_union st_sample st_join
#'   st_buffer as_Spatial st_centroid st_set_precision st_make_valid st_set_agr
#'   st_geometry st_area st_is_empty st_set_crs st_crs sf_use_s2 st_geometry<-
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
#' \dontrun{
#' data(Paracou6_2016)
#' data(PlotMask)
#' data(DTMParacou)
#' # data(HarvestablePolygons)
#' data(MainTrails)
#' # data(PlotSlope)
#' data(SpeciesCriteria)
#' data(CreekDistances)
#'
#' inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask),
#'  volumeparameters = ForestZoneVolumeParametersTable)
#'
#'
#' treeselectionoutputs <- treeselection(inventory,
#' topography = DTMParacou, plotslope = PlotSlope, MainTrails = MainTrails,
#' harvestablepolygons = HarvestablePolygons,
#' speciescriteria = SpeciesCriteria,
#' scenario = "RIL3",
#' objectivelax = TRUE,
#' advancedloggingparameters = loggingparameters())
#'
#' secondtrails <- secondtrailsopening(
#'   topography = DTMParacou,
#'   plotmask = PlotMask,
#'   treeselectionoutputs = treeselectionoutputs,
#'   creekdistances = CreekDistances,
#'   CostMatrix = list(list(list(Slope = 3, Cost = 3),
#'                          list(Slope = 5, Cost = 5),
#'                          list(Slope = 12, Cost = 20),
#'                          list(Slope = 22, Cost = 100),
#'                          list(Slope = 27, Cost = 1000),
#'                          list(Slope = Inf, Cost = Inf)),
#'                     list(list(CostType = "Initial", CostValue = 1000),
#'                          list(CostType = "Access", CostValue = Inf),
#'                          list(CostType = "BigTrees", CostValue = 500),
#'                          list(CostType = "Reserves", CostValue = 1000),
#'                          list(CostType = "Futures", CostValue = 900),
#'                          list(CostType = "MainTrails", CostValue = 1E-4),
#'                          list(CostType = "SecondTrails", CostValue = 1E-2))),
#'   scenario = "RIL3",
#'   fact = 3,
#'   advancedloggingparameters = loggingparameters())
#'
#' library(ggplot2)
#' library(sf)
#' ggplot() +
#'   # Harvestable zones
#'   geom_sf(data = HarvestablePolygons,
#'         aes(alpha = Harvestable),
#'         fill = "olivedrab") +
#'   labs(alpha = "Harvestable") +
#'   labs(title = "P6 zones exploitables") +
#'
#'   # 2ndary trails
#'     geom_sf(data = st_as_sf(secondtrails$SmoothedSecondTrails), col = "darkgreen") +
#'     geom_sf(data = st_as_sf(secondtrails$RawSecondTrails), col = "red")
#'
#' secondtrails[[4]]
#'   }
#'
secondtrailsopening <- function(
  topography,
  plotmask,
  creekdistances,
  treeselectionoutputs,
  CostMatrix = list(list(list(Slope = 3, Cost = 3),
                         list(Slope = 5, Cost = 5),
                         list(Slope = 12, Cost = 20),
                         list(Slope = 22, Cost = 60),
                         list(Slope = 27, Cost = 600),
                         list(Slope = Inf, Cost = 1000)),
                    list(list(CostType = "Initial", CostValue = 1000),
                         list(CostType = "Access", CostValue = 1000),
                         list(CostType = "BigTrees", CostValue = 500),
                         list(CostType = "Reserves", CostValue = 500),
                         list(CostType = "Futures", CostValue = 50),
                         list(CostType = "MainTrails", CostValue = 1E-4),
                         list(CostType = "SecondTrails", CostValue = 0.1))),
  scenario,
  winching = NULL,
  fact = 3,
  advancedloggingparameters = loggingparameters()
){

  # Arguments check

  if(!inherits(treeselectionoutputs, "list"))
    stop("The 'treeselectionoutputs' arguments of the 'secondtrailsopening'
         function must be list following treeselection output format")

  if(!inherits(plotmask, "SpatialPolygonsDataFrame"))
    stop("The 'plotmask' argument of the 'secondtrailsopening' function must be
         SpatialPolygonsDataFrame")

  # if(!any(unlist(lapply(list(MainTrails), inherits, "sf" ))))
  #   stop("The 'MainTrails' argument of the 'secondtrailsopening' function must be sf polygon")

  if(!inherits(topography, "RasterLayer"))
    stop("The 'topography' argument of the 'secondtrailsopening' function must
         be RasterLayer")

  if(st_is_empty(treeselectionoutputs$SelectedTreesPoints)[1]) {
    stop("The 'treeselectionoutputs' argument does not contain any selected
         tree.")
  }

  # Global Variables
  slope <- x <- y <- Harvestable <- idTree <- ID <- type <- ptAcc <- plotslope <- NULL
  EstCost <- n.overlaps <- TypeAcc <- IDpts <- Logged <- harvestablepolygons<- HarvestableAreaDefintionOutputs <- NULL
  Selected <- DeathCause <- ID_Acc <- isEmpty <- gprlAcc <- cblAcc <- NULL


  #### Redefinition of the parameters according to the chosen scenario ####
  scenariosparameters <- scenariosparameters(scenario = scenario, winching = winching)
  WinchingInit <- scenariosparameters$winching
  winching <- WinchingInit

  ##################################

  sf_use_s2(FALSE) # to deal with actual unresolved s2 issues in sf

  # Transformation of the DTM so that the MainTrails are outside the plot


  DTMExtExtended <- raster::extend(topography, c(fact,fact)) # extend the extent

  fill.boundaries <- function(x) {
    center = 0.5 + (fact*fact/2)
    if( is.na(x)[center] ) {
      return( round(mean(x, na.rm=T),5) )
    } else {
      return( x[center] )
    }
  }

  DTMExtended <- raster::focal(DTMExtExtended,
                               matrix(1,fact,fact),
                               fun=fill.boundaries,
                               na.rm=F, pad=T)

  # Transformation of vertical creek height raster


  VerticalCreekHeightExtExtended <- raster::extend(creekdistances$distvert, c(fact,fact))


  VerticalCreekHeightExtended <- raster::focal(VerticalCreekHeightExtExtended,
                                               matrix(1,fact,fact),
                                               fun=fill.boundaries,
                                               na.rm=F, pad=T)

  HorizontalCreekDistanceExtExtended <- raster::extend(creekdistances$distHorz, c(fact,fact))


  HorizontalCreekDistanceExtended <- raster::focal(HorizontalCreekDistanceExtExtended,
                                               matrix(1,fact,fact),
                                               fun=fill.boundaries,
                                               na.rm=F, pad=T)

  CreekDistancesExtended <- list(distvert = VerticalCreekHeightExtended, distHorz = HorizontalCreekDistanceExtended)

  # Set MainTrails outside the plot

  preMainTrails <- DTMExtended > -Inf
  preMainTrails<- rasterToPolygons(preMainTrails, dissolve=T)
  MainTrails <- preMainTrails %>% st_as_sf() %>% st_cast(to = 'LINESTRING', warn= FALSE)



  # Generate accessible area from HarvestablePolygones and winching choice

  HarvestableAreaDefintionOutputs <- harvestableareadefinition(topography = DTMExtended,
                                                               creekdistances = CreekDistancesExtended)

  harvestablepolygons <- HarvestableAreaDefintionOutputs[[1]]

  plotslope <- HarvestableAreaDefintionOutputs[[2]]

  AccessPolygons <- filteraccesexplarea(harvestablepolygons = harvestablepolygons,
                                        MainTrails = MainTrails,
                                        advancedloggingparameters = advancedloggingparameters)


  # Generate accessible area from HarvestablePolygones and winching == "0"
  AccessMainTrails <- AccessPolygons %>% st_buffer(dist = -max((advancedloggingparameters$ScndTrailWidth/2),fact)) %>%
    st_cast("POLYGON") %>%
    st_as_sf() %>%
    mutate(ID = paste0("ID_",row_number()))




  # Generate intersections between accessible area and MainTrails (ID = accessible area index)
  PartMainTrails <- st_intersection(st_geometry(MainTrails %>%
                                                  st_buffer(dist = raster::res(topography) + 1.1*fact)),
                                    st_geometry(AccessMainTrails %>%
                                                  st_buffer(dist = 0))) %>%
    st_union(by_feature = T) %>%
    st_buffer(dist = raster::res(topography)+1) %>%
    st_intersection(st_as_sf(plotmask) %>% st_union()) %>%
    st_cast("MULTIPOLYGON")  %>%
    st_as_sf() %>%
    st_set_agr(value = "constant") %>%
    st_join(AccessMainTrails %>%
              st_buffer(dist = max((advancedloggingparameters$ScndTrailWidth/2),fact)))

  # Generate point access in the intersections between accessible area and MainTrails (ID = accessible area index)
  AccessPointAll <- PartMainTrails  %>%
    st_sample( rep(1,dim(PartMainTrails)[1]) ,type = "random", by_polygon=TRUE) %>% st_as_sf() %>%
    st_join(PartMainTrails) %>%
    mutate(idTree = NA)

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

  #Generate accessible weights raster
  AccessRaster <- raster(extent(DTMExtended),resolution = res(DTMExtended), crs = crs(DTMExtended))
  values(AccessRaster) <- CostMatrix[[2]][[2]]$CostValue

  AccessRaster <- rasterize(x = as_Spatial(AccessPolygons),
                            y = AccessRaster ,
                            field = 0,
                            update = TRUE)
  AccessRaster <- crop(AccessRaster,  CostRaster)
  AccessRaster <- mask(AccessRaster, plotmask)

  #Update Cost Raster with accessible weights raster
  CostRaster<- CostRaster + AccessRaster

  #Generate protection buffer for Big Trees (dist : ScndTrailWidth/2 + 2 m)



  BigTreesRaster <- raster(extent(DTMExtended),resolution = res(DTMExtended), crs = crs(DTMExtended))
  values(BigTreesRaster) <- 0
  BigTreesRaster <- rasterize(x = as_Spatial(treeselectionoutputs$BigTreesPoints %>%
                                               st_buffer(dist = advancedloggingparameters$ScndTrailWidth/2 + 2) ),
                              y = BigTreesRaster,
                              field = CostMatrix[[2]][[3]]$CostValue,
                              update = TRUE)
  BigTreesRaster <- crop(BigTreesRaster, CostRaster)

  #Generate protection buffer for Reserve Trees (dist : ScndTrailWidth/2 + 2 m)

  ReserveRaster <- raster(extent(DTMExtended),resolution = res(DTMExtended), crs = crs(DTMExtended))
  values(ReserveRaster) <- 0
  ReserveRaster <- rasterize(x = as_Spatial(treeselectionoutputs$HarvestableTreesPoints %>%
                                              st_buffer(dist = advancedloggingparameters$ScndTrailWidth/2 + 2) ),
                             y = ReserveRaster,
                             field = CostMatrix[[2]][[4]]$CostValue,
                             update = TRUE)
  ReserveRaster <- crop(ReserveRaster, CostRaster)

  #Generate protection buffer for Futures Trees (dist : ScndTrailWidth/2 + 2 m)
  FutureRaster <- raster(extent(DTMExtended),resolution = res(DTMExtended), crs = crs(DTMExtended))
  values(FutureRaster ) <- 0
  FutureRaster  <- rasterize(x = as_Spatial(treeselectionoutputs$HarvestableTreesPoints %>%
                                              st_buffer(dist =  advancedloggingparameters$ScndTrailWidth/2 +2) ),
                             y = FutureRaster  ,
                             field = CostMatrix[[2]][[5]]$CostValue,
                             update = TRUE)
  FutureRaster  <- crop(FutureRaster, CostRaster)

  #Update Cost Raster with protection buffers

  #Generate protection buffer for selected Trees (dist : ScndTrailWidth/2 + 2 m)
  SelectedRaster <- raster(extent(DTMExtended),resolution = res(DTMExtended), crs = crs(DTMExtended))
  values(SelectedRaster ) <- 0
  SelectedRaster  <- rasterize(x = as_Spatial(treeselectionoutputs$SelectedTreesPoints %>%
                                                st_buffer(dist = advancedloggingparameters$ScndTrailWidth/2 +2) ),
                               y = SelectedRaster,
                               field = CostMatrix[[2]][[3]]$CostValue/2,
                               update = TRUE)
  SelectedRaster <- crop(SelectedRaster, CostRaster)

  #Update Cost Raster with protection buffers

  CostRaster <- CostRaster + BigTreesRaster + ReserveRaster + FutureRaster + SelectedRaster


  #Aggregation each raster to selected resolution



  CostRasterMean <- aggregate(CostRaster, fact=fact, fun=mean)
  CostRasterMean <- crop(CostRasterMean,  CostRaster)
  CostRasterMean <- mask(CostRasterMean, plotmask)

  DTMExtended <- crop(DTMExtended,  CostRasterMean)
  DTMExtended <- mask(DTMExtended, plotmask)

  DTMmean <- aggregate(DTMExtended, fact=fact, fun=mean)
  DTMmean <- crop(DTMmean,  CostRasterMean)
  DTMmean <- mask(DTMmean, plotmask)


  #Generate maintrail intersection cost raster
  CostRasterMean <- rasterize(x = as_Spatial(AccessPointAll %>% st_buffer(dist = advancedloggingparameters$ScndTrailWidth+2)),
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

    CostRasterGrpl <- rasterFromXYZ(CostSlopeRasterGrpl, crs = crs(topography))

    CostRasterGrpl <- CostRaster + CostRasterGrpl

    CostRasterGrpl <- rasterize(x = as_Spatial(AccessPointAll %>% st_buffer(dist = advancedloggingparameters$ScndTrailWidth+2)),
                                y = CostRasterGrpl ,
                                field = CostMatrix[[2]][[6]]$CostValue,
                                update = TRUE)

    CostRasterMeanGrpl <- aggregate(CostRasterGrpl, fact=fact, fun=mean)
    CostRasterMeanGrpl <- crop(CostRasterMeanGrpl,  CostRaster)
    CostRasterMeanGrpl <- mask(CostRasterMeanGrpl, plotmask)


  }


  #Compute conductance raster
  CondSurf <- 1/CostRasterMean


  pathLines <- list() #Initialize storage pathlines
  Lines <- list() #Initialize storage logged trees
  k <- 1 #Initialize pathlines index
  l <- 1 #Initialize lines index

  #Generate appropriate selected trees points format


  ptsAll <- treeselectionoutputs$SelectedTreesPoints %>%
    dplyr::select(idTree) %>%
    st_cast("POINT") %>%
    mutate(ID = NA) %>%
    mutate(type = "Tree") %>%
    st_set_crs(st_crs(AccessPointAll)) %>%
    st_join(AccessMainTrails %>% st_buffer(dist =  max((advancedloggingparameters$ScndTrailWidth/2),fact)) %>%
              mutate(ID_Acc = ID) %>% dplyr::select(-ID)) %>%
    dplyr::select(ID,ID_Acc,type,idTree)

  # Reassign Selected Tree values (= BigTrees) to the aggregated Cost raster
  CostRasterMean <- rasterize(x = as_Spatial(ptsAll %>% st_buffer(dist = max(2*fact,advancedloggingparameters$ScndTrailWidth/2 + 2))),
                              y = CostRasterMean ,
                              field = CostMatrix[[2]][[3]]$CostValue,
                              update = TRUE)
  if (winching == "2") {
    # Reassign Selected Tree values (= BigTrees) to the aggregated Cost raster (Grpl)
    CostRasterMeanGrpl <- rasterize(x = as_Spatial(ptsAll %>% st_buffer(dist = max(2*fact,advancedloggingparameters$ScndTrailWidth/2 + 2))),
                                    y = CostRasterMeanGrpl ,
                                    field = CostMatrix[[2]][[3]]$CostValue,
                                    update = TRUE)
  }

  #Generate maintrail intersection cost raster
  CostRasterMean <- rasterize(x = as_Spatial(AccessPointAll %>% st_buffer(dist = advancedloggingparameters$ScndTrailWidth/2)),
                              y = CostRasterMean ,
                              field = CostMatrix[[2]][[6]]$CostValue,
                              update = TRUE)

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

    winching <- WinchingInit
    pts <- ptsAll %>% filter(ID_Acc == ID_Access) %>%  dplyr::select(-ID_Acc)
    AccessPoint <- AccessPointAll %>% filter(ID == ID_Access)



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
            CostRasterMean  <- rasterize(x = TmpTreePts[TreeId,] ,
                                         y = CostRasterMean  ,
                                         field = CostMatrix[[2]][[7]]$CostValue,update =  TRUE)
          }else{
            #Update Cost raster with LCP second trail
            CostRasterMean  <- rasterize(x = TmpPathWIP[[2]] ,
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

    if (winching == "2") {
      ptsGrpl <- TreePts %>% #def Grpl point
        st_buffer(dist = advancedloggingparameters$GrappleLength) %>%
        st_snap_to_grid(size = .2) %>% # avoid GEOS error (st-intersection issue)
        #st_set_precision(1) %>%
        st_intersection() %>%
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
        st_intersection() %>%
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

      RemainTree <- max(length(unique(unlist(ptsWIP$origins))),length(unique(unlist(ptsWIP$origins))))


      #Select adjacent grpl graph
      SlopeCondRd <- SlopeCondGrpl

    }else{


      ptsCbl <- TreePts %>% #def cbl point
        st_buffer(dist = advancedloggingparameters$CableLength) %>%
        st_snap_to_grid(size = 1) %>% # avoid GEOS error (st-intersection issue)
         st_set_precision(1) %>%
        st_intersection() %>%
        st_make_valid() %>%
        mutate(IDpts = paste0("I.",row_number())) %>%
        mutate(type =  "Inter") %>% dplyr::select(-ID)


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
                             st_geometry(MainTrails %>%
                                           st_as_sf()),
                             sparse = FALSE)) %>%
        arrange(desc(n.overlaps))

      RemainTree <- length(unique(ptsWIP$idTree))

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


      #Switch from grpl to cbl exploitation when grapple accessible tree != 0
      Grpl2CblFlag <- FALSE



      ptsWIPmax <- rbind(PointAcc,ptsWIP %>%
                           filter(n.overlaps == max(ptsWIP$n.overlaps))) %>%
        mutate(TypeAcc = NA) %>%
        mutate(EstCost = NA)  %>%
        mutate(ptsAcc = NA)

      ptsTreeWIP <-  rbind(PointAcc,TreePts %>%
                             mutate(n.overlaps = NA, origins = idTree,IDpts = NA) %>%
                             dplyr::select(-ID)) %>%
        mutate(TypeAcc = NA) %>%
        mutate(EstCost = NA)  %>%
        mutate(ptsAcc = NA)

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
        ptsDirAcc <- ptsTreeWIP %>%  mutate(gprlAcc  = c(FALSE,as.numeric(st_distance(ptsTreeWIP,st_union(paths,PointAcc))[2:dim(ptsTreeWIP)[1],1]) < advancedloggingparameters$GrappleLength)) %>%
          filter(gprlAcc == TRUE | type == "Access") %>% dplyr::select(-gprlAcc)

        TmpTypeAcc <- "Grpl"

      }else{
        ptsDirAcc <- ptsTreeWIP %>%  mutate(cblAcc  = c(FALSE,as.numeric(st_distance(ptsTreeWIP,st_union(paths,PointAcc))[2:dim(ptsTreeWIP)[1],1]) < advancedloggingparameters$CableLength)) %>%
          filter(cblAcc == TRUE | type == "Access") %>% dplyr::select(-cblAcc)

        TmpTypeAcc <- "Cbl"
       }
      }



      if (dim(ptsDirAcc)[1] > 1) {

        PointTreeWIP <- ptsDirAcc %>% filter(type == "Tree")

        Lines[[l]] <- list("LineID" = NA,"LoggedTrees" = PointTreeWIP$origins, "TypeExpl" = TmpTypeAcc)


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




        #Define WIP point according to possible exploitation type
        if (PointTree$TypeAcc[1] == "Grpl") {
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
        CostRasterMean  <- rasterize(x = TmpPathWIP[[2]] ,
                                     y = CostRasterMean  ,
                                     field = CostMatrix[[2]][[7]]$CostValue,update =  TRUE)
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

      Lines[[l]] <- list("LineID" = k,"LoggedTrees" = PointTreeWIP$origins[[1]], "TypeExpl" = TmpTypeAcc)

      k <- k +1
      l <- l+1

      paths <- do.call(rbind, pathLines) %>%
        st_as_sf()  %>%
        st_difference(PartMainTrails %>% st_buffer(dist = 2) %>% st_union()) %>%
        smoothr::smooth(method = "ksmooth", smoothness = 5) %>%
        st_buffer(dist = advancedloggingparameters$ScndTrailWidth/2) %>%
        st_union()



      }


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
            st_intersection() %>%
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
            st_intersection() %>%
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
            st_intersection() %>%
            st_make_valid() %>%
            mutate(IDpts = paste0("I.",row_number())) %>%
            mutate(type =  "Inter") %>% dplyr::select(-ID)


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
                                 st_geometry(MainTrails %>%
                                               st_as_sf()),
                                 sparse = FALSE)) %>%
            arrange(desc(n.overlaps))



          #Select adjacent cbl graph
          SlopeCondRd <- SlopeCond

        }

        }

        RemainTree <- dim(TreePts)[1]





      if (length(unique(unlist(ptsWIP$origins))) == 0 & WinchingSwitch == FALSE & winching == "2") {
        winching <- "1"
        ptsWIP <- ptsWIPCbl
        WinchingSwitch <- TRUE
      }

    }


  }

  }

  paths <- do.call(rbind, pathLines)

  if (length(paths)> 0) {

    lines <- do.call(rbind, Lines)

    secondtrails <- smoothtrails(paths,
                                 plotmask,
                                 advancedloggingparameters = advancedloggingparameters)

    SmoothedSecondTrails <- secondtrails$SmoothedSecondTrails %>% st_set_crs(st_crs(topography))

    TrailsDensity <- secondtrails$TrailsDensity
    #
    #
    # # Records the dead trees
    inventory <- treeselectionoutputs$inventory

    if (!("DeathCause" %in% names(inventory))){
      inventory <- inventory %>%
        add_column(DeathCause = NA) # if "DeathCause" column doesnt exist create it
    }

    DeadTrees <- suppressWarnings(sf::st_intersection(
      st_set_crs(st_as_sf(inventory, coords = c("Xutm", "Yutm")), st_crs(topography)),
      sf::st_make_valid(st_as_sf(SmoothedSecondTrails)) # "make valid" to avoid self-intersection
    )) %>%
      add_column(DeadTrees = "1") %>%
      dplyr::select(idTree, DeadTrees)
    sf::st_geometry(DeadTrees) <- NULL # remove geometry column (sf to data.frame)
    DeadTrees <- unique(DeadTrees)

    inventory <- inventory %>%
      left_join(DeadTrees, by = "idTree") %>%
      mutate(DeathCause = ifelse(is.na(DeathCause) & Selected != "1" & DeadTrees == "1",
                                 "2ndTrail", DeathCause)) %>%  # Damage trees
      dplyr::select(-DeadTrees)

  }else{
    paths <- NULL
    lines <- do.call(rbind, Lines)
    SmoothedSecondTrails <- AccessPointAll %>% st_buffer(dist = 1) %>% st_union() %>% st_set_crs(st_crs(topography))
    TrailsDensity <- 0

    inventory <- treeselectionoutputs$inventory

    if (!("DeathCause" %in% names(inventory))){
      inventory <- inventory %>%
        add_column(DeathCause = NA) # if "DeathCause" column doesnt exist create it
    }

    DeadTrees <- suppressWarnings(sf::st_intersection(
      st_set_crs(st_as_sf(inventory, coords = c("Xutm", "Yutm")), st_crs(topography)),
      sf::st_make_valid(st_as_sf(SmoothedSecondTrails)) # "make valid" to avoid self-intersection
    )) %>%
      add_column(DeadTrees = "1") %>%
      dplyr::select(idTree, DeadTrees)
    sf::st_geometry(DeadTrees) <- NULL # remove geometry column (sf to data.frame)
    DeadTrees <- unique(DeadTrees)

    inventory <- inventory %>%
      left_join(DeadTrees, by = "idTree") %>%
      mutate(DeathCause = ifelse(is.na(DeathCause) & Selected != "1" & DeadTrees == "1",
                                 "2ndTrail", DeathCause)) %>%  # Damage trees
      dplyr::select(-DeadTrees)
  }


  #

  secondtrails <- list("RawSecondTrails" = paths,
                       "TrailsIdentity" = lines,        # "LineID","LoggedTrees", "TypeExpl"
                       "SmoothedSecondTrails" =  SmoothedSecondTrails,
                       "TrailsDensity" =  TrailsDensity,
                       "inventory" =  inventory,
                       "CostRasterAgg" = CostRasterMean)

  return(secondtrails)

}

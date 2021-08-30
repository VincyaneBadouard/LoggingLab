#' Harvestable trees identification
#'
#'@description The function tells you the harvestable volume in the plot, and
#'  which trees are harvestable according to your harvestability criteria
#'
#'@param inventory Your inventory (see the inputs formats and metadata in the
#'  \code{\link{vignette}}) (data.frame)
#'
#'@param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'  or SRTM) (\code{\link{DTMParacou}}) (RasterLayer)
#'
#'@param plotslope Slopes (in radians) of the inventoried plot (with a
#'  neighbourhood of 8 cells) (default: \code{\link{PlotSlope}}) (RasterLayer)
#'
#'@param diversification Taking of other species in addition to the main
#'  commercial species (2 levels of commercial species in the
#'  \code{\link{SpeciesCriteria}} table) (logical)
#'
#'@param specieslax Allow diversification if stand is too poor, = FALSE by
#'  default (logical)
#'
#'@param advancedloggingparameters Other parameters of the logging simulator
#'  \code{\link{loggingparameters}} (list) MainTrail (multiline)
#'
#'@return Your inventory with the exploitability criteria, and if they are
#'  validated for each of the trees. The function returns the harvestable volume
#'  too, in the plot for these criteria.
#'
#'@seealso  \code{\link{Paracou6_2016}}, \code{\link{SpeciesCriteria}},
#'  \code{\link{DTMParacou}}, \code{\link{PlotSlope}},
#'  \code{\link{loggingparameters}}
#'
#'@export
#'
#'@importFrom dplyr filter mutate select left_join if_else
#'@importFrom tibble as_tibble add_column
#'@importFrom sp coordinates proj4string
#'@importFrom sf st_as_sf st_distance
#'@importFrom topoDistance topoDist
#'@importFrom raster crs extract
#'@importFrom utils setTxtProgressBar txtProgressBar
#'
#'
#' @examples
#'
#' data(Paracou6_2016)
#' data(DTMParacou)
#' data(PlotSlope)
#' data(SpeciesCriteria)
#'
#' inventory <- addtreedim(inventorycheckformat(Paracou6_2016),
#' volumeparameters = ForestZoneVolumeParametersTable)
#'
#' inventory <- ONFGuyafortaxojoin(inventory, SpeciesCriteria)
#'
#' harvestableOutputs <- harvestable(inventory, topography = DTMParacou,
#' diversification = TRUE, plotslope = PlotSlope, specieslax = FALSE,
#' advancedloggingparameters = loggingparameters())
#'
harvestable <- function(
  inventory,
  topography,
  plotslope,
  diversification,
  specieslax = FALSE,
  advancedloggingparameters = loggingparameters()
){

  # Arguments check

  if(!inherits(inventory, "data.frame"))
    stop("The 'inventory' argument of the 'harvestable' function must be a data.frame")

  if(!all(unlist(lapply(list(diversification, specieslax), inherits, "logical"))))
    stop("The 'diversification' and 'specieslax' arguments of the 'harvestable' function must be logical")

  if(!all(unlist(lapply(list(topography, plotslope), inherits, "RasterLayer"))))
    stop("The 'topography' and 'plotslope' arguments of the 'harvestable' function must be RasterLayer")

  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <- Commercial <- NULL
  Commercial.genus <- Commercial.species <- Condition <- DBH <- NULL
  DeathCause <- DistCrit <- Family <- NULL
  ForestZoneVolumeParametersTable <- Genus <- Logged <- NULL
  TimberLoggedVolume <- LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  NoHollowTimberLoggedVolume <- ParamCrownDiameterAllometry <- PlotSlope <- NULL
  PlotTopo <- ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- Slope <- SlopeCrit <- Species <- Species.genus <- NULL
  SpeciesCriteria <- Taxo <- Taxo.family <- Taxo.genus <- Taxo.species <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD <- UpMinFD.genus <- NULL
  UpMinFD.species <- VernName.genus <- VernName.genus.genus <- NULL
  VernName.species <- VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL
  alpha <- alpha.family <- alpha.genus <- alpha.species <- bCoef <- NULL
  beta.family <- beta.genus <- beta.species <- geometry <- idTree <- NULL


  # Calculation of spatial information (distance and slope)
  SpatInventory <- inventory %>%
    filter(Commercial!= "0") %>%  # only take commercial sp, the calculation time is long enough
    filter(DBH >= MinFD & DBH <= MaxFD) # already selected commercial DBHs

  sp::coordinates(SpatInventory) <- ~ Xutm + Yutm # transform the inventory into a spatial object by informing the coordinates

  sp::proj4string(SpatInventory) <- raster::crs(topography) # allocate the Paracou crs to our spatial inventory

  SlopeTmp <- as_tibble(raster::extract(x = plotslope, y = SpatInventory)) # extracts the slope values for the inventory spatialized points

  SpatInventory <- st_as_sf(SpatInventory) %>%  # transforming the spatialized inventory into an sf object
    add_column(DistCrit = FALSE) # Create a default 'DistCrit' = FALSE column

  # i = 1
  # ProgressBar <- txtProgressBar(min = 0, max = nrow(SpatInventory),style = 3) # Progression bar

  # Calculating distances between trees of the same species

  for (SpecieI in unique(SpatInventory$ScientificName)) {

    SpatInventorytmp <- SpatInventory %>% # SpatInventorytmp stores only result, that of each turn
      filter(ScientificName == SpecieI)

    # SpatInventorytmp <- as_Spatial(SpatInventorytmp)
    # distSp <- topoDist(topography = PlotTopo, pts = SpatInventorytmp) # calculates topo distances
    # distSp <- as_tibble(distSp)

    distSp <- st_distance(SpatInventorytmp)
    units(distSp) <- NULL
    distSp <- suppressMessages(as_tibble(distSp, .name_repair = "universal"))

    distSp[distSp == 0] <- NA # don't take into account the 0's in the matrix, which are the dist from the tree to itself
    distSp[distSp == Inf] <- NA # don't take into account the Inf which are the trees outside the plot.

    for (ind in 1:dim(distSp)[2]) { # '2' for column
      if (all(is.na(distSp[,ind]))) {FALSE # if all the column contains NA it is an Inf so we don't want it
      }else{
        # if the minimum distance to its congeners is < 100, it is exploitable:
        SpatInventorytmp$DistCrit[ind] <- min(distSp[,ind],na.rm = TRUE) < advancedloggingparameters$IsolateTreeMinDistance
      }
      SpatInventory$DistCrit[SpatInventory$idTree == SpatInventorytmp$idTree[ind]] <- SpatInventorytmp$DistCrit[ind]
      # i = i+1 # and inform the progress bar
      # setTxtProgressBar(ProgressBar, i)
    }
  }


  SlopeCritInventory <- SpatInventory %>%   # SpatInventory <- SpatInventory before
    add_column(Slope = SlopeTmp$value) %>% # add slope values per tree
    # the NaN are the infinite values, which indicate a plateau so slope = 0:
    mutate(Slope = ifelse(is.nan(Slope), 0, Slope)) %>%
    mutate(SlopeCrit = if_else(
      condition = Slope <= atan(advancedloggingparameters$TreeMaxSlope/100), # if slope <= 22% the tree is exploitable (we are in radian)
      TRUE,
      FALSE)) %>%
    dplyr::select(idTree, DistCrit, Slope, SlopeCrit)

  inventory <- inventory %>%
    left_join(SlopeCritInventory, by = "idTree") %>%
    dplyr::select(-geometry)

  # Essences selection
  HarverstableConditions <- # = 1 boolean vector
    if (diversification || (!diversification && specieslax)) {
      inventory$Commercial =="1"| inventory$Commercial == "2" # now or maybe after we will diversify
    } else if (!diversification && !specieslax) {
      inventory$Commercial == "1" # We will never diversify
    }


  # Diameters selection
  HarverstableConditions <- HarverstableConditions & (inventory$DBH >= inventory$MinFD & inventory$DBH <= inventory$MaxFD) # harvestable individuals, accord by their DBH

  # Select spatially
  HarverstableConditions <- HarverstableConditions & (inventory$DistCrit == TRUE & inventory$SlopeCrit == TRUE) # !is.null(ProspectionUnitCode) &
  ## in a PU
  ## slope
  ## isolement
  ## MainTrails out (only for ONF plots)

  inventory <- inventory %>%
    mutate(LoggingStatus = ifelse(HarverstableConditions, #Under the above criteria, designate the harvestable species
                                  "harvestable", "non-harvestable")) %>%
    mutate(LoggingStatus = ifelse(Commercial == "0", #The non-commercial species are non-harvestable.
                                  "non-harvestable", LoggingStatus)) %>%

    mutate(LoggingStatus = ifelse(
      !diversification &
        specieslax & #designate the secondarily harvestable species, because diversification only if necessary
        LoggingStatus == "harvestable" &
        Commercial == "2",
      "harvestable2nd", LoggingStatus))

  HarvestableTable <- inventory %>%
    filter(LoggingStatus == "harvestable")
  HVinit <- sum(HarvestableTable$TreeHarvestableVolume) #compute the harvestable volume in the plot for these criteria

  harvestableOutputs <- list(inventory = inventory, HVinit = HVinit)

  return(harvestableOutputs) # return the new inventory and the HVinit
}

#'Harvestable trees identification
#'
#'@description The function tells you the harvestable volume in the plot, and
#'  which trees are harvestable according to your harvestability criteria
#'
#'@param inventory Input inventory (see the inputs formats and metadata in the
#'  vignette) (data.frame)
#'
#'@param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'  or SRTM) (\code{\link{DTMParacou}}) (RasterLayer **with a crs  in UTM**)
#'  We advise you to generate your raster with Qgis rather than with the
#'  'raster' package on R.
#'
#'@param diversification Taking of other species in addition to the main
#'  commercial species (2 levels of commercial species in the
#'  \code{\link{SpeciesCriteria}} table) (logical)
#'
#'@param specieslax Allow diversification if stand is too poor, = FALSE by
#'  default (logical)
#'
#'@param harvestablepolygons Accessible area of the inventoried plot
#'  (default: \code{\link{harvestableareadefinition}}) (sfc_MULTIPOLYGON)
#'
#'@param plotslope Slopes (in radians) of the inventoried plot (with a
#'  neighbourhood of 8 cells) (default:
#'  \code{\link{HarvestableAreaOutputsCable}})
#'  (RasterLayer **with a crs in UTM**)
#'
#'@param scenario Logging scenario: "RIL1", "RIL2broken", "RIL2", "RIL3",
#'  "RIL3fuel", "RIL3fuelhollow" or "manual"(character) (see the
#'  vignette)
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
#'@param advancedloggingparameters Other parameters of the logging simulator
#'  (\code{\link{loggingparameters}}) (list)
#'
#'@return input inventory with new columns:
#' - The exploitability criteria ("DistCriteria", "Slope"(in radians), "SlopeCriteria"), and if
#'        they are validated for each of the trees ("LoggingStatus").
#' - The probability of a tree having visible defects ("VisibleDefectProba")
#'         and the visible defect trees ("VisibleDefect").
#'
#' The function returns the harvestable volume too, in the
#'  plot for these criteria.
#'
#'@details Trees will be designated as "**harvestable**" if they:
#'- belonging to species of 1st economic rank or more if diversification
#'- DBH between the MinFD and the MaxFD.
#'- not isolated ( >100m ('IsolateTreeMinDistance' in
#'   \code{\link{loggingparameters}})) from other individuals of the same
#'   species in the aggregative species case (\code{\link{SpeciesCriteria}},
#'   'Aggregative' column).
#'- on slopes < 22% ('TreeMaxSlope'in \code{\link{loggingparameters}})
#'- off the main trails.
#'
#'Trees with visible defects are identified ('VisiblyDefectModel' in
#''advancedloggingparameters' argument) among the trees with harvestable
#'criteria and are therefore considered 'non-harvestable'.
#'
#'@seealso  \code{\link{Paracou6_2016}}, \code{\link{SpeciesCriteria}},
#'  \code{\link{DTMParacou}}, \code{\link{HarvestableAreaOutputsCable}},
#'  \code{\link{loggingparameters}}
#'
#'@export
#'
#'@importFrom dplyr filter mutate select left_join if_else
#'@importFrom tibble as_tibble add_column
#'@importFrom sp coordinates proj4string
#'@importFrom sf st_as_sf st_distance st_contains st_union
#'@importFrom raster crs extract
#'@importFrom utils setTxtProgressBar txtProgressBar
#'
#'
#'@examples
#' data(Paracou6_2016)
#' data(DTMParacou)
#' data(PlotMask)
#' data(SpeciesCriteria)
#' data(HarvestableAreaOutputsCable)
#'
#' inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask),
#' volumeparameters = ForestZoneVolumeParametersTable)
#'
#' inventory <- commercialcriteriajoin(inventory, SpeciesCriteria)
#'
#' harvestableOutputs <- harvestable(inventory, topography = DTMParacou,
#' diversification = TRUE, specieslax = FALSE,
#' plotslope = HarvestableAreaOutputsCable$PlotSlope,
#' harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons,
#' scenario = "manual", winching = "0",
#' advancedloggingparameters = loggingparameters())
#'
#' new <- harvestableOutputs$inventory
#'
harvestable <- function(
  inventory,
  topography,
  diversification,
  specieslax = FALSE,
  harvestablepolygons,
  plotslope,
  scenario,
  winching = NULL,
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
  Accessible <- Circ <- CircCorr <- CodeAlive <- CommercialLevel <- NULL
  Condition <- DBH <- NULL
  DistCriteria <- Family <- VisibleDefect <- VisibleDefectProba <- NULL
  DeathCause <- ForestZoneVolumeParametersTable <- Genus <- Logged <- NULL
  TimberLoggedVolume <- LoggingStatus <- MaxFD <- MaxFD.genus <- LogDBH <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  NoHollowTimberLoggedVolume <- ParamCrownDiameterAllometry <- PlotSlope <- NULL
  ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- Slope <- SlopeCriteria <- Species <- Species.genus <- NULL
  SpeciesCriteria <- Taxo <- Taxo.family <- Taxo.genus <- Taxo.species <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- Aggregative <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD <- UpMinFD.genus <- NULL
  UpMinFD.species <- NULL
  VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL
  alpha <- alpha.family <- alpha.genus <- alpha.species <- bCoef <- NULL
  beta.family <- beta.genus <- beta.species <- geometry <- idTree <- HarvestableZone <- NULL


  #### Redefinition of the parameters according to the chosen scenario ####
  scenariosparameters <- scenariosparameters(scenario = scenario, winching = winching)
  winching <- scenariosparameters$winching

  ##################################

  # Calculation of spatial information (distance and slope)
  SpatInventory <- inventory %>%
    dplyr::filter(CommercialLevel!= "0") %>%  # only take commercial sp, the calculation time is long enough
    dplyr::filter(DBH >= MinFD & DBH <= MaxFD) # already selected commercial DBHs

  coordinates(SpatInventory) <- ~ Xutm + Yutm # transform the inventory into a spatial object by informing the coordinates

  proj4string(SpatInventory) <- crs(topography) # allocate the Paracou crs to our spatial inventory

  SlopeTmp <- as_tibble(extract(x = plotslope, y = SpatInventory)) # extracts the slope values for the inventory spatialized points

  SpatInventory <- st_as_sf(SpatInventory) %>%  # transforming the spatialized inventory into an sf object
    add_column(DistCriteria = NA) # Create a default 'DistCriteria' = FALSE column

  # i = 1
  # ProgressBar <- txtProgressBar(min = 0, max = nrow(SpatInventory),style = 3) # Progression bar


  # Calculating distances between trees of the same species, in aggregative species case
  AggregativeSp <- unique(filter(SpatInventory, Aggregative)$ScientificName)

  if(length(AggregativeSp)>0){ # if there are any aggregative species
    for (SpecieI in AggregativeSp) { # for each sp

      SpatInventorytmp <- SpatInventory %>% # SpatInventorytmp stores only result, that of each turn
        filter(ScientificName == SpecieI) %>%
        mutate(DistCriteria = FALSE)

      # SpatInventorytmp <- as_Spatial(SpatInventorytmp)
      # distSp <- topoDist(topography = topography, pts = SpatInventorytmp) # calculates topo distances
      # distSp <- as_tibble(distSp)

      distSp <- st_distance(SpatInventorytmp) # calculates distances without topo
      units(distSp) <- NULL
      distSp <- suppressMessages(as_tibble(distSp, .name_repair = "universal"))

      distSp[distSp == 0] <- NA # don't take into account the 0's in the matrix, which are the dist from the tree to itself
      distSp[distSp == Inf] <- NA # don't take into account the Inf which are the trees outside the plot.

      for (ind in 1:dim(distSp)[2]) { # '2' for column
        if (all(is.na(distSp[,ind]))) {FALSE # if all the column contains NA it is an Inf so we don't want it
        }else{
          # if the minimum distance to its congeners is < 100, it is harvestable:
          SpatInventorytmp$DistCriteria[ind] <- min(distSp[,ind],na.rm = TRUE) < advancedloggingparameters$IsolateTreeMinDistance
        }
        SpatInventory$DistCriteria[SpatInventory$idTree == SpatInventorytmp$idTree[ind]] <- SpatInventorytmp$DistCriteria[ind]
        # i = i+1 # and inform the progress bar
        # setTxtProgressBar(ProgressBar, i)
      }
    } # Calculating distances end
  }

 if (winching == "0") {
   TreeMaxSlope <- advancedloggingparameters$MaxTrailCenterlineSlope # 22%
 }else{
   TreeMaxSlope <- advancedloggingparameters$CableTreesMaxSlope # 35%
 }


  DistCriteriaInventory <- SpatInventory %>%   # SpatInventory <- SpatInventory before
    add_column(Slope = SlopeTmp$value) %>% # add slope values per tree
    # the NaN are the infinite values, which indicate a plateau so slope = 0:
    mutate(Slope = ifelse(is.nan(Slope), 0, Slope)) %>%
    mutate(SlopeCriteria = if_else(
      condition = Slope <= atan(TreeMaxSlope/100), # if slope <= 22% the tree is exploitable (we are in radian)
      TRUE,
      FALSE)) %>%
    dplyr::select(idTree, DistCriteria, Slope, SlopeCriteria)

  inventory <- inventory %>%
    left_join(DistCriteriaInventory, by = "idTree") %>%
    dplyr::select(-geometry)

  # Check that the trees are contained in a accessible area (HarvestableZone)

   AccessPolygons <- harvestablepolygons

   # Calculation of spatial information (distance and slope)
   SpatInventoryAll <- inventory  # already selected commercial DBHs

   coordinates(SpatInventoryAll) <- ~ Xutm + Yutm # transform the inventory into a spatial object by informing the coordinates

   proj4string(SpatInventoryAll) <- crs(topography) # allocate the Paracou crs to our spatial inventory


   HarvestableZoneSpatInventory <- st_as_sf(SpatInventoryAll) %>%
     mutate(HarvestableZone = as.vector(st_contains(AccessPolygons %>%
                                         st_union(), st_as_sf(SpatInventoryAll),sparse = F))) %>% # check if trees are contained in HarvestableZones
     dplyr::select(idTree, HarvestableZone)

   inventory <- inventory %>% left_join(HarvestableZoneSpatInventory, by = "idTree")%>%
     dplyr::select(-geometry)

  # Essences selection
  HarverstableConditions <- # = 1 boolean vector
    if (diversification || (!diversification && specieslax)) {
      inventory$CommercialLevel =="1"| inventory$CommercialLevel == "2" # now or maybe after we will diversify
    } else if (!diversification && !specieslax) {
      inventory$CommercialLevel == "1" # We will never diversify
    }


  # Diameters selection
  HarverstableConditions <- HarverstableConditions & (inventory$DBH >= inventory$MinFD & inventory$DBH <= inventory$MaxFD) # harvestable individuals, accord by their DBH

  # Select spatially
  HarverstableConditions <- HarverstableConditions & (
    (!inventory$DistCriteria %in% FALSE) & # take the TRUE and NA values
      inventory$SlopeCriteria %in% TRUE &
      inventory$HarvestableZone %in% TRUE ) # !is.null(ProspectionUnitCode) &
  ## in a HarvestableZone
  ## slope
  ## isolement
  ## MainTrails out (only for ONF plots)

  inventory <- inventory %>%
    mutate(LoggingStatus = ifelse(HarverstableConditions, #Under the above criteria, designate the harvestable species
                                  "harvestable", "non-harvestable")) %>%
    mutate(LoggingStatus = ifelse(CommercialLevel == "0", #The non-commercial species are non-harvestable.
                                  "non-harvestable", LoggingStatus)) %>%

    mutate(LoggingStatus = ifelse(
      !diversification &
        specieslax & #designate the secondarily harvestable species, because diversification only if necessary
        LoggingStatus == "harvestable" &
        CommercialLevel == "2",
      "harvestable2nd", LoggingStatus))


  # Visible defect trees detection:
  inventory <- inventory %>%
    mutate(LogDBH = log(DBH)) %>% # DBH is logged in the model

    mutate(VisibleDefectProba = ifelse(LoggingStatus == "harvestable" | LoggingStatus == "harvestable2nd",
                                       advancedloggingparameters$VisiblyDefectModel(LogDBH), NA)) %>%  # Proba to have defects for each tree


    # generate either "1" or "0" randomly for each line, depending on the proba associated with the line:
    rowwise() %>%
    mutate(VisibleDefect = ifelse(!is.na(VisibleDefectProba),
                                  sample(c(1,0), size = 1, replace = F,
                                         prob = c(VisibleDefectProba, 1-VisibleDefectProba)), NA)) %>% # 1 = visible defects tree, 0 = no visible defects
    ungroup() %>%
    mutate(VisibleDefect = as.factor(VisibleDefect)) %>%
    mutate(LoggingStatus = ifelse(VisibleDefect %in% "1", "non-harvestable", #& !is.na(VisibleDefect)
                                  LoggingStatus))


  #compute the harvestable volume in the plot for these criteria
  HarvestableTable <- inventory %>%
    filter(LoggingStatus == "harvestable")
  HVinit <- sum(HarvestableTable$TreeHarvestableVolume)

  harvestableOutputs <- list(inventory = inventory, HVinit = HVinit)

  return(harvestableOutputs) # return the new inventory and the HVinit
}

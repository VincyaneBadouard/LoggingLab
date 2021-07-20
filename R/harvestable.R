#'harvestable
#'
#'@description The function tells you the harvestable volume in the plot, and
#'  which trees are harvestable according to your harvestability criteria
#'
#'@param inventory your inventory (see the inputs formats and metadata in the
#'  \code{\link{vignette}}) (data.frame)
#'@param diversification Taking of other species in addition to the main
#'  commercial species (2 levels of commercial species in the
#'  \code{\link{SpeciesCriteria}} table) (logical)
#'
#'@param specieslax Allow diversification if stand is too poor, = FALSE by
#'  default (logical)
#'
#'@param DEM Digital terrain model (DTM) of the inventoried plot (LiDAR or SRTM)
#'  (default: \code{\link{DemParacou}}) (RasterLayer)
#'
#'@param plotslope Slopes (in radians) of the inventoried plot (with a
#'  neighbourhood of 8 cells) (default: \code{\link{PlotSlope}}) (RasterLayer)
#'
#'@param advancedloggingparameters Other parameters of the logging simulator
#'  \code{\link{loggingparameters}} (list) MainTrail (multiline)
#'
#'@return Your inventory with the exploitability criteria, and if they are
#'  validated for each of the trees. The function returns the harvestable volume
#'  too, in the plot for these criteria.
#'
#' @seealso  \code{\link{Paracou6_2016}}, \code{\link{SpeciesCriteria}},
#'  \code{\link{DemParacou}}, \code{\link{PlotSlope}},
#'  \code{\link{loggingparameters}}
#'
#'@export
#'
#'@import sf
#'@importFrom sp coordinates proj4string
#'@importFrom topoDistance topoDist
#'@importFrom raster crs extract
#'@importFrom  utils setTxtProgressBar txtProgressBar
#'
#'
#' @examples
#'
#' data(Paracou6_2016)
#' data(DemParacou)
#'
#' inventory <- ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016))))
#' test <- harvestable(inventory, diversification = TRUE, specieslax = FALSE,
#' DEM = DemParacou, plotslope = PlotSlope,advancedloggingparameters = loggingparameters())
#'
harvestable <- function(
  inventory,
  diversification,
  specieslax = FALSE,
  DEM = DemParacou,
  plotslope = PlotSlope,
  advancedloggingparameters = loggingparameters()
){

  # Arguments check

  if(!inherits(inventory, "data.frame"))
    stop("The 'inventory' argument of the 'harvestable' function must be a data.frame")

  if(!any(unlist(lapply(list(diversification, specieslax), inherits, "logical"))))
    stop("The 'diversification' and 'specieslax' arguments of the 'harvestable' function must be logical") # any() don't take a list

  if(!any(unlist(lapply(list(DEM, plotslope), inherits, "RasterLayer"))))
    stop("The 'DEM' and 'plotslope' arguments of the 'harvestable' function must be RasterLayer")

  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <- Commercial <- NULL
  Commercial.genus <- Commercial.species <- Condition <- DBH <- NULL
  DeathCause <- DistCrit <- Family <- NULL
  ForestZoneVolumeParametersTable <- Genus <- Logged <- NULL
  LoggedVolume <- LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  NoHollowLoggedVolume <- ParamCrownDiameterAllometry <- PlotSlope <- NULL
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
    filter(Commercial!= "0") %>%  # ne prendre que les individus d'sp commerciales, le temps de calcul est déjà bien assez long
    filter(DBH >= MinFD & DBH <= MaxFD) # ne prendre que les individus d'sp commerciales, le temps de calcul est déjà bien assez long

  sp::coordinates(SpatInventory) <- ~ Xutm + Yutm # transformer l'inventaire en objet spatialisé en en informant les coordonnées

  sp::proj4string(SpatInventory) <- raster::crs(DEM) # attribuer le crs de Paracou à notre inventaire spatialisé

  SlopeTmp <- as_tibble(raster::extract(x = plotslope, y = SpatInventory)) # extrait les valeurs de pentes pour les points spatialisés de l'inventaire

  SpatInventory <- st_as_sf(SpatInventory) %>%  # transformer l'inventaire spatialisé en objet sf
    add_column(DistCrit = FALSE) # Créer une colonne DistCrit FALSE par défaut

  # i = 1
  # ProgressBar <- txtProgressBar(min = 0, max = nrow(SpatInventory),style = 3) # barre de progression du calcul

  # Calcul des distances entre les arbres de la même espèce

  for (SpecieI in unique(SpatInventory$ScientificName)) {

    SpatInventorytmp <- SpatInventory %>% # SpatInventorytmp stocke 1 seul résulat, celui de chaque tour
      filter(ScientificName == SpecieI)

    # SpatInventorytmp <- as_Spatial(SpatInventorytmp)
    # distSp <- topoDist(DEM = PlotTopo, pts = SpatInventorytmp) # calcul des distances topo
    # distSp <- as_tibble(distSp)

    distSp <- st_distance(SpatInventorytmp)
    units(distSp) <- NULL
    distSp <- suppressMessages(as_tibble(distSp, .name_repair = "universal"))

    distSp[distSp == 0] <- NA # ne pas prendre en compte les 0 dans la matrice, qui sont les dist de l'arbre à lui-même.
    distSp[distSp == Inf] <- NA # ne pas prendre en compte les Inf qui sont les arbres hors de parcelle.

    for (ind in 1:dim(distSp)[2]) { # 2 pour colonne
      if (all(is.na(distSp[,ind]))) {FALSE # si toutes la colonne contient des NA c'est un Inf donc on ne le veut pas
      }else{
        SpatInventorytmp$DistCrit[ind] <- min(distSp[,ind],na.rm = TRUE) < advancedloggingparameters$IsolateTreeMinDistance # si la distance minimale à ses congénaires est < 100, il est exploitable
      }
      SpatInventory$DistCrit[SpatInventory$idTree == SpatInventorytmp$idTree[ind]] <- SpatInventorytmp$DistCrit[ind]
      # i = i+1 # et en informer la progress bar
      # setTxtProgressBar(ProgressBar, i)
    }
  }


  SlopeCritInventory <- SpatInventory %>%   # SpatInventory <- SpatInventory before
    add_column(Slope = SlopeTmp$value) %>% # ajouter les valeurs de pentes par arbre
    # les NaN ce sont les valeurs infinies, qui témoignent d'un plateau donc pente = 0
    mutate(Slope = ifelse(is.nan(Slope), 0, Slope)) %>%
    mutate(SlopeCrit = if_else(
      condition = Slope <= atan(advancedloggingparameters$TreeMaxSlope/100), # si pente <= 22% l'arbre est exploitable (on est en radian)
      TRUE,
      FALSE)) %>%
    dplyr::select(idTree, DistCrit, Slope, SlopeCrit)

  inventory <- inventory %>%
    left_join(SlopeCritInventory, by = "idTree") %>%
    dplyr::select(-geometry)

  #select essences
  HarverstableConditions <- # = 1 boolean vector
    if (diversification || (!diversification && specieslax)) {
      inventory$Commercial =="1"| inventory$Commercial == "2" # now or maybe after we will diversify
    } else if (!diversification && !specieslax) {
      inventory$Commercial == "1" # We will never diversify
    }


  #select diameters
  HarverstableConditions <- HarverstableConditions & (inventory$DBH >= inventory$MinFD & inventory$DBH <= inventory$MaxFD) # harvestable individuals, accord by their DBH

  #select spatially
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

#' harvestable
#'
#' @description The function tells you the harvestable volume in the plot,
#'and which trees are harvestable according to your harvestability criteria
#'
#' @param inventory (data.frame)
#' @param diversification (logical)
#' @param specieslax = FALSE by default (logical)
#' @param DEM (RasterLayer)
#' @param plotslope (RasterLayer)
#' @param otherloggingparameters (list)
#'
#' @return Your inventory with the exploitability criteria, and if they are validated for each of the trees.
#' The function returns the harvestable volume too, in the plot for these criteria.
#'
#' @export
#'
#' @importFrom raster coordinates
#' @importFrom topoDistance topoDist
#'
#' @examples
#' inventory = ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016))))
#' test <- harvestable(inventory, diversification = TRUE, specieslax = FALSE, DEM = DemParacou, plotslope = PlotSlope,otherloggingparameters = loggingparameters())
#'
harvestable <- function(
  inventory,
  diversification,
  specieslax = FALSE,
  DEM = DemParacou,
  plotslope = PlotSlope,
  otherloggingparameters = loggingparameters()
){
  # Arguments check

  if(!inherits(inventory, "data.frame"))
    stop("The 'inventory' argument of the 'harvestable' function must be a data.frame")

  if(!any(unlist(lapply(list(diversification, specieslax), inherits, "logical"))))
    stop("The 'diversification' and 'specieslax' arguments of the 'harvestable' function must be logical") # any() don't take a list

  # Calculation of spatial information (distance and slope)
  SpatInventory <- inventory %>%
    filter(Commercial!= "0") %>%  # ne prendre que les individus d'sp commerciales, le temps de calcul est déjà bien assez long
    filter(DBH >= MinFD & DBH <= MaxFD) # ne prendre que les individus d'sp commerciales, le temps de calcul est déjà bien assez long



  coordinates(SpatInventory) <- ~ Xutm + Yutm # transformer l'inventaire en objet spatialisé en en informant les coordonnées

  proj4string(SpatInventory) <- raster::crs(DEM) # attribuer le crs de Paracou à notre inventaire spatialisé

  Slope_tmp <- as_tibble(raster::extract(x = plotslope, y = SpatInventory)) # extrait les valeurs de pentes pour les points spatialisés de l'inventaire

  SpatInventory <- st_as_sf(SpatInventory) %>%  # transformer l'inventaire spatialisé en objet sf
    add_column(DistCrit = FALSE) # Créer une colonne DistCrit FALSE par défaut

  i = 1
  ProgressBar <- txtProgressBar(min = 0, max = nrow(SpatInventory),style = 3) # barre de progression du calcul

  # Calcul des distances entre les arbres de la même espèce

  for (SpecieI in unique(SpatInventory$ScientificName)) {

    SpatInventorytmp <- SpatInventory %>% # SpatInventorytmp stocke 1 seul résulat, celui de chaque tour
      filter(ScientificName == SpecieI)
    SpatInventorytmp <- as_Spatial(SpatInventorytmp)

    distSp <- topoDist(DEM = PlotTopo, pts = SpatInventorytmp) # calcul des distances
    distSp <- as_tibble(distSp)
    distSp[distSp == 0] <- NA # ne pas prendre en compte les 0 dans la matrice, qui sont les dist de l'arbre à lui-même.
    distSp[distSp == Inf] <- NA # ne pas prendre en compte les Inf qui sont les arbres hors de parcelle.

    for (ind in 1:dim(distSp)[2]) { # 2 pour colonne
      if (all(is.na(distSp[,ind]))) {FALSE # si toutes la colonne contient des NA c'est un Inf donc on ne le veut pas
      }else{
        SpatInventorytmp$DistCrit[ind] <- min(distSp[,ind],na.rm = TRUE) < otherloggingparameters$IsolateTreeMinDistance # si la distance minimale à ses congénaires est < 100, il est exploitable
      }
      SpatInventory$DistCrit[SpatInventory$idTree == SpatInventorytmp$idTree[ind]] <- SpatInventorytmp$DistCrit[ind]
      i = i+1 # et en informer la progress bar
      setTxtProgressBar(ProgressBar, i)
    }
  }


  SlopeCritInventory <- SpatInventory %>%   # SpatInventory <- SpatInventory before
    add_column(Slope = Slope_tmp$value) %>% # ajouter les valeurs de pentes par arbre
    # les NaN ce sont les valeurs infinies, qui témoignent d'un plateau donc pente = 0
    mutate(Slope = ifelse(is.nan(Slope), 0, Slope)) %>%
    mutate(SlopeCrit = if_else(
      condition = Slope <= atan(otherloggingparameters$TreeMaxSlope/100), # si pente <= 22% l'arbre est exploitable (on est en radian)
      TRUE,
      FALSE)) %>%
    dplyr::select(idTree, DistCrit, Slope, SlopeCrit)

  inventory <- inventory %>%
    left_join(SlopeCritInventory) %>%
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

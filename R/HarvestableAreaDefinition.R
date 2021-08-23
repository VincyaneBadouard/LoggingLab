#' HarvestableAreaDefinition
#'
#' @param plot Studied plots (sf data.frame)
#' @param DEM Digital elevation model (raster)
#' @param HCrique Relative elevation from nearest channel network (raster)
#' @param advancedloggingparameters Other parameters of the logging simulator
#'
#' @return A collection of polygones defined as 1 : harvestable area / 0 : non-harvestable area
#'
#' @export
#'
#' @import sf
#' @importFrom  raster mask terrain rasterFromXYZ rasterToPolygons
#' @import dplyr
#'
#' @examples
#'
#' ExploitPolygones <- HarvestableAreaDefinition(Plot = Plots,DEM = DEM,HCrique = HCrique$distance)
#'
HarvestableAreaDefinition <- function(plot = Plots,
                                      DEM = DEMParacou,
                                      HCrique = Hcrique,
                                      advancedloggingparameters = loggingparameters()) {


  # On commence par decouper par l'emprise du plot
  plotTopo <- raster::mask(x = DEM,
                           mask = plot) # découpage de la topo par l'emprise du plot
  HCriqueplot <- raster::mask(x = HCrique,
                              mask = plot) # la même chose pour la hauteur à la crique

  # Calcul de la pente
  plotSlope <- raster::terrain(plotTopo,
                               opt = "slope",
                               units = 'radians',
                               neigbors = 8)
  # RastersToPoints

  plotSlopePoint <-
    dplyr::as_tibble(raster::rasterToPoints(plotSlope))

  HCriqueplotPoint <-
    dplyr::as_tibble(raster::rasterToPoints(HCriqueplot))

  # left join par x et y
  plotTib <-
    dplyr::left_join(plotSlopePoint, HCriqueplotPoint, by = c('x', 'y'))

    SlpCrit <- atan(advancedloggingparameters$MaxAreaSlope/100)

  plotTib %>% dplyr::rename("HauteurCrique" = names(plotTib[4]))  %>%
    mutate(Exploit = if_else(
      condition = HauteurCrique > 2 &
        slope <= SlpCrit , # on est en radians pour la pente, donc .264 radians = 27% de pente
      true = 1,
      false = 0
    )) -> plotSlopeHCrique

  # on a pu determiner les zones exploitables ou non
  # maintenant on transforme ça en polygones, plotSlopeHCrique pourrait etre reutilise pour la pente

  # on retransforme en raster
  RasterExploit <-
    raster::rasterFromXYZ(plotSlopeHCrique, crs = 32622) # set crs to WGS84 UTM 22N

  # raster en polygone
  PolygoneExploit <-
    raster::rasterToPolygons(x = RasterExploit$Exploit,
                             n = 16,
                             dissolve = TRUE)

  ## On quitte le monde merveilleux du package raster pour passer a celui de sf

  sf_PolygoneExploit <- sf::st_as_sf(PolygoneExploit)

  ## On transforme nos deux gros polygones (Exploit=1 et Exploit=0 en pleins de petits, par ce qui s'appelait disaggregate dans le package sp)

  ExploitPolygones <-
    sf::st_cast(x = sf_PolygoneExploit, to = "POLYGON") # on a maintenant pleins de polygones, dont pleins de petits isolés

  return(ExploitPolygones)

}

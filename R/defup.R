

require(raster)
require(tidyverse)
require(sf)


HarvestableAreaDefinition <- function(Plot, DEM, HCrique) {
  # On commence par decouper par l'emprise du plot
  PlotTopo <- raster::mask(x = DEM,
                           mask = Plot) # découpage de la topo par l'emprise du plot
  HCriquePlot <- raster::mask(x = HCrique,
                              mask = Plot) # la même chose pour la hauteur à la crique

  # Calcul de la pente
  PlotSlope <- raster::terrain(PlotTopo,
                               opt = "slope",
                               units = 'radians',
                               neigbors = 8)
  # RastersToPoints

  PlotSlopePoint <-
    dplyr::as_tibble(raster::rasterToPoints(PlotSlope))

  HCriquePlotPoint <-
    dplyr::as_tibble(raster::rasterToPoints(HCriquePlot))

  # left join par x et y
  PlotTib <-
    dplyr::left_join(PlotSlopePoint, HCriquePlotPoint, by = c('x', 'y'))

  PlotTib %>% dplyr::rename("HauteurCrique" = names(PlotTib[4]))  %>%
    mutate(Exploit = if_else(
      condition = HauteurCrique > 2 &
        slope <= .264, # on est en radians pour la pente, donc .264 radians = 27% de pente
      true = 1,
      false = 0
    )) -> PlotSlopeHCrique

  # on a pu determiner les zones exploitables ou non
  # maintenant on transforme ça en polygones, PlotSlopeHCrique pourrait etre reutilise pour la pente

  # on retransforme en raster
  RasterExploit <-
    raster::rasterFromXYZ(PlotSlopeHCrique, crs = 32622) # set crs to WGS84 UTM 22N

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

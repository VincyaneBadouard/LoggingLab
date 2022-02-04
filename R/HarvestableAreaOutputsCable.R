#' 'harvestableareadefinition' outputs when the cable will be used
#' (Paracou plot 6)
#'
#'
#' Harvestable and non-harvestable zones of Paracou plot 6 (French Guiana)
#'
#' A list with:
#' - 'HarvestablePolygons': a collection of polygons (sfc_MULTIPOLYGON) defined as:
#'    1 : harvestable area,
#'    0 : non-harvestable area
#'
#' - 'PlotSlope': a raster with slope (in radians) characteristic of the studied
#'    plot (Large RasterLayer)
#'
#' - HarvestableArea : the harvestable area in hectares (double)
#'
#' - MachinePolygons : a collection of polygons (sf (sfc_POLYGON)) defined as:
#'    1 : accessible machine area,
#'    0 : non-accessible machine area
#'
#' @format list
#'
"HarvestableAreaOutputsCable"

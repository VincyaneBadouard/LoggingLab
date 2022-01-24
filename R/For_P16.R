# data(SpeciesCriteria)
# DTMParacou <- DTMP16 <- raster("C:/Users/Vincyane/Downloads/Paracou_P16/DEM_2016_P16.tif")
#
# library(readr)
# ParacouP16_2020 <- read_delim("C:/Users/Vincyane/Downloads/ParacouP16_2020.csv",
#                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
#                                                                                   encoding = "Latin1"), trim_ws = TRUE,guess_max = 10000)
#
# inventory <- addtreedim(cleaninventory(ParacouP16_2020, PlotMask),
#                                                volumeparameters = ForestZoneVolumeParametersTable)
#
# HarvestableAreaOutputs <- harvestableareadefinition(
#     topography = DTMParacou,
#     creekdistances = CreekDistances,
#     advancedloggingparameters = loggingparameters()
#     )
#
# PlotSlope <- HarvestableAreaOutputs$PlotSlope
#
# # faire des maintrails
# DTMExtExtended <- raster::extend(DTMParacou, c(3,3)) # extend the extent
#
# fill.boundaries <- function(x) {
#   center = 0.5 + (3*3/2)
#   if( is.na(x)[center] ) {
#     return( round(mean(x, na.rm=T),5) )
#   } else {
#     return( x[center] )
#   }
# }
#
# DTMExtended <- raster::focal(DTMExtExtended,
#                              matrix(1,3,3),
#                              fun=fill.boundaries,
#                              na.rm=F, pad=T)
#
# preMainTrails <- DTMExtended > -Inf
# preMainTrails<- rasterToPolygons(preMainTrails, dissolve=T)
# MainTrails <- preMainTrails %>% st_as_sf() %>% st_cast(to = 'LINESTRING', warn= FALSE)
#
#
#
# treeselectionoutputs <- treeselection(inventory,
#                                       topography = DTMParacou, plotslope = PlotSlope, MainTrails = PlotMask %>% st_as_sf() %>% st_set_crs(raster::crs(DTMP16)),
#                                       harvestablepolygons = HarvestablePolygons,
#                                       speciescriteria = SpeciesCriteria,
#                                       scenario = "RIL1",
#                                       objectivelax = TRUE,
#                                       advancedloggingparameters = loggingparameters())
#
# secondtrails <- secondtrailsopening(
#   topography = DTMParacou,
#   plotmask = PlotMask,
#   treeselectionoutputs = treeselectionoutputs,
#   creekdistances = CreekDistances,
#   CostMatrix = list(list(list(Slope = 3, Cost = 3),
#                          list(Slope = 5, Cost = 5),
#                          list(Slope = 12, Cost = 20),
#                          list(Slope = 22, Cost = 100),
#                          list(Slope = 27, Cost = 1000),
#                          list(Slope = Inf, Cost = Inf)),
#                     list(list(CostType = "Initial", CostValue = 1000),
#                          list(CostType = "Access", CostValue = Inf),
#                          list(CostType = "BigTrees", CostValue = 500),
#                          list(CostType = "Reserves", CostValue = 1000),
#                          list(CostType = "Futures", CostValue = 900),
#                          list(CostType = "MainTrails", CostValue = 1E-4),
#                          list(CostType = "SecondTrails", CostValue = 1E-2))),
#   scenario = "RIL1",
#   fact = 3,
#   advancedloggingparameters = loggingparameters())

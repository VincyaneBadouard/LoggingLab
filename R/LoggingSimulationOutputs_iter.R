#' Logging Simulation Outputs with iteration and parallelization
#'
#' Outputs of the logging simulation on the plot 6 of Paracou (French Guiana)
#'
#' With the parameters:
#'   scenario : manual
#'   objective : 10 m3/ha
#'   fuel : 2
#'   diversification : TRUE
#'   winching : 2
#'   directionalfelling : 2
#'   specieslax : FALSE
#'   objectivelax : TRUE
#'
#'   iter = 2
#'   cores = 2
#'
#' @format A large list of 40 elements for each iteration, contained in a list.
#' \describe{
#'   \item{inventory}{Output forest inventory (data.frame)}
#'   \item{HarvestableArea}{Harvestable area in ha (numeric)}
#'   \item{VO}{Objective volume in m3 (numeric)}
#'   \item{HVinit}{Initial harvestable volume in m3 (numeric)}
#'   \item{TimberLoggedVolume}{Timber logged volume in m3 (numeric)}
#'   \item{NoHollowTimberLoggedVolume}{No hollow timber logged volume (only
#'   healthy trees) in m3 (numeric)}
#'   \item{TimberExtractedVolume}{Timber volume after purge in m3 (numeric)}
#'   \item{FuelWoodBiomass}{Harvestable fuel wood biomass in ton (numeric)}
#'   \item{LoggingResidualBiomass}{The unused degraded biomass in ton (numeric)}
#'   \item{LostBiomass}{Total lost biomass in ton (the total forest biomass lost
#'   due to logging (the sum of the AGB of all dead trees, whatever the logging
#'   cause) (numeric)}
#'   \item{TrailsDensity}{Trails density (preliminary if FWE) in m/ha (units)}
#'   \item{AdjustTrailsDensity}{Adjusted trails density if FWE in m/ha (units)}
#'
#'   \item{MainTrails}{Main trails (sf)}
#'
#'   \item{HarvestablePolygons}{Harvestable zones (MULTIPOLYGON with crs)}
#'   \item{MachinePolygons}{Exploitable zones accessible to machines
#'   (MULTIPOLYGON with crs)}
#'   \item{PlotSlope}{Slopes of the plot (in radians) (RasterLayer with crs)}
#'
#'   \item{SmoothedTrails}{Smoothed secondary trails (MULTIPOLYGON with crs)}
#'   \item{MainTrailsAccess}{Random access point of main trail for each
#'   harvestable zone (POINT with crs)}
#'   \item{TrailsIdentity}{Information on sections of the trails (matrix), with
#'   LineID: , LoggedTrees: idTree of trees reached by the trails and TypeExpl:
#'   type of winching}
#'   \item{RawSecondTrails}{Non-smoothed secondary trails (SpatialLines with
#'   crs)}
#'   \item{CostRasterAgg}{The cost raster (RasterLayer)}
#'
#'   \item{AdjustSmoothedTrails}{Adjusted smoothed secondary trails for FWE
#'   (MULTIPOLYGON with crs)}
#'   \item{AdjustTrailsIdentity}{Adjusted version of TrailsIdentity for FWE
#'   (matrix)}
#'   \item{AdjustRawSecondTrails}{Adjusted non-smoothed secondary trails for FWE
#'   (SpatialLines with crs)}
#'
#'   \item{HarvestableTreesPoints}{Harvestable trees points (sf)}
#'   \item{SelectedTreesPoints}{Selected trees points (sf)}
#'   \item{FutureTreesPoints}{Future trees points (sf)}
#'   \item{ReserveTreesPoints}{Reserve trees points (sf)}
#'   \item{HollowTreesPoints}{Hollow trees points (sf)}
#'   \item{EnergywoodTreesPoints}{Energywood trees points (sf)}
#'
#'   \item{INPUTinventory}{Input forest inventory (data.frame)}
#'   \item{scenario}{Input value for 'scenario' argument(character)}
#'   \item{objective}{Input value for 'objective' argument (numeric)}
#'   \item{fuel}{Input value for 'fuel' argument (character)}
#'   \item{diversification}{Input value for 'diversification' argument
#'   (logical)}
#'   \item{winching}{Input value for 'winching' argument (numeric)}
#'   \item{directionalfelling}{Input value for 'directionalfelling' argument
#'   (character)}
#'   \item{specieslax}{Input value for 'specieslax' argument (logical)}
#'   \item{objectivelax}{Input value for 'objectivelax' argument (logical)}
#'   ...
#' }

"LoggingSimulationOutputs_iter"

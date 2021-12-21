#' Asymmetric adjacent matrix - Topographic least cost paths and distances
#'
#' @description Calculates topographic least cost distances and paths with an asymmetric
#' adjacent matrix
#'
#' @param DTM A RasterLayer for digital terrain model (DTM) data.
#'
#' @param costSurface A RasterLayer for the conductance (inverse of resistance)
#'   values for each cell.
#'
#' @param slopeRdCond A categorical transition layer of longitudinal and
#'   transversal slope.
#'
#' @param pts A SpatialPointsDataFrame object or two-column matrix with xy
#'   coordinates for the geographic points from which to calculate pairwise
#'   distances and paths.
#'
#' @param directions numeric (default = 8). The number of directions for
#'   movement between cells, either 4 or 8.
#'
#' @param paths logical. Default is FALSE, in which case only topographic
#'   distances are calculated.  If TRUE, topographic paths are also identified.
#'
#' @param zweight numeric (default = 1). The weight to be applied to the
#'   elevation (z) distances relative to the horizontal (xy) distances.
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @return Matrix of topographic distances (if paths = FALSE), or a list
#'   containing a matrix of topographic distances and the topographic paths as
#'   an object of class SpatialLines (if paths = TRUE).
#'
#' @details The values of the raster for costSurface should be conductance
#'   values rather than resistance values. These can be calculating by taking
#'   the inverse of resistance values.
#'
#' @importFrom gdistance costDistance transition geoCorrection shortestPath
#' @importFrom sp SpatialPoints
#' @importFrom raster adjacent
#' @importFrom utils combn
#'
#' @export
#'
AdjTopoLCP <- function(
  DTM,
  costSurface,
  slopeRdCond,
  pts,
  directions = 8,
  paths = TRUE,
  zweight = 1,
  advancedloggingparameters = loggingparameters()
){

  pts <- SpatialPoints(pts) # Set spatial points in sp format

  h.dist <- transition(DTM, transitionFunction = function(x){1}, directions = directions, symm = TRUE) # Horizontal distance from DTM to transition layer format
  h.dist <- geoCorrection(h.dist, scl = FALSE) # Divide weights by centroïds distance
  adj <- adjacent(DTM, cells = 1:ncell(DTM), pairs = TRUE, directions = directions, sorted = TRUE) # Compute adjacent matrix from DTM raster
  h.dist[adj] <- 1/h.dist[adj] # Convert horizontal distance resistance into conductance
  elevDiff <- function(x){zweight * abs(x[2] - x[1])} # Define elevation difference function
  v.dist <- transition(DTM, elevDiff, 8, symm = TRUE) # Absolute vertical distance from DTM to transition layer format
  t.dist <- v.dist
  t.dist[adj] <- 1/sqrt((h.dist[adj]^2)+(v.dist[adj]^2)) # Convert tangential distance resistance into conductance

  lcp.dist <- transition(costSurface, transitionFunction = mean, directions = 8) # Convert cost raster into transition layer

  tlcp <- t.dist * lcp.dist * slopeRdCond
  # Compute weights : 1/sqrt(h.dist^2 + v.dist^2) *
  # 1/Cost * min((atan(Slope.lg.lim) - atan(Slope.lg))/atan(Slope.lg.lim),0) *
  # min((atan(Slope.tr.lim) - atan(Slope.tr))/atan(Slope.tr.lim),0)

  topoDistances <- as.matrix(costDistance(tlcp, pts))
  if (paths){ # If paths == TRUE
    sp.pairs <- combn(1:length(pts), m = 2)
    pathLines <- list()
    for(i in 1:ncol(sp.pairs)){
      pathLines[[i]] <- shortestPath(tlcp, pts[sp.pairs[1,i]], pts[sp.pairs[2,i]], output = "SpatialLines")
      pathLines[[i]]@lines[[1]]@ID <- paste("Path", sp.pairs[1,i], "-", sp.pairs[2,i], sep = " ")
    }
    paths <- do.call(rbind, pathLines) # Concatenate Spatial lines
    return(list(topoDistances, paths))
  } else {
    return(topoDistances)
  }
}

#' Assymetric adjacent matrix T*topographic least cost paths and distances
#'
#' Calculates topographic least cost distances and paths with an assymetric adjacent matrix
#'
#' @param DEM A RasterLayer for digital elevation model (DEM) data.
#' @param costSurface A RasterLayer for the conductance (inverse of resistance) values for each cell.
#' @param slopeRdCond A categorical transition layer of longitudinal and transversal slope.
#' @param pts A SpatialPoints object or two-column matrix with xy coordinates for the geographic points from which to calculate pairwise distances and paths.
#' @param directions numeric (default = 8). The number of directions for movement between cells, either 4 or 8.
#' @param paths logical. Default is FALSE, in which case only topographic distances are calculated.  If TRUE, topographic paths are also identified.
#' @param zweight numeric (default = 1). The weight to be applied to the elevation (z) distances relative to the horizontal (xy) distances.
#' @param advancedloggingparameters Other parameters of the logging simulator
#'
#'
#' @return Matrix of topographic distances (if paths = FALSE), or a list containing a matrix of topographic distances and the topographic paths as an object of class SpatialLines (if paths = TRUE).
#' @details
#' The values of the raster for costSurface should be conductance values rather than resistance values.  These can be calculating by taking the inverse of resistance values.
#' @examples
#' xy <- matrix(ncol = 2, byrow = TRUE,
#'    c(-119.5566, 37.72474,
#'    -119.4718, 37.76078))
#' topoLCP(Yosemite$DEM, Yosemite$SDM, xy, paths = TRUE)
#' @importFrom gdistance costDistance transition geoCorrection shortestPath
#' @importFrom sp SpatialPoints
#' @importFrom raster adjacent
#' @export
AdjTopoLCP <- function(DEM,
                       costSurface,
                       slopeRdCond,
                       pts,
                       directions = 8,
                       paths = TRUE,
                       zweight = 1,
                       advancedloggingparameters = loggingparameters()){

  pts <- SpatialPoints(pts)

  h.dist <- gdistance::transition(DEM, transitionFunction = function(x){1}, directions = directions, symm = TRUE)
  h.dist <- gdistance::geoCorrection(h.dist, scl = FALSE)
  adj <- raster::adjacent(DEM, cells = 1:ncell(DEM), pairs = TRUE, directions = directions, sorted = TRUE)
  h.dist[adj] <- 1/h.dist[adj]
  elevDiff <- function(x){zweight * abs(x[2] - x[1])}
  v.dist <- gdistance::transition(DEM, elevDiff, 8, symm = TRUE)
  t.dist <- v.dist
  t.dist[adj] <- 1/sqrt((h.dist[adj]^2)+(v.dist[adj]^2))

  lcp.dist <- gdistance::transition(costSurface, transitionFunction = mean, directions = 8)
  tlcp <- t.dist * lcp.dist * slopeRdCond
  topoDistances <- as.matrix(gdistance::costDistance(tlcp, pts))
  if (paths){
    sp.pairs <- combn(1:length(pts), m = 2)
    pathLines <- list()
    for(i in 1:ncol(sp.pairs)){
      pathLines[[i]] <- gdistance::shortestPath(tlcp, pts[sp.pairs[1,i]], pts[sp.pairs[2,i]], output = "SpatialLines")
      pathLines[[i]]@lines[[1]]@ID <- paste("Path", sp.pairs[1,i], "-", sp.pairs[2,i], sep = " ")
    }
    paths <- do.call(rbind, pathLines)
    return(list(topoDistances, paths))
  } else {
    return(topoDistances)
  }
}

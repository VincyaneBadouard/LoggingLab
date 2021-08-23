#' SlopeRdCond
#'
#' @param DEM A RasterLayer for digital elevation model (DEM) data.
#' @param fact numeric (default = 3). The reduction factor of aggregated DEM.
#' @param advancedloggingparameters Other parameters of the logging simulator.
#' @param grapple boolean (default = FALSE).Use grapple in harvest.
#'
#' @return
#' @export
#'
#'
#' @importFrom gdistance transition geoCorrection
#' @importFrom raster adjacent aggregate resample
#' @examples
#'
#' data(DemParacou)
#'
#' SlopeCond <- SlopeRdCond((DEM, fact=3))
#'
SlopeRdCond <- function(DEM,
             fact=1,
             advancedloggingparameters = loggingparameters(),
             grapple = FALSE){

DEMmean <- aggregate(DEM, fact=fact, fun=mean) # aggregate DEM by the reduction factor

altDiff <- function(x){abs(x[2] - x[1] + 1E-12)} # set absolute elevation difference function
s.dist <- gdistance::transition(DEMmean, altDiff, 8, symm=FALSE) # compute adjacent matrix with absolute elevation difference weights
s.dist <- gdistance::geoCorrection(s.dist) # divide each link by the absolute distance between centroids

# s.dist : adjacent matrix with abs(delta elevation)/dist = absolute slope weights

adj <- raster::adjacent(DEMmean, cells = 1:ncell(DEMmean), pairs = TRUE, directions = 8, sorted = TRUE) # compute TRUE 8 neighbors

AdjTr <- matrix(c(adj[,1], rep(0,dim(adj)[1])), nrow = dim(adj)[1])
#
# k = 1
# ProgressBar <- txtProgressBar(min = 0, max = ncell(DEMmean),style = 3)
#
# for (i in 1:ncell(DEMmean)) {
#   Adj8 <- c(i -1  - ncol(DEMmean), #Z1     # compute HYPOTHETICAL 8 neighbors adjacent FOCAL matrix
#             i  - ncol(DEMmean), #Z2
#             i  + 1  - ncol(DEMmean),#Z3
#             i - 1,
#             i +1,
#             i -1  + ncol(DEMmean),
#             i  + ncol(DEMmean),
#             i  +1   +  ncol(DEMmean))
#   RotAntiAdj8 <- c(i +1  - ncol(DEMmean), # compute HYPOTHETICAL antirotated adjacent focal matrix for transversal slope ( - 90 degree)
#                i  +1,
#                i  + 1  + ncol(DEMmean),
#                i -ncol(DEMmean),
#                i + ncol(DEMmean),
#                i -1 - ncol(DEMmean),
#                i -1  ,
#                i  -1 +  ncol(DEMmean))
#
#   RotAdj8 <- c(i -1  + ncol(DEMmean), # compute HYPOTHETICAL rotated adjacent focal matrix for transversal slope ( + 90 degree)
#                    i - 1,
#                    i -1  - ncol(DEMmean),
#                    i  + ncol(DEMmean),
#                    i - ncol(DEMmean),
#                    i  + 1  + ncol(DEMmean),
#                    i + 1,
#                    i +1 - ncol(DEMmean))
#
#   InAdj <- as.numeric(Adj8 %in% adj[adj[,1] == i,2]) * as.numeric(RotAdj8 %in% adj[adj[,1] == i,2]) * RotAdj8 # compute TRUE rotated 8 neighbors adjacent FOCAL matrix
#
#   AntIndAdj <- as.numeric(Adj8 %in% adj[adj[,1] == i,2]) * as.numeric(RotAntiAdj8 %in% adj[adj[,1] == i,2]) * RotAntiAdj8 # compute TRUE antirotated 8 neighbors adjacent FOCAL matrix
#
#   Mat8 <- matrix(c(Adj8,InAdj,AntIndAdj),nrow = 8) # concatenated adjacent links
#
#
#
#   x <- pmax(Mat8[Mat8[,1] %in% adj[adj[,1]==i,2],2], Mat8[Mat8[,1] %in% adj[adj[,1]==i,2],3]) # Select +90 degree link except in boundaries conditions
#   x[x==0] <- NA # exclude missing links
#
#   AdjTr[AdjTr[,1]==i,2] <- x # replace longitudinal links to rotated links
#
#   k = k+1 # update progress bar
#   setTxtProgressBar(ProgressBar, k) # plot progress bar
# }


v4 <- function(DEMmean, AdjTr){
  k = 1
  ProgressBar <- txtProgressBar(min = 0, max = ncell(DEMmean),style = 3)

  limits <- c(
    1:ncol(DEMmean), # top
    (1:(nrow(DEMmean)-1))*ncol(DEMmean)+1, # left
    (1:(nrow(DEMmean)-1))*ncol(DEMmean), # right
    ((nrow(DEMmean)-1)*ncol(DEMmean)+1):ncell(DEMmean) # bottom
  ) %>%
    unique()

  others <- 1:ncell(DEMmean)
  others <- others[!(others %in% limits)]

  for (i in limits) {
    Adj8 <- c(i -1  - ncol(DEMmean), #Z1     # compute HYPOTHETICAL 8 neighbors adjacent FOCAL matrix
              i  - ncol(DEMmean), #Z2
              i  + 1  - ncol(DEMmean),#Z3
              i - 1,
              i +1,
              i -1  + ncol(DEMmean),
              i  + ncol(DEMmean),
              i  +1   +  ncol(DEMmean))
    RotAntiAdj8 <- c(i +1  - ncol(DEMmean), # compute HYPOTHETICAL antirotated adjacent focal matrix for transversal slope ( - 90 degree)
                     i  +1,
                     i  + 1  + ncol(DEMmean),
                     i -ncol(DEMmean),
                     i + ncol(DEMmean),
                     i -1 - ncol(DEMmean),
                     i -1  ,
                     i  -1 +  ncol(DEMmean))
    RotAdj8 <- c(i -1  + ncol(DEMmean), # compute HYPOTHETICAL rotated adjacent focal matrix for transversal slope ( + 90 degree)
                 i - 1,
                 i -1  - ncol(DEMmean),
                 i  + ncol(DEMmean),
                 i - ncol(DEMmean),
                 i  + 1  + ncol(DEMmean),
                 i + 1,
                 i +1 - ncol(DEMmean))
    InAdj <- as.numeric(Adj8 %in% adj[adj[,1] == i,2]) * as.numeric(RotAdj8 %in% adj[adj[,1] == i,2]) * RotAdj8 # compute TRUE rotated 8 neighbors adjacent FOCAL matrix
    AntIndAdj <- as.numeric(Adj8 %in% adj[adj[,1] == i,2]) * as.numeric(RotAntiAdj8 %in% adj[adj[,1] == i,2]) * RotAntiAdj8 # compute TRUE antirotated 8 neighbors adjacent FOCAL matrix
    Mat8 <- matrix(c(Adj8,InAdj,AntIndAdj),nrow = 8) # concatenated adjacent links
    x <- pmax(Mat8[Mat8[,1] %in% adj[adj[,1]==i,2],2], Mat8[Mat8[,1] %in% adj[adj[,1]==i,2],3]) # Select +90 degree link except in boundaries conditions
    x[x==0] <- NA # exclude missing links
    AdjTr[AdjTr[,1]==i,2] <- x
    k = k+1 # update progress bar
    setTxtProgressBar(ProgressBar, k) # plot progress bar
  }

  for (i in others) {
    Adj8 <- c(i -1  - ncol(DEMmean), #Z1     # compute HYPOTHETICAL 8 neighbors adjacent FOCAL matrix
              i  - ncol(DEMmean), #Z2
              i  + 1  - ncol(DEMmean),#Z3
              i - 1,
              i +1,
              i -1  + ncol(DEMmean),
              i  + ncol(DEMmean),
              i  +1   +  ncol(DEMmean))
    RotAntiAdj8 <- c(i +1  - ncol(DEMmean), # compute HYPOTHETICAL antirotated adjacent focal matrix for transversal slope ( - 90 degree)
                     i  +1,
                     i  + 1  + ncol(DEMmean),
                     i -ncol(DEMmean),
                     i + ncol(DEMmean),
                     i -1 - ncol(DEMmean),
                     i -1  ,
                     i  -1 +  ncol(DEMmean))
    RotAdj8 <- c(i -1  + ncol(DEMmean), # compute HYPOTHETICAL rotated adjacent focal matrix for transversal slope ( + 90 degree)
                 i - 1,
                 i -1  - ncol(DEMmean),
                 i  + ncol(DEMmean),
                 i - ncol(DEMmean),
                 i  + 1  + ncol(DEMmean),
                 i + 1,
                 i +1 - ncol(DEMmean))
    Mat8 <- matrix(c(Adj8,RotAdj8,RotAntiAdj8),nrow = 8) # concatenated adjacent links
    x <- pmax(Mat8[Mat8[,1] %in% adj[adj[,1]==i,2],2], Mat8[Mat8[,1] %in% adj[adj[,1]==i,2],3]) # Select +90 degree link except in boundaries conditions
    AdjTr[AdjTr[,1]==i,2] <- x # replace longitudinal links to rotated links

    k = k+1 # update progress bar
    setTxtProgressBar(ProgressBar, k) # plot progress bar
  }

  return(AdjTr)
}

AdjTr <- v4(DEMmean, AdjTr)

adjCorr <- cbind(adj,AdjTr[,2]) # concatenated longitudinal and transversal links
adjCorr <- na.exclude(adjCorr) # exclude missing links

SlpTr <-atan(advancedloggingparameters$MaxTrailCrossSlope+2/100) # transerval condition

s.distTr <- s.dist
s.distTr[adjCorr[,c(1,2)]] <- as.numeric(atan(s.dist[adjCorr[,c(1,3)]]) <  SlpTr & s.dist[adjCorr[,c(1,3)]] != 0) * (SlpTr - atan(s.dist[adjCorr[,c(1,3)]]))/(SlpTr)  +
  (1 - as.numeric(atan(s.dist[adjCorr[,c(1,3)]]) <=  atan(advancedloggingparameters$MaxTrailCrossSlope+2/100) &
                                                                                   s.dist[adjCorr[,c(1,3)]] != 0)) * 1E-6
# Transversal weight : longitudinal link = binary(atan(Tr slope) < atan(slope Tr limit)) * (atan(slope Tr limit) - atan(Tr slope))/atan(slope Tr limit) +
# (1 - binary(atan(Tr slope) < atan(slope Tr limit)) * 1E-6 (inversibility condition : != 0 )

if (grapple == TRUE) {
  SlpLg <- atan(advancedloggingparameters$GrappleMaxslope/100) # longitudinal condition
}else{
  SlpLg <- atan(advancedloggingparameters$MaxTrailCenterlineSlope/100) # longitudinal condition
}



s.distLg <- s.dist
s.distLg[adjCorr[,c(1,2)]] <- as.numeric(atan(s.dist[adjCorr[,c(1,2)]]) <  SlpLg & s.dist[adjCorr[,c(1,2)]] != 0) * (SlpLg - atan(s.dist[adjCorr[,c(1,2)]]))/(SlpLg) +  (1- as.numeric(atan(s.dist[adjCorr[,c(1,2)]]) <=  SlpLg &
                                                                                 s.dist[adjCorr[,c(1,2)]] != 0)) * 1E-6
# longitudinal weight : longitudinal link = binary(atan(Lg slope) < atan(slope lg limit)) * (atan(slope Lg limit) - atan(Lg slope))/atan(slope Lg limit) +
# (1 - binary(atan(Lg slope) < atan(slope Lg limit)) * 1E-6 (inversibility condition : != 0 )

s.distF <- s.dist
s.distF<- (s.distTr * s.distLg) # Cumulated conditions (element-wise multiplication)


return(s.distF)
}

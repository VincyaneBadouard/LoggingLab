#' sloperdcond
#'
#'@param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'  or SRTM) (\code{\link{DTMParacou}}) (RasterLayer **with a crs in UTM**)
#'  We advise you to generate your raster with Qgis rather than with the
#'  'raster' package on R.
#'
#'@param directions numeric (default = 4). The number of directions for
#'   movement between cells, either 4 or 8.
#'
#'@param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#'@param grapple Use grapple in harvest. Default = FALSE (boolean)
#'
#'@return A transition layer object with weighted adjacent graph according to
#'  longitudinal / transversal slope condition
#'
#'
#'@importFrom gdistance transition geoCorrection
#'@importFrom raster adjacent aggregate resample ncol ncell
#'@importFrom utils txtProgressBar setTxtProgressBar
#'@importFrom stats na.exclude
#'
#' @examples
#'\dontrun{
#' data(DTMParacou)
#'
#' SlopeCond <- sloperdcond(topography = DTMParacou)
#'}
#'
sloperdcond <- function(
  topography,
  directions = 4,
  advancedloggingparameters = loggingparameters(),
  grapple = FALSE
){

  altDiff <- function(x){abs(x[2] - x[1] + 1E-12)} # set absolute elevation difference function
  s.dist <- gdistance::transition(topography, altDiff,directions = directions, symm=FALSE) # compute adjacent matrix with absolute elevation difference weights
  s.dist <- try(gdistance::geoCorrection(s.dist), silent=TRUE) # divide each link by the absolute distance between centroids

  # s.dist : adjacent matrix with abs(delta elevation)/dist = absolute slope weights

  adj <- adjacent(topography, cells = 1:ncell(topography), pairs = TRUE, directions = directions, sorted = TRUE) # compute TRUE 4 or 8 neighbors

  AdjTr <- matrix(c(adj[,1], rep(0,dim(adj)[1])), nrow = dim(adj)[1])

  v4 <- function(topography, AdjTr,directions){
    k = 1
    ProgressBar <- txtProgressBar(min = 0, max = ncell(topography),style = 3)

    limits <- c(
      1:ncol(topography), # top
      (1:(nrow(topography)-1))*ncol(topography)+1, # left
      (1:(nrow(topography)-1))*ncol(topography), # right
      ((nrow(topography)-1)*ncol(topography)+1):ncell(topography) # bottom
    ) %>%
      unique()

    others <- 1:ncell(topography)
    others <- others[!(others %in% limits)]

    if (directions == 8) {
      for (i in limits) {
        Adj8 <- c(i -1  - ncol(topography), #Z1     # compute HYPOTHETICAL 8 neighbors adjacent FOCAL matrix
                  i  - ncol(topography), #Z2
                  i  + 1  - ncol(topography),#Z3
                  i - 1,
                  i +1,
                  i -1  + ncol(topography),
                  i  + ncol(topography),
                  i  +1   +  ncol(topography))
        RotAntiAdj8 <- c(i +1  - ncol(topography), # compute HYPOTHETICAL antirotated adjacent focal matrix for transversal slope ( - 90 degree)
                         i  +1,
                         i  + 1  + ncol(topography),
                         i -ncol(topography),
                         i + ncol(topography),
                         i -1 - ncol(topography),
                         i -1  ,
                         i  -1 +  ncol(topography))
        RotAdj8 <- c(i -1  + ncol(topography), # compute HYPOTHETICAL rotated adjacent focal matrix for transversal slope ( + 90 degree)
                     i - 1,
                     i -1  - ncol(topography),
                     i  + ncol(topography),
                     i - ncol(topography),
                     i  + 1  + ncol(topography),
                     i + 1,
                     i +1 - ncol(topography))
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
        Adj8 <- c(i -1  - ncol(topography), #Z1     # compute HYPOTHETICAL 8 neighbors adjacent FOCAL matrix
                  i  - ncol(topography), #Z2
                  i  + 1  - ncol(topography),#Z3
                  i - 1,
                  i +1,
                  i -1  + ncol(topography),
                  i  + ncol(topography),
                  i  +1   +  ncol(topography))
        RotAntiAdj8 <- c(i +1  - ncol(topography), # compute HYPOTHETICAL antirotated adjacent focal matrix for transversal slope ( - 90 degree)
                         i  +1,
                         i  + 1  + ncol(topography),
                         i -ncol(topography),
                         i + ncol(topography),
                         i -1 - ncol(topography),
                         i -1  ,
                         i  -1 +  ncol(topography))
        RotAdj8 <- c(i -1  + ncol(topography), # compute HYPOTHETICAL rotated adjacent focal matrix for transversal slope ( + 90 degree)
                     i - 1,
                     i -1  - ncol(topography),
                     i  + ncol(topography),
                     i - ncol(topography),
                     i  + 1  + ncol(topography),
                     i + 1,
                     i +1 - ncol(topography))
        Mat8 <- matrix(c(Adj8,RotAdj8,RotAntiAdj8),nrow = 8) # concatenated adjacent links
        x <- pmax(Mat8[Mat8[,1] %in% adj[adj[,1]==i,2],2], Mat8[Mat8[,1] %in% adj[adj[,1]==i,2],3]) # Select +90 degree link except in boundaries conditions
        AdjTr[AdjTr[,1]==i,2] <- x # replace longitudinal links to rotated links

        k = k+1 # update progress bar
        setTxtProgressBar(ProgressBar, k) # plot progress bar
      }
    }else{
      for (i in limits) {
        Adj4 <- c(#i -1  - ncol(topography), #Z1     # compute HYPOTHETICAL 4 neighbours adjacent FOCAL matrix
                  i  - ncol(topography), #Z2
                  #i  + 1  - ncol(topography),#Z3
                  i - 1,
                  i +1,
                  #i -1  + ncol(topography),
                  i  + ncol(topography)#,
                  #i  +1   +  ncol(topography)
                  )
        RotAntiAdj4 <- c(#i +1  - ncol(topography), # compute HYPOTHETICAL antirotated adjacent focal matrix for transversal slope ( - 90 degree)
                         i  +1,
                         #i  + 1  + ncol(topography),
                         i -ncol(topography),
                         i + ncol(topography),
                         #i -1 - ncol(topography),
                         i -1  #,
                         #i  -1 +  ncol(topography)
                         )
        RotAdj4 <- c(#i -1  + ncol(topography), # compute HYPOTHETICAL rotated adjacent focal matrix for transversal slope ( + 90 degree)
                     i - 1,
                     #i -1  - ncol(topography),
                     i  + ncol(topography),
                     i - ncol(topography),
                     #i  + 1  + ncol(topography),
                     i + 1#,
                     #i +1 - ncol(topography)
                     )
        InAdj <- as.numeric(Adj4 %in% adj[adj[,1] == i,2]) * as.numeric(RotAdj4 %in% adj[adj[,1] == i,2]) * RotAdj4 # compute TRUE rotated 4 neighbors adjacent FOCAL matrix
        AntIndAdj <- as.numeric(Adj4 %in% adj[adj[,1] == i,2]) * as.numeric(RotAntiAdj4 %in% adj[adj[,1] == i,2]) * RotAntiAdj4 # compute TRUE antirotated 4 neighbors adjacent FOCAL matrix
        Mat4 <- matrix(c(Adj4,InAdj,AntIndAdj),nrow = 4) # concatenated adjacent links
        x <- pmax(Mat4[Mat4[,1] %in% adj[adj[,1]==i,2],2], Mat4[Mat4[,1] %in% adj[adj[,1]==i,2],3]) # Select +90 degree link except in boundaries conditions
        x[x==0] <- NA # exclude missing links
        AdjTr[AdjTr[,1]==i,2] <- x
        k = k+1 # update progress bar
        setTxtProgressBar(ProgressBar, k) # plot progress bar
      }

      for (i in others) {
        Adj4 <- c(#i -1  - ncol(topography), #Z1     # compute HYPOTHETICAL 4 neighbors adjacent FOCAL matrix
                  i  - ncol(topography), #Z2
                  #i  + 1  - ncol(topography),#Z3
                  i - 1,
                  i +1,
                  #i -1  + ncol(topography),
                  i  + ncol(topography)#,
                  #i  +1   +  ncol(topography)
                  )
        RotAntiAdj4 <- c(#i +1  - ncol(topography), # compute HYPOTHETICAL antirotated adjacent focal matrix for transversal slope ( - 90 degree)
                         i  +1,
                         #i  + 1  + ncol(topography),
                         i -ncol(topography),
                         i + ncol(topography),
                         #i -1 - ncol(topography),
                         i -1  #,
                         #i  -1 +  ncol(topography)
                         )
        RotAdj4 <- c(#i -1  + ncol(topography), # compute HYPOTHETICAL rotated adjacent focal matrix for transversal slope ( + 90 degree)
                     i - 1,
                     #i -1  - ncol(topography),
                     i  + ncol(topography),
                     i - ncol(topography),
                     #i  + 1  + ncol(topography),
                     i + 1#,
                     #i +1 - ncol(topography)
                     )
        Mat4 <- matrix(c(Adj4,RotAdj4,RotAntiAdj4),nrow = 4) # concatenated adjacent links
        x <- pmax(Mat4[Mat4[,1] %in% adj[adj[,1]==i,2],2], Mat4[Mat4[,1] %in% adj[adj[,1]==i,2],3]) # Select +90 degree link except in boundaries conditions
        AdjTr[AdjTr[,1]==i,2] <- x # replace longitudinal links to rotated links

        k = k+1 # update progress bar
        setTxtProgressBar(ProgressBar, k) # plot progress bar
      }
    }



    return(AdjTr)
  }

  AdjTr <- v4(topography, AdjTr,directions)

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

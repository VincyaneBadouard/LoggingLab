#'Create the canopy
#'
#'@description Create a vertical projection of the the crown of each tree
#'
#'@param inventory Input inventory containing the crown diameter (CrownDiameter,
#'  in m) (calculable with the addtreedim function) of each tree
#'  (data.frame)(see the inputs formats and metadata in the
#'  vignette)
#'
#'@return A dataframe with a column 'Crowns' containing the circles
#'  (sfc_POLYGON) as trees crown, with their diameter filled in, representing
#'  trees from the sky.
#'@export
#'
#'@importFrom dplyr group_by do left_join
#'@importFrom sf st_as_text
#'@importFrom tidyr unnest
#'
#' @examples
#' data(Paracou6_2016)
#' Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:10) # inventory reduction
#'
#' inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask),
#' volumeparameters = ForestZoneVolumeParametersTable)
#'
#' inventory <- createcanopy(inventory)
#'
#' # The small ones first so that they are plotted below the big ones
#' inventory <- dplyr::arrange(inventory, TreeHeight)
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = getgeometry(inventory, Crowns),
#'           aes(alpha = TreeHeight),
#'           fill = "forestgreen") +
#'           labs(alpha = "Tree height")
#'
createcanopy <- function(inventory){

  # Arguments check

  if(!inherits(inventory, "data.frame"))
    stop("The 'inventory' argument of the 'createcanopy' function must be a data.frame")

  if(!"CrownDiameter" %in% names(inventory)) # check that "CrownDiameter" column exists
    stop("CrownDiameter is not part of the inventory columns")

  # Global variables
  idTree <- Crowns <- . <- NULL

  # Function

  Canopy <- inventory %>%

    group_by(idTree) %>% # for each tree

    do(Crowns = # inform geometry. # do: filling a column from a function whose input is a table
         treefromthesky(.) %>%

         st_as_text()) %>% # as text to easy join with a non spacial table
    tidyr::unnest(Crowns) # here to pass from list to character

  inventory <- left_join(inventory, Canopy, by = "idTree") # join the column 'Crowns' to the inventory

  return(inventory)

}

#' treefromthesky
#'
#' @param dat 1 row data.frame with columns:
#' "Xutm", "Yutm" (Tree coordinates) and "CrownDiameter".
#'
#' @return A circle (sfc_POLYGON) representing the tree crown, with its diameter
#'   filled in, representing the tree from the sky.
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom sf st_as_sf
#' @importFrom nngeo st_ellipse
#'
#' @examples
#' data(Paracou6_2016)
#' data(ForestZoneVolumeParametersTable)
#'
#' inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask),
#' volumeparameters = ForestZoneVolumeParametersTable)
#'
#' dat <- inventory %>%
#'   dplyr::filter(CrownDiameter == max(CrownDiameter))
#'
#' Crown <- treefromthesky(dat)
#'
#' library(ggplot2)
#' ggplot() +
#'  geom_sf(data = sf::st_as_sf(inventory, coords = c("Xutm", "Yutm"))) +
#'  geom_sf(data = Crown, fill = "forestgreen") # trees polygons
#'
treefromthesky <- function(dat){

  # Arguments check

  if(!inherits(dat, "data.frame"))
    stop("The 'dat' argument of the 'treefromthesky' function must be a data.frame")

  if(nrow(dat)!=1)
    stop("the data.frame given in the 'dat' argument
         of the 'treefromthesky' function must contain only 1 row")

  if(!"CrownDiameter" %in% names(dat)) # check that "CrownDiameter" column exists
    stop("CrownDiameter is not part of the dat columns")


  # Global variables
  Xutm <- Yutm <- CrownDiameter <- NULL

  # Function

  Crown <- dat %>%
    mutate(xCrown = Xutm, # X centroid
           yCrown = Yutm, # Y ventroid
           exCrown = CrownDiameter/2, # crown radius
           eyCrown = CrownDiameter/2) %>% # crown radius
    st_as_sf(coords = c("xCrown", "yCrown")) # ellipse centroid coordinates

  Crown <- st_ellipse(Crown, Crown$exCrown, Crown$eyCrown) # create the ellipse

  return(Crown)

}

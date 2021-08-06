#'getgeometry
#'
#'@description Converts a dataframe with a column of characters, which contains
#'  the WKT encoded geometries, into an sf object. The column is converted to sfc.
#'
#'@param inventory (data.frame) With a column of characters, which contains the
#'  WKT encoded geometries.
#'@param var Name of the character column that holds WKT encoded geometries, as a data/env-variable (type: "environment") .
#'
#'@return
#'@export
#'
#'@importFrom dplyr filter
#'@importFrom sf st_as_sf
#'
#' @examples
#' data(Paracou6_2016)
#'
#' felttrees <- Paracou6_2016 %>%
#' dplyr::filter(VernName == "wacapou") %>%
#'   dplyr::group_by(idTree) %>%
#'   dplyr::do(LocPts = # inform geometry.
#'        sf::st_point(c(.$Xutm,.$Yutm)) %>%
#'        sf::st_as_text()) %>% # as text to easy join with a non spacial table
#'   tidyr::unnest(LocPts) # here to pass from list to character
#'
#' inventory <- dplyr::left_join(Paracou6_2016, felttrees, by = "idTree")
#'
#' Rslt <- getgeometry (inventory, LocPts)
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = Rslt)
#'
getgeometry  <- function(
  inventory,
  var
){

  # Arguments check
  col_name <- deparse(substitute(var)) # object name to this name in character

  if(!inherits(inventory, "data.frame"))
    stop("The 'inventory' argument of the 'getgeometry ' function must be data.frame")

  # if(class(var) == "character") # chais pas comment l'écrire
  #   stop("The 'var' argument of the 'getgeometry ' function must be a variable of your data.frame")

  if(!inherits(inventory[,col_name], "character")) # ne marche pas lors du check...weird
    stop("The column filled in the 'var' argument of the 'getgeometry ' function must be of type character")

  inventory %>%
    dplyr::filter(!is.na( {{ var }} )) %>%
    st_as_sf(wkt = col_name)
}

# R Base versions:
# # V0
# column_name <- "Position" # character string in an object
# column <- inventory[,column_name]
# line_nonna <- which(!is.na(column)) # which rows
# subinventory <- inventory[line_nonna,] # filter base version

# # V1
# column_name <- "Position"
# column <- inventory[,column_name]
# nonna <- !is.na(column)
# subinventory <- inventory[nonna,]

# # V2
# column_name <- "Position"
# inventory[!is.na(inventory[,column_name]),] %>%
#   st_as_sf(wkt = column_name)

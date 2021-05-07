#' @examples
#' data(Paracou6_2016)
#' if (!("DBH" %in% names(inventory))) {add_column(inventory, DBH = NA) #if DBH doesn't exist create it
#' inventory$DBH = inventory$CircCorr/pi} # and compute it
#' cleaninventory(inventory = Paracou6_2016)

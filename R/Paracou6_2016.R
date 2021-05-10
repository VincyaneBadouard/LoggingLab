#' Paracou6_2016
#'
#' Species inventory
#' 2016 inventory of Paracou plot 6.Public dataset extracted from the Guyafor database.
#'
#' @format A tibble with 3620 rows and 31 variables:
#' \describe{
#' Forest "character"
#'   \item{var1}{description, in units ()}
#'   \item{var2}{description, in units ()}
#'   \item{VernName}{Species vernacular/economic name (character)}
#'   \item{ScientificName}{Species scientific name (character)}
#'   \item{Genus}{Genus associated at the vernacular name (character)}
#'   \item{Species}{Species (without genus part) associated at the vernacular name (character)}
#'   ...
#' }
#' @source \url{http://paracou.cirad.fr}
#'
#' EcoFoG::Guyafor2df(WHERE = "Forest='Paracou' AND Plot='6' AND CensusYear=2016",UID = NULL,PWD = NULL,Driver = "SQL Server Native Client 10.0")

"Paracou6_2016"

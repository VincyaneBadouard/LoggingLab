#' Paracou6_2016
#'
#' Species inventory
#' 2016 inventory of Paracou plot 6.Public dataset extracted from the Guyafor database.
#'
#' @format A tibble with 3620 rows and 31 variables:
#' \describe{
#'   \item{Forest}{description, in units (character)}
#'   \item{Plot}{description, in units (character)}
#'   \item{PlotArea}{description, in units (numeric)}
#'   \item{SubPlot}{description, in units (integer)}
#'   \item{TreeFieldNum}{description, in units (numeric)}
#'   \item{idTree}{description, in units (integer)}
#'   \item{Project}{description, in units (character)}
#'   \item{Protocole}{description, in units (character)}
#'   \item{Xfield}{description, in units (numeric)}
#'   \item{Yfield}{description, in units (numeric)}
#'   \item{Xutm}{description, in units (numeric)}
#'   \item{Yutm}{description, in units (numeric)}
#'   \item{UTMZone}{description, in units (integer)}
#'   \item{Lat}{description, in units (numeric)}
#'   \item{Lon}{description, in units (numeric)}
#'   \item{Family}{description, in units (character)}
#'   \item{Genus}{Genus associated at the vernacular name (character)}
#'   \item{Species}{Species (without genus part) associated at the vernacular name (character)}
#'   \item{BotaSource}{description, in units (character)}
#'   \item{BotaCertainty}{description, in units (numeric)}
#'   \item{idVern}{description, in units (integer)}
#'   \item{VernName}{Species vernacular/economic name (character)}
#'   \item{CommercialSp}{description, in units (logical)}
#'   \item{CensusYear}{description, in units (integer)}
#'   \item{CensusDate}{description, in units (character)}
#'   \item{CensusDateCertainty}{description, in units (logical)}
#'   \item{CodeAlive}{description, in units (logical)}
#'   \item{MeasCode}{description, in units (integer)}
#'   \item{Circ}{description, in units (numeric)}
#'   \item{CircCorr}{description, in units (numeric)}
#'   \item{CorrCode}{description, in units (character)}
#'   ...
#' }
#' @source \url{http://paracou.cirad.fr}
#'
#' EcoFoG::Guyafor2df(WHERE = "Forest='Paracou' AND Plot='6' AND CensusYear=2016",UID = NULL,PWD = NULL,Driver = "SQL Server Native Client 10.0")

"Paracou6_2016"

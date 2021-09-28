#' Species exploitability criteria
#'
#' Table of species exploitability criteria:
#' species names, economic interest level, minimum and maximum felling diameter
#' @format A tibble with 96 rows and 7 variables:
#' \describe{
#'   \item{VernName}{Vernacular/economic name (character)}
#'   \item{Genus}{Genus associated to the vernacular name (character)}
#'   \item{Species}{Species (without genus part) associated to the vernacular name (character)}
#'   \item{Commercial}{Economic interest level ("1" principal economic species,
#'   "2" species logged in a objective of diversification, "0" species
#'   whose genus is covered by a commercial name, but which are not logged) (factor)}
#'   \item{MinFD}{Minimum Felling Diameter, in centimeter (numeric)}
#'   \item{UpMinFD}{Enhanced Minimum Felling Diameter (over-rich stand case), in centimeter (numeric)}
#'   \item{MaxFD}{Maximum Felling Diameter, in centimeter (numeric)}
#'   ...
#' }
#'
"SpeciesCriteria"

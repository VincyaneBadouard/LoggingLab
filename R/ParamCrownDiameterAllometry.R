#' Parameters of the crown-diameter allometry (Species, Genus, Family levels)
#'
#' Parameters of crown-diameter allometry used to compute the crown diameter of
#' each tree, depending on its DBH (Diameter at Breast Height) and its Species,
#' Genus or Family names (Aubry-Kientz et al.2019).
#'
#' @references Aubry-Kientz, Mélaine, et al. "A comparative assessment of the
#'   performance of individual tree crowns delineation algorithms from ALS data
#'   in tropical forests." Remote Sensing 11.9 (2019): 1086.
#'
#' @format A tibble with 371 rows and 7 variables: \describe{
#'
#'   \item{alpha}{Coefficient of the equation: ln(DBH) = alpha + beta ln(H*CD) +
#'   error, with error~N(0,sigma^2). DBH: tree diameter at breast hight, H: tree
#'   height, CD: tree crown diameter. Mean sigma^2 = 0.0295966977 (double)}
#'
#'   \item{beta}{Coefficient of the equation: ln(DBH) = alpha + beta ln(H*CD) +
#'   error, with error~N(0,sigma^2).DBH: tree diameter at breast hight, H: tree
#'   height, CD: tree crown diameter. Mean sigma^2 = 0.0295966977 (double)}
#'
#'   \item{Taxo}{Taxonomic level of the parameters (character)}
#'
#'   \item{ScientificName}{Scientific name (character)}
#'
#'   \item{Genus}{Genus name (character)}
#'
#'   \item{Species}{Species name (character)}
#'
#'   \item{Family}{Family name (character)} ... }
#'
"ParamCrownDiameterAllometry"

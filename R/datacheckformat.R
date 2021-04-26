#' Check & format your inventory data for the package "Maria"
#'
#' @param data (dataframe) Your inventory data or the "Paracou6_2016" test data
#' @param verbose (boolean) Print in console
#'
#' @return
#' @export
#'
#' @examples
#' datacheckformat(data=Paracou6_2016)
datacheckformat <- function(
  data = "Paracou6_2016",
  verbose = TRUE # never use T or F
){
  if(!all("Plot" %in% names(data),
          "CensusYear" %in% names(data),
          "idTree" %in% names(data),
          "Family" %in% names(data),
          "Genus" %in% names(data),
          "Species" %in% names(data),
          "CircCorr" %in% names(data),
          "CodeAlive" %in% names(data),
          "CommercialSp" %in% names(data),
          "UTMZone" %in% names(data),
          'Lat' %in% names(data),
          'Lon' %in% names(data),
          'VernName' %in% names(data),
          'Xfield' %in% names(data),
          'Yfield' %in% names(data),
          'Xutm' %in% names(data),
          'Yutm' %in% names(data),
          if (!inherits(data$idTree, "integer") && !is.null(idTree)) #majuscule ?
            stop("idTree parameter should be an integer (column name), or NULL if no presence column."),

          if (!inherits(data$Plot, "character") && !is.null(Plot))
            stop("Plot parameter should be a character (column name), or NULL if no presence column."),

          if (!inherits(data$Xfield, "numeric") && !is.null(Xfield))
            stop("Xfield parameter should be numeric (column name), or NULL if no presence column."),

          if (!inherits(data$Yfield, "numeric") && !is.null(Yfield))
            stop("Yfield parameter should be numeric (column name), or NULL if no presence column."),

          if (!inherits(data$Xutm, "numeric") && !is.null(Xutm))
            stop("Xutm parameter should be numeric (column name), or NULL if no presence column."),

          if (!inherits(data$Yutm, "numeric") && !is.null(Yutm))
            stop("Yutm parameter should be numeric (column name), or NULL if no presence column."),

          if (!inherits(data$UTMZone, "integer") && !is.null(UTMZone))
            stop("UTMZone parameter should be an integer (column name), or NULL if no presence column."),

          if (!inherits(data$Lat, "numeric") && !is.null(Lat))
            stop("Lat parameter should be numeric (column name), or NULL if no presence column."),

          if (!inherits(data$Lon, "numeric") && !is.null(Lon))
            stop("Lon parameter should be numeric (column name), or NULL if no presence column."),

          if (!inherits(data$Family, "character") && !is.null(Family))
            stop("Family parameter should be a character (column name), or NULL if no presence column."),

          if (!inherits(data$Genus, "character") && !is.null(Genus))
            stop("Genus parameter should be a character (column name), or NULL if no presence column."),

          if (!inherits(data$Species, "character") && !is.null(Species))
            stop("Species parameter should be a character (column name), or NULL if no presence column."),

          if (!inherits(data$VernName, "character") && !is.null(VernName))
            stop("VernName parameter should be a character (column name), or NULL if no presence column."),

          if (!inherits(data$CommercialSp, "logical") && !is.null(CommercialSp))
            stop("CommercialSp parameter should be logical (column name), or NULL if no presence column."),

          if (!inherits(data$CensusYear, "integer") && !is.null(CensusYear))
            stop("CensusYear parameter should be an integer (column name), or NULL if no presence column."),

          if (!inherits(data$CodeAlive, "logical") && !is.null(CodeAlive))
            stop("CodeAlive parameter should be logical (column name), or NULL if no presence column."),

          if (!inherits(data$CircCorr, "numeric") && !is.null(CircCorr)) #DBH
            stop("CircCorr parameter should be numeric (column name), or NULL if no presence column.")))
  stop("Your data format, as specified in the 'data' argument, is not compatible. See the package vignette for the required format.")

}

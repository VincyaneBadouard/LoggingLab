#' harvestable
#'
#' @description The function tells you the harvestable volume in the plot,
#'and which trees are harvestable according to your harvestability criteria
#'
#' @param inventory (data.frame)
#' @param diversification (logical)
#' @param specieslax = FALSE by default (logical)
#'
#' @return Your inventory with the exploitability criteria, and if they are validated for each of the trees.
#' The function returns the harvestable volume too, in the plot for these criteria.
#'
#' @export
#'
#' @examples
#' inventory = ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016))))
#' harvestable(inventory, diversification = TRUE, specieslax = FALSE)
#'
harvestable <- function(
  inventory,
  diversification,
  specieslax = FALSE
){
  # Arguments check

  if(!inherits(inventory, "data.frame"))
    stop("The 'inventory' argument of the 'harvestable' function must be a data.frame")

  if(!any(unlist(lapply(list(diversification, specieslax), inherits, "logical"))))
    stop("The 'diversification' and 'specieslax' arguments of the 'harvestable' function must be logical") # any() don't take a list


  # Il manque les scénarios dans les conditions -_-

  #select essences
  HarverstableConditions <- # = 1 boolean vector
    if (diversification || (!diversification && specieslax)) {
      inventory$Commercial =="1"| inventory$Commercial == "2" # now or maybe after we will diversify
    } else if (!diversification && !specieslax) {
      inventory$Commercial == "1" # We will never diversify
    }


  #select diameters
  HarverstableConditions <- HarverstableConditions & (inventory$DBH >= inventory$MinFD & inventory$DBH <= inventory$MaxFD) # harvestable individuals, accord by their DBH

  #select spatially
  # HarverstableConditions <- HarverstableConditions & ()
  ##slope
  ##isolement
  ##MainTrails out

  inventory <- inventory %>%
    mutate(LoggingStatus = ifelse(HarverstableConditions, #Under the above criteria, designate the harvestable species
                                  "harvestable", "non-harvestable")) %>%
    mutate(LoggingStatus = ifelse(Commercial == "0", #The non-commercial species are non-harvestable.
                                  "non-harvestable", LoggingStatus)) %>%

    mutate(LoggingStatus = ifelse(
      !diversification &
        specieslax & #designate the secondarily harvestable species, because diversification only if necessary
        LoggingStatus == "harvestable" &
        Commercial == "2",
      "harvestable2nd", LoggingStatus))

  HarvestableTable <- inventory %>%
    filter(LoggingStatus == "harvestable")
  HVinit <- sum(HarvestableTable$TreeHarvestableVolume) #compute the harvestable volume in the plot for these criteria

  harvestableOutputs <- list(inventory = inventory, HVinit = HVinit)
  return(harvestableOutputs) # return the new inventory and the HVinit
}

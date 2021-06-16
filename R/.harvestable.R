#' .harvestable
#'
#' @param inventory (data.frame) type "RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel", "RIL3fuelhollow" or "manual"(character)
#'
#' @return
#' @export
#'
#' @examples
#'
.harvestable <- function(
  inventory,
  # type = c("RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel", "RIL3fuelhollow", "manual"),
  diversification,
  specieslax = FALSE,
){
  # Il manque les scénarios dans les conditions -_-

  #select essences
  HarverstableConditions <- # = 1 boolean vector
    if (diversification || (!diversification && specieslax)) {
      inventoryA$Commercial =="1"| inventoryA$Commercial == "2" # now or maybe after we will diversify
    } else if (!diversification && !specieslax) {
      Commercial == "1" # We will never diversify
    }

  #select diameters
  HarverstableConditions <- HarverstableConditions & (inventoryA$DBH >= inventoryA$MinFD & inventoryA$DBH <= inventoryA$MaxFD) # harvestable individuals, accord by their DBH

  #select spatially
  # HarverstableConditions <- HarverstableConditions & ()
  ##slope
  ##isolement
  ##MainTrails out

  inventoryB <- inventoryA %>%
    mutate(LoggingStatus = ifelse(HarverstableConditions, #Under the above criteria, designate the harvestable species
                                  "harvestable", "non-harvestable")) %>%
    mutate(LoggingStatus = ifelse(is.na(Commercial), #The non-commercial species are non-harvestable.
                                  "non-harvestable", LoggingStatus)) %>%

    mutate(LoggingStatus = ifelse(
      !diversification &
        specieslax & #designate the secondarily harvestable species, because diversification only if necessary
        LoggingStatus == "harvestable" &
        Commercial == "2",
      "harvestable2nd", LoggingStatus)) %>%

    mutate(LoggingStatus = ifelse(
      LoggingStatus == "harvestable" &
        Commercial == "2" &
        DBH >= UpMinFD, #designate preferred species of 2nd rank if the plot is species-rich.
      "UPeco2", LoggingStatus))

  HarvestableTable <- inventoryB %>%
    filter(LoggingStatus == "harvestable"| LoggingStatus == "UPeco2")
  HVinit <- sum(HarvestableTable$TreeHarvestableVolume) #compute the harvestable volume in the plot for these criteria

}

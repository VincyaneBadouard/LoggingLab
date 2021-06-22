#' .harvestable
#'
#' @param inventory (data.frame)
#' @param diversification (logical)
#' @param specieslax = FALSE by default (logical)
#'
#' @return
#' @export
#'
#' @examples
#' inventory = addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))
#' .harvestable(inventory, diversification = TRUE, specieslax = FALSE)
#'
.harvestable <- function(
  inventory,
  diversification,
  specieslax = FALSE,
){
  # Il manque les scénarios dans les conditions -_-

  #select essences
  HarverstableConditions <- # = 1 boolean vector
    if (diversification || (!diversification && specieslax))
      inventory$Commercial =="1"| inventory$Commercial == "2" # now or maybe after we will diversify
      else if (!diversification && !specieslax)
      Commercial == "1" # We will never diversify

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
  mutate(LoggingStatus = ifelse(is.na(Commercial), #The non-commercial species are non-harvestable.
                                "non-harvestable", LoggingStatus)) %>%

  mutate(LoggingStatus = ifelse(
    !diversification &
      specieslax & #designate the secondarily harvestable species, because diversification only if necessary
      LoggingStatus == "harvestable" &
      Commercial == "2",
    "harvestable2nd", LoggingStatus)) %>%

  HarvestableTable <- inventory %>%
  filter(LoggingStatus == "harvestable")
HVinit <- sum(HarvestableTable$TreeHarvestableVolume) #compute the harvestable volume in the plot for these criteria

}

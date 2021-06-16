.selected <- function(
  inventory,
  speciescriteria = SpeciesCriteria,
  type = c("RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel", "RIL3fuelhollow", "manual"),
  fuel = "0",
  objective,
  # diversification,
  # specieslax = FALSE,
  # objectivelax = FALSE,
  otherloggingparameters = loggingparameters(),
  # MainTrail
  VO = VO, # objective volume
  HVinit = HVinit # initial Harvestable Volume
){
  # if objective achieved at the first attempt
  inventoryC <- inventoryB %>%
    mutate(Selected = ifelse(HVinit == VO & LoggingStatus == "harvestable",
                             "1", NA))# if we have our volume, harvestable ind = selected ind


  if (HVinit < VO){ #diversification is necessary, designate the secondary-economic-rank species too
    if (!diversification && specieslax){
      inventoryD <- inventoryC %>%
        mutate(Condition = ifelse(LoggingStatus == "harvestable2nd" | LoggingStatus == "harvestable", TRUE, FALSE)) %>%
        group_by(Condition) %>%
        arrange(desc(TreeHarvestableVolume)) %>%
        mutate(VolumeCumSum = cumsum(TreeHarvestableVolume)) %>%
        ungroup() %>%
        mutate(Selected = ifelse(Condition & VolumeCumSum <= VO, "1", NA)) %>%
        select(-Condition)

      HarvestableTable <- inventoryD %>%
        filter(Selected == "1")
      HVlax <- sum(HarvestableTable$TreeHarvestableVolume) #49.69643 Harvestable volume, with specieslax permission


      if (HVlax < VO && objectivelax) message("The exploitable volume (= ",paste(HVlax),") is still lower (by ",paste(VO- HVlax),") than your objective volume despite the diversification you have allowed (without the diversification HVinit= ",paste(HVinit),"). In this case, you have chosen to continue harvesting with a volume lower than your objective.")

      if (HVlax < VO && !objectivelax) stop("The harvestable volume = ",paste(HVlax),") is still lower (by ",paste(VO- HVlax),") than your objective volume despite the diversification you have allowed (without the diversification HVinit= ",paste(HVinit),"). By default or by your choice, the simulation stops. If you wish to continue the exploitation in spite of an exploitable volume lower than your objective volume, you have the argument 'objectivelax'.")

      if (!HVlax == VO) message("The harvestable volume (=",paste(HVinit),") was lower (by ",paste(VO- HVinit),") than your objective volume. You have chosen to diversify your species selection in this case. The exploitation was therefore carried out on this diversified selection of species.")
    }

    if (!diversification && !specieslax && objectivelax)
      message("The harvestable volume (=",paste(HVinit),") is less (by ",paste(VO- HVinit),") than your objective volume. In this case you have chosen to continue logging without diversifying your species.")

    if (diversification && objectivelax)
      message("The harvestable volume (=",paste(HVinit),") is less (by ",paste(VO- HVinit),") than your objective volume. In this case you have chosen to continue logging.")


    if ((!specieslax & !objectivelax) | (diversification && !objectivelax))
      stop("The harvestable volume (=",paste(HVinit),") is lower (by ",paste(VO- HVinit),") than your objective volume. By default or by your choice, the simulation stops. If you wish to continue the exploitation in spite of a harvestable volume lower than your objective volume, you can use the argument 'objectivelax' or the diversification of species (if it is not already the case).")
  }



}

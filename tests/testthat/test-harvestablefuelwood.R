test_that("harvestablefuelwood", {

  # library(LoggingLab)
  # library(tidyverse)
  # library(sf)

  # Data
  data(Paracou6_2016)
  data(DTMParacou)
  data(PlotMask)
  data(ForestZoneVolumeParametersTable)
  data(MainTrails)
  data(HarvestableAreaOutputsCable)
  MatrixInventory <- as.matrix(Paracou6_2016)

  inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask),
                          volumeparameters = ForestZoneVolumeParametersTable)

  inventory <- suppressMessages(treeselection(inventory,
                                              topography = DTMParacou,
                                              speciescriteria = SpeciesCriteria,
                                              scenario = "manual", objective = 30, fuel = "2", diversification = TRUE,
                                              winching = "0", specieslax = FALSE, objectivelax = TRUE,
                                              harvestablearea = HarvestableAreaOutputsCable$HarvestableArea,
                                              plotslope = HarvestableAreaOutputsCable$PlotSlope,
                                              maintrails = MainTrails,
                                              harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons,
                                              advancedloggingparameters = loggingparameters())$inventory)

  if (!("DeathCause" %in% names(inventory))){
    inventory <- inventory %>%
      add_column(DeathCause = NA) # if "DeathCause" column doesnt exist create it
  }

  inventory <- inventory %>%
    mutate(DeathCause = ifelse(is.na(DeathCause) & Selected == "1" & ProbedHollow == "0",
                               "cutted", DeathCause)) %>% # timber exploitation
    mutate(DeathCause = ifelse(is.na(DeathCause) & Selected == "1" & ProbedHollow == "1",
                               "hollowfuel", DeathCause)) # fuel wood exploitation

  inventory[is.na(inventory$DeathCause), ]$DeathCause[1] <- "maintrail"
  inventory[is.na(inventory$DeathCause), ]$DeathCause[2] <- "2ndtrail"
  inventory[is.na(inventory$DeathCause), ]$DeathCause[3] <- "treefall2nd"
  inventory[is.na(inventory$DeathCause), ]$DeathCause[4] <- "landing"

  inventory_Fuel <- inventory

  inventory_NoFuel <- inventory
  inventory_NoFuel[inventory_NoFuel$DeathCause %in% "hollowfuel", ]$Selected <- "0"
  inventory_NoFuel[inventory_NoFuel$DeathCause %in% "hollowfuel", ]$DeathCause <- NA

  TimberVFuel <- timberharvestedvolume(inventory_Fuel, scenario = "manual", fuel = "2",
                                       advancedloggingparameters = loggingparameters())

  TimberVNoFuel <- timberharvestedvolume(inventory_NoFuel, scenario = "manual", fuel = "0", # no hollow
                                         advancedloggingparameters = loggingparameters())


  inventory_Fuel <- TimberVFuel$inventory
  TimberLoggedVolume_Fuel <- TimberVFuel$TimberLoggedVolume
  NoHollowTimberLoggedVolume_Fuel <- TimberVFuel$NoHollowTimberLoggedVolume

  inventory_NoFuel <- TimberVNoFuel$inventory
  TimberLoggedVolume_NoFuel <- TimberVNoFuel$TimberLoggedVolume
  NoHollowTimberLoggedVolume_NoFuel <- TimberVNoFuel$NoHollowTimberLoggedVolume



  # Run function

  Rslt_Fuel <- harvestablefuelwood(inventory_Fuel, scenario = "manual", fuel = "2",
                                   TimberLoggedVolume = TimberLoggedVolume_Fuel,
                                   NoHollowTimberLoggedVolume = NoHollowTimberLoggedVolume_Fuel,
                                   advancedloggingparameters = loggingparameters())

  Rslt_NoFuel <- harvestablefuelwood(inventory_NoFuel, scenario = "manual", fuel = "0",
                                     TimberLoggedVolume = TimberLoggedVolume_NoFuel,
                                     NoHollowTimberLoggedVolume = NoHollowTimberLoggedVolume_NoFuel,
                                     advancedloggingparameters = loggingparameters())

  Rsltinventory_Fuel <- Rslt_Fuel$inventory
  LoggingResidualBiomass_Fuel <- Rslt_Fuel$LoggingResidualBiomass
  FuelWoodBiomass_Fuel <-Rslt_Fuel$FuelWoodBiomass

  Rsltinventory_NoFuel <- Rslt_NoFuel$inventory
  LoggingResidualBiomass_NoFuel <- Rslt_NoFuel$LoggingResidualBiomass
  FuelWoodBiomass_NoFuel <-Rslt_NoFuel$FuelWoodBiomass

  # Tests

  # Check the function arguments ####

  expect_error(harvestablefuelwood(MatrixInventory),
               regexp = "The 'inventory' argument of the 'harvestablefuelwood' function must be a data.frame")

  expect_error(harvestablefuelwood(inventory, scenario = "RIL"),
               regexp = "The 'scenario' argument of the 'harvestablefuelwood' function must be
         'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  expect_error(harvestablefuelwood(inventory, scenario = "manual", fuel = TRUE),
               regexp = "The 'fuel' argument of the 'harvestablefuelwood' function must be '0', '1', '2' or NULL")

  expect_error(harvestablefuelwood(inventory, scenario = "manual", fuel = "2",
                                   advancedloggingparameters = 20),
               regexp = "The 'advancedloggingparameters' argument
         of the 'harvestablefuelwood' function must be a list")

  expect_error(harvestablefuelwood(inventory, scenario = "manual", fuel = "2",
                                   advancedloggingparameters = loggingparameters(),
                                   TimberLoggedVolume = TRUE, NoHollowTimberLoggedVolume = TRUE),
               regexp = "The 'TimberLoggedVolume' and 'NoHollowTimberLoggedVolume' arguments
         of the 'harvestablefuelwood' function must be numeric")

  # if no fuel wood exploitation : FuelWoodBiomass = Null, NA in the column.
  expect_true(is.null(FuelWoodBiomass_NoFuel))
  expect_true(all(is.na(Rsltinventory_NoFuel$FuelWoodBiomass)))

  # if fuel wood exploitation : FuelWoodBiomass > 0
  expect_true(FuelWoodBiomass_Fuel > 0)
  ## fuel in killed trees
  expect_true(all(
    Rsltinventory_Fuel[!is.na(Rsltinventory_Fuel$DeathCause), ]$FuelWoodBiomass > 0))


  # LoggingResidualBiomass not null
  LoggingResidualBiomass <- list(LoggingResidualBiomass_Fuel, LoggingResidualBiomass_NoFuel)
  for(i in 1:length(LoggingResidualBiomass)){
    expect_true(!is.null(LoggingResidualBiomass[i]))
  }


  Rsltinventory <- list(Rsltinventory_Fuel, Rsltinventory_NoFuel)

  for(i in 1:length(Rsltinventory)){
    # Timber in selected trees
    expect_true(all(
      Rsltinventory[[i]][Rsltinventory[[i]]$Selected == "1" &
                           !is.na(Rsltinventory[[i]]$Selected), ]$TimberLoggedBiomass > 0))

    # AGB = TimberLoggedBiomass + FuelWoodBiomass + LoggingResidualBiomass
    expect_true(all(na.omit(
      Rsltinventory[[i]]$AGB == (Rsltinventory[[i]]$TimberLoggedBiomass +
                                   Rsltinventory[[i]]$FuelWoodBiomass +
                                   Rsltinventory[[i]]$LoggingResidualBiomass))))
  }

})

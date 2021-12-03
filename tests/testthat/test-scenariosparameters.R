test_that("scenariosparameters", {

  expect_error(scenariosparameters(fuel = "0", diversification = TRUE, objective = 20,
                                   scenario = "CL"),
               regexp = '"scenario" argument should be in "RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel",
               "RIL3fuelhollow" or "manual"; not')

  expect_error(scenariosparameters(scenario ="manual", fuel = "0", diversification = TRUE,
                             objective = "RIL"),
               regexp = "'objective' argument should be numeric or null.")

  expect_error(scenariosparameters(scenario ="manual", objective = 20, diversification = TRUE,
                                   fuel = TRUE),
               regexp = "'fuel', 'winching', and 'directionalfelling' arguments should be character or null.")

  expect_error(scenariosparameters(scenario ="manual", fuel = "0", objective = 20,
                             diversification = "1"),
               regexp = "'diversification' argument should be logical or null.")


  scenarii <- lapply(c(RIL1 = "RIL1",
                       RIL2broken = "RIL2broken",
                       RIL2 = "RIL2",
                       RIL3 = "RIL3",
                       RIL3fuel = "RIL3fuel",
                       RIL3fuelhollow = "RIL3fuelhollow"
  ), scenariosparameters)

  expect_true(all(unlist(scenarii$RIL1) == c("25", "0", "FALSE", "0", "0")))
  expect_true(all(unlist(scenarii$RIL2broken) == c("25", "0", "FALSE", "0", "0")))
  expect_true(all(unlist(scenarii$RIL2) == c("25", "0", "FALSE", "1", "0")))
  expect_true(all(unlist(scenarii$RIL3) == c("30", "0", "TRUE", "2", "2")))
  expect_true(all(unlist(scenarii$RIL3fuel) == c("30", "1", "TRUE", "2", "2")))
  expect_true(all(unlist(scenarii$RIL3fuelhollow) == c("30", "2", "TRUE", "2", "2")))
})

# scenario	SpatialDatatype	Winching 	DirectionalFelling 	Objective 	Diversification
# RIL1	           SRTM	    0	           0	            20-25	         FALSE
# RIL2broken	    LIDAR	    0	           0	            20-25	         FALSE
# RIL2	          LIDAR     1	           0	            20-25	         FALSE
# RIL3	          LIDAR	    2	           2	            25-30	          TRUE
# RIL3fuel	      LIDAR	    2	           2	            25-30	          TRUE
# RIL3fuelhollow	LIDAR	    2	           2	            25-30         	TRUE

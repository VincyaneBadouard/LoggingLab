test_that("scenariosparameters", {

  #RIL1
  expect_true(scenariosparameters(type = "RIL1")$objective == 25)

  expect_true(scenariosparameters(type = "RIL1")$fuel == "0")

  expect_true(scenariosparameters(type = "RIL1")$diversification == FALSE)

  expect_true(scenariosparameters(type = "RIL1")$winching =="0")

  expect_true(scenariosparameters(type = "RIL1")$directionalfelling == "0")

  #RIL2broken
  expect_true(scenariosparameters(type = "RIL2broken")$objective == 25)

  expect_true(scenariosparameters(type = "RIL2broken")$fuel == "0")

  expect_true(scenariosparameters(type = "RIL2broken")$diversification == FALSE)

  expect_true(scenariosparameters(type = "RIL2broken")$winching =="0")

  expect_true(scenariosparameters(type = "RIL2broken")$directionalfelling == "1")

  #RIL2
  expect_true(scenariosparameters(type = "RIL2")$objective == 25)

  expect_true(scenariosparameters(type = "RIL2")$fuel == "0")

  expect_true(scenariosparameters(type = "RIL2")$diversification == FALSE)

  expect_true(scenariosparameters(type = "RIL2")$winching =="1")

  expect_true(scenariosparameters(type = "RIL2")$directionalfelling == "1")

  #RIL3
  expect_true(scenariosparameters(type = "RIL3")$objective == 30)

  expect_true(scenariosparameters(type = "RIL3")$fuel == "0")

  expect_true(scenariosparameters(type = "RIL3")$diversification == TRUE)

  expect_true(scenariosparameters(type = "RIL3")$winching =="2")

  expect_true(scenariosparameters(type = "RIL3")$directionalfelling == "2")

  #RIL3fuel
  expect_true(scenariosparameters(type = "RIL3fuel")$objective == 30)

  expect_true(scenariosparameters(type = "RIL3fuel")$fuel == "1")

  expect_true(scenariosparameters(type = "RIL3fuel")$diversification == TRUE)

  expect_true(scenariosparameters(type = "RIL3fuel")$winching =="2")

  expect_true(scenariosparameters(type = "RIL3fuel")$directionalfelling == "2")

  #RIL3fuelhollow
  expect_true(scenariosparameters(type = "RIL3fuelhollow")$objective == 30)

  expect_true(scenariosparameters(type = "RIL3fuelhollow")$fuel == "2")

  expect_true(scenariosparameters(type = "RIL3fuelhollow")$diversification == TRUE)

  expect_true(scenariosparameters(type = "RIL3fuelhollow")$winching =="2")

  expect_true(scenariosparameters(type = "RIL3fuelhollow")$directionalfelling == "2")


})


#


# Type	SpatialDatatype	Winching 	DirectionalFelling 	Objective 	Diversification
# RIL1	           SRTM	    0	           0	            20-25	         FALSE
# RIL2broken	    LIDAR	    0	           1	            20-25	         FALSE
# RIL2	          LIDAR     1	           1	            20-25	         FALSE
# RIL3	          LIDAR	    2	           2	            25-30	          TRUE
# RIL3fuel	      LIDAR	    2	           2	            25-30	          TRUE
# RIL3fuelhollow	LIDAR	    2	           2	            25-30         	TRUE

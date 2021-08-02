test_that("successfail", {

  # Test data
  data(Paracou6_2016)
  # Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:2000)
  MatrixInventory <- as.matrix(Paracou6_2016)

  # Check the function arguments
  expect_error(successfail(MatrixInventory,
                           fuel = "0", directionalfelling = "2",
                           advancedloggingparameters = loggingparameters()),
               regexp = "The 'inventory'argument of the 'successfail' function must be data.frame")

  expect_error(successfail(Paracou6_2016, directionalfelling = "2",
                           advancedloggingparameters = loggingparameters(),
                           fuel = TRUE),
               regexp = "The 'fuel' argument of the 'successfail' function must be '0', '1', '2' or NULL")

  expect_error(successfail(Paracou6_2016, fuel = "2",
                           advancedloggingparameters = loggingparameters(),
                           directionalfelling = TRUE),
               regexp = "The 'directionalfelling' argument of the 'successfail' function must be '0', '1', '2' or NULL")

  expect_error(successfail(Paracou6_2016, fuel = "2", directionalfelling = "2",
                           advancedloggingparameters = as.matrix(loggingparameters())),
               regexp = "The 'advancedloggingparameters' argument of the 'successfail' function must be a list")


  # Check scenarios
  inventory <- Paracou6_2016 %>%
    add_column(Accessible = rep_len(c("0", "1", "1", NA), length.out = nrow(Paracou6_2016))) %>%
    add_column(Selected = rep_len(c("0", "1", "1", "1", NA), length.out = nrow(Paracou6_2016))) %>%
    add_column(ProbedHollow = rep_len(c("0", "1", NA), length.out = nrow(Paracou6_2016)))



  Random <- successfail(inventory, fuel = "0",directionalfelling = "0", advancedloggingparameters = loggingparameters())
  Accessible <- successfail(inventory, fuel = "0",directionalfelling = "2", advancedloggingparameters = loggingparameters()) %>%
    filter(Selected == "1") # Accessible == "1" !!

  Damage <- successfail(inventory, fuel = "1",directionalfelling = "2", advancedloggingparameters = loggingparameters())%>%
    filter(Selected == "1")

  Hollow <- successfail(inventory, fuel = "2",directionalfelling = "2", advancedloggingparameters = loggingparameters())%>%
    filter(Selected == "1" | ProbedHollow == "1")


  expect_true(all(Random$TreeFellingOrientationSuccess == "0"))

  TestList <- list(Accessible$TreeFellingOrientationSuccess,
       Damage$TreeFellingOrientationSuccess,
       Hollow$TreeFellingOrientationSuccess)

  lapply(TestList,
         function(element) expect_true(all(element == "0" | element == "1"))
         )

  # NA not tested


})


# check args errors
# if (fuel == "0" && directionalfelling == "0") TreeFellingOrientationSuccess = 0
# if (fuel == "0" && directionalfelling != "0" && Accessible == "1") TreeFellingOrientationSuccess == "0" || TreeFellingOrientationSuccess == "1"
# if (fuel =="1" && Selected == "1") TreeFellingOrientationSuccess == "0" || TreeFellingOrientationSuccess == "1"
# if (fuel =="2" && (Selected == "1" || ProbedHollow == "1")) TreeFellingOrientationSuccess == "0" || TreeFellingOrientationSuccess == "1"
# else is.na(TreeFellingOrientationSuccess)

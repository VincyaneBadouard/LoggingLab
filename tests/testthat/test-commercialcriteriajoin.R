test_that("commercialcriteriajoin", {

  # Data loading
  data(Paracou6_2016)
  data(SpeciesCriteria)

  Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:500)

  MatrixInventory <- as.matrix(Paracou6_2016)


  Matrixspeciescriteria <- as.matrix(SpeciesCriteria)

  # Check the function arguments
  expect_error(commercialcriteriajoin(MatrixInventory, Matrixspeciescriteria),
               regexp = "The function arguments must be data.frames")


  # speciescriteria columns check
  Badspeciescriteria <- SpeciesCriteria %>%
    dplyr::rename(Vernacular = CommercialName)

  testinventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask), volumeparameters = ForestZoneVolumeParametersTable)

  expect_error(commercialcriteriajoin(testinventory, Badspeciescriteria),
               regexp = "The columns requested in the data frame given in the speciescriteria argument
                  are not found")

  # Check variables class

  ## Test data preparation
  testinventory <- commercialcriteriajoin(testinventory, SpeciesCriteria) # compute the new inventory

  TestList <- list( # list the variables to check
    testinventory$MinFD,
    testinventory$UpMinFD,
    testinventory$MaxFD)

  lapply(TestList,
         function(element) expect_type(element, "double"))

  expect_type(testinventory$CommercialName, "character")
  expect_s3_class(testinventory$CommercialLevel, "factor")

  # Check that commercial sp have logging info, and non-commercial have not:
  TestCommercial <- testinventory %>%
    filter(CommercialLevel == "0")

  TestList <- list( # list the variables to check
    TestCommercial$CommercialName,
    TestCommercial$MinFD,
    TestCommercial$UpMinFD,
    TestCommercial$MaxFD)

  lapply(TestList,
         function(element) expect_true(all(is.na(element))))

  TestCommercial <- testinventory %>%
    filter(CommercialLevel != "0")

  TestList <- list( # list the variables to check
    TestCommercial$CommercialName,
    TestCommercial$MinFD,
    TestCommercial$UpMinFD,
    TestCommercial$MaxFD)

  lapply(TestList,
         function(element) expect_true(all(!is.na(element))))

  # Check species attribution exceptions

  SpExceptions <- testinventory %>%
    filter(ScientificName == "Lecythis_poiteaui"| ScientificName == "Lecythis_praeclara" |
             ScientificName == "Lecythis_holcogyne" | ScientificName == "Lecythis_pneumatophora"|
             ScientificName == "Lecythis chartacea"| ScientificName == "Lecythis zabucajo" |
             ScientificName == "Micropholis_melinoniana"| ScientificName == "Micropholis_egensis" |
             ScientificName == "Micropholis_cayennensis" | ScientificName == "Micropholis_obscura" |
             ScientificName == "Pradosia_cochlearia"| ScientificName == "Pradosia_huberi") %>%
    filter(CommercialName != "mamantin"| is.na(CommercialName)) #because filter() drop NA

  TestList <- list( # list the variables to check
    SpExceptions$CommercialName,
    SpExceptions$MinFD,
    SpExceptions$UpMinFD,
    SpExceptions$MaxFD)

  lapply(TestList,
         function(element) expect_true(all(is.na(element))))

})


# check colonnes présentes et classe:
# CommercialName(character), CommercialLevel(factor), MinFD(numeric), UpMinFD(numeric), MaxFD(numeric)
# Quand CommercialLevel diff de "0" : CommercialName, MinFD, UpMinFD, MaxFD dif de NA
# Check species attribution exceptions

# Quand CommercialLevel == "0" : CommercialName, MinFD, UpMinFD, MaxFD == NA

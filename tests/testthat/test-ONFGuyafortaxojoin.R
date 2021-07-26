test_that("ONFGuyafortaxojoin", {

  # Check the function arguments

  data(Paracou6_2016)
  data(SpeciesCriteria)

  Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:500)

  MatrixInventory <- as.matrix(Paracou6_2016)


  Matrixspeciescriteria <- as.matrix(SpeciesCriteria)

  expect_error(ONFGuyafortaxojoin(MatrixInventory, Matrixspeciescriteria), regexp = "The function arguments must be data.frames")


  # speciescriteria columns check

  Badspeciescriteria <- SpeciesCriteria %>%
    dplyr::rename(Vernacular = VernName)

  testinventory <- addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))

  expect_error(ONFGuyafortaxojoin(testinventory, Badspeciescriteria), regexp = "The columns requested in the data frame given in the speciescriteria argument are not found")

  # Check variables class

  ## Test data preparation
  testinventory <- ONFGuyafortaxojoin(testinventory) # compute the new inventory

  TestList <- list( # list the variables to check
    testinventory$MinFD,
    testinventory$UpMinFD,
    testinventory$MaxFD)

  lapply(TestList,
         function(element) expect_type(element, "double"))

  expect_type(testinventory$ONFName, "character")
  expect_s3_class(testinventory$Commercial, "factor") #pq S3? je ne sais pas

  # Check that commercial sp have logging info, and non-commercial have not:
  TestCommercial <- testinventory %>%
    filter(Commercial == "0")

  TestList <- list( # list the variables to check
    TestCommercial$ONFName,
    TestCommercial$MinFD,
    TestCommercial$UpMinFD,
    TestCommercial$MaxFD)

  lapply(TestList,
         function(element) expect_true(all(is.na(element))))

  TestCommercial <- testinventory %>%
    filter(Commercial != "0")

  TestList <- list( # list the variables to check
    TestCommercial$ONFName,
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
    filter(ONFName != "mamantin"| is.na(ONFName)) #because filter() drop NA

  TestList <- list( # list the variables to check
    SpExceptions$ONFName,
    SpExceptions$MinFD,
    SpExceptions$UpMinFD,
    SpExceptions$MaxFD)

  lapply(TestList,
         function(element) expect_true(all(is.na(element))))

})


# check colonnes présentes et classe:
# ONFName(character), Commercial(factor), MinFD(numeric), UpMinFD(numeric), MaxFD(numeric)
# Quand Commercial diff de "0" : ONFName, MinFD, UpMinFD, MaxFD dif de NA
# Check species attribution exceptions

# Quand Commercial == "0" : ONFName, MinFD, UpMinFD, MaxFD == NA

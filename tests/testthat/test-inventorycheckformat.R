test_that("inventorycheckformat", {

  # check if the stops work

  ## check the function argument
  data(Paracou6_2016)
  Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:1000)

  MatrixInventory <- as.matrix(Paracou6_2016)
  expect_error(inventorycheckformat(MatrixInventory), regexp = "inventory must be a data.frame")


  ## check if all the variables are detected
  ### Create the test inventory
  DetectVar <- Paracou6_2016 %>%
    dplyr::select(-Species,-idTree,-Family,-Genus,-Species,-CircCorr,-CodeAlive,
                  -UTMZone,-Lat,-Lon,-Xfield,-Yfield,-Xutm,-Yutm)


  ### Test
  errors <- capture_error(inventorycheckformat(DetectVar))
  lapply(c("idTree","Family","Genus","Species","CircCorr","CodeAlive",
           "UTMZone","Xutm","Yutm"), function(element)
             expect_match(errors$message, regexp = element))

  #Create the test inventory
  CleanedInventory <- cleaninventory(Paracou6_2016, PlotMask)

  expect_false(any(is.na(CleanedInventory$DBH))) #check that all the DBH are computed


  #check if the stops work
  ##Create the test inventory
  StopTestInventory <- Paracou6_2016
  StopTestInventory[1:3, "idTree"] <- 200L # create a 3 time present tree
  StopTestInventory[5, "Plot"] <- "100" # a tree in another plot
  StopTestInventory[20, "CensusYear"] <- 2017L # different years inventory


  errors <- capture_error(inventorycheckformat(StopTestInventory))
  lapply(c("idTree","CensusYear","Plot"), function(element)
  expect_match(errors$message, regexp = element)) # check if idTree's are unique, if it is always the same plot and the same year

})

#check if all the variables are here -> detected
#and if they are of the right class -> check if class to detect are the right ones
#stop when at least one condition is not checked -> check if the stops work



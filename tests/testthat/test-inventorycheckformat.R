test_that("inventorycheckformat", {

  # check if the stops work

  ## check if all the variables are detected
  ### Create the test inventory
  DetectVar <- Paracou6_2016 %>%
    dplyr::select(-Species,-CensusYear,-idTree,-Family,-Genus,-Species,-CircCorr,-CodeAlive,-CommercialSp,-UTMZone,-Lat,-Lon,-Xfield,-Yfield,-Xutm,-Yutm) %>%
    dplyr::rename(Vernacular = VernName) %>%
    dplyr::rename(PLOT = Plot)

  ### Test
  lapply(c("Plot","CensusYear","idTree","Family","Genus","Species","CircCorr","CodeAlive","CommercialSp",
           "UTMZone","Lat","Lon","VernName","Xfield","Yfield","Xutm","Yutm"), function(element)
             expect_error(inventorycheckformat(DetectVar), regexp = element))

  ## check if class to detect are the right ones
  ### Create the test inventory
  if (!("DBH" %in% names(Paracou6_2016))) {add_column(Paracou6_2016, DBH = NA) #if DBH (cm) doesn't exist create it
    Paracou6_2016$DBH <- Paracou6_2016$CircCorr/pi} # and compute it
  RightClasses <- Paracou6_2016 %>%
    dplyr::mutate_if(is.character, as.factor) %>%
    dplyr::mutate_if(is.double, as.character) %>%
    dplyr::mutate_if(is.integer, as.double) %>%
    dplyr::mutate_if(is.logical, as.factor)

  ### Test
  lapply(c("Plot","CensusYear","idTree","Family","Genus","Species","CircCorr","CodeAlive","CommercialSp",
           "UTMZone","Lat","Lon","VernName","Xfield","Yfield","Xutm","Yutm"), function(element)
             expect_error(inventorycheckformat(RightClasses), regexp = element))

  expect_identical(inventorycheckformat(Paracou6_2016), Paracou6_2016) # test if the function's ouptut is the same that its input

})

#check if all the variables are here -> detected
#and if they are of the right class -> check if class to detect are the right ones
#stop when at least one condition is not checked -> check if the stops work



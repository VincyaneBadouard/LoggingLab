test_that("inventorycheckformat", {

  #load the test inventory
  load(system.file("extdata", "BrokenParacou6_2016.rda", package = "Maria"))

   #check if all the variables are detected
  expect_true(all(c("Plot","CensusYear","idTree","Family","Genus","Species","CircCorr","CodeAlive","CommercialSp",
                    "UTMZone","Lat","Lon","VernName","Xfield","Yfield","Xutm","Yutm")%in% names(inventorycheckformat(Paracou6_2016))))

  #check if class to detect are the right ones
  expect_type(c(inventorycheckformat(Paracou6_2016)$idTree, inventorycheckformat(Paracou6_2016)$UTMZone,
                inventorycheckformat(Paracou6_2016)$CensusYear), "integer") #idTree, UTMZone, CensusYear

  expect_type(c(inventorycheckformat(Paracou6_2016)$Plot, inventorycheckformat(Paracou6_2016)$Family, inventorycheckformat(Paracou6_2016)$Genus,
                inventorycheckformat(Paracou6_2016)$Species, inventorycheckformat(Paracou6_2016)$VernName), "character") #Plot, Family, Genus, Species, VernName

  expect_type(c(inventorycheckformat(Paracou6_2016)$Xfield, inventorycheckformat(Paracou6_2016)$Yfield, inventorycheckformat(Paracou6_2016)$Xutm,
                inventorycheckformat(Paracou6_2016)$Yutm, inventorycheckformat(Paracou6_2016)$Lat,
                inventorycheckformat(Paracou6_2016)$Lon, inventorycheckformat(Paracou6_2016)$CircCorr), "double") #Xfield, Yfield, Xutm, Yutm, Lat, Lon, CircCorr | DBH

  expect_type(c(inventorycheckformat(Paracou6_2016)$CommercialSp, inventorycheckformat(Paracou6_2016)$CodeAlive), "logical") #CommercialSp, CodeAlive

  #check if the stops work
  expect_error(datacheckformat(BrockenParacou6_2016))
})

#check if all the variables are here -> detected
#and if they are of the right class -> check if class to detect are the right ones
#stop when at least one condition is not checked -> check if the stops work



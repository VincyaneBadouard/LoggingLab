test_that("cleaninventory", {
  load(system.file("extdata", "BrokenParacou6_2016.rda", package = "Maria"))
  # expect_true(cleaninventory(data), InventoryClean$CodeAlive == "TRUE") #ca marche pas
  # expect_true(data$CodeAlive, InventoryClean$CodeAlive == "TRUE") #ca marche pas*
  expect_type(BrokenParacou6_2016$idTree, 'character')
  expect_error(cleaninventory(inventory = BrokenParacou6_2016))
})
#si ma fct fctne, dans inventory=BrockenParacou6_2016 :
# CodeAlive == TRUE
# CircCorr >= 10
# et ils me renvoie des msg d'erreurs

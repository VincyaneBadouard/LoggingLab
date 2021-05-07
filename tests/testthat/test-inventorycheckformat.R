test_that("datacheckformat", {
  load(system.file("extdata", "BrokenParacou6_2016.rda", package = "Maria"))
  expect_error(datacheckformat(data=BrockenParacou6_2016))
})


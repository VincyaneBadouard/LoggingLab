# context("bonjour") # deprecated in the 3rd edition
test_that("bonjour", {
  expect_equal(bonjour(verbose = F), "Bonjour! Maria")
})

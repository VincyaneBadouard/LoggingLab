test_that("datacheckformat", {
  # Create a broken dataset (in inst/extdata)
  BrokenParacou6_2016 <- Paracou6_2016 %>%
    select(-Species) %>% # missing column
    rename(DBH = CircCorr) %>% # other column name
    rename(PLOT = Plot) %>% # variable name in upper case
    mutate(idTree = as.character(as.numeric(idTree))) %>% #an other variable class
    mutate(Lon = round(Lon, digits =0)) %>% # no digits
    mutate(Genus = toupper(Genus)) # values in upper case,
  expect_error(datacheckformat(data=BrockenParacou6_2016))
})


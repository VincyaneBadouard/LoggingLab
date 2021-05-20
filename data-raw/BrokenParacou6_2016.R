## code to prepare `BrokenParacou6_2016` dataset goes here

# Create a broken dataset (in inst/extdata)
library(schoolmath)
library(dplyr)

BrokenParacou6_2016 <- Paracou6_2016 %>%
  select(-Species) %>% # missing column
  rename(Vernacular = VernName) %>% # other column name
  rename(PLOT = Plot) %>% # variable name in upper case
  mutate(idTree = as.character(as.numeric(idTree))) %>% #an other variable class
  mutate(Lon = round(Lon, digits =0)) %>% # no digits
  mutate(Genus = toupper(Genus)) %>%  # values in upper case
  mutate(CodeAlive = ifelse(is.even(Paracou6_2016$idTree), "FALSE", CodeAlive)) %>% # Some FALSE in CodeAlive
  mutate(CensusYear = ifelse(is.even(Paracou6_2016$idTree), CensusYear, CensusYear+1)) # different years inventory
BrokenParacou6_2016[1:3, "idTree"] <- "200" # create a 2 time present tree
BrokenParacou6_2016[4, "CircCorr"] <- 6 # DBH < 10
BrokenParacou6_2016[5, "PLOT"] <- "100" # a tree in another plot

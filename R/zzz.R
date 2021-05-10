#' Maria package global options
NULL

.PkgEnv <- new.env()
.onAttach <- function(libname, pkgname){
  packageStartupMessage('Welcome to the ForestLogging package.')
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.Maria <- list(
    Maria.path = "~/R-dev",
    Maria.install.args = "",
    Maria.name = "Vincyane Badouard, Guillaume Salzet, Thomas Gaquiere, Géraldine Derroire and Sylvain Schmitt",
    Maria.desc.author = c('person("Vincyane","Badouard",
         role = c("aut", "cre"),
          "vincyane.badouard@gmail.com"),
  person("Guillaume","Salzet",
         role = "aut",
          "Guillaume.Salzet@ecofog.gf"),
  person("Thomas","Gaquiere",
         role = "aut",
          "gaquiere.thomas@yahoo.com "),
  person("Géraldine","Derroire",
         role = "ctb",
          "geraldine.derroire@cirad.fr"),
  person("Sylvain","Schmitt",
         role = "ctb",
          "sylvain.m.schmitt@gmail.com "'))
    Maria.desc.license = "GPL-3"
}


# LoggingLab package global options
NULL

.PkgEnv <- new.env()
.onAttach <- function(libname, pkgname){
  packageStartupMessage('Welcome to the LoggingLab package.')
}


.onLoad <- function(libname, pkgname) {
  op <- options() #Get the current options from the envmt and store them in op
  op.LoggingLab <- list( # creation of objects (future options)
    # Package options
    LoggingLab.path = "~/R-dev",
    LoggingLab.install.args = "",
    LoggingLab.name = "Vincyane Badouard, Sylvain Schmitt, Guillaume Salzet, Thomas Gaquiere and Geraldine Derroire",
    LoggingLab.desc.author = c('person("Vincyane","Badouard",
         role = c("aut", "cre"),
          "vincyane.badouard@gmail.com"),
  person("Sylvain","Schmitt",
         role = "aut",
          "sylvain.m.schmitt@gmail.com "),
  person("Guillaume","Salzet",
         role = "aut",
          "Guillaume.Salzet@ecofog.gf"),
  person("Thomas","Gaquiere",
         role = "aut",
          "gaquiere.thomas@yahoo.com "),
  person("Geraldine","Derroire",
         role = "aut",
          "geraldine.derroire@cirad.fr"'),
    LoggingLab.desc.license = "GPL-3"
  )

  toset <- !(names(op.LoggingLab) %in% names(op)) #if these objects do not exist in the envmt, put them there -> no conflict,
  if(any(toset)) options(op.LoggingLab[toset]) #but the existing envmt takes precedence -> my homonyms objects are not taken.


  invisible() # send no msg to the user.
}

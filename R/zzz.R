.onLoad <- function(libname, pkgname) {

  topicnames <- paste0("topic", 1:100)
  save(topicnames, file = "R/sysdata.rda")

}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the surveyr package v0.1 (Aug-2019). For more information contact christopher.brownlie@education.gov.uk")
}

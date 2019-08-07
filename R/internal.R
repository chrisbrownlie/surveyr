# function for loading data and getting vector of items included
load_internal_data <- function() {

  eval(quote(internalnames <- load("R/sysdata.rda")), envir = parent.frame())
  load("R/sysdata.rda", envir = parent.frame())

}

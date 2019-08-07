# function for loading data and getting vector of items included
load_internal_data <- function() {

  internalnames <<- load("R/sysdata.rda")
  load("R/sysdata.rda", envir = globalenv())

}

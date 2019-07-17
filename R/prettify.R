#' Function which takes in a formattable and abstracts the use of the formattable package by
#' creating a simple, standardised output
#'
#' @importFrom dplyr %>%
#'
#' @param table formattable object created by common_words function
#' @param alias character vector indicating what the columns should be renamed, must be a named vector or
#' the same length as the number of columns
#' @param count_bar logical indicating whether a bar should be added to the count column to visualise the count
#' @param colour_groups logical indicating whether the groups of the first column should be distinguished by colours
#'
#' @return Newly formatted formattable object
prettify <- function(object,
                     alias = c(""),
                     count_bar = FALSE,
                     colour_groups = FALSE,
                     flatten = FALSE) {

  if (alias != c("")) {
    new_names <- names(object)
    for (i in 1:length(alias)) {
      new_names[new_names == alias[i]] <- names(alias[i])
    }
    names(object) <- new_names
  }

  if (count_bar == TRUE) {
    object <- object %>%
      formattable::formattable(list(`Count` = formattable::color_bar(color = "lightblue", fun = function(x){as.numeric(x)/max(as.numeric(x))})))
  }

  if (colour_groups == TRUE) {
    groups <- object %>%
      dplyr::select(names(object)[1]) %>%
      group_by() %>%
      summarise(group = runif(1, 1, 100000))
    object <- object %>%
      formattable::formattable(list(formattable::area(col=1) ~ lapply(1:nrow(object), function(row) {
        area(row, col = -1) ~ color_tile("lightpink", "lightblue")
      })))
  }

  return(object)
}

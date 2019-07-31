#' Function which takes in a formattable and abstracts the use of advanced elements
#' of the formattable package by creating a simple, standardised formatted output
#'
#' @importFrom dplyr %>%
#'
#' @param object dataframe
#' @param alias character vector indicating what the columns should be renamed, must be a named vector or
#' the same length as the number of columns
#' @param count_bar logical indicating whether a bar should be added to the count column to visualise the count
#' @param colour_groups logical indicating whether the groups of the first column should be distinguished by colours
#'
#' @return Newly formatted formattable object
prettify <- function(object,
                     alias = c(""),
                     align = c("l"),
                     count_bar = FALSE,
                     colour_groups = FALSE) {

  new_object <- object

  if (alias != c("")) {
    new_names <- names(object)
    for (i in 1:length(alias)) {
      new_names[new_names == alias[i]] <- names(alias[i])
    }
    names(new_object) <- new_names
  }

  colours <- c(RColorBrewer::brewer.pal(9, "Pastel1"),
               RColorBrewer::brewer.pal(8, "Pastel2"),
               RColorBrewer::brewer.pal(9, "Set1"),
               RColorBrewer::brewer.pal(8, "Set2"),
               RColorBrewer::brewer.pal(12, "Set3"))

  groups <- new_object %>%
    dplyr::ungroup() %>%
    dplyr::group_by("id" = .[[1]]) %>%
    dplyr::summarise(rows = n())
  groups <- groups %>%
    dplyr::mutate(colour = colours[1:nrow(groups)],
                  cumul = cumsum(rows))

  groups <- data.frame("id" = "NA", "colour" = "NA", rows = 1, cumul = 0,
                       stringsAsFactors = FALSE) %>%
    bind_rows(groups)

  if (count_bar == TRUE & colour_groups == TRUE) {
    colour_reps <- c()
    for (i in 2:nrow(groups)) {
      colour_reps <- c(colour_reps, rep(groups$colour[i], groups$rows[i]))
    }

    formatlist <- c(list(Count = formattable::color_bar(color = "lightblue", fun = function(x) {as.numeric(x)/max(as.numeric(x))})),
                       lapply(2:nrow(groups), function(rownum) {
                         formattable::area(col = 1, row = (groups$cumul[[rownum-1]]+1):groups$cumul[[rownum]]) ~ formattable::color_tile(groups$colour[[rownum]],
                                                                                                                           groups$colour[[rownum]])}))
  } else if (count_bar == TRUE & colour_groups == FALSE) {
    formatlist <- list(Count = formattable::color_bar(color = "lightblue", fun = function(x) {as.numeric(x)/max(as.numeric(x))}))
  } else if (count_bar == FALSE & colour_groups == TRUE) {
    colour_reps <- c()
    for (i in 2:nrow(groups)) {
      colour_reps <- c(colour_reps, rep(groups$colour[i], groups$rows[i]))
    }
    formatlist <- lapply(2:nrow(groups), function(rownum) {
                      formattable::area(col = 1, row = (groups$cumul[[rownum-1]]+1):groups$cumul[[rownum]]) ~ formattable::color_tile(groups$colour[[rownum]],
                                                                                                                                      groups$colour[[rownum]])})
  } else {
    formatlist <- list()
  }

  return_ft <- new_object %>%
    formattable::formattable(align = c(rep("l", which(names(new_object)=="Count"|names(new_object)=="count")-2), "c", "l", "c"),
                                                   formatlist)

  return(return_ft)
}

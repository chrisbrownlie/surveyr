#' Function to tabulate a text column, showing the most commonly used words, optionally broken down by another column(s)
#'
#' @importFrom dplyr %>%
#'
#' @param data dataframe or tibble with a row per survey response
#' @param column name of a character column in the data frame to be tabulated
#' @param by character vector of column(s) to split into groups (must be less than 20 unique groups)
#'
#' @return Original dataframe with text column cleaned and standardised ready for analysing
common_words <- function(data,
                         column,
                         by = c("")) {

  column <- dplyr::enquo(column)



}

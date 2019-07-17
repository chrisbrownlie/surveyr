#' Function to clean single text column, remove punctuation and standardise
#'
#' @importFrom dplyr %>%
#'
#' @param data dataframe or tibble with a row per survey response
#' @param column name of a character column in the data frame to be cleaned
#' @param num logical indicating whether numbers should be removed as part of cleaning process
#'
#' @return Original dataframe with text column cleaned and standardised ready for analysing
clean_column <- function(data,
                         column,
                         num = TRUE,
                         null_response = TRUE) {

  column <- dplyr::enquo(column)

  new_col <- dplyr::pull(data, {{ column }}) %>%
    stringr::str_replace_all(pattern = "[^[:alnum:][:space:]\\[\\]]", replacement = "") %>%
    stringr::str_replace_all(pattern = "[[:space:]]+", replacement = " ") %>%
    tolower()

  if (num == TRUE) {
    new_col <- new_col %>%
      stringr::str_replace_all(pattern = "[[:digit:]]", replacement = "")
  }

  if (null_response == TRUE) {
    new_col <- new_col %>%
      stringr::str_replace_all(pattern = "^no$|^nothing$|^na$", replacement = "")
  }

  data[[dplyr::quo_name(column)]] <- new_col
  return(data)
}

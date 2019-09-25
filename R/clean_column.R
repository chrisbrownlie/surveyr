#' Function to clean single text column, remove punctuation and standardise
#'
#'
#' @param data dataframe or tibble with a row per survey response
#' @param column name of a character column in the data frame to be cleaned
#' @param num logical indicating whether numbers should be removed as part of cleaning process
#' @param null_response logical indicating whether to remove non-answers (e.g. 'no', 'none', 'na' etc.)
#'
#' @return Original dataframe with text column cleaned and standardised ready for analysing
#'
#' @export
clean_column <- function(data,
                         column,
                         num = FALSE,
                         null_response = TRUE) {

  column <- enquo(column)

  new_col <- pull(data, {{ column }}) %>%
    stringr::str_replace_all(pattern = "[^[:alnum:][:space:]\\[\\]\\.\\?]", replacement = "") %>%
    stringr::str_replace_all(pattern = "[[:space:]]+", replacement = " ") %>%
    tolower()

  if (num == TRUE) {
    new_col <- new_col %>%
      stringr::str_replace_all(pattern = "[[:space:]][[:digit:]]+", replacement = "[number]")
  }

  if (null_response == TRUE) {
    new_col <- new_col %>%
      stringr::str_replace_all(pattern = "^(no|nothing|none|na|not really|no thanks|no thank you|n/a)(\\.)?[[:space:]]*$", replacement = "")
  }

  data[[quo_name(column)]] <- new_col
  return(data)
}

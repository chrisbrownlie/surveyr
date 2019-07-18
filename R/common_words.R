#' Function to tabulate a text column, showing the most commonly used words, optionally broken down by another column(s)
#'
#' @importFrom dplyr %>%
#'
#' @param data dataframe or tibble with a row per survey response
#' @param column name of a character column in the data frame to be tabulated
#' @param ... optional column(s) to split into groups (must be less than 20 unique combinations of values between columns).
#' @param n number indicating how many most common words to return for each group. Defaults to 3
#' @param min number indicating the minimum number of times a word needs to appear for it to be included in output, defaults to 5
#' @param stopwords logical indicating whether to remove stopwords or not. Defaults to TRUE
#' @param remove character vector of additional words to remove
#'
#' @return Original dataframe with text column cleaned and standardised ready for analysing
common_words <- function(data,
                         column,
                         ...,
                         n = 3,
                         min = 5,
                         stopwords = TRUE,
                         remove = c("")) {

  column <- dplyr::enquo(column)
  by <- dplyr::enquo(by)

  remove <- c("", remove)

  words <- data %>%
    dplyr::select({{ column }}, ...) %>%
    dplyr::mutate(word = stringr::str_split(string = {{ column }}, pattern = "[[:space:]]")) %>%
    tidyr::unnest() %>%
    dplyr::group_by(..., word) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(..., -count) %>%
    dplyr::filter(!word %in% remove)

    if (stopwords == TRUE) {
      words <- words %>%
        dplyr::filter(!word %in% tm::stopwords("en"))
    }
    c_w <- words %>%
      dplyr::filter(count>min) %>%
      dplyr::slice(1:n) %>%
      dplyr::arrange(..., -count) %>%
      dplyr::rename("Word" = word,
             "Count" = count)


    return(c_w)
}


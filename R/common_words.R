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
                         remove = c(""),
                         proportion = FALSE) {

  column <- dplyr::enquo(column)
  cols <- enquos(...)

  remove <- c("", remove)

  words <- data %>%
    dplyr::mutate(response_id = 1:nrow(data)) %>%
    dplyr::select({{ column }}, !!!cols, response_id) %>%
    tidytext::unnest_tokens(input = {{ column }},
                            output = "word",
                            token = "words") %>%
    dplyr::group_by(!!!cols, word) %>%
    dplyr::summarise(count = n(),
                     responses = n_distinct(response_id)) %>%
    dplyr::arrange(!!!cols, -count) %>%
    dplyr::filter(!word %in% remove)

    if (stopwords == TRUE) {
      words <- words %>%
        dplyr::filter(!word %in% tm::stopwords("en"))
    }
    c_w <- words %>%
      dplyr::filter(count>min) %>%
      dplyr::slice(1:n) %>%
      dplyr::arrange(!!!cols, -count) %>%
      dplyr::rename("Word" = word,
             "Count" = count)

    if (proportion == TRUE) {
      if (length(cols)==0) {
        c_w <- c_w %>%
          dplyr::mutate(Proportion = round(responses/nrow(data), digits = 2)) %>%
          dplyr::select(-responses)
      } else {
        datagroups <- data %>%
          tidyr::unite(group,
                       !!!cols,
                       remove = FALSE) %>%
          dplyr::group_by(group) %>%
          dplyr::summarise(count = n())
        c_w <- c_w %>%
          tidyr::unite(group,
                       !!!cols,
                       remove = FALSE) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(total_responses = datagroups$count[which(datagroups$group == group)],
                        Proportion = round(responses/total_responses,
                                           digits = 2)) %>%
          dplyr::select(-group, -total_responses, -responses)
      }
    } else {
      c_w <- c_w %>%
        dplyr::select(-responses)
    }


    return(c_w)
}


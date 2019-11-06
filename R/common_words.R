#' common_words
#'
#' Tabulate a text column, showing the most commonly used words, optionally broken down by another demographics column(s)
#'
#' @param data dataframe or tibble with a row per survey response
#' @param column name of a character column in the data frame to be tabulated
#' @param ... optional column(s) to use to split into groups
#' @param n number indicating how many most common words to return for each group. Defaults to 3
#' @param min number indicating the minimum number of times a word needs to appear for it to be included in output, defaults to 5
#' @param remove_stopwords logical indicating whether to remove stopwords or not. Defaults to TRUE
#' @param remove character vector of additional words to remove
#' @param proportion logical indicating whether to include the proportion of responses that contained the word
#' @param lemmatise logical indicating whether to use the textstem package to lemmatise the strings before calculating common words
#' @param pretty one of either 'no', 'plot' or 'return'. Defaults to 'no'. 'plot' will end the function call by
#' applying the prettify() function to the output with plot = TRUE. 'return' will apply the prettify() function with plot = FALSE.
#'
#' @return Table of most common words with the number of times they appear in each group
#'
#' @export
common_words <- function(data,
                         column,
                         ...,
                         n = 3,
                         min = 5,
                         remove_stopwords = TRUE,
                         remove = c(""),
                         proportion = FALSE,
                         lemmatise = FALSE,
                         pretty = 'no') {

  # Argument check on 'pretty'
  if (!pretty %in% c('no', 'plot', 'return')) {
    stop("Error: argument 'pretty' must be one of: 'no', 'plot' or 'return'. See documentation for info.")
  }


  column <- enquo(column)
  cols <- enquos(...)

  words <- data %>%
    mutate(response_id = 1:nrow(data)) %>%
    select(col = {{ column }}, !!!cols, response_id)
  if(lemmatise == TRUE) {
    words <- words %>%
      mutate(col = textstem::lemmatize_strings(col))
  }
  words <- words %>%
    tidytext::unnest_tokens(input = col,
                            output = "word",
                            token = "words") %>%
    group_by(!!!cols, word) %>%
    summarise(count = n(),
              responses = n_distinct(response_id)) %>%
    arrange(!!!cols, -count) %>%
    filter(!word %in% remove)

    if (remove_stopwords == TRUE) {
      words <- words %>%
        filter(!word %in% tm::stopwords("en"))
    }
    c_w <- words %>%
      filter(count>min) %>%
      slice(1:n) %>%
      arrange(!!!cols, -count) %>%
      rename("Word" = word,
             "Count" = count)

    if (proportion == TRUE) {
      if (length(cols)==0) {
        c_w <- c_w %>%
          mutate(Proportion = round(responses/nrow(data), digits = 2)) %>%
          select(-responses)
      } else {
        datagroups <- data %>%
          tidyr::unite(group,
                       !!!cols,
                       remove = FALSE) %>%
          group_by(group) %>%
          summarise(count = n())
        c_w <- c_w %>%
          tidyr::unite(group,
                       !!!cols,
                       remove = FALSE) %>%
          rowwise() %>%
          mutate(total_responses = datagroups$count[which(datagroups$group == group)],
                        Proportion = round(responses/total_responses,
                                           digits = 2)) %>%
          select(-group, -total_responses, -responses)
      }
    } else {
      c_w <- c_w %>%
        select(-responses)
     }


      if (pretty == "no") {
        return(c_w)
      } else if (pretty == "plot") {
        c_w %>%
          prettify(plot = TRUE,
                   title = paste0("Most common words in column ", quo_name(column)))
      } else if (pretty == "return") {
        c_w <- c_w %>%
          prettify(plot = FALSE)
        return(c_w)
      }
}



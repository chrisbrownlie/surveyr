#' n_grams
#'
#' Determine the most common n-grams used in a column of text responses, optionally broken down by a demographic column(s).
#'
#' @param data dataframe or tibble with a row per survey response
#' @param column name of a character column in the data frame to be tabulated
#' @param ... optional column(s) to split into groups
#' @param words number indicating what kind of n-grams to return (bigram, trigram...), defaults to 2 (bigrams)
#' @param filter_word optional word to filter results by (i.e. only show n-grams containing this word)
#' @param remove optional vector of words to exclude (i.e. remove all n-grams containing at least one of these words)
#' @param n number of n-grams to show for each group, defaults to 3
#' @param min number indicating the minimum number of times a word needs to appear for it to be included in output, defaults to 3
#' @param stop_thresh numeric indicating the threshold to remove stopwords (i.e. maximum proportion of stopwords to words
#' allowed). 1 includes all n-grams regardless of stop words, 0 excludes all n-grams containing one or more stopwords. Defaults to 0.7.
#' @param proportion logical indicating whether to include the proportion of responses containing this n-gram, defaults to FALSE
#' @param pretty one of either 'no', 'plot' or 'return'. Defaults to 'no'. 'plot' will end the function call by
#' applying the prettify() function to the output with plot = TRUE. 'return' will apply the prettify() function with plot = FALSE.
#'
#' @return Table of n-grams with the number of times they appear
#'
#' @export
n_grams <- function(data,
                    column,
                    ...,
                    words = 2,
                    filter_word = "",
                    remove = c(""),
                    n = 5,
                    min = 3,
                    stop_thresh = 0.7,
                    proportion = FALSE,
                    pretty = 'no') {

  column <- enquo(column)
  cols <- enquos(...)

  if (min == 'auto') {
    min <- 0.01*nrow(data)
  } else if (!is.numeric(min)) {
    stop("Error: 'min' must be set to either 'auto' (3% of the total number of responses) or a number.")
  }

  n_grams <- data %>%
    mutate(response_id = 1:nrow(data)) %>%
    select(col = {{ column }}, !!!cols, response_id) %>%
    tidytext::unnest_tokens(input = col,
                            output = "ngram",
                            token = "ngrams",
                            n = words) %>%
    group_by(!!!cols, ngram) %>%
    summarise(Count=n(),
              responses = n_distinct(response_id)) %>%
    arrange(-Count) %>%
    filter(!is.na(ngram),
           Count >= min) %>%
    rowwise() %>%
    mutate(sw1 = sum(stringr::str_extract_all(ngram, pattern = "[[:alpha:]]+", simplify = TRUE) %in% tm::stopwords("en")),
           prop = sw1/length(stringr::str_extract_all(ngram, pattern = "[[:alpha:]]+", simplify = TRUE))) %>%
    ungroup() %>%
    filter(prop <= stop_thresh) %>%
    select(-sw1, -prop)

  if (remove != c("")) {
    n_grams <- n_grams %>%
      filter(!stringr::str_detect(ngram, pattern = paste0("(", paste(remove, collapse = ")|("), ")")))
  }


  if (filter_word == "") {

    n_grams <- n_grams

  } else {

    n_grams <- n_grams %>%
      filter(stringr::str_detect(ngram, pattern = filter_word))

  }

  if (proportion == TRUE) {
    if (length(cols)==0) {
      n_g <- n_grams %>%
        mutate(Proportion = round(responses/nrow(data), digits = 2)) %>%
        select(-responses)
    } else {
      datagroups <- data %>%
        tidyr::unite(group,
                     !!!cols,
                     remove = FALSE) %>%
        group_by(group) %>%
        summarise(count = n())
      n_g <- n_grams %>%
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
    n_g <- n_grams %>%
      select(-responses)
  }

  output <- n_g %>%
    ungroup() %>%
    group_by(...) %>%
    arrange(-Count) %>%
    slice(1:n)

  if (pretty == "no") {
    return(output)
  } else if (pretty == "plot") {
    output %>%
      prettify(plot = TRUE,
               title = paste0("Most common ", words, "-grams in column ", quo_name(column)))
  } else if (pretty == "return") {
    output <- output %>%
      prettify(plot = FALSE)
    return(c_w)
  }
}

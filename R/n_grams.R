#' Function to tabulate the most commonly appearing n-grams in a text column
#'
#' @importFrom dplyr %>%
#'
#' @param data dataframe or tibble with a row per survey response
#' @param column name of a character column in the data frame to be tabulated
#' @param word optional string indicating how to filter the resulting dataframe (only return ngrams containing 'word')
#' @param n number indicating what kind of n-grams to return (bigram, trigram...)
#' @param min number indicating the minimum number of times a word needs to appear for it to be included in output, defaults to 5
#' @param stop_thresh numeric indicating the threshold to remove stopwords (i.e. maximum proportion of stopwords to words
#' allowed). 1 includes all n-grams regardless of stop words, 0 excludes all n-grams containing one or more stopwords.
#'
#' @return Table of n-grams with the number of times they appear
#'
#' @export
n_grams <- function(data,
                   column,
                   word = "",
                   n = 2,
                   min = 2,
                   stop_thresh = 0.7) {

  column <- dplyr::enquo(column)

  n_grams <- select(data, 1, {{ column }}) %>%
    tidytext::unnest_tokens(input = {{ column }},
                            output = "ngram",
                            token = "ngrams",
                            n = n) %>%
    dplyr::group_by(ngram) %>%
    dplyr::summarise(Count=n()) %>%
    dplyr::arrange(-Count) %>%
    dplyr::filter(Count >= min,
                  !is.na(ngram)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sw1 = sum(stringr::str_extract_all(ngram, pattern = "[[:alpha:]]+", simplify = TRUE) %in% tm::stopwords("en")),
                  prop = sw1/length(stringr::str_extract_all(ngram, pattern = "[[:alpha:]]+", simplify = TRUE))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(prop <= stop_thresh) %>%
    dplyr::select(-sw1, -prop)

  if (word == "") {

    output <- n_grams

  } else {

    output <- n_grams %>%
      dplyr::filter(stringr::str_detect(ngram, pattern = word))

  }

  return(output)
}

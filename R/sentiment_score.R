#' Function to return a sentiment score for each response
#'
#' @importFrom dplyr %>%
#'
#' @param data dataframe or tibble with a row per survey response
#' @param column name of a character column in the data frame which can be used to calculate the sentiment
#' @param output string representing the name of the new sentiment column, defaults to '[column]_sentiment'
#' @param display string denoting whether the sentiment should be represented by: words denoting the sentiment ('simple' - the default) the actual score ('num') or
#' the sentiment of every response should be ranked with a higher rank denoting more positive sentiment i.e. 1 being the most positive ('rank')
#'
#' @return Original dataframe with an additional column denoting the sentiment of the response
sentiment_score <- function(data,
                            column,
                            output = "",
                            display = 'simple') {

  column <- dplyr::enquo(column)

  # Custom tweaks for sentiment analysis
  add_polarity <- data.frame(x = c("ambushed", "tick box", "ticking boxes", "box ticking", "waste of time", "wasteful", "out of order", "felt like", "out of context", "missed opportunity", "accessible", "direct impact","outward looking", "collaborative"),
                             y = c(-1, -1, -1, -1, -2, -1, -2, 0, -0.5, -0.5, 1, 0.5, 0.8, 0.8),
                             stringsAsFactors = FALSE)

  add_valence <- data.frame(x = c("had hoped", "could not be more", "did not", "far from", "always", "lack of", "would be", "could be", "could have", "if", "needs to be better", "not convinced"),
                            y = c("1", "2", "1", "1", "2", "1", "3", "3", "3", "3", "1", "1"),
                            stringsAsFactors = FALSE)

  p_key <- sentimentr::update_polarity_table(key = lexicon::hash_sentiment_jockers_rinker,
                                               drop = c("wasteful", "accessible", "would be", "could have"),
                                               x = add_polarity)
  v_key <- sentimentr::update_valence_shifter_table(key = lexicon::hash_valence_shifters,
                                                      comparison = p_key_2,
                                                      x = add_valence)

  # Get default for output name from column name
  if (output == "") {
    output <- paste0(dplyr::quo_name(column), "_sentiment")
  }


  # Apply sentiment analysis to get a column of sentiments
  sentiments <- data %>%
    dplyr::pull({{ column }}) %>%
    sentimentr::get_sentences() %>%
    sentimentr::sentiment(polarity_dt = p_key,
                          valence_shifters_dt = v_key,
                          n.before = 5,
                          n.after = 8) %>%
    dplyr::group_by(element_id) %>%
    dplyr::summarise(sentiment = round(sentimentr::average_weighted_mixed_sentiment(sentiment,
                                                                                    mixed.less.than.zero.weight = 5),
                                       digits = 2))

  if (display == "simple") {
    sentiments <- sentiments %>%
      dplyr::mutate(sentiment = case_when(
        sentiment < (-0.5) ~ "very negative",
        sentiment < (-0.1) ~ "negative",
        sentiment < 0 ~ "mostly neutral",
        sentiment == 0 ~ "completely neutral",
        sentiment < 0.1 ~ "mostly neutral",
        sentiment < 0.4 ~ "positive",
        TRUE ~ "very positive"
      ))
    sentiments$sentiment <- factor(sentiments$sentiment, levels = c("very negative", "negative", "completely neutral", "mostly neutral", "positive", "very positive"))
  } else if (display == "rank") {
    sentiments <- sentiments %>%
      dplyr::mutate(sentiment = dplyr::min_rank(desc(sentiment)))
  }

  names(sentiments) = c("id", output)

  new_data <- data %>%
    dplyr::bind_cols(sentiments[,2])

  return(new_data)

}

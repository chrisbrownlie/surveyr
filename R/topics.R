#' determine_topics
#'
#' Takes in a dataframe and column and abstracts the use of
#' the topicmodels package by applying an LDA model.
#' Used by other *_topics functions
#'
#' @param dataframe dataframe or tibble of survey responses
#' @param column string variable of free text responses to which the LDA model will be applied
#' @param num_topics integer denoting the number of topics (k) to be specified for the LDA model, defaults to 2
#'
#' @return a dataframe, where each column is a topic and contains the 5 most common words in those topics
determine_topics <- function(dataframe,
                             column,
                             num_topics = 2) {

  if (class(column)[1] != "quosure") {
    column <- enquo(column)
  }

  model_matrix <- dataframe %>%
    mutate(id = seq_along(pull(., 1))) %>%
    select(id, quo_name(column)) %>%
    tidytext::unnest_tokens(output = "word",
                            input = {{ column }},
                            token = "words") %>%
    group_by(id, word) %>%
    summarise(count = n()) %>%
    tidytext::cast_dtm(document = "id",
                       term = "word",
                       value = "count") %>%
    topicmodels::LDA(k = num_topics, control = list(seed = 1066))

  return(model_matrix)
}




#' summarise_topics
#'
#' Function to return to top unique words for each topic by fitting
#' an LDA model
#'
#' @family topics
#'
#' @param dataframe dataframe or tibble of survey responses
#' @param column string variable of free text responses which has a specified LDA model
#' @param num_words integer denoting the number of top words to return for each topic
#' @param exclude character vector of words to exclude from topic modelling
#' @param num_topics integer denoting the number of distinct topics to assume are present. Defaults to 2
#'
#' @return a dataframe, where each column is a topic and contains the most common words in those topics
#'
#' @export
summarise_topics <- function(dataframe,
                             column,
                             num_words = 3,
                             exclude = "",
                             num_topics = 2) {

  beta_matrix <- determine_topics(dataframe = dataframe,
                                  column = enquo(column),
                                  num_topics = num_topics) %>%
    tidytext::tidy(matrix = "beta")

  topic_words_initial <- beta_matrix %>%
    filter(!term %in% tm::stopwords("en")) %>%
    group_by(topic) %>%
    top_n(n = 5*num_words, wt = beta)
  exclude_words <- topic_words_initial %>%
    group_by(term) %>%
    summarise(count = n()) %>%
    filter(count > 1) %>%
    pull(term)
  exclude_words <- c(exclude_words, tolower(exclude))
  final_topic_words <- topic_words_initial %>%
    filter(!term %in% exclude_words) %>%
    top_n(n = num_words, wt = beta) %>%
    mutate(rank = dense_rank(beta)) %>%
    select(topic, term, rank) %>%
    arrange(topic, rank)

  return(final_topic_words)
}


#' classify_topics
#'
#' Function which takes in a dataframe and column and produces another column,
#' denoting which topic each response is most relevant to
#'
#' @family topics
#'
#' @param dataframe dataframe or tibble of survey responses
#' @param column string variable of free text responses to which the determine_topics() function
#' has already been applied in this session
#' @param output name of the new column to be produced, defaults to '{column}_topic'
#' @param topic_aliases named string vector, denoting for each topic (1,2.. etc.) what it is to
#' be renamed. Leave blank to stay as 'topic1', 'topic2' etc.
#' @param num_topics integer denoting the number of distinct topics to assume are present. Defaults to 2
#' @param confidence logical indicating whether to include topic confidence in output (how likely
#' that the classified topic is definitive)
#'
#' @return the original dataframe with an additional column (name specified by output) containing which
#' topic each response in 'column' falls in to, optionally aliased
#'
#' @export
classify_topics <- function(dataframe,
                            column,
                            output = '',
                            topic_aliases = '',
                            num_topics = 2,
                            confidence = FALSE) {

  gamma_matrix <- determine_topics(dataframe = dataframe,
                                   column = enquo(column),
                                   num_topics = num_topics) %>%
    tidytext::tidy(matrix = "gamma") %>%
    group_by(document) %>%
    summarise(topic = topic[which.max(gamma)],
                    confidence = max(gamma)/(max(gamma)+nth(gamma, n=2, order_by = desc(gamma))))

  if (output == '') {
    output <- paste0(quo_name(enquo(column)), "_topic")
  }

  return_df <- dataframe %>%
    mutate(document = as.character(seq_along(pull(., 1)))) %>%
    left_join(gamma_matrix, by = c("document")) %>%
    mutate(confidence = ifelse(!is.na(confidence),
                                paste0(round(confidence*100, digits = 1), "%"),
                                confidence)) %>%
    tidyr::replace_na(list(topic = "none", confidence = "n/a")) %>%
    select(-document)

  if (topic_aliases[1] != "") {
    if (length(topic_aliases)!=length(unique(return_df$topic[return_df$topic!="none"]))) {
      stop(paste0("Error: The argument 'topic_aliases' contains ",  length(topic_aliases), " elements, it must have the same number of elements as there are topics specified by 'num_topics' (", length(unique(return_df$topic[return_df$topic!="none"])), ")."))
    } else {
      if (sum(names(topic_aliases) %in% return_df$topic)!=length(topic_aliases)) {
        stop(paste0("Error: topic_aliases must be a named character vector, which the name of each element corresponding to the number of the topic."))
      } else {
        new_topics <- sapply(return_df$topic, function(x) {if(x %in% names(topic_aliases)) {x <- topic_aliases[names(topic_aliases)==x]} else {x <- "none"}})
        return_df$topic <- new_topics
      }
    }
  }

  names(return_df)[names(return_df)=="topic"] <- output

  if (confidence == FALSE) {
    return_df <- return_df %>%
      select(-confidence)
  }

  return(return_df)
}

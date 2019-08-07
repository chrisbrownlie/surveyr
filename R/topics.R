#' determine_topics()
#'
#' Function which takes in a dataframe and column and abstracts the use of
#' the topicmodels package by applying an LDA model and saving data internally
#' to be used later by other *_topics functions
#'
#' @importFrom dplyr %>%
#'
#' @param dataframe dataframe or tibble of survey responses
#' @param column string variable of free text responses to which the LDA model will be applied
#' @param num_topics integer denoting the number of topics (k) to be specified for the LDA model, defaults to 2
#'
#' @return a dataframe, where each column is a topic and contains the 5 most common words in those topics
#'
#' @export
determine_topics <- function(dataframe,
                             column,
                             num_topics = 2) {

  if (class(column)[1] != "quosure") {
    column <- dplyr::enquo(column)
  }

  load_internal_data()

  model_matrix <- dataframe %>%
    dplyr::mutate(id = 1:nrow(.)) %>%
    dplyr::select(id, dplyr::quo_name(column)) %>%
    tidytext::unnest_tokens(output = "word",
                            input = {{ column }},
                            token = "words") %>%
    dplyr::group_by(id, word) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::filter(!word %in% tm::stopwords("en")) %>%
    tidytext::cast_dtm(document = "id",
                       term = "word",
                       value = "count") %>%
    topicmodels::LDA(k = num_topics, control = list(seed = 1066)) %>%
    tidytext::tidy(matrix = "beta")

  assign(paste0("model_", dplyr::quo_name(column)), value = model_matrix)
  save(list = c(paste0("model_", dplyr::quo_name(column)), internalnames),
       file = "R/sysdata.rda")

}




#' summarise_topics
#'
#' Function to return to top unique words for each topic, running
#' determine_topics if there is no model for that column yet
#'
#' @importFrom dplyr %>%
#'
#' @param dataframe dataframe or tibble of survey responses
#' @param column string variable of free text responses which has a specified LDA model
#' @param num_words integer denoting the number of top words to return for each topic
#'
#' @return a dataframe, where each column is a topic and contains the most common words in those topics
#'
#' @export
summarise_topics <- function(dataframe,
                             column,
                             num_words = 3) {

  load_internal_data()

  if (!paste0("model_", dplyr::quo_name(column <- dplyr::enquo(column))) %in% internalnames) {
    message(paste0("Warning: There is currently no model for column ", dplyr::quo_name(column), ", function determine_topics will be run first with default arguments."))
    determine_topics(dataframe = dataframe,
                     column = dplyr::enquo(column))
    load_internal_data()
  }

  column <- dplyr::enquo(column)

  model_matrix <- get(paste0("model_", dplyr::quo_name(column)))

  topic_words_initial <- model_matrix %>%
    dplyr::mutate(topic = topicnames[topic]) %>%
    dplyr::group_by(topic) %>%
    dplyr::top_n(n = 5*num_words, wt = beta)
  exclude_words <- topic_words_initial %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::filter(count > 1) %>%
    dplyr::pull(term)
  final_topic_words <- topic_words_initial %>%
    dplyr::filter(!term %in% exclude_words) %>%
    dplyr::top_n(n = num_words, wt = beta) %>%
    dplyr::mutate(rank = dplyr::dense_rank(beta)) %>%
    dplyr::select(topic, term, rank) %>%
    dplyr::arrange(topic, rank)

  return(final_topic_words)
}


#' classify_topics()
#'
#' Function which takes in a dataframe and column which has been
#' modelled via determine_topics() and produces another column,
#' saying which topic each response is most relevant to
#'
#' @importFrom dplyr %>%
#'
#' @param dataframe dataframe or tibble of survey responses
#' @param column string variable of free text responses to which the determine_topics() function
#' has already been applied in this session
#' @param output name of the new column to be produced, defaults to '{column}_topic'
#' @param topic_aliases named string vector, denoting for each topic (1,2.. etc.) what it is to
#' be renamed. Leave blank to stay as 'topic1', 'topic2' etc.
#'
#' @return the original dataframe

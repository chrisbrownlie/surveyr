#' determine_themes()
#'
#' Function which takes in a dataframe and column and abstracts the use of
#' the topicmodels package by applying an LDA model and saving data internally
#' to be used later by other *_themes functions
#'
#' @importFrom dplyr %>%
#'
#' @param dataframe dataframe or tibble of survey responses
#' @param column string variable of free text responses to which the LDA model will be applied
#' @param num_topics integer denoting the number of topics (k) to be specified for the LDA model, defaults to 2
#'
#' @return a dataframe, where each column is a theme and contains the 5 most common words in those themes
#'
#' @export
determine_themes <- function(dataframe,
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




#' summarise_themes
#'
#' Function to return to top unique words for each theme, running
#' determine_themes if there is no model for that column yet
#'
#' @importFrom dplyr %>%
#'
#' @param dataframe dataframe or tibble of survey responses
#' @param column string variable of free text responses which has a specified LDA model
#' @param num_words integer denoting the number of top words to return for each theme
#'
#' @return a dataframe, where each column is a theme and contains the most common words in those themes
#'
#' @export
summarise_themes <- function(dataframe,
                             column,
                             num_words = 3) {

  load_internal_data()

  if (!paste0("model_", dplyr::quo_name(column <- dplyr::enquo(column))) %in% internalnames) {
    message(paste0("Warning: There is currently no model for column ", dplyr::quo_name(column), ", function determine_themes will be run first with default arguments."))
    determine_themes(dataframe = dataframe,
                     column = dplyr::enquo(column))
    load_internal_data()
  }

  column <- dplyr::enquo(column)

  model_matrix <- get(paste0("model_", dplyr::quo_name(column)))

  theme_words_initial <- model_matrix %>%
    dplyr::mutate(topic = topicnames[topic]) %>%
    dplyr::group_by(topic) %>%
    dplyr::top_n(n = 5*num_words, wt = beta)
  exclude_words <- theme_words_initial %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::filter(count > 1) %>%
    dplyr::pull(term)
  final_theme_words <- theme_words_initial %>%
    dplyr::filter(!term %in% exclude_words) %>%
    dplyr::top_n(n = num_words, wt = beta) %>%
    dplyr::mutate(rank = dplyr::dense_rank(beta)) %>%
    dplyr::select(topic, term, rank) %>%
    dplyr::arrange(topic, rank)

  return(final_theme_words)
}


#' rename_themes()
#'
#' Function which takes in a dataframe and column and abstracts the use of
#' the topicmodels package by applying an LDA model and showing the most common
#' unique words for each topic
#'
#' @importFrom dplyr %>%
#'
#' @param dataframe dataframe or tibble of survey responses
#' @param column string variable of free text responses to which the LDA model will be applied
#' @param num_topics integer denoting the number of topics (k) to be specified for the LDA model, defaults to 3
#' @param num_words number of top words to show for each theme, defaults to 5
#'
#' @return a dataframe, where each column is a theme and contains the 5 most common words in those themes

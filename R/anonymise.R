#' Function to scan a column of free text responses and remove any
#' obvious references to names
#'
#' @importFrom dplyr %>%
#'
#' @param data dataframe or tibble with a row per survey response
#' @param column name of a character column in the data frame to be anonymised
#' @param add_names character vector of names to anonymise (optional)
#' @param auto logical indicating whether to look for names to anonymise in the column. Defaults to TRUE.
#' @param complete logical indicating whether to anonymise every capitalised word that doesn't start a sentence. Defaults to FALSE.
#' @param identify logical indicating whether anonymised names should be distinguishable from each other by numbers. Defaults to FALSE.
#' @param gender logical indicating whether to remove references to gender pronouns (he/she him/her etc.). Defaults to FALSE.
#'
#' @return Original dataframe with column anonymised
#'
#' @export
anonymise <- function(data,
                      column,
                      add_names = c(""),
                      auto = TRUE,
                      complete = FALSE,
                      identify = FALSE,
                      gender = FALSE) {

  column <- dplyr::enquo(column)

  if (auto == TRUE) {
    fullnames <- unlist(stringr::str_extract_all(dplyr::pull(data, {{ column }}),
                                 pattern = "(?<!^)(?<!\\. )[[:upper:]][[:lower:]]+ [[:upper:]][[:lower:]]+(-[[:upper:]][[:lower:]]+)?"))

    allnames <- unlist(stringr::str_extract_all(dplyr::pull(data, {{ column }}),
                                  pattern = "(?<!^)(?<!\\. )[[:upper:]][[:lower:]]+"))
  } else {
    fullnames <- c()
    allnames <- c()
  }

  fullnames <- c(fullnames, add_names[stringr::str_detect(string = add_names, pattern = "[[:space:]]")])
  fullnames <- fullnames[!is.na(fullnames)]
  allnames <- c(allnames, add_names[add_names!=""])
  allnames <- allnames[!tolower(allnames) %in% tm::stopwords("en")]
  allnames <- allnames[!is.na(allnames)]

  anon_col <- dplyr::pull(data, {{ column }})

  if (length(fullnames)>0|complete == TRUE) {
    if (identify == TRUE) {
      fn_replacement_df <- data.frame(name = fullnames,
                                      replacement = paste0("[name", sample(x = 1:(length(fullnames)),
                                                                             size = length(fullnames),
                                                                             replace = FALSE), "]"),
                                      stringsAsFactors = FALSE)
    } else {
      fn_replacement_df <- data.frame(name = fullnames,
                                      replacement = "[name]",
                                      stringsAsFactors = FALSE)
    }

    for (i in 1:nrow(fn_replacement_df)) {
      anon_col <- stringr::str_replace_all(anon_col,
                                           pattern = fn_replacement_df$name[[i]],
                                           replacement = fn_replacement_df$replacement[[i]])
    }

    if (complete == TRUE) {
      for (string in allnames) {
        anon_col <- stringr::str_replace_all(anon_col, pattern = string, replacement = "{name}")
      }
    }


  } else {

    warning("No names detected in column - no replacements made.")

  }

  if (gender == TRUE) {
    anon_col <- stringr::str_replace_all(anon_col, pattern = " (he|she) ", replacement = " [they] ")
    anon_col <- stringr::str_replace_all(anon_col, pattern = " (his|hers) ", replacement = " [their] ")
    anon_col <- stringr::str_replace_all(anon_col, pattern = " (him|her) ", replacement = " [them] ")
  }

  data[[dplyr::quo_name(column)]] <- anon_col
  return(data)
}

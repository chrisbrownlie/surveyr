#' anonymise
#'
#' Scan a column of free text responses and remove any
#' obvious references to names
#'
#' @family anonymiser functions
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
#' @import dplyr
#' @export
anonymise <- function(data,
                      column,
                      add_names = c(""),
                      auto = TRUE,
                      complete = FALSE,
                      identify = FALSE,
                      gender = FALSE) {

  column <- enquo(column)

  if (auto == TRUE) {
    fullnames <- unlist(stringr::str_extract_all(pull(data, {{ column }}),
                                 pattern = "(?<!^)(?<!\\. )[[:upper:]][[:lower:]]+ [[:upper:]][[:lower:]]+(-[[:upper:]][[:lower:]]+)?"))

    allnames <- unlist(stringr::str_extract_all(pull(data, {{ column }}),
                                  pattern = "(?<!^)(?<!\\. )[[:upper:]][[:lower:]]+"))
  } else {
    fullnames <- c()
    allnames <- c()
  }

  fullnames <- c(fullnames, add_names)
  fullnames <- fullnames[!is.na(fullnames)]
  allnames <- c(allnames, add_names[add_names!=""])
  allnames <- allnames[!tolower(allnames) %in% tm::stopwords("en")]
  allnames <- allnames[!is.na(allnames)]

  anon_col <- pull(data, {{ column }})

    if (identify == TRUE) {
      fn_replacement_df <- data.frame(name = fullnames,
                                      replacement = paste0("[name", sample(x = 1:(length(fullnames)),
                                                                             size = length(fullnames),
                                                                             replace = FALSE), "]"),
                                      stringsAsFactors = FALSE) %>%
        filter(name != "")
    } else {
      fn_replacement_df <- data.frame(name = fullnames,
                                      replacement = "[name]",
                                      stringsAsFactors = FALSE) %>%
        filter(name != "")
    }

    if (nrow(fn_replacement_df) != 0) {
      for (i in 1:nrow(fn_replacement_df)) {
        anon_col <- stringr::str_replace_all(anon_col,
                                             pattern = fn_replacement_df$name[[i]],
                                             replacement = fn_replacement_df$replacement[[i]])
      }
    } else {
      warning(paste0("Warning: No names found in column '", quo_name(column), "' so no replacements made.\n"))
    }

    if (complete == TRUE & length(allnames)>0) {
      for (string in allnames) {
        anon_col <- stringr::str_replace_all(anon_col, pattern = string, replacement = "{name}")
      }
    } else if (complete == TRUE & length(allnames)==0) {
      warning(paste0("Warning: No potential names found in column '", quo_name(column), "' so no replacements were made even though complete = TRUE.\n"))
    }


    if (gender == TRUE) {
      anon_col <- stringr::str_replace_all(anon_col, pattern = " (he|she) ", replacement = " [they] ")
      anon_col <- stringr::str_replace_all(anon_col, pattern = " (his|hers) ", replacement = " [their] ")
      anon_col <- stringr::str_replace_all(anon_col, pattern = " (him|her) ", replacement = " [them] ")
    }


  data[[quo_name(column)]] <- anon_col
  return(data)
}


#' anonymise_all
#'
#' Simplify application of anonymise function to every column in the dataset
#'
#' @family anonymiser functions
#'
#' @param data dataframe or tibble with a row per survey response
#' @param add_names character vector of names to anonymise (optional)
#' @param auto logical indicating whether to look for names to anonymise in the column. Defaults to TRUE.
#' @param complete logical indicating whether to anonymise every capitalised word that doesn't start a sentence. Defaults to FALSE.
#' @param identify logical indicating whether anonymised names should be distinguishable from each other by numbers. Defaults to FALSE.
#' @param gender logical indicating whether to remove references to gender pronouns (he/she him/her etc.). Defaults to FALSE.
#' @param quietly logical indicating whether to suppress warnings for columns that don't contain names
#'
#' @return Original dataframe with all columns anonymised
#'
#' @export
anonymise_all <- function(data,
                          add_names = c(""),
                          auto = TRUE,
                          complete = FALSE,
                          identify = FALSE,
                          gender = FALSE,
                          quietly = FALSE) {


  if (quietly == TRUE) {
    suppressWarnings(
      new_data <- purrr::reduce(.x = names(data),
                                .f = anonymise,
                                .init = data,
                                add_names = add_names,
                                auto = auto,
                                complete = complete,
                                identify = identify,
                                gender = gender)
    )
  } else {
    new_data <- purrr::reduce(.x = names(data),
                              .f = anonymise,
                              .init = data,
                              add_names = add_names,
                              auto = auto,
                              complete = complete,
                              identify = identify,
                              gender = gender)
  }

  return(new_data)
}

#' anonymise_at
#'
#' Simplify application of anonymise function to multiple specified columns in the dataset
#'
#' @family anonymiser functions
#'
#' @param data dataframe or tibble with a row per survey response
#' @param ... columns for the anonymise function to be applied to
#' @param add_names character vector of names to anonymise (optional)
#' @param auto logical indicating whether to look for names to anonymise in the column. Defaults to TRUE.
#' @param complete logical indicating whether to anonymise every capitalised word that doesn't start a sentence. Defaults to FALSE.
#' @param identify logical indicating whether anonymised names should be distinguishable from each other by numbers. Defaults to FALSE.
#' @param gender logical indicating whether to remove references to gender pronouns (he/she him/her etc.). Defaults to FALSE.
#'
#' @return Original dataframe with all columns anonymised
#'
#' @export
anonymise_at <- function(data,
                          ...,
                          add_names = c(""),
                          auto = TRUE,
                          complete = FALSE,
                          identify = FALSE,
                          gender = FALSE) {

  cols <- purrr::map(enquos(...), quo_name) %>%
    unlist()
  new_data <- purrr::reduce(.x = cols,
                            .f = anonymise,
                            .init = data,
                            add_names = add_names,
                            auto = auto,
                            complete = complete,
                            identify = identify,
                            gender = gender)

  return(new_data)
}

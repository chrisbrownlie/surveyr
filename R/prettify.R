#' Function which takes in a dataframe and abstracts the use of
#' the formattable package by creating a colourful standardised table, output as an image
#'
#' @importFrom dplyr %>%
#'
#' @param object dataframe
#' @param alias character vector indicating what the columns should be renamed, must be a named vector or
#' the same length as the number of columns
#' @param title a string denoting the title of the table, defaults to the name of 'object'
#' @param plot logical flag indicating whether to plot the table or simply return the formatted object for
#' subsequent arranging/plotting
#'
#' @return Newly formatted object
#'
#' @export
prettify <- function(object,
                     alias = c(""),
                     title = "",
                     plot = TRUE) {

  obj_name <- quo_name(enquo(object))
  cols <- ncol(object)
  rows <- nrow(object)

  # Checks on alias argument
  named <- length(names(alias))>0
  length <- length(alias)
  matched <- sum(names(alias) %in% names(object))
  unmatched <- names(alias)[!names(alias) %in% names(object)]

  error_flag <- FALSE
  if (named == FALSE & length != cols & alias != c("")) {
    error_flag <- TRUE
    error_string <- paste0("\n'alias' is not a named character vector and has length ", length, " whereas object has ", cols, " columns.")
  } else if (named == TRUE & matched != length(alias)) {
    error_flag <- TRUE
    error_string <- paste0("\nThe names of 'alias' do not all match to columns in 'object'. The following columns are not present in object: '", paste(unmatched, collapse = "', '"), "'.")
  }

  if (error_flag == TRUE) {
    stop(paste0("Error: Argument 'alias' must be a character vector that is either named (with each name corresponding to a column in object), or have the same length as the number of columns in object. The following issue was found: ", error_string))
  } else if (error_flag == FALSE) {
    if (length(alias)==1) {
      if (alias != c("")) {
        new_object <- object
        names(new_object)[names(new_object) %in% names(alias)] <- alias
      } else {
        new_object <- object
      }
    } else {
      if (length(alias)!=cols) {
        new_object <- object
        names(new_object)[names(new_object) %in% names(alias)] <- alias
      } else {
        new_object <- object
        names(new_object) <- alias
      }
    }
  }

  # Checks on title argument
  if (title == "") {
    title <- obj_name
  }

  # Make a table from the object
  out_table <- object %>%
    tableGrob(rows = NULL) %>%
    gtable_add_grob(grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                    t = 2, b = rows + 1, l = 1, r = cols) %>%
    gtable_add_grob(grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                    t = 1, b = 1, l = 1, r = cols) %>%
    gtable_add_rows(
      heights = unit(max(10, round(nchar(title)/10)*2), "mm"),
      pos = 0) %>%
    gtable_add_grob(
      textGrob(paste(strwrap(title, width = 10*(cols+1), simplify = TRUE), collapse = "\n"), gp = gpar(fontsize = 10)),
      t = 1, b = 1, l = 1, r = cols)


  if (plot == TRUE) {
    plot(out_table)
  } else {
    return(out_table)
  }
}


% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prettify.R
\name{prettify}
\alias{prettify}
\title{prettify}
\usage{
prettify(object, alias = c(""), title = "", plot = TRUE)
}
\arguments{
\item{object}{dataframe}

\item{alias}{character vector indicating what the columns should be renamed, must be a named vector or
the same length as the number of columns}

\item{title}{a string denoting the title of the table, defaults to the name of 'object'}

\item{plot}{logical flag indicating whether to plot the table or simply return the formatted object for
subsequent arranging/plotting}
}
\value{
Newly formatted object
}
\description{
Takes in a dataframe and abstracts the use of
the grid packages by creating a clean standardised table, output as an image
}

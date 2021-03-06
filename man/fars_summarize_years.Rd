% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summarise months in FARS data for a specified year}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A numeric or vector with the year(s) used to select the csv files}
}
\value{
This function returns a \code{tbl_df} table with one column per selected year.
}
\description{
Function that reads one or more csv files, converts them into \code{tbl_df} tables, extracts year and month
information from these tables, counts the number of observations per month for each year and stores the result
in a \code{tbl_df} table with one column per year.
}
\details{
The function selects files with names containing one of the year(s) specified by the argument \code{years}.

\code{fars_summarize_years} is built on the \link{fars_read_years} function.
}
\examples{
\dontrun{
fars_summarize_years(2014)
fars_summarize_years(2013:2015)
}

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Map FARS accident locations data for a specified US state and year}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{Number of the state for which to extract data. Only}

\item{year}{Year to be included in the file name}
}
\value{
This function returns a map plot created with \link{map}, displaying
location information for the selected US state and year. The function returns an error it the
\code{state.num} or \code{year} are not valid or if there are no observations for the chosen \code{state.num}
and \code{year}.
}
\description{
Function that plots location data for a US state and year specified by the arguments \code{state.num}
and \code{year}.
}
\details{
\code{fars_map_state} is built on the \link{fars_read} and \link{make_filename} functions.
}
\examples{
\dontrun{
fars_map_state(28,2014)
}

}

setwd(system.file("extdata", package = "farsAJS"))

#' Read FARS file
#'
#' Function that reads a csv file and converts it into a \code{tbl_df} table.
#'
#' @param filename A character string giving the name of the file to read/path to that file
#'
#' @return This function returns the selected file as a tbl_df table, or an error if the
#' file does not exists.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' fars_read("file.csv")
#' fars_read("dir/file.csv")
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Make a filename with a specified year
#'
#' Function that returns a file name as character string containing a year specified by the argument \code{year}.
#' Can be used in combination with \link{fars_read}.
#'
#' @param year Year to be included in the file name
#'
#' @return This function returns a file name as character string
#'
#' @examples
#' make_filename(2014)
#' fars_read(make_filename(2015))
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Read FARS data for a specified year
#'
#' Function that reads one or more csv files, converts them into \code{tbl_df} tables, extracts year and month
#' information from these tables and stores the result in a \code{list} object with one entry per year.
#'
#' The function selects files with names containing one of the year(s) specified by the argument \code{years}.
#'
#' \code{fars_read_years} is built on the \link{fars_read} and \link{make_filename} functions.
#'
#' @param years A numeric or vector with the year(s) used to select the csv files
#'
#' @return This function returns a list with \code{tbl_df} tables containing MONTH and YEAR information for
#' each selected year or NULL if the \code{year} is not valid.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#'
#' @examples
#' fars_read_years(2014)
#' fars_read_years(2013:2015)
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}


#' Summarise months in FARS data for a specified year
#'
#' Function that reads one or more csv files, converts them into \code{tbl_df} tables, extracts year and month
#' information from these tables, counts the number of observations per month for each year and stores the result
#' in a \code{tbl_df} table with one column per year.
#'
#' The function selects files with names containing one of the year(s) specified by the argument \code{years}.
#'
#' \code{fars_summarize_years} is built on the \link{fars_read_years} function.
#'
#' @inheritParams fars_read_years
#'
#' @return This function returns a \code{tbl_df} table with one column per selected year.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr bind_rows
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom magrittr "%>%"
#'
#' @examples
#' fars_summarize_years(2014)
#' fars_summarize_years(2013:2015)
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' Map FARS accident locations data for a specified US state and year
#'
#' Function that plots location data for a US state and year specified by the arguements \code{state.num}
#' and \code{year}.
#'
#' \code{fars_map_state} is built on the \link{fars_read} and \link{make_filename} functions.
#'
#' @inheritParams make_filename
#'
#' @param state.num Number of the state for which to extract data. Only
#'
#' @return This function returns a map plot created with \link{maps::map}, displaying
#' location information for the selected US state and year. The function returns an error it the
#' \code{state.num} or \code{year} are not valid or if there are no observations for the chosen \code{state.num}
#' and \code{year}.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom magrittr "%>%"
#'
#' @examples
#' fars_map_state(28,2014)
#'
#' \dontrun{
#' fars_map_state("12","2014")
#' fars_map_state(43,2013:2015)
#' fars_map_state(14,"2014-01-09")
#' fars_map_state(16, make_filename(2014))
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}

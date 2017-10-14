library(dplyr)

#' Read CSV file
#'
#' This function reads a CSV file specified by the /code{filename} argument and
#' returns a tibble containing the data. If the file does not exist, the function
#' will stop and will return an error
#'
#' @param filename A character string containing the path of the CSV file
#'
#' @return This function returns a tibble rendered from the CSV file
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{accident_2014 <- fars_read("./data/accident_2014.csv.bz2")}
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

#' Create a filename
#'
#' This function creates a filename for the .csv.bz2 file based on the \code{year} parameter. The parameter must be
#' an integer or the function will return an error message.
#'
#' @param year A numerical value containing the year that is being searched
#'
#' @return A character string that can be used as a filename. Format: "accident_<year>.csv.bz2"
#'
#' @examples
#' \dontrun{make_filename(2016)}
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read the month and year data from the CSV file
#'
#' This function uses a vector or list of years and returns a list of data frames containing the month and years for each
#' accident in the corresponding CSV file. It will display a warning and return NULL if the year does not exist
#'
#' @param years A list or vector containing the years to search
#'
#' @return Returns a list of data frames containing MONTH and year columns based on the "accident_<year>.csv.bz2 files
#' Returns NULL if the year does not exist
#'
#' @importFrom dplyr %>% mutate select
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2014, 2015, 2016))
#' fars_read_years(2014:2016)
#' fars_read_years(list(2014, 2015, 2016))
#' }
#'
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

#' Summarize number of accidents per month and year
#'
#' This function calculates the number of accidents per year on a monthly basis. The years are passed as a vector or a list
#'
#' @param years A list or vector containing the years to search
#'
#' @return This function returns a tibble containing the summarizes accident count per year and month.
#' The function will return an error if the year value is not numeric or integer
#'
#' @importFrom dplyr %>% bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{fars_summarize_years(2013:2016)}
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot accident data per state
#'
#' This function will plot the accidents for a selected state and year. If the state number is not a valid integer or numeric,
#' the function will stop with an error message. The function will also return an error if the year is invalid
#'
#' @param state.num A numeric or integer value representing the state in the Fatality Analysis Reporting System
#' @param year A numeric or integer valaue representing the year that needs to be plotted
#'
#' @return Returns a plot of all accidents using the \code{state.num} and \code{year} parameters.
#' Returns an error if the state number or year is invalid.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(25,2014)
#'
#' # Error
#' fars_map_state(111,2014)
#' fars_map_state(25,2016)
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

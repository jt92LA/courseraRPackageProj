#'
#'
#'
#'
#'
#' Read File
#'
#' This is a simple read function that reads in csv file for a given filename.
#'
#' @param filename A character string containing the name of the csv data file to read in.
#'
#' @return This function returns a tibble containing the data from the filename provided.
#'
#' @details This function uses the \code{readr::read_csv()} and \code{dplyr::tbl_df()} functions.
#'
#'

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Get filename from year
#'
#' This function outputs the filename corresponding to a given year.
#'
#' @param year An integer representing the year.
#'
#' @return This function returns a character string containing the filename corresponding to the year input.
#'


make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' List cases by month and year
#'
#' This helper function lists, by year, tibbles featuring the year and month of all cases of given year.
#'
#'@param years A vector with years to be listed.
#'
#'@details This function uses \code{dplyr}'s \code{mutate} and \code{select} functions.
#'
#'@return This function returns a list of tibbles for each valid year provided in the input.
#'


fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file) %>% dplyr::mutate(year = year) %>% dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize count of cases per month by year
#'
#' This function summarizes the counts of cases per month for each year provided in the input.
#'
#' @param years Vector of years to be summarized.
#'
#' @return This function returns a tibble featuring the counts of cases with Month as rows, and Year as columns.
#'
#' @details This function uses \code{tidyr::spread()} and \code{dplyr}'s \code{group_by}, \code{bind_rows}, and \code{summarize} functions.
#'


fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot cases over state map for a given year
#'
#' This function takes a year and state code as arguments and plots the cases over the state map.
#'
#' @param state.num Intger representing the state code of the state to be plotted.
#'
#' @param year Integer representing the year of the cases to be plotted.
#'
#' @return This function returns a map with the cases in given year over the state.
#'
#' @details This function uses the \code{maps::map()} and \code{graphics::points()} functions.
#'




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

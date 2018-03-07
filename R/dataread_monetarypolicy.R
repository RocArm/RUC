#' Data for monetary policy analysis.
#'
#' Downloads selected data series from FRED (FRB St.Louis), with some rather
#' minor organization and cleaning.
#'
#' @param start.date A string with format 'YYYY-MM-DD'.
#' @param quarterly Logical, TRUE averages all data to quarterly frequency.
#' @return A tibble with the data.
#' @export
#' @importFrom magrittr %>%
#' @import dplyr

dataread_monetarypolicy <- function( start.date = '1972-01-01',
                                     quarterly = FALSE){

  # Definitions
  #------------

  fred.names <- c('DPCCRV1Q225SBEA',
                  'DFF',
                  'GDPC1',
                  'GDPPOT',
                  'UNRATE',
                  'NROU')

  my.names <- c('CorePCE',
                'EFFR',
                'GDP',
                'PGDP',
                'U',
                'NAIRU')

  # Get FRED data
  #--------------
  raw.data <- tidyquant::tq_get(fred.names,
                                "economic.data",
                                rom = start.date)

  # Minimal dress up
  names(raw.data) <- c('Variable','Date','Value')
  raw.data$Variable <- factor(raw.data$Variable,
                              levels = fred.names,
                              labels = my.names)

  # Drop NA in EFFR (for non-business days)
  raw.data <-dplyr::filter(raw.data, !is.na(Value))


  # rename for tidying
  return.data <- raw.data

  # Average to quarterly data if requested
  #---------------------------------------
  if (quarterly){
    # Create yearqtr variable
    raw.data$Date.yq <- zoo::as.yearqtr(raw.data$Date,'%Y-%m-%d')

    # Aggregate by quarters
    raw.data <- select(raw.data, -Date) %>%
      group_by(Variable, Date.yq) %>%
      summarise_all(mean) %>%
      dplyr::rename( Date = Date.yq)

    return.data <- raw.data
  }

  return(raw.data)
}

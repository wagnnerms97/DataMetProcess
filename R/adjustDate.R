#' @title
#' Fix the time zone
#'
#' @description
#' Allows you to correct the timezone based on a date column and another time column
#'
#' @param data Data frame containing the data
#' @param col_date Column containing the dates
#' @param col_hour Column containing the time. It must be in the format "hh", "hh:mm", or "hh:mm:ss"; only the hours "hh" will be used for conversion.
#' @param fuso Time zone for correction. Query OlsonNames()
#'
#' @return
#' Data frame with the corrected timezone
#'
#' @export
#'
#' @import tidyr
#' @import dplyr
#' @import lubridate
#' @import rlang
#'
#' @examples
#' address <-
#'  base::system.file("extdata",
#'                     "ex1_inmet.CSV",
#'                     package = "DataMetProcess")
#'
#' df <-
#'   read.table(
#'     address,
#'     h=TRUE,
#'     sep = ";",
#'     dec = ",",
#'     skip = 8,
#'     na.strings = -9999,
#'     check.names = FALSE
#'   )
#'
#' df$Data = as.Date(df$Data,format = "%d/%m/%Y")
#'
#'
#' df <-
#'   adjustDate(df,
#'              colnames(df)[1],
#'              colnames(df)[2],
#'              fuso = "America/Bahia")
#'
#' head(df[1:2])
#'

adjustDate <- function(
    data = NULL,
    col_date = NULL,
    col_hour = NULL,
    fuso = NULL
){
  pad_two_digits <- function(x) {
    x <- as.character(x) # convert string

    if (any(grepl(":", x))) {
      parts <- strsplit(x, ":", fixed = TRUE)
      formatted <- sapply(parts, function(p) {
        paste0(sprintf("%02d", as.integer(p[1])), ":", p[2])
      })
      return(formatted)
    } else {
      return(sprintf("%02d", as.integer(x)))
    }
  }

  # Initialize the variable Date_Hour as NULL
  Date_Hour <- NULL

  # Extract the first two characters (hours) from the col_hour column and overwrite the column in the data frame
  data[col_hour] <- pad_two_digits(data[[col_hour]])
  data[col_hour] <- substr(unlist(data[col_hour], use.names = FALSE), 1, 2)

  # Combine the col_date and col_hour columns into a new column called 'Date_Hour'
  data <- tidyr::unite(data, 'Date_Hour',
                       {{col_date}},
                       {{col_hour}},
                       remove = TRUE, sep = " ")

  # Convert the 'Date_Hour' column to the appropriate datetime format
  data <- dplyr::mutate(
    data,
    Date_Hour = lubridate::as_datetime(
      base::format(
        base::as.POSIXct(
          base::strptime(Date_Hour, "%Y-%m-%d %H"),
          usetz = TRUE,
          tz = "Etc/GMT-0"
        ),
        tz = fuso
      )
    )
  )

  # Return the modified data frame
  return(data)
}

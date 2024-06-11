#' @title
#' Fix the time zone
#'
#' @description
#' Allows you to correct the timezone based on a date column and another time column
#'
#' @param data Data frame containing the data
#' @param col_date Column containing the dates
#' @param col_hour Column containing the time
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
  Date_Hour <- NULL
  #carregando funcao necessarisa
  col_string <- function(
    data = NULL,
    ncol = 1,
    str = NULL,
    usestr = FALSE
  ){
    if(usestr){
      base::unlist(data[str],use.names = F)
    }else{
      base::unlist(data[base::colnames(data)[ncol]],use.names = F)
    }
  }



  #extraindo somente a hora
  new_hour <- base::substr(col_string(data = data,usestr = T,str = col_hour),1,2)
  data <-dplyr::mutate(data,!!rlang::sym(col_hour) :=  new_hour)


  #correção do fuso
  data <-
    tidyr::unite(data,'Date_Hour',
                 dplyr::any_of(col_date),
                 dplyr::any_of(col_hour),
                 remove = T,sep = " ")
  data <-
    dplyr::mutate(
      data,
      Date_Hour = lubridate::as_datetime(
        base::format(
          base::as.POSIXct(
            base::strptime(Date_Hour, "%Y-%m-%d %H"),
            usetz = T,
            tz = "Etc/GMT-0"
          ),
          tz = fuso
        )
      )
    )

  return(data)
}

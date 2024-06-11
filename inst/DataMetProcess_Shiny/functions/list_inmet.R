#' @title
#' List of data available at INMET by year
#'
#' @description
#' Collects the available files for the year and returns a list containing: 1) a table containing the addresses of each file inside the zip for later extraction by the down_inmet() function, 2) another structured table with the information available in the file name (e,g, city, station code, year, date of start and end date) and 3) the address of the zip file.
#'
#' @param year year for download in the INMET database
#' @param filename string containing the path and name of the file with the extension ".zip", if NULL (default) it will be saved in a temporary file
#'
#' @return
#' List containing: 1) a table containing the addresses of each file inside the zip for later extraction by the unzip() function of the utils package, 2) another structured table with the information available in the file name (e,g, city, station code, year, date of start and end date) and 3) the address of the zip file.
#'
#'
#' @export
#'
#' @import tidyr
#' @import dplyr
#' @import utils
#'
#' @examples
#'
#' file.down <- tempfile()
#' file.save <- tempfile()
#'
#' info.inmet <-
#'   DataMetProcess::list_inmet(year="2000",file.down)
#'
#' unzip.file <-
#'   utils::unzip(
#'     zipfile = file.down, #or info.inmet$Saved
#'     exdir = file.save
#'   )
#'
#' unzip.file
#'
#'

list_inmet <- function(
    year=NULL,
    filename = NULL
){

  Name <- Origin <- End.Date <- Start.Date <- delete <- City <- NULL

  if(!base::is.null(filename)){
    temp <- filename
  }

  if(base::exists("temp")){
    base::invisible()
  }else{
    temp <- base::tempfile()
  }

  utils::download.file(base::paste0(
    "https://portal.inmet.gov.br/uploads/dadoshistoricos/",
    year,
    ".zip"
  ),temp)

  df <- utils::unzip(zipfile = temp, list = TRUE)[1]
  suppressWarnings(
  df2 <-
    tidyr::separate(df,Name,
                    c("Ano","Origin"),
                    sep=c("/")
    )
  )
  if(base::all(base::is.na(df2$Origin[4:base::nrow(df2)]))){
    df2$Origin <- df2$Ano
    df2$Ano <- year
  }

  suppressWarnings(
    df2 <-
      tidyr::separate(
        df2,
        Origin,
        c("Origin","Region","State","Id","City","Start.Date","delete","End.Date"),
        sep=c("_")
      )
  )

  df2 <-
    dplyr::filter(df2, !dplyr::row_number() == 1)

  df2 <-
    dplyr::mutate(df2,End.Date = base::as.Date(base::sub(".CSV","",End.Date),"%d-%m-%Y"),
                  Start.Date = base::as.Date(Start.Date,"%d-%m-%Y"))
  df2 <-
    dplyr::select(df2,-delete)
  df2 <-
    dplyr::arrange(df2,City)

  return(base::list(Adresses = df,Details = df2,Saved = temp))
}

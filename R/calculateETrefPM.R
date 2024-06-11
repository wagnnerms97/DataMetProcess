#' @title
#' The FAO Penman–Monteith for calculating daily reference evapotranspiration
#'
#' @description
#' Calculation of daily reference evapotranspiration using the PM method for a dataset stored in a data.frame (Allen et al., 1998).
#'
#' @param data Data frame containing the data
#' @param lat Numeric, latitude in decimals
#' @param alt Numeric, altitude in meters
#' @param alt_an Numeric, anemometer height in meters
#' @param dap Numeric, days after planting for the first column date
#' @param date String with the column name containing date records (R default date: "\%Y-\%m-\%d")
#' @param temp String with the column name containing temperature records in °C
#' @param g Optional, if NULL will be considered as zero. String with the column name containing soil heat flux (MJ/m²/day)
#' @param humid String with the column name containing relative humidity records in \%
#' @param rad String with the column name containing global radiation records in MJ/m²
#' @param press String with the column name containing atmospheric pressure records in hPa
#' @param wind String with the column name containing wind speed records in m/s
#' @param kc Optional, when not NULL the crop evapotranspiration ETc is calculated based on ETref. String with the column name containing crop coefficient (Kc) records
#'
#' @details
#' The FAO Penman–Monteith method:
#'
#' \deqn{ETrefPM = \frac{0.408 \Delta(Rn-G) + \gamma \frac{900}{T+273}u_{2}(e_{s}-e_{a})}{\Delta+\gamma(1+0.34u_{2})}}
#'
#' where: ETref - reference evapotranspiration (mm/dia), delta - slope of the saturated water–vapor-pressure curve (kPA/°C), Rn - net radiation (MJ/m²/dia), G - soil heat flux (MJ/m²/day), y - psychrometric constant (kPA/°C), T - average daily air temperature (°C), u2 -wind speed at 2m height (m/s), es - saturation vapor pressure (kPa), e ea - actual vapor pressure (kPa)
#'
#' @references Allen, R.G., Pereira, L.S., Raes, D., Smith, M., 1998. Crop evapotranspiration – guidelines for computing crop water requirements – FAO Irrigation and Drainage Paper 56. FAO, 1998. ISBN 92-5-104219-5.
#'
#' @return
#' Data frame with:
#' date;
#' etref - reference evapotranspiration (mm/dia);
#' dj - julian day;
#' dap - days after planting;
#' es - saturation vapor pressure (kPa);
#' ea - actual vapor pressure (kPa);
#' delta - slope of the saturated water–vapor-pressure curve (kPA/°C);
#' y - psychrometric constant (kPA/°C);
#' rn - net radiation (MJ/m²/dia);
#' etc - crop evapotranspiration (mm/dia) (depends on supply of Kc)
#'
#'
#' @export
#'
#'
#' @examples
#' address <-
#'  base::system.file("extdata",
#'                     "ex2_daily.CSV",
#'                     package = "DataMetProcess")
#'
#' df <- read.table(
#' address,
#' h = TRUE,
#' sep = ";"
#' )
#'
#' #converting to Mj/m
#' df$radiacao_global_kj_m <- df$radiacao_global_kj_m/1000
#' colnames(df)[3] <- "radiacao_global_mj_m"
#'
#' df.Eto <-
#'   calculateETrefPM(
#'     data = df,
#'     lat = -21.980353,
#'     alt = 859.29,
#'     alt_an = 10,
#'     dap = 1,
#'     date = colnames(df)[1],
#'     temp = colnames(df)[7],
#'     g = NULL,
#'     humid = colnames(df)[15],
#'     rad = colnames(df)[3],
#'     press = colnames(df)[4],
#'     wind = colnames(df)[18],
#'     kc = NULL
#'   )
#'
#'


calculateETrefPM <- function(data = NULL,
                             lat = NULL,
                             alt = NULL,
                             alt_an = NULL,
                             dap = 1,
                             date = NULL,
                             temp = NULL,
                             g = NULL,
                             humid = NULL,
                             rad = NULL,
                             press = NULL,
                             wind = NULL,
                             kc = NULL

){
  # Define a helper function to extract a column from the data frame
  col_string <- function(
    data = NULL,
    ncol = 1,
    str = NULL,
    usestr = FALSE
  ){
    if(usestr){
      base::unlist(data[str], use.names = FALSE)
    }else{
      base::unlist(data[base::colnames(data)[ncol]], use.names = FALSE)
    }
  }

  # If g (soil heat flux density) is not provided, set it to 0
  if(length(g) == 0){
    data$g <- 0
    g <- "g"
  }

  # Store the initial number of columns in the data frame
  ncolbk <- base::ncol(data)

  # Calculate the day of the year (dj) from the date column
  data$dj <- base::as.numeric(
    base::format(
      base::as.Date(col_string(data, str = date, usestr = TRUE)), "%j"
    )
  )

  # Generate a sequence for dap (Day After Planting) starting from the provided dap
  data$dap <- seq(from = dap, length.out = nrow(data), by = 1)

  # Calculate es (saturation vapor pressure)
  data$es <- 0.611 * 10^((7.5 * col_string(data, str = temp, usestr = TRUE)) /
                           (237.3 + col_string(data, str = temp, usestr = TRUE)))

  # Calculate delta (slope of the vapor pressure curve)
  data$delta <- (4098 * col_string(data, ncolbk + 3)) /
    ((237.3 + col_string(data, str = temp, usestr = TRUE))^2)

  # Calculate ea (actual vapor pressure)
  data$ea <-
    col_string(data, str = humid, usestr = TRUE) / 100 * col_string(data, ncolbk + 3)

  # Convert pressure to kPa
  data$p_kpa <- col_string(data, str = press, usestr = TRUE) / 10

  # Calculate psychrometric constant (gamma)
  data$y <- 0.665 * (10^(-3)) * col_string(data, ncolbk + 6)

  # Convert wind speed at 2 meters height
  data$u2 <-
    col_string(data, 5) * 4.87 / (base::log(67.8 * alt_an - 5.42))

  # Calculate dr (inverse relative distance Earth-Sun)
  data$dr <-
    1 + 0.033 * base::cos(2 * pi * col_string(data, ncolbk + 1) / 365)

  # Calculate delta_rad (solar declination)
  data$delta_rad <-
    0.409 * base::sin((2 * pi * col_string(data, ncolbk + 1) / 365) - 1.39)

  # Calculate sunset hour angle (h)
  data$h <-
    base::acos(-(base::tan(lat * (pi / 180)) *
                   base::tan(col_string(data, ncolbk + 10))))

  # Calculate extraterrestrial radiation (qo)
  data$qo <-
    (24 * (60) / pi) * 0.082 * col_string(data, ncolbk + 9) *
    (col_string(data, ncolbk + 11) *
       base::sin(lat * (pi / 180)) * base::sin(col_string(data, ncolbk + 10)) +
       base::cos(lat * (pi / 180)) * base::cos(col_string(data, ncolbk + 10)) *
       base::sin(col_string(data, ncolbk + 11)))

  # Calculate clear sky radiation (rgo)
  data$rgo <-
    (0.75 + 2 * 10^-5 * alt) * col_string(data, ncolbk + 12)

  # Calculate net shortwave radiation (boc)
  data$boc <- col_string(data, str = rad, usestr = TRUE) * (1 - 0.23)

  # Calculate net longwave radiation (bol)
  data$bol <-
    4.903 * 10^-9 * (col_string(data, str = temp, usestr = TRUE) + 273.15)^4 *
    (0.34 - 0.14 * base::sqrt(col_string(data, ncolbk + 5))) *
    (1.35 * (col_string(data, str = rad, usestr = TRUE) /
               col_string(data, ncolbk + 13)) - 0.35)

  # Calculate net radiation (rn)
  data$rn <-
    col_string(data, ncolbk + 14) -
    col_string(data, ncolbk + 15)

  # Calculate reference evapotranspiration (etref) using the Penman-Monteith equation
  data$etref <-
    ((0.408 * col_string(data, ncolbk + 4) *
        (col_string(data, ncolbk + 16) - col_string(data, str = g, usestr = TRUE))) +
       ((col_string(data, ncolbk + 7) * (900 /
                                           (col_string(data, str = temp, usestr = TRUE) + 273))) *
          (col_string(data, ncolbk + 8) *
             (col_string(data, ncolbk + 3) -
                col_string(data, ncolbk + 5))))) /
    (col_string(data, ncolbk + 4) +
       col_string(data, ncolbk + 7) * (1 + 0.34 *
                                         col_string(data, ncolbk + 8)))

  # Calculate crop evapotranspiration (etc) if kc is provided
  if(length(kc) != 0){
    if(kc != ""){
      data$etc <- data$etref * col_string(data, str = kc, usestr = TRUE)
    }
  }

  # Identify the index of the date column
  ndate <- which(colnames(data) == date)

  # Reorder and return the final data frame
  data <- data[c(ndate, (ncolbk + 17):base::ncol(data), (ncolbk + 1):(ncolbk + 3), (ncolbk + 5), (ncolbk + 4), (ncolbk + 7), (ncolbk + 16))]

  return(data)
}

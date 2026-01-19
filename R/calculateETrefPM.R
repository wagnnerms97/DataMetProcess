#' @title
#' FAO Penman-Monteith method for daily reference evapotranspiration
#'
#' @description
#' Calculation of daily reference evapotranspiration using the FAO-56
#' Penman-Monteith method for a dataset stored in a data.frame.
#'
#' @param data Data frame containing the data
#' @param lat Numeric, latitude in decimal degrees
#' @param alt Numeric, altitude in meters
#' @param za Numeric, anemometer height in meters
#' @param DAP Numeric, days after planting for the first date
#' @param date String with the column name containing date records
#' @param Ta Optional. String with the column name containing mean air temperature (degC)
#' @param Tmin Optional. String with the column name containing minimum air temperature (degC)
#' @param Tmax Optional. String with the column name containing maximum air temperature (degC)
#' @param RH String with the column name containing mean relative humidity (percent)
#' @param RHmin Optional. String with the column name containing minimum relative humidity (percent)
#' @param RHmax Optional. String with the column name containing maximum relative humidity (percent)
#' @param Rg String with the column name containing global radiation (MJ/m2/day)
#' @param AP String with the column name containing atmospheric pressure (hPa)
#' @param WS String with the column name containing wind speed (m/s)
#' @param G Optional. If NULL, soil heat flux is assumed to be zero (MJ/m2/day)
#' @param Kc Optional. Crop coefficient column name
#'
#' @export
calculateETrefPM <- function(
    data,
    lat,
    alt,
    za,
    DAP = 1,
    date,
    Ta = NULL,
    Tmin = NULL,
    Tmax = NULL,
    RH = NULL,
    RHmin = NULL,
    RHmax = NULL,
    Rg,
    AP,
    WS,
    G = NULL,
    Kc = NULL
){

  # Helper to extract columns
  get_col <- function(x) base::unlist(data[[x]], use.names = FALSE)

  # Soil Heat Flux (G)
  if (is.null(G)) {
    G_val <- rep(0, nrow(data))
  } else {
    G_val <- get_col(G)
  }

  # Dates and time constants
  data$date <- as.Date(get_col(date))
  data$JD   <- as.numeric(format(data$date, "%j"))
  data$DAP  <- seq(from = DAP, length.out = nrow(data))

  # Unit conversion
  Rs   <- get_col(Rg)
  PkPa <- get_col(AP) / 10  # hPa to kPa
  uz   <- get_col(WS)

  # --- 1. Temperature and Saturation Vapor Pressure (es) ---
  if (!is.null(Tmin) && !is.null(Tmax)) {
    Tmin_v <- get_col(Tmin)
    Tmax_v <- get_col(Tmax)
    Tmean  <- (Tmin_v + Tmax_v) / 2

    es_Tmin <- 0.6108 * exp((17.27 * Tmin_v) / (Tmin_v + 237.3))
    es_Tmax <- 0.6108 * exp((17.27 * Tmax_v) / (Tmax_v + 237.3))
    es <- (es_Tmin + es_Tmax) / 2
  } else if (!is.null(Ta)) {
    Tmean <- get_col(Ta)
    es <- 0.6108 * exp((17.27 * Tmean) / (Tmean + 237.3))
  } else {
    stop("Provide Ta or both Tmin and Tmax.")
  }

  # --- 2. Actual Vapor Pressure (ea) ---
  if (!is.null(RHmin) && !is.null(RHmax) && !is.null(Tmin) && !is.null(Tmax)) {
    ea <- (es_Tmin * get_col(RHmax) / 100 +
             es_Tmax * get_col(RHmin) / 100) / 2
  } else if (!is.null(RH)) {
    ea <- es * get_col(RH) / 100
  } else {
    stop("Provide RH or RHmin/RHmax (requires Tmin/Tmax).")
  }

  # --- 3. Delta and Gamma ---
  # Delta calculated on Tmean (FAO-56 Eq. 13)
  es_Delta <- 0.6108 * exp((17.27 * Tmean) / (Tmean + 237.3))
  Delta    <- (4098 * es_Delta) / (Tmean + 237.3)^2
  gamma    <- 0.000665 * PkPa

  # --- 4. Wind speed at 2m ---
  u2 <- uz * 4.87 / log(67.8 * za - 5.42)

  # --- 5. Extraterrestrial Radiation (Ra) ---
  dr <- 1 + 0.033 * cos(2 * pi * data$JD / 365)
  delta_rad <- 0.409 * sin(2 * pi * data$JD / 365 - 1.39)
  lat_rad   <- lat * pi / 180

  # Sunset hour angle
  ws <- acos(-tan(lat_rad) * tan(delta_rad))

  Ra <- (24 * 60 / pi) * 0.082 * dr * (ws * sin(lat_rad) * sin(delta_rad) +
                                         cos(lat_rad) * cos(delta_rad) * sin(ws))

  # --- 6. Net Radiation (Rn) ---
  Rso <- (0.75 + 2e-5 * alt) * Ra
  Rns <- (1 - 0.23) * Rs # Default Albedo = 0.23

  # Solar radiation ratio (limited to 1.0)
  ratio_Rs_Rso <- Rs / Rso
  ratio_Rs_Rso[ratio_Rs_Rso > 1] <- 1
  ratio_Rs_Rso[ratio_Rs_Rso < 0.33] <- 0.33

  fcloud <- (1.35 * ratio_Rs_Rso - 0.35)

  if (!is.null(Tmin) && !is.null(Tmax)) {
    Rnl <- 4.903e-9 * (((Tmax_v + 273.15)^4 + (Tmin_v + 273.15)^4) / 2) * (0.34 - 0.14 * sqrt(ea)) * fcloud
  } else {
    Rnl <- 4.903e-9 * (Tmean + 273.15)^4 * (0.34 - 0.14 * sqrt(ea)) * fcloud
  }

  Rn <- Rns - Rnl

  # --- 7. Reference Evapotranspiration (ET0) ---
  ET0 <- (
    0.408 * Delta * (Rn - G_val) +
      gamma * (900 / (Tmean + 273)) * u2 * (es - ea)
  ) / (
    Delta + gamma * (1 + 0.34 * u2)
  )

  # Output organization
  data_return <- data.frame(Date = data$date)
  data_return$es    <- es
  data_return$ea    <- ea
  data_return$Delta <- Delta
  data_return$gamma <- gamma
  data_return$Rn    <- Rn
  data_return$u2    <- u2
  data_return$ET0   <- ET0

  if (!is.null(Kc)) {
    data_return$ETc <- ET0 * get_col(Kc)
  }

  return(data_return)
}

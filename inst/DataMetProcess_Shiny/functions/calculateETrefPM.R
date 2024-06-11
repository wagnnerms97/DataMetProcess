#' @title
#' The FAO Penman–Monteith for calculating daily reference evapotranspiration
#'
#' @description
#' Calculation of daily reference evapotranspiration using the PM method for a dataset stored in a data.frame (Allen et al., 1998).
#'
#' @param data Data frame containing the data
#' @param Lat Numeric, latitude in decimals
#' @param Alt Numeric, altitude in meters
#' @param Alt_an Numeric, anemometer height in meters
#' @param DAP Numeric, days after planting for the first column date
#' @param Date String with the column name containing date records (R default date: "\%Y-\%m-\%d")
#' @param Temp String with the column name containing temperature records in °C
#' @param G Optional, if NULL will be considered as zero. String with the column name containing soil heat flux (MJ/m²/day)
#' @param Humid String with the column name containing relative humidity records in \%
#' @param Rad String with the column name containing global radiation records in MJ/m²
#' @param Press String with the column name containing atmospheric pressure records in hPa
#' @param Wind String with the column name containing wind speed records in m/s
#' @param Kc Optional, when not NULL the crop evapotranspiration ETc is calculated based on ETref. String with the column name containing crop coefficient (Kc) records
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
#' Date;
#' ETref - reference evapotranspiration (mm/dia);
#' LLI - irrigation level (mm/dia);
#' DJ - julian day;
#' DAP - days after planting;
#' es - saturation vapor pressure (kPa);
#' ea - actual vapor pressure (kPa);
#' delta - slope of the saturated water–vapor-pressure curve (kPA/°C);
#' y - psychrometric constant (kPA/°C);
#' Rn - net radiation (MJ/m²/dia);
#' ETc - crop evapotranspiration (mm/dia) (depends on supply of Kc)
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
#'     Lat = -21.980353,
#'     Alt = 859.29,
#'     Alt_an = 10,
#'     DAP = 1,
#'     Date = colnames(df)[1],
#'     Temp = colnames(df)[7],
#'     G = NULL,
#'     Humid = colnames(df)[15],
#'     Rad = colnames(df)[3],
#'     Press = colnames(df)[4],
#'     Wind = colnames(df)[18],
#'     Kc = NULL
#'   )
#'
#'


calculateETrefPM <- function(data = NULL,
                             Lat = NULL,
                             Alt = NULL,
                             Alt_an = NULL,
                             DAP = 1,
                             Date = NULL,
                             Temp = NULL,
                             G = NULL,
                             Humid = NULL,
                             Rad = NULL,
                             Press = NULL,
                             Wind = NULL,
                             Kc = NULL

){
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

  if(length(G) == 0){
    data$G <- 0
    G <- "G"
  }

  ncolbk <- base::ncol(data)

  data$DJ <- base::as.numeric(
    base::format(
      base::as.Date(col_string(data,str = Date,usestr = T)),"%j"
    )
  )

  data$DAP <- seq(from = DAP, length.out = nrow(data),by=1)

  data$es <- 0.611*10^((7.5*col_string(data,str = Temp,usestr = T))/ #temperatura
                         (237.3+col_string(data,str = Temp,usestr = T))) #temperatura

  data$delta <- (4098*col_string(data,ncolbk+3))/ #es
    ((237.3+col_string(data,str = Temp,usestr = T))^2) #temperatura


  data$ea <-
    col_string(data,str = Humid,usestr = T)/ #umidade
    100*col_string(data,ncolbk+3) #es

  data$P_Kpa <- col_string(data,str = Press,usestr = T)/10 #pressao

  data$y <- 0.665*(10^(-3))*col_string(data,ncolbk+6) #pressao kpa

  data$u2 <-
    col_string(data,5)*4.87/ #vento
    (base::log(67.8*Alt_an-5.42))

  data$dr <-
    1+0.033*base::cos(2*pi*col_string(data,ncolbk+1)/365) #DJ

  data$delta_rad <-
    0.409*base::sin((2*pi*col_string(data,ncolbk+1)/365)-1.39) #DJ

  data$H <-
    base::acos(-(base::tan(Lat*(pi/180))*
                   base::tan(col_string(data,ncolbk+10))))

  data$Qo <-
    (24*(60)/pi)*0.082*col_string(data,ncolbk+9)* #dr
    (col_string(data,ncolbk+11)* #H
       base::sin(Lat*(pi/180))*base::sin(col_string(data,ncolbk+10))+ #delta radi
       base::cos(Lat*(pi/180))*base::cos(col_string(data,ncolbk+10))*
       base::sin(col_string(data,ncolbk+11))) #H

  data$Rgo <-
    (0.75+2*10^-5*Alt)*
    col_string(data,ncolbk+12)#Qo

  data$BOC <- col_string(data,str = Rad,usestr = T)*(1-0.23) #rad

  data$BOL <-
    4.903*10^-9*(col_string(data,str = Temp,usestr = T)+273.15)^4* #temp
    (0.34-0.14*base::sqrt(col_string(data,ncolbk+5)))* #ea
    (1.35*(col_string(data,str = Rad,usestr = T)/ #radiacao
             col_string(data,ncolbk+13))-0.35)

  data$Rn <-
    col_string(data,ncolbk+14) -
    col_string(data,ncolbk+15)

  #calculo ETref-----------------------------------------
  data$ETref <-
    ((0.408*col_string(data,ncolbk+4)* #delta
        (col_string(data,ncolbk+16)-col_string(data,str = G,usestr = T)))+ #rn
       ((col_string(data,ncolbk+7)*(900/#y
                                      (col_string(data,str = Temp,usestr = T)+273)))*#temp
          (col_string(data,ncolbk+8)*#u2
             (col_string(data,ncolbk+3)- #es
                col_string(data,ncolbk+5)))))/ #ea
    (col_string(data,ncolbk+4)+ #delta
       col_string(data,ncolbk+7)*(1+0.34* #y
                                    col_string(data,ncolbk+8))) #u2

  if(length(Kc) != 0){
    if(Kc != ""){
    data$ETc <- data$ETref * col_string(data,str = Kc,usestr = T)
    }
    #data$LLI <- data$ETc-0.75*col_string(data,str = Prec,usestr = T)
  }

  #data$LLI <- base::ifelse(data$LLI < 0,0, data$LLI)


  ndate <- which(colnames(data) == Date)

  data <- data[c(ndate,(ncolbk+17):base::ncol(data),(ncolbk+1):(ncolbk+3),(ncolbk+5),(ncolbk+4),(ncolbk+7),(ncolbk+16))]

  return(data)
}

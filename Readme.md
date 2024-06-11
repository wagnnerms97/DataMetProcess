DataMetProcess
================
Wagner Martins dos Santos

- [Collecting packet information with
  `list_inmet()`](#collecting-packet-information-with-list_inmet)
- [Time zone correction with
  `adjustDate()`](#time-zone-correction-with-adjustdate)
- [Calculation of daily, monthly and annual scales with
  `calculateDMY()`](#calculation-of-daily-monthly-and-annual-scales-with-calculatedmy)
  - [Daily](#daily)
  - [Monthly](#monthly)
  - [Yearly](#yearly)
- [Reference evapotranspiration with
  `calculateETrefPM()`](#reference-evapotranspiration-with-calculateetrefpm)

``` r
library(DataMetProcess)
```

# Collecting packet information with `list_inmet()`

First, we add a pathway for the file to be downloaded and converted into
useful information. The file is retained, and its content can be
extracted later. For this example, we will use a temporary file that
doesn’t have much influence at this stage.

``` r
file.down <- tempfile()

info.inmet <- DataMetProcess::list_inmet(
  year="2000",
  filename = file.down
)

head(info.inmet)
#> $Adresses
#>                                                                  Name
#> 1                                                               2000/
#> 2          2000/INMET_CO_DF_A001_BRASILIA_07-05-2000_A_31-12-2000.CSV
#> 3          2000/INMET_NE_BA_A401_SALVADOR_13-05-2000_A_31-12-2000.CSV
#> 4             2000/INMET_N_AM_A101_MANAUS_09-05-2000_A_31-12-2000.CSV
#> 5 2000/INMET_SE_RJ_A601_ECOLOGIA AGRICOLA_07-05-2000_A_31-12-2000.CSV
#> 6       2000/INMET_S_RS_A801_PORTO ALEGRE_22-09-2000_A_31-12-2000.CSV
#> 
#> $Details
#>    Ano Origin Region State   Id              City Start.Date   End.Date
#> 1 2000  INMET     CO    DF A001          BRASILIA 2000-05-07 2000-12-31
#> 2 2000  INMET     SE    RJ A601 ECOLOGIA AGRICOLA 2000-05-07 2000-12-31
#> 3 2000  INMET      N    AM A101            MANAUS 2000-05-09 2000-12-31
#> 4 2000  INMET      S    RS A801      PORTO ALEGRE 2000-09-22 2000-12-31
#> 5 2000  INMET     NE    BA A401          SALVADOR 2000-05-13 2000-12-31
#> 
#> $Saved
#> [1] "C:\\Users\\wagne\\AppData\\Local\\Temp\\RtmpshGqaK\\file6e047b3b5c46"
```

Now we have an object containing a list of files in `$Addresses` within
the path shown in `$Saved`, and a structured table with information
extracted from the file names.

Having done that, now we can make use of a very useful function from the
`utils` package, called `unzip()`. At this point, we can also use the
files parameter of the `unzip()` function to extract only the files of
interest. Please refer to ?utils::unzip for more details.

``` r

file.save <- tempfile()

unzip.file <-
   utils::unzip(
     zipfile = file.down, #or info.inmet$Saved
     exdir = file.save
   )

#specific file
unzip.file <-
   utils::unzip(
     zipfile = file.down, #or info.inmet$Saved
     files = info.inmet$Adresses[2,],
     exdir = file.save
   )
```

# Time zone correction with `adjustDate()`

To perform the time zone correction, we can use the `adjustDate()`
function. To do this, we will use an example file provided with the
package, At this point, we could indeed use a file obtained in the
previous topic. You can check the available time zones by using
`OlsonNames()`.

``` r
address <-
 base::system.file("extdata",
                    "ex1_inmet.CSV",
                    package = "DataMetProcess")

df <-
  read.table(
    address,
    h=TRUE,
    sep = ";",
    dec = ",",
    skip = 8, 
    na.strings = -9999,
    check.names = FALSE
  ) #see ?read.table for more details...

#Converting to R standard (when necessary)
df$Data = as.Date(df$Data,format = "%d/%m/%Y")

head(df[1:3]) #We are only viewing a part of it.
#>         Data  Hora precipita_o_total_hor_rio_mm
#> 1 2008-01-01 00:00                            0
#> 2 2008-01-01 01:00                            0
#> 3 2008-01-01 02:00                            0
#> 4 2008-01-01 03:00                            0
#> 5 2008-01-01 04:00                            0
#> 6 2008-01-01 05:00                            0
```

``` r

df <-
  adjustDate(df,
             colnames(df)[1],
             colnames(df)[2],
             fuso = "America/Bahia")

#date and time are now in a single column
head(df[1:2]) #We are only viewing a part of it.
#>             Date_Hour precipita_o_total_hor_rio_mm
#> 1 2007-12-31 21:00:00                            0
#> 2 2007-12-31 22:00:00                            0
#> 3 2007-12-31 23:00:00                            0
#> 4 2008-01-01 00:00:00                            0
#> 5 2008-01-01 01:00:00                            0
#> 6 2008-01-01 02:00:00                            0
```

# Calculation of daily, monthly and annual scales with `calculateDMY()`

We can then calculate daily, monthly, and yearly data using the
`calculateDMY()` function. First, we adjust the data_hora column defined
by the previous function to ensure there are no differences between the
same dates. Then, we populate the parameters with the column names in
string format (“string”) and define the type as
`"Daily," "Monthly," or "Yearly"`.

``` r
df.new <- df
df.new$Data_Hora <- as.Date(df$Date_Hour)
```

### Daily

``` r
df.daily <-
  calculateDMY(
    data = df.new,
    col_date = colnames(df)[c(1)],
    col_sum = colnames(df)[c(2,6)], #simplest way to pass column names as string
    col_mean = colnames(df)[-c(1,2,6)], #remove the previous steps in the parameter above
    type = "Daily"
  )

head(df.daily[1:2]) #We are only viewing a part of it.
#> # A tibble: 6 × 2
#>   Date                precipita_o_total_hor_rio_mm
#>   <dttm>                                     <dbl>
#> 1 2007-12-31 21:00:00                            0
#> 2 2007-12-31 22:00:00                            0
#> 3 2007-12-31 23:00:00                            0
#> 4 2008-01-01 00:00:00                            0
#> 5 2008-01-01 01:00:00                            0
#> 6 2008-01-01 02:00:00                            0
```

### Monthly

We use the processed `df.daily` file from the previous topic.

``` r
df.monthly <-
  calculateDMY(
    data = df.daily,
    col_date = colnames(df.daily)[c(1)],
    col_sum = colnames(df.daily)[c(2)],
    col_mean = colnames(df.daily)[-c(1,2)],
    type = "Monthly"
  )

head(df.monthly[1:2]) #We are only viewing a part of it.
#> # A tibble: 6 × 2
#>   Date       precipita_o_total_hor_rio_mm
#>   <date>                            <dbl>
#> 1 2007-12-01                          0  
#> 2 2008-01-01                        183. 
#> 3 2008-02-01                        191. 
#> 4 2008-03-01                        174. 
#> 5 2008-04-01                         98.8
#> 6 2008-05-01                         31.6
```

### Yearly

``` r
df.yearly <-
  calculateDMY(
    data = df.monthly,
    col_date = colnames(df.monthly)[c(1)],
    col_sum = colnames(df.monthly)[c(2)],
    col_mean = colnames(df.monthly)[-c(1,2)],
    type = "Yearly"
  )

head(df.yearly[1:2]) #We are only viewing a part of it.
#> # A tibble: 2 × 2
#>    Date precipita_o_total_hor_rio_mm
#>   <dbl>                        <dbl>
#> 1  2007                           0 
#> 2  2008                        1105.
```

# Reference evapotranspiration with `calculateETrefPM()`

We can calculate reference evapotranspiration for daily data using the
`calculateETrefPM()` function. This function is based on the FAO
Penman-Monteith method, according to:”

> Allen, R.G., Pereira, L.S., Raes, D., Smith, M., 1998. Crop
> evapotranspiration – guidelines for computing crop water requirements
> – FAO Irrigation and Drainage Paper 56. FAO, 1998. ISBN 92-5-104219-5

``` r
 address <-
  base::system.file("extdata",
                     "ex2_daily.CSV",
                     package = "DataMetProcess")

 df <- read.table(
 address,
 h = TRUE,
 sep = ";"
 )

 #converting to Mj/m
 df$radiacao_global_kj_m <- df$radiacao_global_kj_m/1000
 colnames(df)[3] <- "radiacao_global_mj_m"

 df.Eto <-
   calculateETrefPM(
     data = df,
     lat = -21.980353,
     alt = 859.29,
     alt_an = 10,
     dap = 1,
     date = colnames(df)[1],
     temp = colnames(df)[7],
     g = NULL,
     humid = colnames(df)[15],
     rad = colnames(df)[3],
     press = colnames(df)[4],
     wind = colnames(df)[18],
     kc = NULL
  )
 
 head(df.Eto)
#>         Data    etref dj dap       es       ea     delta          y        rn
#> 1 2008-01-01 8.269305  1   1 3.138558 2.198246 0.1871697 0.06096587 10.534448
#> 2 2008-01-02 5.987792  2   2 2.877179 2.201042 0.1734963 0.06101973  9.618784
#> 3 2008-01-03 8.090657  3   3 2.913816 1.998295 0.1754227 0.06113810 10.746086
#> 4 2008-01-04 9.051521  4   4 2.943771 1.919633 0.1769954 0.06115673 13.339185
#> 5 2008-01-05 6.495420  5   5 2.842662 2.109539 0.1716782 0.06105032 10.076827
#> 6 2008-01-06 2.308457  6   6 2.387142 2.131480 0.1473864 0.06096122  8.308235
```

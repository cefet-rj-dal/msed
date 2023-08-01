require(lubridate)
library(hht)

source("moghtaderi-filter.R")

ceemd_trend_filter <- function(serie=NULL, noise.amp = 6.4e-07 , trials = 100, limit_ri_sup = 2.14, limit_ri_inf = 1.50, frequency_date=12, start_date=c(2004,1)
                               , stop_rule="type1"){
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  serie$Data<- as.Date(as.yearmon(serie$time))
  time_series <- vector()
  time_series <- c(time_series, strtoi(trim(as.numeric(as.POSIXct(serie$Data, format = "%Y-%m")))))
  
  results <- NULL
  components <- NULL
  results <- hht::CEEMD(serie$value, time_series, noise.amp, trials, max.sift = 200, boundary = "wave", stop.rule = stop_rule)
  components <- energy_ratio_approach(limit_ri_sup, limit_ri_inf , results)

  return(list(components=components, time=serie$Data, imf=results$imf, residue= results$residue))
}
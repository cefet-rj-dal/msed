library(xts)
source("multi-scale-detect.R")
source("load_time_series.R")
library(tidyr)
install.packages('devtools')
library(devtools)
source("harbinger/examples/load_harbinger.R")
source("harbinger.R")
devtools::install_github("cefet-rj-dal/harbinger")
library(tsbox)
library(readxl)
library(slider)
source("utils.R")
source("harbinger.R")
library(tidyr)
load_harbinger() 
load_library("reticulate")
source("harbinger/examples/ts_tlstm.R")
reticulate::source_python("harbinger/examples/ts_tlstm.py")
data(har_examples)

###########################################  Anomaly


#lstm
# anomaly MSED


limit_ri_sup <- 2.14 # limite sup definido para obtenção do RI
limit_ri_inf <- 1.50 # limite inf definido para obtenção do RI
frequency_date <- 12 # frequencia definida para as datas 
start_date <- c(2022,1) # data de inicio da base
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

reference <-df_anomalies
serie_df = df_series_anomalies




### window 5
results <- multi_scale_event_detect(
  serie = serie_df,
  limit_ri_sup = limit_ri_sup,
  limit_ri_inf = limit_ri_inf,
  frequency_date = frequency_date,
  start_date = start_date,
  type_events = list("anomaly"),
  rf_dataframe = detect_anomalies(time_series),
  soft_window = 5
)


### window 10
debug(multi_scale_event_detect)
results <- multi_scale_event_detect(
  serie = serie_df,
  limit_ri_sup = limit_ri_sup,
  limit_ri_inf = limit_ri_inf,
  frequency_date = frequency_date,
  start_date = start_date,
  type_events = list("anomaly"),
  rf_dataframe = detect_anomalies(time_series),
  soft_window = 10
)



#### window 20
results <- multi_scale_event_detect(
  serie = serie_df,
  limit_ri_sup = limit_ri_sup,
  limit_ri_inf = limit_ri_inf,
  frequency_date = frequency_date,
  start_date = start_date,
  type_events = list("anomaly"),
  rf_dataframe = detect_anomalies(time_series),
  soft_window = 20
)

#lstm


model <- har_tsreg_sw(ts_tlstm(ts_diff(), input_size=4, epochs=4000))
model <- fit(model, serie_df$value)

detection <- detect(model, serie_df$value)

detection[is.na(detection)] <- FALSE


convert_boolean_df <- function(df) {
  df[is.na(df)] <- FALSE
  df2 <- data.frame(time =  seq( as.Date("2022-01-01", "%Y-%m-%d"), length.out = nrow(df), by = "day"), event = df$event, type='anomaly')
  return(df2)
}


ds <- convert_boolean_df(detection)
events <- ds |> dplyr::filter(event==TRUE)
print(events)
metrics <- soft_evaluate(events =events, reference = reference, k = 10)
evtplot(serie_df, events, reference)



metrics <- soft_evaluate(events =events, reference = reference, k = 5)
evtplot(serie_df, events, reference)


metrics <- soft_evaluate(events =events, reference = reference, k = 20)
evtplot(serie_df, events, reference)


# anomalie package

serie_df <- data.frame(time=serie_df$time, x=serie_df$value)

events_an <- evtdet.an_outliers(serie_df, w=5, alpha=1.5)
metric=c("accuracy","sensitivity","specificity","precision",
         "recall","F1","balanced_accuracy")
print(evtplot(serie_df, events_an,reference))
metrics <- soft_evaluate(events_an, reference, k=5) 

events_an <- evtdet.an_outliers(serie_df, w=10, alpha=1.5)
metric=c("accuracy","sensitivity","specificity","precision",
         "recall","F1","balanced_accuracy")

print(evtplot(serie_df, events_an,reference))
metrics <- soft_evaluate(events_an, reference, k=10) 


events_an <- evtdet.an_outliers(serie_df,w=20, alpha=1.5)
metric=c("accuracy","sensitivity","specificity","precision",
         "recall","F1","balanced_accuracy")

print(evtplot(serie_df, events_an,reference))
metrics <- soft_evaluate(events_an, reference, k=20) 



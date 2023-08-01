#install.packages('devtools')
#devtools::install_github("cefet-rj-dal/harbinger", force = TRUE)

source("multi-scale-detect.R")
source("load_time_series.R")

library(magrittr) 
library(dplyr)   
library(ggplot2)
library(tsbox)
library(readxl)
library(slider)
library(tidyr)
library(devtools)

source("harbinger/examples/load_harbinger.R")
folder_path <- "harbinger/R"
files <- list.files(folder_path, pattern = "\\.R$", full.names = TRUE)
for (file in files) {
  source(file)
}
load_library("reticulate")
source("harbinger/examples/ts_tlstm.R")
reticulate::source_python("harbinger/examples/ts_tlstm.py")
load_harbinger() 
data(har_examples)





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
  rf_dataframe = reference,
  soft_window = 5
)

sprintf("Method MSED Metrics: F1 %s | Precision %s | Recall %s to softwindow 5", results["F1"], results["precision"], results["recall"])

### window 10
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

sprintf("Method MSED Metrics: F1 %s | Precision %s | Recall %s to softwindow 10", results["F1"], results["precision"], results["recall"])


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

sprintf("Method MSED Metrics: F1 %s | Precision %s | Recall %s to softwindow 20", results["F1"], results["precision"], results["recall"])


serie_df <- data.frame(time = serie_df$time, serie = serie_df$value)

model <- har_tsreg_sw(ts_tlstm(ts_diff(), input_size=4, epochs=4000))
model <- fit(model, serie_df$serie)
detection <- detect(model, serie_df$serie)

detection[is.na(detection)] <- FALSE


convert_boolean_df <- function(df) {
  df[is.na(df)] <- FALSE
  df2 <- data.frame(time =  seq( as.Date("2022-01-01", "%Y-%m-%d"), length.out = nrow(df), by = "day"), event = df$event, type='anomaly')
  return(df2)
}


ds <- convert_boolean_df(detection)
events <- ds |> dplyr::filter(event==TRUE)
print(events)
metrics <- soft_evaluate(events =events, reference = reference, k = 5)
evtplot(serie_df, events, reference)
sprintf("Method lstm Metrics: F1 %s | Precision %s | Recall %s to softevaluate 5", metrics["F1"], metrics["precision"], metrics["recall"])



metrics <- soft_evaluate(events =events, reference = reference, k = 10)
evtplot(serie_df, events, reference)

sprintf("Method lstm Metrics: F1 %s | Precision %s | Recall %s to softevaluate 10", metrics["F1"], metrics["precision"], metrics["recall"])

metrics <- soft_evaluate(events =events, reference = reference, k = 20)
sprintf("Method lstm Metrics: F1 %s | Precision %s | Recall %s to softevaluate 20", metrics["F1"], metrics["precision"], metrics["recall"])

evtplot(serie_df, events, reference)


# anomalie package


model <- fbiad()
model <- fit(model, serie_df$serie)
detection <- detect(model, serie_df$serie)
print(detection |> dplyr::filter(event==TRUE))
rf_boolean_list <- as.logical(reference$value)
soft_evaluate <- evaluate(model, detection$event, rf_boolean_list, evaluation = soft_evaluation(sw=5))
sprintf("Method FBIAD: F1 %s | Precision %s | Recall %s to softwindow 5", soft_evaluate$F1, soft_evaluate$precision, soft_evaluate$recall)


soft_evaluate <- evaluate(model, detection$event, rf_boolean_list, evaluation = soft_evaluation(sw=10))
sprintf("Method FBIAD: F1 %s | Precision %s | Recall %s to softwindow 10", soft_evaluate$F1, soft_evaluate$precision, soft_evaluate$recall)


soft_evaluate <- evaluate(model, detection$event, rf_boolean_list, evaluation = soft_evaluation(sw=20))
sprintf("Method FBIAD: F1 %s | Precision %s | Recall %s to softwindow 20", soft_evaluate$F1, soft_evaluate$precision, soft_evaluate$recall)


source("multi-scale-detect.R")
source("load_time_series.R")
source("harbinger-v1.R")

library(magrittr)
library(xts)

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

limit_ri_sup <- 2.14 # limite sup definido para obtenção do RI
limit_ri_inf <- 1.50 # limite inf definido para obtenção do RI
frequency_date <- 12 # frequencia definida para as datas 
start_date <- c(2010,1) # data de inicio da base
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

reference <-variance_change_df
serie_df <-df_serie_amplitude


######################### Change Variance
results <- multi_scale_event_detect(
  serie = serie_df,
  limit_ri_sup = limit_ri_sup,
  limit_ri_inf = limit_ri_inf,
  frequency_date = frequency_date,
  start_date = start_date,
  type_events = list("cp_variance"),
  rf_dataframe = variance_change_df,
  soft_window = 5
)


sprintf("Method MSED Metrics: F1 %s | Precision %s | Recall %s to softwindow 5", results["F1"], results["precision"], results["recall"])

results <- multi_scale_event_detect(
  serie = serie_df,
  limit_ri_sup = limit_ri_sup,
  limit_ri_inf = limit_ri_inf,
  frequency_date = frequency_date,
  start_date = start_date,
  type_events = list("cp_variance"),
  rf_dataframe = variance_change_df,
  soft_window = 10
)

sprintf("Method MSED Metrics: F1 %s | Precision %s | Recall %s to softwindow 10", results["F1"], results["precision"], results["recall"])

results <- multi_scale_event_detect(
  serie = serie_df,
  limit_ri_sup = limit_ri_sup,
  limit_ri_inf = limit_ri_inf,
  frequency_date = frequency_date,
  start_date = start_date,
  type_events = list("cp_variance"),
  rf_dataframe = variance_change_df,
  soft_window = 20
)

sprintf("Method MSED Metrics: F1 %s | Precision %s | Recall %s to softwindow 20", results["F1"], results["precision"], results["recall"])

serie_df <- data.frame(time=serie_df$time, serie=serie_df$value, type = "change point")

##### GARCH
model <- har_garch()
model <- fit(model, serie_df$serie)
detection <- detect(model, serie_df$serie)
print(detection |> dplyr::filter(event==TRUE))
rf_boolean_list <- as.logical(reference$value)
soft_evaluate <- evaluate(model, detection$event, rf_boolean_list, evaluation = soft_evaluation(sw=5))

sprintf("Method GARCH: F1 %s | Precision %s | Recall %s to softwindow 5", soft_evaluate$F1, soft_evaluate$precision, soft_evaluate$recall)


soft_evaluate <- evaluate(model, detection$event, rf_boolean_list, evaluation = soft_evaluation(sw=10))
sprintf("Method GARCH: F1 %s | Precision %s | Recall %s to softwindow 10", soft_evaluate$F1, soft_evaluate$precision, soft_evaluate$recall)


soft_evaluate <- evaluate(model, detection$event, rf_boolean_list, evaluation = soft_evaluation(sw=20))
sprintf("Method GARCH: F1 %s | Precision %s | Recall %s to softwindow 20", soft_evaluate$F1, soft_evaluate$precision, soft_evaluate$recall)

##### Method Seminal Change Point

events_scp <- evtdet.seminalChangePoint2(serie_df, w=90,mdl=linreg,na.action=na.omit)
metric=c("accuracy","sensitivity","specificity","precision",
         "recall","F1","balanced_accuracy")
print(evtplot(serie_df, events_scp,reference))

metrics <- soft_evaluate(events_scp, reference, k=5)
sprintf("Method SCP: F1 %s | Precision %s | Recall %s to softwindow 5", metrics['F1'], metrics['precision'], metrics['recall'])

metrics <-  soft_evaluate(events_scp, reference, k =10)
sprintf("Method SCP: F1 %s | Precision %s | Recall %s to softwindow 10", metrics['F1'], metrics['precision'], metrics['recall'])

metrics <-  soft_evaluate(events_scp, reference, k =20)
sprintf("Method SCP: F1 %s | Precision %s | Recall %s to softwindow 20",metrics['F1'], metrics['precision'], metrics['recall'])

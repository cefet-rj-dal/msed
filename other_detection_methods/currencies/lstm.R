library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)
library(xts)
library(tsbox)
library(readxl)
library(slider)
library(xts)
library(tidyr)
install.packages('devtools')
library(devtools)
source("harbinger/examples/load_harbinger.R")
source("harbinger.R")
devtools::install_github("cefet-rj-dal/harbinger")

load_harbinger() 
load_library("reticulate")
source("harbinger/examples/ts_tlstm.R")
reticulate::source_python("harbinger/examples/ts_tlstm.py")
data(har_examples)



convert_boolean_df <- function(df) {
  df[is.na(df)] <- FALSE
  df2 <- data.frame(time = df$Data, event = df$Experiment1, type='anomaly')
  return(df2)
}

form_df_events <- function(dates, df) {
  df[is.na(df)] <- FALSE
  df2 <- data.frame(time = dates, event = df$event, type='anomaly')
  return(df2)
}



experiment = "Experiment1"
currencies <- read_xlsx("/home/diego/Documentos/datascience/time-series/volatility/avaliacao-experimental/mestrado/files/Base de Dados Consolidada base line.xlsx",sheet="Currency")
method_emd <- "CEEMD"

currencies_name_columns <- c (
  'Data',
  experiment,
  'EUR/BRL',
  'CLP/BRL',
  'USD/BRL',
  'CNY/BRL',
  'ARS/BRL'
)
currencies <- currencies[currencies_name_columns]

get_break_dates <- function(breakpoints_term, currencies){
  return (currencies$Data[na.omit(breakpoints_term)])
}
df_results <- NULL

# 
name_currencies <- ""
accuracy <- c(0)
sensitivity <- c(0)
specificity <- c(0)
precision <- c(0)
recall <- c(0)
f1 <- c(0)
df_results <- data.frame(name_currencies, f1, accuracy, precision, recall)
# 

for (name_column in currencies_name_columns){
  if(name_column == experiment || name_column == "Data"){
    next
  }
  serie_df <- data.frame(time=currencies$Data, serie=currencies[[name_column]])
  
  
  
  model <- har_tsreg_sw(ts_tlstm(ts_diff(), input_size=4, epochs=4000))
  model <- fit(model, serie_df$serie)
  
  detection <- detect(model, serie_df$serie)
  
  detection[is.na(detection)] <- FALSE
  reference <-currencies[c('Data',experiment)]
  
  ds <- form_df_events(serie_df$time, detection)
  events <- ds |> dplyr::filter(event==TRUE)
  
  metric=c("accuracy","sensitivity","specificity","precision",
           "recall","F1","balanced_accuracy")
  print(evtplot(serie_df, events, reference))
  metrics <- soft_evaluate(events =events, reference = reference, k = 20)
  df_results <- add_row(df_results, name_currencies= name_column,
                        f1=metrics['F1'],
                        accuracy=metrics['accuracy'],
                        precision=metrics['precision'],
                        recall=metrics['recall'])
}

file <- sprintf("metodos-harbinger/files/soft-lstm-currencies-exp1.csv" )
write.csv(df_results, file)

library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)
library(xts)
library(tsbox)
library(readxl)
library(slider)
source("utils.R")
source("harbinger/harbinger.R")


experiment = "Experiment1"
stocks <- read_xlsx("/home/diego/Documentos/datascience/time-series/volatility/avaliacao-experimental/mestrado/files/Base de Dados Consolidada  EPU.xlsx",sheet="china-series")
method_emd <- "CEEMD"


stocks_name_columns <- c (
  'Data',
  experimento,
  'CNY/DOL',
  'CPI_BY_MONTHY',
  'HSCE'
)

stocks <- stocks[stocks_name_columns]
## Soft Evaluate
df_results <- NULL


name_stocks <- ""
accuracy <- c(0)
sensitivity <- c(0)
specificity <- c(0)
precision <- c(0)
recall <- c(0)
f1 <- c(0)
df_results <- data.frame(name_stocks, accuracy,sensitivity, specificity, precision, recall, f1)


for (name_column in currencies_name_columns){
  if(name_column == "Event" || name_column == "Data"){
    next
  }
  df <- data.frame(time=stocks$Data, x=stocks[[name_column]])
  ARIMA <- function(data) forecast::auto.arima(df)
  events_cf <- evtdet.changeFinder(test,mdl=ARIMA,m=5)
  metric=c("accuracy","sensitivity","specificity","precision",
           "recall","F1","balanced_accuracy")
  reference <-currencies[c('Data','Event')]
  print(evtplot(df, events_cf,reference))
  metrics <- soft_evaluate(events_cf, reference) 
  df_results <- add_row(df_results, name_stocks= name_column,
                        f1=metrics['F1'],
                        accuracy=metrics['accuracy'],
                        sensitivity=metrics['sensitivity'],
                        specificity=metrics['specificity'],
                        precision=metrics['precision'],
                        recall=metrics['recall'])
}

file <- sprintf("metodos-harbinger/files/stocks-soft-evaluate-change-finder.csv" )
write.csv(df_results, file)

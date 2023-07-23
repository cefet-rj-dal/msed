library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)
library(xts)
library(tsbox)
library(readxl)
library(slider)
source("utils.R")
source("harbinger/harbinger.R")


currencies <- read_xlsx("/home/diego/Documentos/datascience/time-series/volatility/avaliacao-experimental/mestrado/files/Base de Dados Consolidada base line.xlsx",sheet="Currency")
currencies_name_columns <- colnames(currencies[1:20])
method_emd <- "CEEMD"


currencies_name_columns <- c (
  'Data',
  'Event',
  # 'AUD/BRL',
  'CAD/BRL',
  'CHF/BRL',
  'EUR/BRL',
  'GBP/BRL',
  'SGD/BRL',
  'USD/BRL',
  'CNY/BRL',
  'DKK/BRL',
  'MXN/BRL',
  'NOK/BRL',
  'NZD/BRL',
  'PLN/BRL',
  'SEK/BRL',
  'TRY/BRL',
  'ZAR/BRL',
  'PEN/BRL'
)

## Soft Evaluate
currencies <- currencies[currencies_name_columns]
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

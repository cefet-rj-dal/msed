install.packages('TSPred')
install.packages('zoo')
install.packages('ICSS')
install.packages('hht')
install.packages('cpm')
install.packages('lubridate')
library(tibble)

library(tidyr)

.libPaths()

library(magrittr) # needs to be run every time you start R and want to use %>%
library(quantmod)
library("TSPred")
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)
library(xts)
library(tsbox)
library(readxl)
library(slider)
source("utils.R")
source("harbinger.R")




experimento = "Experiment1"


currencies_name_columns <- c (
  'Data',
  experimento,
  #'AUD/BRL',
  #' 'CAD/BRL',
  #' 'CHF/BRL',
  #' # 'EUR/BRL',
  #'  'GBP/BRL',
  #' # 'SGD/BRL',
  #' 'USD/BRL',
  #' 'CNY/BRL',
  #' # 'DKK/BRL',
  #' # 'MXN/BRL',
  #' #'NOK/BRL',
  #' 'NZD/BRL',
  #' #'PLN/BRL',
  #' 'SEK/BRL',
  'TRY/BRL'
  #'PEN/BRL'
  )


currencies <- read_xlsx("/home/diego/Documentos/datascience/time-series/volatility/avaliacao-experimental/mestrado/files/Base de Dados Consolidada base line.xlsx",sheet="Currency")

currencies <- currencies[currencies_name_columns]
ARIMA <- function(data) forecast::auto.arima(data)

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
df_results <- data.frame(name_currencies, accuracy,sensitivity, specificity, precision, recall, f1)
# 

for (name_column in currencies_name_columns){
  if(name_column == experimento || name_column == "Data"){
    next
  }
  print(name_column)
  
  df <- data.frame(time=currencies$Data, x=currencies[[name_column]])
  events_cf <- evtdet.changeFinder(df,mdl=ARIMA,m=5)
  metric=c("accuracy","sensitivity","specificity","precision",
           "recall","F1","balanced_accuracy")
  reference <-currencies[c('Data',experimento)]
  print(evtplot(df,events_cf, reference, mark.cp=TRUE))
  metrics <- evaluate(events_cf, reference) 
  df_results <- add_row(df_results, name_currencies= name_column,
                        f1=metrics['F1'],
                        accuracy=metrics['accuracy'],
                        sensitivity=metrics['sensitivity'],
                        specificity=metrics['specificity'],
                        precision=metrics['precision'],
                        recall=metrics['recall'])
}

file <- sprintf("metodos-harbinger/files/currencies-change-finder.csv" )
write.csv(df_results, file)

#### Soft Evaluate


name_currencies <- ""
accuracy <- c(0)
sensitivity <- c(0)
specificity <- c(0)
precision <- c(0)
recall <- c(0)
f1 <- c(0)
df_results <- data.frame(name_currencies, accuracy,sensitivity, specificity, precision, recall, f1)
# 

for (name_column in currencies_name_columns){
  if(name_column == "Event" || name_column == "Data"){
    next
  }
  
  df <- data.frame(time=currencies$Data, x=currencies[[name_column]])
  ARIMA <- function(data) forecast::auto.arima(df)
  events_cf <- evtdet.changeFinder(test,mdl=ARIMA,m=5)
  metric=c("accuracy","sensitivity","specificity","precision",
           "recall","F1","balanced_accuracy")
  reference <-currencies[c('Data','Event')]
  print(evtplot(df, events_cf,reference))
  metrics <- soft_evaluate(events_cf, reference) 
  df_results <- add_row(df_results, name_currencies= name_column,
                        f1=metrics['F1'],
                        accuracy=metrics['accuracy'],
                        sensitivity=metrics['sensitivity'],
                        specificity=metrics['specificity'],
                        precision=metrics['precision'],
                        recall=metrics['recall'])
}

file <- sprintf("metodos-harbinger/files/currencies-soft-evaluate-change-finder.csv" )
write.csv(df_results, file)


### Stocks

stocks <- read_xlsx("files/Base de Dados Consolidada base line.xlsx",sheet="Stocks")
stocks_name_columns <- colnames(stocks[1:11])

stocks_name_columns <- c (
  'Data',
  'Event',
  'Ibovespa',
  'Ibrx100',
  'Ibrx50', 
  'Ibra',
  # 'MidLarge', 
  # 'SmallCap', 
  'IGC', 
  'ITAG', 
  'IGCT', 
  'IFNC', 
  'IEEX'
)
stocks <- stocks[stocks_name_columns]

df_results <- NULL


name_stocks <- ""
accuracy <- c(0)
sensitivity <- c(0)
specificity <- c(0)
precision <- c(0)
recall <- c(0)
f1 <- c(0)
df_results <- data.frame(name_stocks, accuracy,sensitivity, specificity, precision, recall, f1)


for (name_column in stocks_name_columns){
  if(name_column == "Event" || name_column == "Data"){
    next
  }
  df <- data.frame(time=stocks$Data, x=stocks[[name_column]])
  ARIMA <- function(data) forecast::auto.arima(df)
  events_cf <- evtdet.changeFinder(df,mdl=ARIMA,m=5)
  metric=c("accuracy","sensitivity","specificity","precision",
           "recall","F1","balanced_accuracy")
  reference <-currencies[c('Data','Event')]
  print(evtplot(df, events_cf,reference))
  metrics <- evaluate(events_cf, reference) 
  df_results <- add_row(df_results, name_stocks= name_column,
                        f1=metrics['F1'],
                        accuracy=metrics['accuracy'],
                        sensitivity=metrics['sensitivity'],
                        specificity=metrics['specificity'],
                        precision=metrics['precision'],
                        recall=metrics['recall'])
}

file <- sprintf("metodos-harbinger/files/stocks-change-finder.csv" )
write.csv(df_results, file)



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


for (name_column in stocks_name_columns){
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

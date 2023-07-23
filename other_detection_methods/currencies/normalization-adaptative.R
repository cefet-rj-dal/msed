library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)
library(xts)
library(tsbox)
library(readxl)
library(slider)
source("utils.R")
source("harbinger.R")
library(tidyr)


experiment = "comb"
currencies <- read_xlsx("/home/diego/Documentos/datascience/time-series/volatility/avaliacao-experimental/mestrado/files/Base de Dados Consolidada base line.xlsx",sheet="Currency")
currencies_name_columns <- colnames(currencies[1:20])
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
df_results <- data.frame(name_currencies,  f1, accuracy, precision, recall)
# 

for (name_column in currencies_name_columns){
  if(name_column == experiment || name_column == "Data"){
    next
  }
  df <- data.frame(time=currencies$Data, x=currencies[[name_column]])
  df <- drop_na(df)
  
  
  events_an <- evtdet.an_outliers(df, w=20,alpha=1.5)
  metric=c("accuracy","sensitivity","specificity","precision",
           "recall","F1","balanced_accuracy")
  reference <-currencies[c('Data',experiment)]
  print(evtplot(df, events_an,reference))
  metrics <- soft_evaluate(events_an, reference) 
  df_results <- add_row(df_results, name_currencies= name_column,
                        f1=metrics['F1'],
                        accuracy=metrics['accuracy'],
                        precision=metrics['precision'],
                        recall=metrics['recall'])
}

file <- sprintf("metodos-harbinger/files/currencies-soft-normalizacao-adaptativa-comb.csv" )
write.csv(df_results, file)


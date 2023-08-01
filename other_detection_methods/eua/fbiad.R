library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)
library(xts)
install.packages("strucchange", repos = "http://cran.r-project.org")
library(strucchange)

library(tsbox)
library(readxl)
library(slider)
source("utils.R")
source("harbinger.R")
library(tidyr)

#Experiment1	0,25	0,375	0,5	0250/0125	0375/0125	0,5/0,25

experiment = "0,5/0,25"
stocks <- read_xlsx("/home/diego/Documentos/datascience/time-series/volatility/avaliacao-experimental/mestrado/files/Base de Dados Consolidada base line.xlsx",sheet="EUA")
method_emd <- "CEEMD"


stocks_name_columns <- c (
  'Data',
  experiment,
  'Dow Jones',
  'Nasdaq',
  'SP500',
  'USSMALLCAP2000',
  'NYSE Composite'
)

stocks <- stocks[stocks_name_columns]

get_break_dates <- function(breakpoints_term, stocks){
  return (stocks$Data[na.omit(breakpoints_term)])
}
df_results <- NULL

# 
name_stocks <- ""
accuracy <- c(0)
sensitivity <- c(0)
specificity <- c(0)
precision <- c(0)
recall <- c(0)
f1 <- c(0)
df_results <- data.frame(name_stocks, f1, accuracy, precision, recall)
# 

for (name_column in stocks_name_columns){
  if(name_column == experiment || name_column == "Data"){
    next
  }
  df <- data.frame(time=stocks$Data, x=stocks[[name_column]])
  #df <- drop_na(df)
  
  
  events_an <- evtdet.an_outliers(df, w=20,alpha=1.5)
  metric=c("accuracy","sensitivity","specificity","precision",
           "recall","F1","balanced_accuracy")
  reference <-stocks[c('Data',experiment)]
  print(evtplot(df, events_an,reference))
  metrics <- soft_evaluate(events_an, reference) 
  df_results <- add_row(df_results, name_stocks= name_column,
                        f1=metrics['F1'],
                        accuracy=metrics['accuracy'],
                        precision=metrics['precision'],
                        recall=metrics['recall'])
}

file <- sprintf("metodos-harbinger/files/stocks-soft-normalizacao-adaptativa-eua-05025.csv" )
write.csv(df_results, file)

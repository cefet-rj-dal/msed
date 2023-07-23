
install.packages('rugarch',repos = "http://cran.r-project.org")


library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)
library(xts)
library(tsbox)
library(readxl)
library(slider)
source("utils.R")
source("harbinger/harbinger.R")
library(rugarch)

experiment = "0. 125"
stocks <- read_xlsx("/home/diego/Documentos/datascience/time-series/volatility/avaliacao-experimental/mestrado/files/Base de Dados Consolidada base line.xlsx",sheet="Stocks")

stocks_name_columns <- colnames(stocks[1:20])
method_emd <- "CEEMD"

garch11 <- 
  rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), distribution.model = "norm")

stocks_name_columns <- c (
  'Data',
  experiment,
  'Ibovespa',
  'Ibrx100',
  'Ibrx50', 
  'Ibra',
  'IGC'
)

stocks <- stocks[stocks_name_columns]

get_break_dates <- function(breakpoints_term, stocks){
  return (stocks$Data[na.omit(breakpoints_term)])
}
df_results <- NULL


name_stocks <- ""
accuracy <- c(0)
sensitivity <- c(0)
specificity <- c(0)
precision <- c(0)
recall <- c(0)
f1 <- c(0)
df_results <- data.frame(name_stocks,f1, accuracy, precision, recall)
# 

for (name_column in stocks_name_columns){
  if(name_column == experiment || name_column == "Data"){
    next
  }
  
  df <- data.frame(time=stocks$Data, x=stocks[[name_column]])
  events_garch <- evtdet.garch_volatility_outlier(df, spec=garch11,alpha=1.5)
  metric=c("accuracy","sensitivity","specificity","precision",
           "recall","F1","balanced_accuracy")
  reference <-stocks[c('Data', experiment)]
  print(evtplot(df, events_garch,reference))
  metrics <- soft_evaluate(events_garch, reference) 
  df_results <- add_row(df_results, name_stocks= name_column,
                        f1=metrics['F1'],
                        accuracy=metrics['accuracy'],
                        precision=metrics['precision'],
                        recall=metrics['recall'])
}

file <- sprintf("metodos-harbinger/files/stocks-garch-0125" )
write.csv(df_results, file)


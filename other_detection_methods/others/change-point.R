library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)
library(xts)
library(tsbox)
library(readxl)
library(slider)
source("utils.R")
source("harbinger.R")
linreg <- function(data) {
  #browser()
  data <- as.data.frame(data)
  colnames(data) <- "x"
  data$t <- 1:nrow(data)
  
  #Adjusting a linear regression to the whole window
  lm(x~t, data)
}

experiment = "Experiment1"
stocks <- read_xlsx("/home/diego/Documentos/datascience/time-series/volatility/avaliacao-experimental/mestrado/files/Base de Dados Consolidada base line.xlsx",sheet="others")
method_emd <- "CEEMD"


stocks_name_columns <- c (
  'Data',
  experiment,
  'IPCA'
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
df_results <- data.frame(name_stocks, accuracy,sensitivity, specificity, precision, recall, f1)
# 

 for (name_column in stocks_name_columns){
   if(name_column == experiment || name_column == "Data"){
     next
   }


  df <- data.frame(time=stocks$Data, serie=stocks[[name_column]])

  events_scp <- evtdet.seminalChangePoint2(df, w=60,mdl=linreg,na.action=na.omit)
  reference <-stocks[c('Data',experiment)]
  metric=c("accuracy","sensitivity","specificity","precision",
           "recall","F1","balanced_accuracy")
  print(evtplot(df,events_scp, reference))
  metrics <- soft_evaluate(events_scp, reference) 
  df_results <- add_row(df_results, name_stocks= name_column,
                          f1=metrics['F1'],
                          accuracy=metrics['accuracy'],
                          sensitivity=metrics['sensitivity'],
                          specificity=metrics['specificity'],
                          precision=metrics['precision'],
                          recall=metrics['recall'])
      
 }

file <- sprintf("metodos-harbinger/files/stocks-change-point-exp1.csv" )
write.csv(df_results, file)

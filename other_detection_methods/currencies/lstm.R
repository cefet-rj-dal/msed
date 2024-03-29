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

experiment = "Event"
currencies <- read_xlsx("dataset/Base de Dados Consolidada base line.xlsx",sheet="Currency")
method_emd <- "CEEMD"

currencies_name_columns <- c(
  "Data",
  experiment,
  "USD/BRL",
  "EUR/BRL",
  "CNY/BRL",
  "CLP/BRL",
  "ARS/BRL"
)

currencies <- currencies[currencies_name_columns]

get_break_dates <- function(breakpoints_term, currencies){
  return (currencies$Data[na.omit(breakpoints_term)])
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

for (name_column in currencies_name_columns){
  if(name_column == experiment || name_column == "Data"){
    next
  }
  serie_df <- data.frame(time=currencies$Data, serie=currencies[[name_column]])
  
  
  
  model <- har_tsreg_sw(ts_tlstm(ts_diff(), input_size=4, epochs=4000))
  model <- fit(model, serie_df$serie)
  
  detection <- detect(model, serie_df$serie)
  rf_boolean_list <- as.logical(reference$Event)
  soft_evaluate <- evaluate(model, detection$event, rf_boolean_list, evaluation = soft_evaluation(sw=15))
  df_results <- add_row(df_results, name_stocks= name_column,
                        f1=soft_evaluate$F1,
                        precision=soft_evaluate$precision,
                        recall=soft_evaluate$recall)
}

file <- sprintf("files/stocks-soft-lstm-currencies.csv" )
write.csv(df_results, file)

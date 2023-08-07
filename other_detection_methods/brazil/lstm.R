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
stocks <- read_xlsx("dataset/Base de Dados Consolidada base line.xlsx",sheet="Stocks")
method_emd <- "CEEMD"

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

# 
name_stocks <- ""
sensitivity <- c(0)
specificity <- c(0)
precision <- c(0)
recall <- c(0)
f1 <- c(0)
df_results <- data.frame(name_stocks, f1, precision, recall)
model <- har_tsreg_sw(ts_tlstm(ts_diff(), input_size=4, epochs=4000))

for (name_column in stocks_name_columns){
  if(name_column == experiment || name_column == "Data"){
    next
  }
  serie_df <- data.frame(time=stocks$Data, serie=stocks[[name_column]])
  reference <-stocks[c('Data',experiment)]
  model <- fit(model, serie_df$serie)
  detection <- detect(model, serie_df$serie)
  rf_boolean_list <- as.logical(reference$Event)
  soft_evaluate <- evaluate(model, detection$event, rf_boolean_list, evaluation = soft_evaluation(sw=15))
  df_results <- add_row(df_results, name_stocks= name_column,
                        f1=soft_evaluate$F1,
                        precision=soft_evaluate$precision,
                        recall=soft_evaluate$recall)
}

file <- sprintf("files/stocks-soft-lstm-brasil.csv" )
write.csv(df_results, file)

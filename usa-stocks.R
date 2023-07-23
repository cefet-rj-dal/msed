install.packages('cpm',repos = "http://cran.r-project.org")
#install.packages('tidyverse',repos = "http://cran.r-project.org")
install.packages("magrittr", repos = "http://cran.r-project.org")
install.packages("strucchange", repos = "http://cran.r-project.org")
install.packages("anomalize", repos = "http://cran.r-project.org")
library(dplyr)
library(ggplot2)
library(strucchange)
require(lubridate)
library(ICSS)
library(EMD)
library(hht)
library(xts)
library(tsbox)
library(readxl)
library(slider)
library(cpm)
library(tidyverse)
library(anomalize)
library(magrittr)
#library(anomalize)

source("utils.R")
source("functions-models.R")
source("anomalies.R")
source("utils.R")
source("harbinger.R")
source("multi-scale-detect.R")

experimento = "Experiment1"

stocks_usa_name_columns <- c (
  'Data',
  experimento,
   'DJI',
   'Nasdaq',
   'SP500',
   'DJ Composite',
  'USSMALLCAP2000'
)

stocks <- read_xlsx("dataset/Base de Dados Consolidada base line.xlsx",sheet="EUA")
method_emd <- "CEEMD"

reference <- stocks[c("Data", experimento)]

df_results <- NULL

name_serie <- ""
sensitivity <- c(0)
specificity <- c(0)
precision <- c(0)
recall <- c(0)
f1 <- c(0)
df_results <- data.frame(name_serie, f1, precision, recall)


df_domain <- NULL
name_domain_group <- ""


SFVE <- c(0)
SSCP <- c(0)
STCP <- c(0)
SSVE <- c(0)
LT <- c(0)
AD <- c(0)

df_domain <- data.frame(name_domain_group, SFVE, SSCP,STCP, SSVE, LT, AD)

for (name_column in stocks_usa_name_columns) {
  if (name_column == experimento || name_column == "Data" || name_column == "Tipo") {
    next
  }
  serie_df <- data.frame(setNames(list(stocks$Data, stocks[[name_column]]), c("time", "value")))
  #debug(multi_scale_event_detect)
  metrics <- multi_scale_event_detect(
    serie = serie_df,
    frequency_date = frequency_date,
    start_date = start_date,
    type_events = list("all"),
    rf_dataframe = reference,
    soft_window = 5
  )
  
  df_results <- add_row(df_results,
                        name_serie = name_column,
                        f1 = metrics["F1"],
                        precision = metrics["precision"],
                        recall = metrics["recall"])
}
file_metrics <- sprintf("files/stocks_usa_%s.csv", "metrics")
write.csv(df_results, file_metrics)



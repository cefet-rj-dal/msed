#install.packages('cpm',repos = "http://cran.r-project.org")
#install.packages('tidyverse',repos = "http://cran.r-project.org")
#install.packages("magrittr", repos = "http://cran.r-project.org")
#install.packages("strucchange", repos = "http://cran.r-project.org")
#install.packages("anomalize", repos = "http://cran.r-project.org")
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
library(dplyr)
library(magrittr)

source("functions-models.R")
source("anomalies.R")
source("utils.R")
source("harbinger.R")

experimento = "Experiment1"
stocks_name_columns <- c (
    'Data',
    experimento,
    'Ibovespa',
    'Ibrx100',
    'Ibrx50',
    'Ibra',
    'IGC'
 )

stocks <- read_xlsx("/home/diego/Documentos/datascience/time-series/volatility/avaliacao-experimental/mestrado/files/Base de Dados Consolidada base line.xlsx",sheet="Stocks")
method_emd <- "CEEMD"

stocks <- stocks[stocks_name_columns]
# # cpm change points

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
stocks$Data <- as.Date(as.yearmon(stocks$Data))
# stocks <- stocks[stocks$Data < "2019-01-01",]

linreg <- function(data) {
  #browser()
  data <- as.data.frame(data)
  colnames(data) <- "x"
  data$t <- 1:nrow(data)
  
  #Adjusting a linear regression to the whole window
  lm(x~t, data)
}

get_events <- function(component) {
  
  serie <- c(rep("x", length(component)))
  type <- "events"
  df <- NULL
  events <- data.frame(time=component, serie=serie, type=type)
  
  return(events)
}

trim <- function(x) gsub("^\\s+|\\s+$", "", x)
stocks$Data <- as.Date(as.yearmon(stocks$Data))
time_series <- vector()

get_break_dates <- function(breakpoints_term, stocks) {
  return(stocks$Data[na.omit(breakpoints_term)])
}

stocks <- subset(stocks, !is.na(strtoi(trim(as.numeric(as.POSIXct(stocks$Data, format = "%Y-%m"))))))
time_series <- c(time_series, strtoi(trim(as.numeric(as.POSIXct(stocks$Data, format = "%Y-%m")))))
noise.amp <- 6.4e-9
trials <- 100

df_results <- NULL

name_stock <- ""
accuracy <- c(0)
sensitivity <- c(0)
specificity <- c(0)
precision <- c(0)
recall <- c(0)
f1 <- c(0)
df_results <- data.frame(name_stock, f1, accuracy, precision, recall)


df_domain <- NULL
name_domain_group <- ""


SFVE <- c(0)
STCP <- c(0)
SSCP <- c(0)
SSVE <- c(0)
LT <- c(0)
AD <- c(0)

df_domain <- data.frame(name_domain_group, SFVE,STCP, SSCP, SSVE, LT, AD)

for (name_column in stocks_name_columns) {
  if (name_column == experimento || name_column == "Data" || name_column == "Tipo") {
    next
  }

  stocks_tmp <- stocks
  results <- NULL
  ea <- NULL
  results <- hht::CEEMD(stocks_tmp[[name_column]], time_series, noise.amp, trials, max.sift = 200, boundary = "wave", stop.rule = "type1")
  ea <- energy_ratio_approach(2.14, 1.50, results)

  volatility_events <- ea$ve
  shock_from_significant_events <- ea$sst
  short_term_fluctuation <- ea$stf
  overall_adaptative_trend <- ea$oat
  long_term_price <- ea$lt
  if (length(results$residue) == 0){
    overall_adaptative_trend <- results$imf[,3] + results$imf[,4] + results$imf[,5] 
    long_term_price <- results$imf[,5]
  }
  

  ############## Variance change #########################
  breakpoints_short_term_fluctuation <- get_break_dates(ICSS(short_term_fluctuation), stocks_tmp)
  breakpoints_volatility_events <- get_break_dates(ICSS(volatility_events), stocks_tmp)
  breakpoints_shock_from_siginificant_events <- get_break_dates(ICSS(shock_from_significant_events), stocks_tmp)

  ############## TREND ###############################

  anomalies <-get_anomalyze(stocks_tmp[["Data"]], short_term_fluctuation, shock_from_significant_events, volatility_events)
  #anomalies_trend <- get_anomalyze_trend(stocks_tmp[["Data"]], df_type, overall_adaptative_trend, long_term_price)


  print(name_column)
  df <- data.frame(time = stocks_tmp$Data, x = short_term_fluctuation)

  reference <- stocks_tmp[c("Data", experimento)]
  
  df_short_term_fluctuation_events <- data.frame(time=stocks$Data, serie=short_term_fluctuation)
  STCP <- evtdet.seminalChangePoint2(df_short_term_fluctuation_events, w=90,mdl=linreg,na.action=na.omit)
  
  df_shock_from_significant_events <- data.frame(time=stocks$Data, serie=shock_from_significant_events)
  SSCP <- evtdet.seminalChangePoint2(df_shock_from_significant_events, w=90,mdl=linreg,na.action=na.omit)
  
  df_long_term_price_events<- data.frame(time=stocks$Data, serie=long_term_price)
  tryCatch(
    expr = {
      LT <- evtdet.seminalChangePoint2(df_long_term_price_events, w=90,mdl=linreg,na.action=na.omit)
    },
    error = function(e){ 
      print("Não existe change point")
      LT<- data.frame(time=c(), serie=c())
    }
  )
  
 
  
  
  
  reference <- stocks_tmp[c("Data", experimento)]
  time <- SSCP
  serie <- c(rep("x", nrow(SSCP)))
  type <- "events"
  events <- data.frame(time, serie, type)

  if(length(anomalies) > 0){
    length_events_anomaly = length(anomalies)
  }else {
    length_events_anomaly = 0
  }

  break_s_whithout_windowm <- vector()
  break_s_whithout_windowm <- append(break_s_whithout_windowm, as.Date(c(anomalies, anomalies_trend), format="%Y-%m-%d"))
  break_s_whithout_windowm <- append(break_s_whithout_windowm, as.Date(LT$time, format="%Y-%m-%d"))
  break_s_whithout_windowm <- append(break_s_whithout_windowm, as.Date(STCP$time, format="%Y-%m-%d"))
  break_s_whithout_windowm <- append(break_s_whithout_windowm, as.Date(SSCP$time, format="%Y-%m-%d"))
  break_s_whithout_windowm <- append(break_s_whithout_windowm, as.Date(breakpoints_short_term_fluctuation, format="%Y-%m-%d"))
  break_s_whithout_windowm <- append(break_s_whithout_windowm, as.Date(breakpoints_shock_from_siginificant_events, format="%Y-%m-%d"))
  break_s_whithout_windowm <- append(break_s_whithout_windowm, as.Date(breakpoints_volatility_events, format="%Y-%m-%d"))

 # break_s_whithout_windowm <- unique(break_s_whithout_windowm[break_s_whithout_windowm > "2004-01-01" & !is.na(break_s_whithout_windowm)])

  
  SFVE_soft = NULL
  SSCP_soft = NULL
  SSVE_soft = NULL
  LT_soft = NULL
  AD_soft = NULL
  
  
  if(length(breakpoints_short_term_fluctuation) > 0){
    SFVE_soft <-  soft_evaluate_components(get_events(breakpoints_short_term_fluctuation),stocks_tmp)
  }
  
  if(nrow(STCP) > 0){
    STCP_soft <-  soft_evaluate_components(get_events(STCP$time),stocks_tmp)
  }
  if(nrow(SSCP) > 0){
    SSCP_soft <-  soft_evaluate_components(get_events(SSCP$time),stocks_tmp)
  }
  
  if(length(breakpoints_shock_from_siginificant_events) > 0){
    SSVE_soft <-  soft_evaluate_components(get_events(breakpoints_shock_from_siginificant_events), stocks_tmp)
  }
  
  if(nrow(LT) > 0){
    LT_soft <-  soft_evaluate_components(get_events(LT$time), stocks_tmp)
  }
  
  if(length(anomalies) > 0){
    AD_soft <-  soft_evaluate_components(get_events(anomalies), stocks_tmp)
  }
  
  
  df_domain <- add_row(df_domain,
       name_domain_group = name_column,
       SFVE = length(SFVE_soft),
       STCP = length(STCP_soft),
       SSCP = length(SSCP_soft),
       SSVE = length(SSVE_soft),
       LT = length(LT_soft),
       AD = length(AD_soft)
  )
  

   break_s_whithout_windowm <- unique(break_s_whithout_windowm)
   
  
   time <- break_s_whithout_windowm
   serie <- c(rep("x", length(break_s_whithout_windowm)))
   type <- "events"
   df <- NULL
   events <- data.frame(time, serie, type)
   df <- data.frame(time = stocks_tmp$Data, x = stocks_tmp[[name_column]])
   metric <- c(
     "accuracy", "sensitivity", "specificity", "precision",
     "recall", "F1", "balanced_accuracy"
   )
   reference <- stocks_tmp[c("Data", experimento)]
   evtplot(df, events, reference)
   
   print(evtplot(df, events, reference))  
   metrics <- soft_evaluate(events, reference)
   df_results <- add_row(df_results,
     name_stock = name_column,
     f1 = metrics["F1"],
     accuracy = metrics["accuracy"],
     precision = metrics["precision"],
     recall = metrics["recall"]
   )

   print(df_results)
   print(df_domain)
}

file_metrics <- sprintf("files/stocks-msed-exp1-%s.csv", "metrics")
file_domain_groups <- sprintf("files/stocks-msed-exp1-%s.csv", "domain_groups")
write.csv(df_results, file_metrics)
write.csv(df_domain, file_domain_groups)

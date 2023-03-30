library(xts)
source("multi-scale-detect.R")
source("load_time_series.R")
library(ggplot2)
library(dplyr)


limit_ri_sup <- 2.14 # limite sup definido para obtenção do RI
limit_ri_inf <- 1.50 # limite inf definido para obtenção do RI
frequency_date <- 12 # frequencia definida para as datas 
start_date <- c(2010,1) # data de inicio da base
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

reference <-variance_change_df
serie_df <-df_serie_amplitude

#debug(multi_scale_event_detect)
results <- multi_scale_event_detect(
  serie = serie_df,
  limit_ri_sup = limit_ri_sup,
  limit_ri_inf = limit_ri_inf,
  frequency_date = frequency_date,
  start_date = start_date,
  type_events = list("cp_variance"),
  rf_dataframe = variance_change_df,
  soft_window = 5
)

######################### Change Variance
results <- multi_scale_event_detect(
  serie = serie_df,
  limit_ri_sup = limit_ri_sup,
  limit_ri_inf = limit_ri_inf,
  frequency_date = frequency_date,
  start_date = start_date,
  type_events = list("cp_variance"),
  rf_dataframe = variance_change_df,
  soft_window = 5
)



results <- multi_scale_event_detect(
  serie = serie_df,
  limit_ri_sup = limit_ri_sup,
  limit_ri_inf = limit_ri_inf,
  frequency_date = frequency_date,
  start_date = start_date,
  type_events = list("cp_variance"),
  rf_dataframe = variance_change_df,
  soft_window = 10
)


results <- multi_scale_event_detect(
  serie = serie_df,
  limit_ri_sup = limit_ri_sup,
  limit_ri_inf = limit_ri_inf,
  frequency_date = frequency_date,
  start_date = start_date,
  type_events = list("cp_variance"),
  rf_dataframe = variance_change_df,
  soft_window = 20
)








garch11 <- 
  rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), distribution.model = "norm")



df <- data.frame(time=serie_df$time, x=serie_df$value)
df <- drop_na(df)
events_garch <- evtdet.garch_volatility_outlier(df, spec=garch11,alpha=1.5)
metric=c("accuracy","sensitivity","specificity","precision",
         "recall","F1","balanced_accuracy")
print(evtplot(df, events_garch,reference))
metrics <- soft_evaluate(events_garch, reference, k =20)

df <- data.frame(time=serie_df$time, serie=serie_df$value, type = "change point")

events_scp <- evtdet.seminalChangePoint2(df, w=40,mdl=linreg,na.action=na.omit)
metric=c("accuracy","sensitivity","specificity","precision",
         "recall","F1","balanced_accuracy")
print(evtplot(df, events_scp,reference))
metrics <- soft_evaluate(events_scp, reference, k=5) 


metrics <-  soft_evaluate(events_garch, reference, k =10)



metrics <-  soft_evaluate(events_garch, reference, k =15)

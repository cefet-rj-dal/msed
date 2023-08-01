source("harbinger-v1.R")

get_anomalyze <- function(date, short_term_fluctuation) {
      # Shock short_term_fluctuation
      df_events_anomalyze <- NULL
      df_events_anomalyze <- data.frame(time = date, serie = short_term_fluctuation)
      anomalies_short_term_fluctuation <- evtdet.outliers_v1(df_events_anomalyze)
      

      anomalies = unique(c(anomalies_short_term_fluctuation$time))

      return(anomalies)
}
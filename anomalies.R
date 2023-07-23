get_anomalyze <- function(date, short_term_fluctuation, shock_from_significant_events, volatility_events) {
      # Shock short_term_fluctuation
      df_events_anomalyze <- NULL
      df_events_anomalyze <- data.frame(time = date, serie = short_term_fluctuation)
      anomalies_short_term_fluctuation <- outliers(df_events_anomalyze)
      
      
      # # Shock Significant Events
      # df_events_anomalyze_sse <- NULL
      # df_events_anomalyze_sse <- data.frame(time = date, serie = shock_from_significant_events)
      # anomalies_ssf <- outliers(df_events_anomalyze_sse)
      # 
      
      # Volatility_events
      # df_events_vo_anomalyze <- NULL
      # df_events_vo_anomalyze <- data.frame(time = date, serie = volatility_events)
      # anomalies_vo <- outliers(df_events_vo_anomalyze)
      
      #anomalies = unique(c(anomalies_short_term_fluctuation$time, anomalies_vo$time, anomalies_ssf$time))
      anomalies = unique(c(anomalies_short_term_fluctuation$time))
      
      return(anomalies)
}

get_anomalyze_trend <- function(date, overall_adaptative_trend, long_term_price) {
    ################ Anomalies Trend ##################
    df_events_anomalyze_trend_ova <- NULL
    df_events_anomalyze_trend_ova  <- data.frame(time = date, serie = overall_adaptative_trend)
    anomalies_anomalies_trend_ova <- outliers(df_events_anomalyze_trend_ova )
    
    
    df_events_anomalyze_trend <- NULL
    df_events_anomalyze_trend  <- data.frame(time = date, serie=long_term_price)
    anomalies_anomalies_trend <- outliers(df_events_anomalyze_trend)
    
    anomalies_trend = unique(c(anomalies_anomalies_trend_ova$time, anomalies_anomalies_trend$time))
    
    return(anomalies_trend)
}
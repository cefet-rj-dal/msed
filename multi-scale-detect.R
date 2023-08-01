source("componentes.R")
library(ggplot2)
library(strucchange)
require(lubridate)
library(ICSS)
library(cpm)

source("utils.R")
source("functions-models.R")
source("anomalies.R")

method_emd <- "CEEMD"

linreg <- function(data) {
  # browser()
  data <- as.data.frame(data)
  colnames(data) <- "x"
  data$t <- 1:nrow(data)

  # Adjusting a linear regression to the whole window
  lm(x ~ t, data)
}

get_events <- function(component) {
  serie <- c(rep("x", length(component)))
  type <- "events"
  events <- data.frame(time = component, serie = serie, type = type)

  return(events)
}

get_break_dates <- function(breakpoints_term, serie) {
  return(serie$time[na.omit(breakpoints_term)])
}


detect_ICSS <- function(stf, ve, sse, serie_tmp) {
  breakpoints_short_term_fluctuation <- get_break_dates(ICSS(stf), serie_tmp)
  breakpoints_volatility_events <- get_break_dates(ICSS(ve), serie_tmp)
  breakpoints_shock_from_siginificant_events <- get_break_dates(ICSS(sse), serie_tmp)

  return(list(bsse = breakpoints_shock_from_siginificant_events, bve = breakpoints_volatility_events, bstf = breakpoints_short_term_fluctuation))
}

detect_cp <- function(serie_tmp, stf, sse, lt) {
  df_short_term_fluctuation <- data.frame(time = serie_tmp$time, serie = stf)
  STCP <- NULL
  SSCP <- NULL
  LT <- NULL

  tryCatch(
    {
      STCP <- evtdet.seminalChangePoint2(df_short_term_fluctuation, w = 90, mdl = linreg, na.action = na.omit)
    },
    error = function(cond) {
      STCP <- data.frame(time = c(), serie = c())
    }
  )


  df_shock_from_significant_events <- data.frame(time = serie_tmp$time, serie = sse)

  tryCatch(
    {
      SSCP <- evtdet.seminalChangePoint2(df_shock_from_significant_events, w = 90, mdl = linreg, na.action = na.omit)
    },
    error = function(cond) {
      SSCP <- data.frame(time = c(), serie = c())
    }
  )

  df_long_term_price_events <- data.frame(time = serie_tmp$time, serie = lt)

  tryCatch(
    {
      LT <- evtdet.seminalChangePoint2(df_long_term_price_events, w = 90, mdl = linreg, na.action = na.omit)
    },
    error = function(cond) {
      LT <- data.frame(time = c(), serie = c())
    }
  )

  return(list(STCP = STCP, SSCP = SSCP, LT = LT))
}



create_breakpoints <- function(icss_detect = NULL, anomalies = NULL, cp_detect = NULL) {
  break_s_whithout_windowm <- vector()
  break_s_whithout_windowm <- append(break_s_whithout_windowm, c(anomalies))
  break_s_whithout_windowm <- append(break_s_whithout_windowm, as.Date(cp_detect$LT$time, format = "%Y-%m-%d"))
  break_s_whithout_windowm <- append(break_s_whithout_windowm, as.Date(cp_detect$STCP$time, format = "%Y-%m-%d"))
  break_s_whithout_windowm <- append(break_s_whithout_windowm, as.Date(cp_detect$SSCP$time, format = "%Y-%m-%d"))
  break_s_whithout_windowm <- append(break_s_whithout_windowm, icss_detect$bstf)
  break_s_whithout_windowm <- append(break_s_whithout_windowm, icss_detect$bsse)
  break_s_whithout_windowm <- append(break_s_whithout_windowm, icss_detect$bve)

  return(break_s_whithout_windowm)
}

soft_cp_breaks <- function(serie_tmp = serie_tmp, cp_detect = cp_detect, soft_window = soft_window) {
  SSCP_soft <- NULL
  LT_soft <- NULL
  STCP_soft <- NULL

  if (is.null(cp_detect) && nrow(cp_detect["SSCP"]) > 0) {
    SSCP_soft <- soft_evaluate_components(get_events(cp_detect$SSCP["time"]), serie_tmp, soft_window)
  }


  if (is.null(cp_detect) && nrow(cp_detect["STCP"]) > 0) {
    STCP_soft <- soft_evaluate_components(get_events(cp_detect$STCP["time"]), serie_tmp, soft_window)
  }


  if (is.null(cp_detect) && nrow(cp_detect["LT"]) > 0) {
    LT_soft <- soft_evaluate_components(get_events(cp_detect$LT["time"]), serie_tmp, soft_window)
  }

  return(list(LT_soft = LT_soft, STCP_soft = STCP_soft, SSCP_soft = SSCP_soft))
}

soft_icss_breaks <- function(df_serie, icss_detect, soft_window) {
  SFVE_soft <- NULL
  SSVE_soft <- NULL

  if (length(icss_detect$bstf) > 0) {
    SFVE_soft <- soft_evaluate_components(get_events(icss_detect$bstf), df_serie, soft_window)
  }

  if (length(icss_detect$bsse) > 0) {
    SSVE_soft <- soft_evaluate_components(get_events(icss_detect$bsse), df_serie, soft_window)
  }
  return(list(SFVE_soft = SFVE_soft, SSVE_soft = SSVE_soft))
}



multi_scale_event_detect <- function(serie = NULL, noise.amp = 6.4e-07, trials = 100, limit_ri_sup = 2.14, limit_ri_inf = 1.50, frequency_date = 12, start_date = c(2004, 1), stop_rule = "type1", type_events = list("anomaly", "cp", "cp_variance"), rf_dataframe = NULL, soft_window = 15) {
  reference <- rf_dataframe
  serie_tmp <- serie
  results <- NULL
  ea <- NULL
  results <- ceemd_trend_filter(
    serie = serie_tmp,
    limit_ri_sup = limit_ri_sup,
    limit_ri_inf = limit_ri_inf,
    frequency_date = frequency_date,
    start_date = start_date,
    stop_rule = stop_rule
  )

  components <- results$components
  time <- results$time

  ve <- components$ve
  sse <- components$sst
  stf <- components$stf
  oat <- components$oat
  lt <- components$lt

  plot(oat, main = "oat", type = "l", xaxt = "n")
  plot(stf, main = "stf", type = "l", xaxt = "n")
  plot(ve, main = "ve", type = "l", xaxt = "n")

  detect_events <- vector()

  icss_detect <- NULL
  anomalies <- NULL
  cp_detect <- NULL

  AD_soft <- NULL

  soft_icss <- NULL
  soft_cp <- NULL

  if ("cp_variance" == type_events) {
    icss_detect <- detect_ICSS(stf, ve, sse, serie_tmp)
    detect_events <- append(detect_events, as.Date(icss_detect$bstf, format = "%Y-%m-%d"))
    detect_events <- append(detect_events, as.Date(icss_detect$bsse, format = "%Y-%m-%d"))
    detect_events <- append(detect_events, as.Date(icss_detect$bve, format = "%Y-%m-%d"))
    soft_icss <- soft_icss_breaks(serie_tmp, icss_detect, soft_window)
  } else if ("anomaly" == type_events) {
    anomalies <- get_anomalyze(serie_tmp[["time"]], stf)
    detect_events <- append(detect_events, as.Date(c(anomalies), format = "%Y-%m-%d"))

    if (length(anomalies) > 0) {
      AD_soft <- soft_evaluate_components(get_events(anomalies), serie_tmp, soft_window)
    }
  } else if ("cp" == type_events) {
    cp_detect <- detect_cp(serie_tmp, stf, sse, lt)
    soft_cp <- soft_cp_breaks(serie_tmp = serie_tmp, cp_detect = cp_detect, soft_window = soft_window)
    detect_events <- vector()
    detect_events <- append(detect_events, cp_detect$LT$time)
    detect_events <- append(detect_events, cp_detect$SSCP$time)
    detect_events <- append(detect_events, cp_detect$STCP$time)
  } else {
    icss_detect <- detect_ICSS(stf, ve, sse, serie_tmp)
    anomalies <- get_anomalyze(serie_tmp[["time"]], stf)
    cp_detect <- detect_cp(serie_tmp, stf, sse, lt)
    detect_events <- create_breakpoints(icss_detect = icss_detect, anomalies = anomalies, cp_detect = cp_detect)
    soft_icss <- soft_icss_breaks(serie_tmp, icss_detect, soft_window)
    soft_cp <- soft_cp_breaks(serie_tmp = serie_tmp, cp_detect = cp_detect, soft_window = soft_window)

    if (length(anomalies) > 0) {
      AD_soft <- soft_evaluate_components(get_events(anomalies), serie_tmp, soft_window)
    }
  }
  detect_events <- unique(detect_events)
  time <- detect_events
  if (is.null(detect_events)) {
    throw("Dont have detect events in that series")
  }
  result_serie <- c(rep("x", length(detect_events)))
  type <- "events"
  if (length(detect_events) == 0) {
    events <- data.frame(time = c(), serie = c(), type = c())
  } else {
    events <- data.frame(time = as.Date(time, format = "%Y-%m-%d"), serie = result_serie, type = type)
  }
  metrics <- soft_evaluate(events = events, reference = rf_dataframe, k = soft_window)
  return(metrics)
}

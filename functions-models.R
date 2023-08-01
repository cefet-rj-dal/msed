
add_row_component <- function(name, component, df_type, series) {
  series <- series[series$Data %in% component, ]
  print(series)
  df <- data.frame(time = series$Data, Tipo = series$Tipo)
  
  df_type <- add_row(
    df_type,
    name_domain_group = name,
    POLITICA = nrow(df[df$Tipo %in% "Politica", ]),
    ECONOMICA = nrow(df[df$Tipo %in% "Economica", ]),
    PANDEMIA = nrow(df[df$Tipo %in% "Pandemia", ]),
    CALAMIDADE = nrow(df[df$Tipo %in% "Calamidade", ]),
    GUERRA = nrow(df[df$Tipo %in% "Guerra", ])
  )
  
  return(df_type)
}

add_row_anomaly <- function(component, df_type, series) {
  print(series)
  
  series <- series[series$Data %in% component, ]
  df <- data.frame(time = series$Data, Tipo = series$Tipo)
  
  df_type <- add_row(
    df_type,
    name_domain_group = "AD",
    POLITICA = nrow(df[df$Tipo %in% "Politica", ]),
    ECONOMICA = nrow(df[df$Tipo %in% "Economica", ]),
    PANDEMIA = nrow(df[df$Tipo %in% "Pandemia", ]),
    CALAMIDADE = nrow(df[df$Tipo %in% "Calamidade", ]),
    GUERRA = nrow(df[df$Tipo %in% "Guerra", ])
  )
  
  return(df_type)
}

outliers <- function(data, alpha = 1.5) {
  serie_name <- names(data)[-1]
  names(data) <- c("time", "serie")
  serie <- data$serie
  
  # ===== Boxplot analysis of data ======
  outliers.index <- function(data, alpha = 1.5) {
    org <- length(data)
    if (org >= 30) {
      q <- quantile(data)
      IQR <- q[4] - q[2]
      lq1 <- q[2] - alpha * IQR
      hq3 <- q[4] + alpha * IQR
      cond <- data < lq1 | data > hq3
      index.out <- which(cond) # data[cond,]
    }
    return(index.out)
  }
  
  # Returns index of outlier observations
  index <- outliers.index(serie, alpha)
  
  if(length(index)==0){
    return(NULL)
  }
  
  anomalies <- cbind.data.frame(
    time = data[index, "time"],
    serie = serie_name,
    type = "anomaly"
  )
  names(anomalies) <- c("time", "serie", "type")
  
  return(anomalies)
}




soft_evaluate_components <- function(events, reference, k =10) {
  
  if(is.null(events) | is.null(events$time)) stop("No detected events were provided for evaluation",call. = FALSE)
  
  names(reference) <- c("time","event")
  detected <- cbind.data.frame(time=reference$time,event=0)
  detected[detected$time %in% events$time, "event"] <- 1
  reference_vec <- cbind.data.frame(time=reference$time,logical=as.logical(reference$event))
  detected_vec <- cbind.data.frame(time=detected$time,logical=as.logical(detected$event))
  detected_true <- detected_vec[which(detected_vec$logical == TRUE), ]
  
  softScores <- soft_scores(detected_vec$logical, reference_vec$logical, k=k)
  line = 0
  date_find <- c()
  
  for (score in softScores){
    if (score > 0.6){
      date_find <- c(date_find, format(detected_true[line,]$time, "%Y-%m-%d") )
      line = line + 1 
    }
  }
  return(date_find)
}


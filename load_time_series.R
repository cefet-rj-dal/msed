library(ggplot2)
library(dplyr)

# Generate time series of volatility with change points and growth trend
set.seed(123)


events <- c(49, 96, 146, 191,  239,290,336)
x <- seq(-3*pi/2, 3*pi/2, by = 1/5)
fc1 <- sin(x*3)
fc2 <- 0.5*sin(x*3)
fc3 <- 2*sin(x*3)
fc4 <- 3*sin(x*3)

serie_crescente <- c(fc1,fc2,fc3,fc2)

for (i in 1:length(serie_crescente)){
  serie_crescente[i] <- i/100+serie_crescente[i]
}

serie_decrescente <- c(fc2,fc3,fc2,fc1)

for (i in length(serie_decrescente):1){
  serie_decrescente[i] <- 2.2+serie_decrescente[i] -i/100
}

serie_tendencia_amplitude <- c(serie_crescente, serie_decrescente)

# Plot time series with events of changing volatility and growth trend
df_serie_amplitude <- data.frame(time = seq(as.Date("2010/01/01"), length.out = 384, by = "day"), value = serie_tendencia_amplitude)
nrow(df_serie_amplitude)
ggplot(df_serie_amplitude, aes(x = time, y = value)) +
  geom_line() +
  geom_point(data = df_serie_amplitude[events, ], aes(x = time, y = value), color = "red") +
  labs(title = "Trigonometric time series with events of changing volatility and growth trend", x = "Date", y = "Value")


detect_events <- function(x) {
  df <- data.frame(time = seq(as.Date("2010/01/01"), length.out = length(x), by = "day"), value = x)
  df$value <- ifelse(df$time %in% df$time[events], 1, 0)
  return(df)
}


variance_change_df <- detect_events(serie_tendencia_amplitude)





# Generate time series with anomalies and growth trend
set.seed(123)
n <- 100
x <- rnorm(n, mean = 10, sd = 1)
anomalies <- c(20, 30, 70, 80)
x[anomalies] <- x[anomalies] + rnorm(length(anomalies), mean = 5, sd = 1)
trend <- seq(from = 1, to = n, length.out = n) * 0.1
time_series <- x + trend

# Plot time series with anomalies and growth trend
df_series_anomalies <- data.frame(time = seq(as.Date("2022/01/01"), length.out = n, by = "day"), value = time_series)
ggplot(df_series_anomalies, aes(x = time, y = value)) +
  geom_line() +
  geom_point(data = df_series_anomalies[anomalies, ], aes(x = time, y = value), color = "red") +
  labs(title = "Time series with anomalies and growth trend", x = "Date", y = "Value")


detect_anomalies <- function(x) {
  df <- data.frame(time = seq(as.Date("2022/01/01"), length.out = length(x), by = "day"), value = x)
  df$value <- ifelse(df$time %in% df$time[anomalies], 1, 0)
  return(df)
}

# Example usage of detect_anomalies function
df_anomalies <- detect_anomalies(time_series)

# anomalia 1
data(har_examples)
dataset <- har_examples[[8]]
plot(x = 1:length(dataset$serie), y=dataset$serie)
lines(x = 1:length(dataset$serie), y= dataset$serie)

reference_8_cp <- data.frame(time = seq(as.Date("2022/01/01"), length.out = length(dataset$event), by = "day"), value = as.integer(dataset$event), type='anomaly')
df_serie_8_cp <- data.frame(time = seq(as.Date("2022/01/01"), length.out = length(dataset$event), by = "day"), value = dataset$serie, type='anomaly')



# anomalia 2
data(har_examples)
dataset <- har_examples[[1]]
plot(x = 1:length(dataset$serie), y=dataset$serie)
lines(x = 1:length(dataset$serie), y= dataset$serie)

reference_1_ano <- data.frame(time = seq(as.Date("2022/01/01"), length.out = length(dataset$event), by = "day"), value = as.integer(dataset$event), type='anomaly')
df_serie_1_ano <- data.frame(time = seq(as.Date("2022/01/01"), length.out = length(dataset$event), by = "day"), value = dataset$serie, type='anomaly')


# change point

dataset <- har_examples[[4]]

plot(x = 1:length(dataset$serie), y=dataset$serie)
lines(x = 1:length(dataset$serie), y= dataset$serie)

reference_4_cp <- data.frame(time = seq(as.Date("2022/01/01"), length.out = length(dataset$event), by = "day"), value = as.integer(dataset$event), type='anomaly')
df_serie_4_cp <- data.frame(time = seq(as.Date("2022/01/01"), length.out = length(dataset$event), by = "day"), value = dataset$serie, type='anomaly')


#### change point
dataset <- har_examples[[2]]

plot(x = 1:length(dataset$serie), y=dataset$serie)
lines(x = 1:length(dataset$serie), y= dataset$serie)

reference_2_cp <- data.frame(time = seq(as.Date("2022/01/01"), length.out = length(dataset$event), by = "day"), value = as.integer(dataset$event), type='anomaly')
df_serie_2_cp <- data.frame(time = seq(as.Date("2022/01/01"), length.out = length(dataset$event), by = "day"), value = as.integer(dataset$serie), type='anomaly')






# Load required libraries
library(ggplot2)
library(changepoint)

# Generate time series with a priori defined change points and growth trend
set.seed(123)
n <- 200
x <- cumsum(rnorm(n, mean = 0, sd = 0.1))
trend <- seq(from = 1, to = n, length.out = n) * 0.02
x <- x + sin(trend * pi) + rnorm(n, mean = 0, sd = 0.5)
cpts <- c(45, 61,86,  92, 146)
cpt <- cpt.mean(x, method = "PELT", Q = length(cpts), pen.value = Inf)

# Set a priori defined change points
cpt@cpts <- cpts

# Plot time series with change points
df_cp <- data.frame(time = seq(as.Date("2022/01/01"), length.out = n, by = "day"), value = x)
ggplot(df_cp, aes(x = time, y = value)) +
  geom_line() +
  geom_point(data =df_cp[cpts, ], aes(x = time, y = value), color = "red") +
  labs(title = "Time series with a priori defined change points and growth trend", x = "Date", y = "Value")

# Function to detect change points and create data frame
detect_change_points <- function(x, cpts) {
  df <- data.frame(Data = seq(as.Date("2022/01/01"), length.out = length(x), by = "day"))
  cpt <- cpt.mean(x, method = "PELT", Q = length(cpts), pen.value = Inf)
  cpt@cpts <- cpts
  df$event <- ifelse(df$Data %in% df$Data[cpt@cpts], 1, 0)
  return(df)
}

# Example usage of detect_change_points function
debug(detect_change_points)
df_ref_cp <- detect_change_points(x, cpts)
head(df_ref_cp)

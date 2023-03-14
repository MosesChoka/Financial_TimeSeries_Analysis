library(readr)
library(tidyverse)
library(moments)
library(tseries)
library(forecast)
library(dplyr)
library(readxl)

set.seed(123)

#load data
full_dataset <- read_excel('DailyAverages2014.xls')
View(full_dataset)

# view the first 5 rows in the dataset
head(full_dataset, 5)

# print columnnames - rename AVG PRICE to average_price
names(full_dataset)

colnames(full_dataset)[8] <- 'average_price'

head(full_dataset)
names(full_dataset)

# filter data to remain with KNRE as the only issuer
dataset_KNRE <-  full_dataset %>%
  filter(ISSUER == 'KNRE')

head(dataset_KNRE, 5)

# plot a histogram of the average prices
ggplot(dataset_KNRE, aes(dataset_KNRE$average_price)) +
  geom_histogram(binwidth = 0.5, color = "black") + 
  labs(title = "A histogram showing the Distribution of KNRE stock",
       subtitle = '(For year 2014)',
       x = 'average_price')

# a histogram with a normal curve
# Create a histogram with a normal curve

ggplot(dataset_KNRE, aes(x = dataset_KNRE$average_price)) +
  geom_histogram(binwidth = 0.5, color = "black",aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(dataset_KNRE$average_price), sd = sd(dataset_KNRE$average_price)), color = "blue", size = 0.95) +
  labs(title = "A histogram showing the Distribution of KNRE stock",
       subtitle = '(A histogram with a normal curve)',
       x = 'average_price')

# Calculating simple net return Rt
# Used the average price data to calculate the returns value

prices <- dataset_KNRE$average_price
head(prices)
length(prices)

Rt <- (prices[-1] - prices[-length(prices)]) / prices[-length(prices)]
head(Rt,5)

length(Rt)


# Calculating the log returns
rt <- log(1 + Rt)
head(rt, 5)


# log returns differenced
rt2 <- diff(log(prices))
head(rt2, 5)

# Perform Kolmogorov test of normality on Rt

ks_result1 <- ks.test(Rt,"pnorm",
                     mean = mean(Rt), 
                     sd = sd(Rt))

ks_result1

ks_test2 <- ks.test(rt,"pnorm",
                    mean = mean(rt),
                    sd = sd(rt))

ks_test2

ks_test3 <- ks.test(prices, "pnorm",
                    mean = mean(prices),
                    sd = sd(prices))
ks_test3

# Perform Jarque-Bera goodness of fit test
jb_testR <- jarque.test(Rt)
jb_testR

jb_testr <- jarque.test(rt)

jb_testr

jb_testP <- jarque.test(prices)
jb_testP

# ARIMA model for simple returns Rt and rt
# Created a new data frame with Rt and rt values
data_t <- dataset_KNRE
names(data_t)

# i. Add an additional value of 0 in Rt and rt

Rt <- c(0,Rt)
rt <- c(0, rt)

# ii. Add Rt and rt columns inside data_t data

data_t$Rt <- Rt
data_t$rt <- rt

names(data_t)

head(data_t)
View(data_t)

# Simplified data, with Date, average_prices, Rt, and rt - subset

data_subset <- subset(data_t, select = c('DATE_TRADE', 'average_price','Rt', 'rt'))
View(data_subset)

# Convert the data_subset to a time series data
# Convert the 'date' column to a time series index first

library(xts)
data_subset_ts <- xts(data_subset[,-1], order.by = data_subset$DATE_TRADE)
head(data_subset_ts, 5)
class(data_subset_ts)

# Perform ARIMA on the Rt and rt column
# i. create a column for Rt data only and perform arima on it
col_Rt <- data_subset_ts[, 'Rt']
fit1 <- auto.arima(col_Rt)
print(fit1)
summary(col_Rt)

# ii. create a column for rt data only and perform arima on it
col_rt <- data_subset_ts[,'rt']
fit2 <- auto.arima(col_rt)
print(fit2)
summary(col_rt)


# ACF of Rt and rt
acf(col_Rt)
acf(col_rt)

# ACF values
acf(col_Rt, lag.max = 10, plot = FALSE)
acf(col_rt, lag.max = 10, plot = FALSE)

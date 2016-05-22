#read in the data
library(dplyr)
library(lubridate)
library(ggplot2)
setwd("/Users/Jake/Projects/MarketValuation/Data/")

cpi = read.csv("cpi.csv")
cpi$Date = as.Date(as.character(cpi$Date), format = "%d/%m/%Y")
cpi = cpi[c(1,2)]

#use cubic spline to interpolate monthly figures

xao = read.csv("xao.csv")
xao$Date = as.Date(as.character(xao$Date), format = "%d/%m/%Y")
xao = xao[c(1,2)]

data = merge(cpi, xao, by = "Date")
data = data[-c(1,2),]

#Adjust closes for inflation
base_cpi = data$CPI[1]


data = mutate(data, 
              adjusted_xao = XAO/base_cpi,
              log_adjusted_xao = log(adjusted_xao),
              observation = as.numeric(rownames(data)))


#fit
fit = lm(log_adjusted_xao ~ Date, data = data)
#get coefficients:
coef(fit)

data$predictions = predict(fit, data)
ggplot(data = data, aes(Date, log_adjusted_xao)) + geom_line() + geom_smooth(method = 'lm', se = FALSE) +
  ggtitle("All Ordinaries with Trendline") +
  ylab("Real XAO values, log scale")

#CHECK THAT LINE IS ACCURATE:
#ggplot(data = data, aes(Date, log_adjusted_xao)) + geom_line() + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2])

data = mutate(data, difference = (log_adjusted_xao - predictions))

#plot the over vs under trends
ggplot(data, aes(Date, difference)) + geom_line()  + geom_area() +
  ggtitle("Over and Under Trend") +
  ylab("Difference from Trend")


#Calculate subsequent 10 year performance from first obs.
#take obs 40-187, append as column leagged returns, 
ten_years = data$adjusted_xao[40:187]
new_data = data[1:148,]
new_data$ten_years = ten_years
new_data = mutate(new_data, returns = (ten_years - adjusted_xao)/adjusted_xao)

ggplot() + geom_line(data = new_data, colour = "red",aes(observation, difference)) + 
  geom_line(data = new_data, colour = "blue", aes(x = observation, y= returns-1)) +
  ggtitle("Difference from Trend and 10 Year Returns") +
  ylab("Difference/10 Year Returns")


#returns graph
ggplot(new_data, aes(x = Date, y = returns)) + geom_line()

#relationship with difference from trend vs 10 year returns
ggplot(new_data, aes(x = difference, y = returns)) + geom_point()+geom_smooth(method = lm) +
  ggtitle("Relationship between Difference from Trend and 10 Year Returns") +
  xlab("Difference from Trend") +
  ylab("10 Year Returns")

#Where are we now?
#new fit
new_fit = lm(ten_years ~ difference, data = new_data)

#coefficients
coef(new_fit)

#Now predicted 10 year returns:
last_difference = tail(data$difference, n = 1)

#we can also draw a horizontal line at the last difference value, to show where we stand.
ggplot(new_data, aes(x = difference, y = returns)) + geom_point()+geom_smooth(method = lm) +
  ggtitle("Relationship between Difference from Trend and 10 Year Returns") +
  xlab("Difference from Trend") +
  ylab("10 Year Returns") +
  geom_vline(xintercept = last_difference, linetype = "dashed")



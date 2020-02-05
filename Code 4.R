# Question 7:
rm(list=ls())
set.seed(123)

# Download the data into R:

setwd("~/Desktop/Georgia Tech Classes/ISyE 6501/Week 4 - Time Series Model/Homework 4/Data")
data_temps = data.frame(read.csv("temps.csv", sep=""))
summary(data_temps)
library(stats)
data_temps_un = as.vector(unlist(data_temps[, 2:21]))
ts_data_temps = ts(data_temps_un, frequency = 123, start = c(1996), end = c(2015))
plot.ts(ts_data_temps)

ts_data_tempscomponents <- decompose(ts_data_temps)
plot(ts_data_tempscomponents)
# The plot above shows the original time series (top), the estimated trend component (second from top),
# the estimated seasonal component (third from top), and the estimated irregular component (bottom). 
# We see that the estimated trend component does not show a significant increases or decreases over the 
# 20-year period.

# Let's analyze the data using Holt Winters method:

# Single exponential smoothing: Holt Winters with Alpha

hw_alpha <- HoltWinters(ts_data_temps,beta=FALSE,gamma=FALSE)
hw_alpha

# Double exponential smoothing: Holt Winters with Alpha and Beta

hw_alpha_beta <- HoltWinters(ts_data_temps,gamma=FALSE)
hw_alpha_beta
# We observe beta (0.0037) is very close to 0 (-0.0048) as well for b. We conclude that there is not 
# a significant trend.

# Triple exponential smoothing: Holt Winters with Alpha and Beta and Gamma

hw_alpha_beta_gamma <- HoltWinters(ts_data_temps)
hw_alpha_beta_gamma


# The estimated values of alpha, beta and gamma are 0.66, 0.00, and 0.62, respectively. 
# The value of alpha (0.66), indicating that the estimate of the level at 
# the current time point is based upon both recent observations and some observations in the 
# more distant past. The value of beta is 0.00, indicating that the estimate of the slope b 
# of the trend component is not updated over the time series, and instead is set equal to its 
#initial value. This makes good intuitive sense, as the level changes quite a bit over the time 
#series, but the slope b of the trend component remains roughly the same. In contrast, the value 
#of gamma (0.62), indicating that the estimate of the seasonal component at the current 
#time point is just based upon distant past observations. 

# As for simple exponential smoothing and Holt???s exponential smoothing, we can plot the original 
# time series as a black line, with the forecasted values as a red line on top of that:
plot(hw_alpha_beta_gamma)
plot(fitted(hw_alpha_beta_gamma))


# We observe from the plot that the Holt-Winters exponential method is very successful 
# in predicting the seasonal peaks, which occur roughly in the summer and winter.

# Export data to excel
coef_ = coefficients(hw_alpha_beta_gamma)[3:125]
S_t = as.data.frame(hw_alpha_beta_gamma$fitted[, 1])
data = data.frame(matrix(, nrow = 123, ncol = 18))
for (i in 1:18) {
  start = 1 + 123 * (i - 1)
  end = 123 * i
  data[, i] = S_t[start:end, 1]
}

# Exporting the data to apply CUSUM to the results.
write.csv(coef_, file = "Coefficients.csv")
write.csv(data, file = "Exp_Baseline_Responses1997-2005.csv")







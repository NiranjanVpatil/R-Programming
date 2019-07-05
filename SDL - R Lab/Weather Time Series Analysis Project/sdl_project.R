getwd()
options(repos = c(CRAN ="http://cran.rstudio.com"))
install.packages("dplyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("imputeTS")
install.packages("fpp")
install.packages("lubridate")
install.packages("knitr")
install.packages("caret")
install.packages("gmodels")
install.packages("lattice")
install.packages("gridExtra")
install.packages("kmisc")
install.packages("ROCR")
install.packages("corrplot")

library(dplyr)
library(ggplot2)
library(stringr)
library(imputeTS)
library(fpp)
library(lubridate)
library(knitr)
library(caret)
library(gmodels)
library(lattice)
library(gridExtra)
library(kmisc)
library(ROCR)
library(corrplot)

weather_org = read.csv("F:/desktop/weather.csv", header = TRUE, sep = "," ,stringsAsFactors = TRUE) 
kable(head(weather_org))
class(weather_org)

colnames(weather_org)
str(weather_org)

c(as.character(weather_org$datetime_utc[1]))
as.character(weater_org$datetime_utc)

all.equal(weather_org$thunder > 1,
          weather_org$rain == "Yes")

weather_org2 = subset(weater_org, select = -c(datetime_utc,rain,pressure))
colnames(weather_org2)

(cols_withNa <- apply(weather_org2, 2, function(x)
  sum(is.na(x))))

weather_org3 = weather_org2[complete.cases(weather_org2),]

factor_vars = names(which(sapply(weather_org3, class) == "factor"))
factor_vars = setdiff(factor_vars, "heatindex")
chisq_test_res = lapply(factor_vars, function(x) { 
  chisq.test(weather_org3[,x], weather_org3[, "heatindex"], simulate.p.value = TRUE)
})
names(chisq_test_res) = factor_vars
chisq_test_res

weather_org5 = weather_org2[complete.cases(weather_org2),]
colnames(weather_org5)

factor_vars = names(which(sapply(weather_org5, class) == "factor"))
numeric_vars = setdiff(colnames(weather_org5), factor_vars)
numeric_vars = setdiff(numeric_vars, "thunder")
numeric_vars
numeric_vars_mat = as.matrix(weather_org5[, numeric_vars, drop=FALSE])
numeric_vars_cor = cor(numeric_vars_mat)

corrplot(numeric_vars_cor)

pairs(weather_org5[,numeric_vars], col=weather_org5$thunder)

args(ts)
#converting into time series
weather.timeseries <- ts(weather_org,start = c(1997),frequency = 12)
weather.timeseries
colnames(weather.timeseries)
class(weather.timeseries)

#checking for na values
is.na(weather.timeseries)
#impute the missing values with na.mean using option median
na.mean(weather.timeseries,option = "median")

#ploting histogram or high-density vertical lines
plot(x = pressure,type = "h")

#ploting overplotted 
plot(x = pressure,type = "o", col = "red")

#plot(weather.timeseries[10:20],main = 'temp')
plot(weather.timeseries[100:200],ylab = "heatindex",xlab = "Dew_point",main = "Conditions",type = 'h')
plot(weather.timeseries[100:300],type = "o",col = "red", lty = "dashed")

#prints cycles across the years
cycle(weather.timeseries)

#Boxplot
boxplot(weather.timeseries~cycle(weather.timeseries))

#forecasting
plot(forecast(weather.timeseries))

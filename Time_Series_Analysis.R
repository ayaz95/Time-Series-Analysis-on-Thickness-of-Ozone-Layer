#importing the libraries
library(TSA)
library(tseries)

##### Task - 1

#Read the data
ozone_data <- read.csv("./Documents/RMIT_Sem_3/Time Series Analysis/Assignment-1/data1.csv", header = FALSE)
head(ozone_data)
class(ozone_data)

#renaming the rows
rownames(ozone_data) <- seq(from=1927, to=2016)
head(ozone_data)

#convert the data to timeseries format
ozone_data <- ts(as.vector(ozone_data), start=1927, end=2016)

#To check the thickness of the ozone layer thickness over the year shown using correlation value
x = ozone_data
y = zlag(ozone_data)
index = 2:length(y)
cor(x[index], y[index])

#Plotting the time series plot
plot(ozone_data,type='o',xlab="Time in Years",ylab='Thickness level in Doubson Units',main='Ozone Layer Thickness from 1927-2016')

#ScatterPlot to check the correlation
plot(y=ozone_data,x=zlag(ozone_data),col=c("red"),xlab = "Previous Year Thickness of Ozone Layer",main = "Scatter Plot of Ozone Layer Thickness")

#### Linear Model ####

#Applying the linear model to the data
model_lm = lm(ozone_data~time(ozone_data))
summary(model_lm)

#Linear model with regression line
plot(ozone_data,type='o',ylab='Thickness level in Doubson Units',xlab="Time in Years"  ,main ="Linear Trend model - Thickness of Ozone layer from 1927-2016")
# add the fitted least squares line from model1
abline(model_lm)

# Residuals Analysis for the Linear model trends

# 1. Time series residuals
res.model_lm = rstudent(model_lm)
plot(y = res.model_lm, x = as.vector(time(ozone_data)),xlab = 'Time in Year',type='o', ylab='Standardized Residuals',main = "Residuals of linear trend model - Thickness of Ozone layer from 1927 to 2016")

# 2. Residuals vs fitted trend 
plot(y=rstudent(model_lm),x=as.vector(fitted(model_lm)),xlab='Fitted Trend Values', ylab='Standardized Residuals',     
     type='n', main ="Time series plot of standardised residuals")
points(y=rstudent(model_lm),x=as.vector(fitted(model_lm)))    

# 3. Normality of the residual should be checked with histigrams
hist(rstudent(model_lm),xlab='Standardized Residuals')

# 4. Normality of the residual should be checked with qq-plots
y = rstudent(model_lm)
qqnorm(y)
qqline(y, col =2, lwd =1, lty =2)

# 5. Shapiro-Wilk Test for Normality
y = rstudent(model_lm)
shapiro.test(y)

# 6. ACF for standardized residuals
acf(rstudent(model_lm), main ="ACF of the standardized residuals")

#####   Quadratic model  #####

t = time(ozone_data)
t2 = t^2
model_qa = lm(ozone_data~ t + t2) 
summary(model_qa)

#Fitting the line 
plot(ts(fitted(model_qa)), ylim = c(min(c(fitted(model_qa), as.vector(ozone_data))), max(c(fitted(model_qa),as.vector(ozone_data)))),
     ylab='Thickness level in Doubson Units' , xlab="Time in Years" ,main = "Quadratic trend model - Thickness of Ozone layer from 1927 to 2016", type="l",lty=2,col="red")
lines(as.vector(ozone_data),type="o")

# Residuals Analysis for Quadratic model trends
# 1. Time series residuals
res.model_qa = rstudent(model_qa)
plot(y = res.model_qa, x = as.vector(time(ozone_data)),xlab = 'Time in Year',type='o', ylab='Standardized Residuals',main = "Residuals of Quadratic model trend - Thickness of Ozone layer from 1927 to 2016")

# 2. Residuals vs fitted trend 
plot(y=rstudent(model_qa),x=as.vector(fitted(model_qa)),xlab='Fitted Trend Values', ylab='Standardized Residuals',     
     type='n', main ="Time series plot of standardised residuals")
points(y=rstudent(model_qa),x=as.vector(fitted(model_qa)))    

# 3. Normality of the residual should be checked with histograms
hist(rstudent(model_qa),xlab='Standardized Residuals')

# 4. Normality of the residual should be checked with qq-plots
y = rstudent(model_qa)
qqnorm(y)
qqline(y, col =2, lwd =1, lty =2)

# 5. Shapiro-Wilk Test for Normality
y = rstudent(model_qa)
shapiro.test(y)

# 6. ACF for standardized residuals
acf(rstudent(model_qa), main ="ACF of the standardized residuals")

### Forecasting...

# create a vector of time for next five years
t = c(2017,2018,2019,2020,2021) # create a time vector for next five years 
t2 = t^2 
# create a new data frame with t and t2
new_df = data.frame(t,t2)  
forecasts = predict(model_qa,new_df, interval = "prediction")
plot(ozone_data,ylab= "Ozone Layer Thickness in Doubson Units", xlab="Time in Years" , ylim = c(-15,3),xlim=c(1927,2021),main="Time series plot for Ozone layer thickness")
# convert forecasts to time series object starting from the first 
# time steps-ahead to be able to use plot function
lines(ts(as.vector(forecasts[,1]), start = 2016), col="red", type="l",lwd=2)
lines(ts(as.vector(forecasts[,2]), start = 2016), col="blue", type="l",lwd=2)
lines(ts(as.vector(forecasts[,3]), start = 2016), col="blue", type="l",lwd=2)

legend("topright", lty=1, pch=1,bty = "n", col=c("black","blue","red"), text.width = 18,
       c("Data","5% forecast limit", "Forecasts"))


########## Task - 2

#Read the data
ozone_data <- read.csv("./Documents/RMIT_Sem_3/Time Series Analysis/Assignment-1/data1.csv", header = FALSE)
head(ozone_data)
class(ozone_data)

#convert the data to timeseries format
ozone_data <- ts(as.vector(ozone_data), start=1927, end=2016)

plot(ozone_data, type="o",ylab="Thickness of Ozone Layer")

#Plotting the series in acf and pacf plot.
par(mfrow=c(1,2))
acf(ozone_data)
pacf(ozone_data)
par(mfrow=c(1,1))

#Checking the p-value using adf test
adf.test(ozone_data)

#apply box-cox transformation
ozone_transform = BoxCox.ar(ozone_data + abs(min(ozone_data))+1)
ozone_transform$ci

#Checking the normality using qq-plot 
lambda = 1.2 
ozone_data = ozone_data + abs(min(ozone_data))+1
ozone_data_BC = (ozone_data^lambda-1)/lambda # apply the Box-Cox transformation
par(mfrow=c(1,1))
qqnorm(ozone_data_BC)
qqline(ozone_data_BC, col = 2)
shapiro.test(ozone_data_BC)

#plot the graph after applying box-cox transformation
plot(ozone_data_BC,type='o',ylab='Ozone Layer Thickness')

#Let's calculate the first difference and plot the first differenced series
diff_ozone_data_BC = diff(ozone_data)
plot(diff_ozone_data_BC,type='o',ylab='Ozone Layer Thickness')

#Checking the p-value using adf test
adf.test(diff_ozone_data_BC)

#Plotting the series using acf and pacf plot
par(mfrow=c(1,2))
acf(diff_ozone_data_BC)
pacf(diff_ozone_data_BC)
par(mfrow=c(1,1))

#Plotting the series using eacf plot after differentiation
eacf(diff_ozone_data_BC)

#Plotting the series using BIC table after differentiation
par(mfrow=c(1,1))
res4 = armasubsets(y=diff_ozone_data_BC,nar=6,nma=6,y.name='test',ar.method='ols')
plot(res4)

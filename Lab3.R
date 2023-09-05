############# Question 1 ################
data1 = read.csv('data1.csv', header = F)
data2 = read.csv('data2.csv', header = F)
data3 = read.csv('data3.csv', header = F)
data4 = read.csv('data4.csv', header = F)

par(mfrow = c(2,2))
plot(data1$V1, type = 'l', col = 'blue', ylab = 'data1', main = 'Data1')
plot(data2$V1, type = 'l', col = 'blue', ylab = 'data2', main = 'Data2')
plot(data3$V1, type = 'l', col = 'blue', ylab = 'data3', main = 'Data3')
plot(data4$V1, type = 'l', col = 'blue', ylab = 'data4', main = 'Data4')

#install.packages('tseries')
library(tseries)

#DF Test
adf.test(data1$V1, k = 0) #stationary
adf.test(data2$V1, k = 0) #stationary
adf.test(data3$V1, k = 0) #non-stationary
adf.test(data4$V1, k = 0) #non-stationary

#ADF Test
adf.test(data1$V1) #stationary
adf.test(data2$V1) #stationary
adf.test(data3$V1) #non-stationary
adf.test(data4$V1) #non-stationary

#pptest

pp.test(data1$V1) #stationary
pp.test(data2$V1) #stationary
pp.test(data3$V1) #non-stationary
pp.test(data4$V1) #non-stationary

#KPSS test
kpss.test(data1$V1) #stationary
kpss.test(data2$V1) #stationary
kpss.test(data3$V1) #non-stationary
kpss.test(data4$V1) #non-stationary

# data3 and data4 are non stationary

#removing stationarity by differencing
data3_diff1 = diff(data3$V1)
adf.test(data3_diff1)


data4_diff1 = diff(data4$V1)
adf.test(data4_diff1)

################# Question 2 ###################

data <- read.csv('sp500_return.csv')
head(data)

adf.test(data$x, k = 0) #stationary
adf.test(data$x)  #stationary
pp.test(data$x)   #stationary
kpss.test(data$x) #stationary

#data is stationary
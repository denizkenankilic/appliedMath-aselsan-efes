price=read.csv('/Users/deniz.kilic/Desktop/Big Data/R Denemeler/ASELSAN Stocks/efes_table.csv',header=TRUE)
# price=price[order(price$Date,decreasing=FALSE),]
trend=read.csv("/Users/deniz.kilic/Desktop/Big Data/R Denemeler/ASELSAN Stocks/efes_multiTimeline.csv",header=TRUE)
plot(trend$Anadolu.Efes,type='l')
plot(price$Close,type='l',xlab='Time',ylab='ClosePrices',main='Weekly Close Prices of aapl')

gootrend = trend$Anadolu.Efes
trendret = diff(gootrend)/gootrend[-length(gootrend)]
d2 = diff(trend$Anadolu.Efes)

a=price$Close
d1=diff(price$Close)
ret=diff(a)/a[-length(a)]
logd1=diff(log(price$Close))
sd1=diff(sqrt(price$Close))

par(mfrow=c(3,1))
plot(d1,type='l',xlab='Time',ylab='Difference',main='First Degree
     Differencing on Raw Data')
plot(logd1,type='l',xlab='Time',ylab='Difference',main='First
     Degree Differencing on Logged Data')
plot(sd1,type='l',xlab='Time',ylab='Difference',main='First
     Degree Differencing on Square-root Data')

par(mfrow=c(2,1))
acf(sd1,main='Autocorrelation Function of the First Differences')
pacf(sd1,main='Partial Autocorrelation Function of the First
     Differences')

sd2=diff(sd1)
par(mfrow=c(2,1))
acf(sd2,main='Autocorrelation Function of the Second
    Differences')
pacf(sd2,main='Partial Autocorrelation Function of the Second
     Differences')
arima(sqrt(price$Close),order=c(0,2,1))

correlation = cor(ret[1:260],trendret[6:265])
correlation
correlation = cor(a[1:260],trend$Anadolu.Efes[6:265])
correlation
correlation = cor(a[1:260],trend$Anadolu.Efes[1:260])
correlation
correlation = cor(a[1:260],trend$Anadolu.Efes[1:260])
correlation

d_1=data.frame(ret[1:260],trendret[6:265])
mod1=lm(ret[1:260]~trendret[6:265], data=d_1)
summary(mod1)

d_2=data.frame(d1[1:260],trend$Anadolu.Efes[6:265])
mod2=lm(d1[1:260]~trend$Anadolu.Efes[6:265], data=d_2)
summary(mod2)
plot(trend$Anadolu.Efes[6:265],d1[1:260],xlab='News Values',ylab='Weekly Changes in Stock
Prices',main='Changes in Stock Prices VS News with Regression
     Line')
abline(a=summary(mod2)$coefficients[1],b=summary(mod2)$coefficients
       [2])

d_3=data.frame(a[1:260],trend$Anadolu.Efes[6:265])
mod3=lm(a[1:260]~trend$Anadolu.Efes[6:265], data=d_3)
summary(mod3)

ts1 <- ts(price$Close ,frequency = 52)
stl1 <- stl(ts1, s.window = "periodic")
plot(stl1)

ts2 <- ts(trend$Anadolu.Efes ,frequency = 52)
stl2 <- stl(ts2, s.window = "periodic")
plot(stl2)

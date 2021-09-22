rm(list = ls())
library(forecast)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
library(tseries)
library(lmtest)
library(nortest)
library(rugarch)
library(lubridate)

# step zero
# random draw from a normal distribution
set.seed(18749)
n<-1000
z<-rnorm(n,mean=0,sd=1.5)
# set up parameters
mu<-5
delta<-0.1
phi1<-1
phi2<-0.9
# set up vectors
nu1<-numeric(n)
nu2<-numeric(n)
y1<-numeric(n)
y2<-numeric(n)
y1[1]<-mu+delta*1+nu1[1]
y2[1]<-mu+delta*1+nu2[1]
# create y period by period
for (i in 2:n){
  nu1[i]<-phi1*nu1[i-1]+z[i]
  nu2[i]<-phi2*nu2[i-1]+z[i]
  y1[i]<-mu+delta*i+nu1[i]
  y2[i]<-mu+delta*i+nu2[i]
}
# plot and compare
plot(1:n,y1,type="b", pch=19, col="red", xlab="x", ylab="y", ylim=c(0,120))
lines(1:n,y2,pch=18, col="blue", type="b", lty=2,ylim=c(0,120))
legend(1, 55, legend=c("I(1)", "I(0)"),col=c("red", "blue"), lty=1:2, cex=0.8)


## Read data file and name the data file as Vol
Vol<-read.csv(choose.files(), header = TRUE, sep=",")
names(Vol)
## Date format
DATE<-as.Date(as.character(Vol$Date), "%Y%m%d")


## Define variable PriceX and create variable log price,
##which is the log value of PriceX
PriceX<-Vol$PriceX
logprice<-log(PriceX)

##Step 1
jpeg(filename = "Case2_LogPrice.jpeg")
plot(DATE,logprice,col='blue',type= 'l',main = 'Time-series plot of log of SP500 Index Price')
dev.off()

## Load in tseries package and use adf.test() function
##to fullfill Dickey Fuller testing
adf.test(logprice, k=0)

#Step2.2
#augmented Dickey Fuller testing
adf.test(logprice)

#Step3
jpeg(filename = "Case2_MarketReturn.jpeg")
plot(DATE,Vol$SP500RET,type='l',col='red')
dev.off()
#Step3.1
## Define sp500RET
sp500RET<-Vol$SP500RET
## Construct AR(1) model for sp500RET
AR_1<-arima(sp500RET, order= c(1, 0, 0))
## Construct AR(13) model for sp500RET
AR_13<-arima(sp500RET, order= c(13, 0, 0))
coeftest(AR_1)
coeftest(AR_13)

#Step3.2
AR_Multiple<-list(NA)
for(i in 1:13){
  AR_Multiple[[i]]<-arima(sp500RET, order= c(i, 0, 0))
}

#Step3.3
## Create a numeric variable AR_AIC
AR_AIC<-rep(NA, 13)
## Calculate AIC for each AR model in AR_Multiple
for(i in 1:13){
  AR_AIC[i]<-AIC(AR_Multiple[[i]])
}
## Find the AR model that has the lowest AIC
AR_Multiple[[which(AR_AIC==min(AR_AIC))]]
##Create a line plot with AIC values on the y-axis and the number of lags p on
##the x-axis
jpeg(filename = "Case2_AR_AIC.jpeg")
plot(AR_AIC,type = 'line',main = "AIC from AR(p) for SP500 Index Return")
dev.off()
##BIC
AR_BIC<-rep(NA, 13)
for(i in 1:13){
  AR_BIC[i]<-BIC(AR_Multiple[[i]])
}
AR_Multiple[[which(AR_BIC==min(AR_BIC))]]
Name<-1:13
data.frame(AR_AIC,AR_BIC,row.names =Name,check.names = TRUE)
jpeg(filename = "Case2_AR_BIC.jpeg")
plot(AR_BIC,type = 'line',main = "BIC from AR(p) for SP500 Index Return")
dev.off()

#Step4.1
##Construct MA(1) model
MA_1<-arima(sp500RET, order= c(0, 0, 1))
##Construct MA(2) model
MA_2<-arima(sp500RET, order= c(0, 0, 2))

#Step4.2
## Create a list variable to store several AMRA models
ARMA_Multiple<-list(NA)
## Create an index variable to direct the storage of ARMA models
index<-0
## Use for() function to create several ARMA models
## p: AR order
## q: MA order
for(p in 0:2){
  for(q in 0:2){
    index<-index+1
    ARMA_Multiple[[index]]<-arima(sp500RET,order=c(p, 0, q))
  }
}

#Step4.3
## Table
ARMA_AIC<-matrix(data=NA,nrow = 3, ncol = 3)
ARMA_BIC<-matrix(data=NA,nrow = 3, ncol = 3)
index<-0
for(p in 0:2){
  for(q in 0:2){
    index<-index+1
    ARMA_AIC[p+1,q+1]<-AIC(ARMA_Multiple[[index]])
    ARMA_BIC[p+1,q+1]<-BIC(ARMA_Multiple[[index]])
      }
}
rownames(ARMA_AIC)<-0:2
colnames(ARMA_AIC)<-0:2
as.table(ARMA_AIC)
rownames(ARMA_BIC)<-0:2
colnames(ARMA_BIC)<-0:2
as.table(ARMA_BIC)

ARMA_Multiple[[which(ARMA_AIC==min(ARMA_AIC))]]

#Step4.4
ARMA_Multiple[[which(ARMA_BIC==min(ARMA_BIC))]]
#Step4.5
##ARMA(1,1) Diagnosis Test
res<-ARMA_Multiple[[5]]$residuals

jpeg(filename = "Case2_Box_Residuals.jpeg")
boxplot(res,main = "residual box plot")
dev.off()

jpeg(filename = "Case2_ResHist.jpeg")
hist(res,main = "Histogram of Residuals",breaks=50,prob=T )
curve(dnorm(x,mean=0,sd(res)),add = T)
dev.off()

jpeg(filename = "Case2_QQPlot_Residuals.jpeg")
qqnorm(res, main="Q-Q plot of Residual")
qqline(res)
dev.off()

jarque.bera.test(res)
lillie.test(res)

lg<-length(res)
jpeg(filename = "Case2_ScatterPlot_Residuals.jpeg")
plot(res[1:lg-1],res[2:lg], main = "Scatter Plot of Residuals", xlab = "Residuals,t",ylab = "Residuals, t+1")
dev.off()
Box.test(res,type = "Ljung", lag=5)
Box.test(res,type = "Ljung", lag=10)
Box.test(res,type = "Ljung", lag=15)
Box.test(res,type = "Ljung", lag=20)

jpeg(filename = "Case2_ResidualTimeSeries.jpeg")
plot(DATE,res,type="line",main = "Residuals",xlab="DATE",ylab="residuals")
dev.off()
#Step4.6
ARMA11<-ARMA_Multiple[[5]]
fits<-fitted(ARMA11)
jpeg(filename = "Case2_fits.jpeg")
plot(DATE, sp500RET, main="SP500 Returns vs Fits",
     xlab= "DATE", ylab="Returns", type="l",col="black")
lines(DATE,fits,col="red")
dev.off()
accuracy(ARMA11)

#Step4.7
# fig1
jpeg(filename = "Case2_forecast1.jpeg")
lg<-NROW(sp500RET)
fcast <-forecast(ARMA11,h=252)
plot(fcast)
dev.off()


# fig2
lg<-NROW(sp500RET)
jpeg(filename = "Case2_forecast2.jpeg")
fit_no_holdout = arima(ts(sp500RET[-c((lg-252+1):lg)]), order=c(1,0,1))
fcast_no_holdout <- forecast(fit_no_holdout,252)
plot(fcast_no_holdout, main="Forecasts from ARIMA(1,0,1) with non-zero mean h=252",col='blue')
lines(ts(sp500RET))
dev.off()
# fig3
jpeg(filename = "Case2_forecast3.jpeg")
plot(ts(sp500RET[(lg-252+1):lg]), main=" Forecasts h=252")
lines(1:252,fcast_no_holdout$mean ,col='blue',lwd = 5)
dev.off()

accuracy(fit_no_holdout)
accuracy(fcast_no_holdout,sp500RET[(lg-252+1):lg])

#Step5.1
## Create variable of ARMA(1,1) residuals and variable
##of the square of ARMA(1,1) residuals
ARMA_residual<-ARMA_Multiple[[which(ARMA_BIC==min(ARMA_BIC))]]$residuals
ARMA_residual_square<-ARMA_residual^2
## Construct auto-correlation plot for residuals of
##ARMA(1,1) model
jpeg(filename = "Case2_AutocorrelationPlot.jpeg")
acf(ARMA_residual_square, main="Autocorrelation Function of Squared Residuals", ylab="Sample Autocorrelation", ylim=c(-0.2,1),lag.max =50)
dev.off()

#Step5.2
## Construct partial auto-correlation plot for residuals of ARMA(1,1) model
jpeg(filename = "Case2_PAutocorrelationPlot.jpeg")
pacf(ARMA_residual_square, main="Partial Autocorrelation Function of Squared Residuals", ylab="Sample Autocorrelation", ylim=c(-0.2,1),lag.max =50)
dev.off()
library(aTSA)
arch.test(ARMA11)

#5.4
numlags<-4
y<-ARMA_residual_square[(numlags+1):lg]
x1<-ARMA_residual_square[(numlags+1-1):(lg-1)]
x2<-ARMA_residual_square[(numlags+1-2):(lg-2)]
x3<-ARMA_residual_square[(numlags+1-3):(lg-3)]
x4<-ARMA_residual_square[(numlags+1-4):(lg-4)]
Model<-lm( y~x1+x2+x3+x4)
EngleTestStats<-summary(Model)$"r.squared"*lg
1-pchisq(EngleTestStats, df=numlags)

numlags<-4
## Create a variable to store outputs of ARCH model
Arch_Multiple<-list(NA)
## Create a variable to store AIC of ARCH model
Arch_AIC<-rep(NA,4)
## Construct ARCH models and calculate AIC of each ARCH model
for(i in 1:numlags){
  Arch_Multiple[[i]]<-garch(ARMA_residual, order=c(0,i))
  Arch_AIC[i]<-AIC(Arch_Multiple[[i]])
}
## Identify the ARCH model that has the lowest AIC
Arch_Multiple[which(Arch_AIC==min(Arch_AIC))]
p<-0
q<-4
ARCHmodel<-garch(ARMA_residual, order=c(p,q))

#Step5.5
htest<-Box.test(ARMA_residual_square,type = "Ljung",lag = 5)
htest<-Box.test(ARMA_residual_square,type = "Ljung",lag = 10)
htest<-Box.test(ARMA_residual_square,type = "Ljung",lag = 15)
htest<-Box.test(ARMA_residual_square,type = "Ljung",lag = 20)

#Step6.1
## Construct Garch(1, 1) model
garch_11<-garch(ARMA_residual, order=c(1,1))
## Find the fitted value of Garch(1,1) model
garch_11_fitted<-garch_11$fitted.values[,1]
jpeg(filename = "Case2_Time-seriesVolatility.jpeg")
plot(DATE,garch_11_fitted,main = "Time-series plot of GARCH(1,1) Implied Volatitlity",type= 'line',col='blue',ylab='',xlab='')
dev.off()

#Step 6.2
## specify a ARMA(1,1)+ GARCH(1,1)
Garch <- ugarchspec(variance.model = list(model = "sGARCH",
                                          garchOrder = c(1, 1)), mean.model = list(armaOrder = c(1, 1),
                                                                                   include.mean = TRUE),distribution.model = "norm")
## Fit GARCH(1,1) model
Garch_11 <- ugarchfit(Garch, sp500RET)
## Weighted ljung-Box Test
## Weighted ARCH LM Tests
## Nyblom stability test
## Sign bias test
## Adjusted pearson Goodness of fit tests
ugarchfit(Garch, sp500RET)
## Find the fitted value of garch
Garch_11_fitted<-fitted(Garch_11)
plot(DATE[516:5540], Garch_11_fitted,type = "l", xlab="", ylab = "", main = "Time-series plot of ARMA(1,1)+GARCH(1,1) Implied Volatility")
##Information Criteria1
numobs<-5025
AIC<-rep(NA,8)
BIC<-rep(NA,8)
SIC<-rep(NA,8)
HQIC<-rep(NA,8)
ii<-6
for (i in 1:8){
  AIC[i]<- 2*ii/numobs
  BIC[i]<- ii*log(numobs)/numobs
  SIC[i]<- log((numobs2+2*iii)/numobs2)
  HQIC[i]<- 2*ii*log(numobs)/numobs
  ii=ii+2
}
x<-c(6,8,10,12,14,16,18,20)
plot(x,AIC,type='l',lty=1,col="red",xlim=c(6,20),ylim=c(0.00,0.07),main="Penalizing Rates",xlab="",ylab="")
lines(x,BIC,lty=2)
lines(x,SIC,lty=3)
lines(x,HQIC,lty=4)
legend("topleft",c("AIC","BIC","SIC","HQIC"),lty=c(1,2,3,4),col=c("red","black","black","black"))
##Information Criteria12
numobs2<-120
AIC2<-rep(NA,8)
BIC2<-rep(NA,8)
SIC2<-rep(NA,8)
HQIC2<-rep(NA,8)
iii<-6
for (i in 1:8){
  AIC2[i]<- 2*iii/numobs2
  BIC2[i]<- iii*log(numobs2)/numobs2
  SIC2[i]<- log((numobs2+2*iii)/numobs2)
  HQIC2[i]<- 2*iii*log(numobs2)/numobs2
  iii=iii+2
}
x2<-c(6,8,10,12,14,16,18,20)
plot(x2,AIC2,type='l',lty=1,col="red",xlim=c(6,20),ylim=c(0.1,1.6),main="Penalizing Rates",xlab="",ylab="")
lines(x2,BIC2,lty=2)
lines(x2,SIC2,lty=3)
lines(x2,HQIC2,lty=4)
legend("topleft",c("AIC","BIC","SIC","HQIC"),lty=c(1,2,3,4),col=c("red","black","black","black"))

##plot Garch_11
jpeg(filename = "Case2_Garch11fit.png")
plot(Garch_11, which='all')
dev.off()
#Step 6.3 ARMA(1,1)+EGARCH(1,1)
## Specify the eGARCH model and eGARCH model order
eGarch = ugarchspec(variance.model = list(model = "eGARCH",
                                          garchOrder = c(1, 1)))
eGarch_11<-ugarchfit(eGarch, sp500RET)
ugarchfit(eGarch, sp500RET)
jpeg(filename = "Case2_eGarch11fit.png")
plot(eGarch_11, which='all')
dev.off()
#Step 6.4 ARMA(1,1)+GJR(1,1)
## Specify the GJR model and GJR model order
gjr = ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1, 1)))
gjr_11<-ugarchfit(gjr,sp500RET)
ugarchfit(gjr, sp500RET)
jpeg(filename = "Case2_gjrfit.png")
plot(gjr_11, which='all')
dev.off()

#Step 7 Time-Series plot for RV
RV<-Vol$RV
jpeg(filename = "RV.jpeg")
plot(DATE,RV,col='blue',type= 'l',main = 'Time-series plot of log of RV')
dev.off()
#Step 8 Time-Series plot for VIX
VIX<-Vol$VIX2
jpeg(filename = "VIX.jpeg")
plot(DATE,VIX,col='blue',type= 'l',main = 'Time-series plot of log of VIX')
dev.off()
#Step 9 Time-Series plot for VRP
VRP<-Vol$VRP
jpeg(filename = "VRP.jpeg")
plot(DATE,VRP,col='blue',type= 'l',main = 'Time-series plot of log of VRP')
dev.off()


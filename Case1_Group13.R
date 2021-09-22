#example 1.5
setwd("//hd.ad.syr.edu/01/970687/Documents/Fin 654")
CAPM<-read.csv('Case1CAPM.csv', header = TRUE, sep=",")
names(CAPM)


##Step 0: Change the class of vairable DATE to be "Date"
DATE<-as.Date(as.character(CAPM$DATE), "%Y%m%d")
## read in date info in format 'ddmmmyyyy'
x <- c("1jan1960", "2jan1960", "31mar1960", "30jul1960")
z <- as.Date(x, "%d%b%Y")
## read in date/time info in format 'm/d/y'
dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
z <- as.Date(dates, "%m/%d/%y")
#Code Value
#%d Day of the month (decimal number)
#%m Month (decimal number)
#%b Month (abbreviated)
#%B Month (full name)
#%y Year (2 digit)
#%Y Year (4 digit)
##Step 1.1 Create the excess returns of IBM
ibmRET<-CAPM$IBMRET
marketEXERT<-CAPM$MarketEXRET
RF<-CAPM$RF
IBMEXERT<-ibmRET-RF
##Step 1.2 Create yearly excess returns
lg<-length(ibmRET)
IBMEXERT_Annualized<-rep(NA,lg)
marketEXERT_Annualized<-rep(NA,lg)
for (i in 252:lg){
  IBMEXERT_Annualized[i]<-(prod(IBMEXERT[(i-252+1):(i)]/100+1)-1)*100
  marketEXERT_Annualized[i]<-(prod(marketEXERT[(i-252+1):(i)]/100+1)-1)*100
}
## Step 1.3 Time-Series Plot of Yearly returns
jpeg(filename = "Case1_marketEXERT_Annualized.jpeg")
plot(DATE[252:lg], marketEXERT_Annualized[252:lg], type = "l",
     col="blue", xlab="", ylab="",
     main="Daily Market Excess Return (annualized percentage)", ylim=c(-60, 160))
dev.off()

## Step 1.3 Time-Series Plot of Yearly returns
jpeg(filename = "Case1_IBMEXERT_Annualized.jpeg")
plot(DATE[252:lg], IBMEXERT_Annualized[252:lg], type = "l",
     col="blue", xlab="", ylab="",
     main="Daily IBM Excess Return (annualized percentage)", ylim=c(-60, 160))
dev.off()
## find the global maximum
maximum<-max(marketEXERT_Annualized,na.rm=T)
maxvalue<-grepl(maximum, marketEXERT_Annualized)
findmax<-which(maxvalue)
DATE[findmax]
minimum<-min(marketEXERT_Annualized,na.rm=T)
minvalue<-grepl(minimum, marketEXERT_Annualized)
findmin<-which(minvalue)
DATE[findmin]


## find the IBM maximum
maximum<-max(IBMEXERT_Annualized,na.rm=T)
maxvalue<-grepl(maximum, IBMEXERT_Annualized)
findmax<-which(maxvalue)
DATE[findmax]
## find the IBM minimum
minimum<-min(IBMEXERT_Annualized,na.rm=T)
minvalue<-grepl(minimum, IBMEXERT_Annualized)
findmin<-which(minvalue)
DATE[findmin]
range(IBMEXERT_Annualized)

#5 year expected return 
IBMEXERT_5year<-rep(NA,lg)
marketEXERT_5year<-rep(NA,lg)
for (i in 1260:lg){
  IBMEXERT_5year[i]<-(prod(IBMEXERT[(i-1260+1):(i)]/100+1)^0.2-1)*100
  marketEXERT_5year[i]<-(prod(marketEXERT[(i-(1260)+1):(i)]/100+1)^0.2-1)*100
}
mean(IBMEXERT_5year,na.rm = T)

#marketEXERT_5Year is the return computed in step 1.4
jpeg(filename = "Case1_marketEXERT_5year.jpeg")
plot(DATE[(252*5):lg], marketEXERT_5year[(252*5):lg], type = "l",
     col="blue",xlab="", ylab="",
     main="Daily Market Excess Return (annualized percentage)", ylim=c(-10, 60))
dev.off()

#IBMEXERT_5Year is the return computed in step 1.4
jpeg(filename = "Case1_IBMEXERT_5year.jpeg")
plot(DATE[(252*5):lg], IBMEXERT_5year[(252*5):lg], type = "l",
     col="blue",xlab="", ylab="",
     main="Daily Market Excess Return (annualized percentage)", ylim=c(-10, 60))
dev.off()

## find the IBM max 5 year
maximum<-max(IBMEXERT_5year,na.rm=T)
maxvalue<-grepl(maximum,IBMEXERT_5year)
findmax<-which(maxvalue)
DATE[findmax]
## find the IBM minimum 5 year
minimum<-min(IBMEXERT_5year,na.rm=T)
minvalue<-grepl(minimum,IBMEXERT_5year)
findmin<-which(minvalue)
DATE[findmin]
range(IBMEXERT_5year)

#step2BoxPlotIBM&MARKET
jpeg(filename = "Case1_IBMEXER_BOX_PLOT.jpeg")
boxplot(IBMEXERT)
dev.off()

jpeg(filename = "Case1_MarketEXER_BOX_PLOT.jpeg")
boxplot(marketEXERT)
dev.off()

## Step 2.2: Scatter Plot
jpeg(filename = "Case1_ScatterPlotReturn.jpeg")
plot(IBMEXERT~marketEXERT, xlab="Daily Market Excess returns (percentage)",
     ylab="Daily IBM Excess returns (percentage)")
dev.off()

## Step 2.3: Numerical Moments
library(e1071)
##Compute Descriptive Statistics for market excess return in daily percentage.
MKTmean<-round(mean(marketEXERT)*252,4)
MKTsd<-round(sd(marketEXERT)*sqrt(252),4)
MKTskew<-round(skewness(marketEXERT),4)
MKTkurto<-round(kurtosis(marketEXERT),4)
MKTmin<-round(min(marketEXERT),4)
MKTmax<-round(max(marketEXERT),4)
MKTsr<-round(MKTmean/MKTsd,4)
MKTVaR<-round(quantile(marketEXERT, probs = c(0.05)),4)
#Expected Shortfall
numES<-lg*0.05
numESInteger<-floor(numES)
numESDecimal<-numES-numESInteger
datasort<-sort(marketEXERT, decreasing = FALSE)
MKTES<-round(sum(datasort[1:numESInteger]+
                   datasort[numESInteger+1]*numESDecimal)/numES,4)
##Compute Descriptive Statistics for IBM excess return in daily percentage.
IBMmean<-round(mean(IBMEXERT)*252,4)
IBMsd<-round(sd(IBMEXERT)*sqrt(252),4)
IBMskew<-round(skewness(IBMEXERT),4)
IBMkurto<-round(kurtosis(IBMEXERT),4)
IBMmin<-round(min(IBMEXERT),4)
IBMmax<-round(max(IBMEXERT),4)
IBMsr<-round(IBMmean/IBMsd,4)
IBMVaR<-round(quantile(IBMEXERT, probs = c(0.05)),4)
#Expected Shortfall
numES<-lg*0.05
numESInteger<-floor(numES)
numESDecimal<-numES-numESInteger
datasort<-sort(IBMEXERT, decreasing = FALSE)
IBMES<-round(sum(datasort[1:numESInteger]+
                   datasort[numESInteger+1]*numESDecimal)/numES,4)
## compute the correlation
IBMcMarket<-round(cor(IBMEXERT, marketEXERT),4)
## Construct each column of our table.
Name<-c("Mean:", "Std:", "Skewness:", "Kurtosis:",
        "Sharpe Ratio","Value at Risk","Expected Shortfall","Correlation:" )
IBM<-c(IBMmean, IBMsd, IBMskew, IBMkurto, IBMsr, IBMVaR, IBMES, IBMcMarket)
Market<-c(MKTmean, MKTsd, MKTskew, MKTkurto, MKTsr,MKTVaR, MKTES, NA)
data.frame(IBM, Market,row.names =Name,check.names = TRUE)

#step3.2
jpeg(filename = "Case1_Market_Histogram.jpeg")
hist(marketEXERT,breaks=30,prob=T,main = "Daily Market Excess Returns (Percentage)")
D<-seq(from=MKTmin,to=MKTmax,by=0.1)
lines(D,y=dnorm(D,mean(marketEXERT),sd(marketEXERT)),col="blue")
dev.off()

jpeg(filename = "Case1_IBM_Histogram.jpeg")
hist(IBMEXERT,breaks=100,probability = TRUE, main = "Daily IBM Excess returns (percentage)",ylim =c(0, 0.25))
E<-seq(from=IBMmin,to=IBMmax,by =0.1)
lines(E,y=dnorm(E,mean(IBMEXERT),sd(IBMEXERT)),col="blue")
dev.off()


#step 3.2
jpeg(filename = "Case1_QQmarketEXERT.jpeg")
qqnorm(marketEXERT, main="Q-Q plot of Market returns")
qqline(marketEXERT)
dev.off()
#step 3.2 IBM
jpeg(filename = "Case1_QQIBMEXERT.jpeg")
qqnorm(IBMEXERT, main="Q-Q plot of Market returns")
qqline(IBMEXERT)
dev.off()
#step3.3
library(tseries)

jarque.bera.test(IBMEXERT)
jarque.bera.test(marketEXERT)
#step 3.4
library(nortest)
lillie.test(IBMEXERT)
lillie.test(marketEXERT)
Model<-lm(IBMEXERT~marketEXERT)
summary(Model)
#step 8
jpeg(filename = "Case1_OLSLINE.jpeg")
plot(marketEXERT, IBMEXERT,
     main="Scatter Plot of IBM Excess returns Vs. Market Excess returns",
     xlab= "Market Excess returns", ylab="IBM Excess returns")
abline(lm(IBMEXERT~marketEXERT), col="blue")
dev.off()

## Step 9.1: Is the adjusted returns zero
testValue<-0
Model<-lm(IBMEXERT~marketEXERT)
## Calculate test value by extract coefficients and standard error from model
tstats<-(Model$coefficients[1]-testValue)/
  summary(Model)[["coefficients"]][1,2]
## Use ifelse function to return whether we should accept or reject the hypothesis
Result<-ifelse(abs(tstats)>qt(0.975, length(marketEXERT)),
               "Reject", "Can't Reject")



testValue<-1
Model2<-lm(IBMEXERT~marketEXERT)
## Calculate test value by extract coefficients and standard error from model
tstats2<-(Model2$coefficients[2]-testValue)/
  summary(Model2)[["coefficients"]][2,2]
## Use ifelse function to return whether we should accept or reject the hypothesis
Result<-ifelse(tstats2>qt(0.95, length(marketEXERT)),
               "Reject", "Can't Reject")

#Step 10.1 Outliers
##Calculate standardized residuals
standardresidual<-Model$residuals/sd(Model$residuals)
##Create variable "Findoutlier"
Findoutlier<-rep(NA,lg)
##Use for loop to assign 1 to Findoutlier if the absolute value of residual is greater than 3. Otherwise, 0.
for(i in 1:length(standardresidual)){
  Findoutlier[i]<-ifelse(abs(standardresidual[i])>3, 1, 0)
}
## Extract the index of the outliers
Dateindex<-which(Findoutlier==1)
## Find the Date of outlier
OutlierDate<-DATE[Dateindex]
## Find the Date of outlier in oct2008
library(lubridate)
DATEyyymm<-year(DATE)*100+month(DATE)
Dateindex08Oct<-which(Findoutlier==1 & DATEyyymm==200810)
OutlierDate08Oct<-DATE[Dateindex08Oct]
Outlier08Oct<-standardresidual[Dateindex08Oct]
data.frame(Outlier08Oct,row.names =OutlierDate08Oct,check.names = TRUE)

#Step 10.2 Leverage Points
## Calculate leverage values for each fitted value of our model
levModel<-hat(marketEXERT, intercept = TRUE)
## Find the leverage points
k<-1
levIndex<-which(levModel>3*(k+1)/lg)
levDate<-DATE[levIndex]

#Step 10.3 Influential Points
##Calculate cooks distance
cooksdis<- cooks.distance(Model)
##Find the influential points
p1<- k+1
p2<-lg-(k+1)
p<-1-pf(cooksdis, p1, p2)
inflIndex<-which(p < 0.05)
InflDate<-DATE[inflIndex]

#step11.1
jpeg(filename = "Case1_Hist_Rresiduals.jpeg")
hist(Model$residuals,breaks=50,prob=T,main = "Histogram of the Residuals")
curve(dnorm(x,mean=0,sd(Model$residuals)),add = T)
dev.off()

#step11.2
      
jpeg(filename = "Case1_QQPlot_Residuals.jpeg")
qqnorm(Model$residuals, main="Q-Q plot of Residual")
qqline(Model$residuals)
dev.off()

#step11.3
library(tseries)
jarque.bera.test(Model$residuals)
jarque.bera.test(Model$residuals)

#STEP12.1
jpeg(filename = "Case1_ScatterPlot_Residuals.jpeg")
plot(standardresidual[1:lg-1],standardresidual[2:lg], main = "Scatter Plot of Residuals", xlab = "Residuals,t",ylab = "Residuals, t+1")
dev.off()

#step12.2
library(lmtest)
dwtest(Model)

#step12.3
Box.test(Model$residuals,,type = "Ljung", lag=5)

#step13
jpeg(filename = "Case1_ScatterPlot_MarketVSResiduals_testing.jpeg")
plot(marketEXERT,standardresidual, main = "Regression Residuals vs Market Excess Return", xlab = "Market Excess Return",ylab = "Residuals")
dev.off()

#step14
jpeg(filename = "Case1_Residual_timeseries.jpeg")
plot(DATE, standardresidual, type = "l"
     , xlab="DATE", ylab="Residual",
     main="Residuals", ylim=c(-15, 20))
dev.off()

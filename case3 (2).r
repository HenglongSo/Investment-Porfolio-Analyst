setwd("//hd.ad.syr.edu/01/970687/Documents/Fin 654/case3")
Port<-read.csv('Case3Port.csv',header = F,sep = ",")
secid<-read.csv('Case3FirmSECID.csv',header = T,sep = ",")
market<-read.csv('Case3Market.csv',header = F,sep = ",")
DATE<-as.Date(as.character(market$V1),"%Y%m%d")

## Step1 Drop Firms
## calculate the number of Nan for each firm
ncol_Port<-ncol(Port)
nrow_Port<-nrow(Port)
num_Nan<-rep(0, ncol_Port)
for(i in 1:ncol_Port){
  num_Nan[i]<-sum (ifelse(is.nan(Port[, i]), 1, 0))
}
## Find the firms that have no observations in the whole sample period
vec_DELETE<-which(num_Nan == nrow_Port)

## Delete the firms that have no observations in the whole sample period
## TICKER[-c(i)] can return vector TICKER without ith element of TICKER,
TICKER<-secid$Ticker
## given i is a positive integer and is less than the length of TICKER.
TICKER_Clean<-TICKER[-c(vec_DELETE)]
SECID_Clean<-secid$secid[-c(vec_DELETE)]
COMNAM_Clean<-secid$Name[-c(vec_DELETE)]

## By assigning the value of NULL to a certain column of a data.frame,
## we delete the corresponding variable in the data.frame.
Port[, vec_DELETE]<-NULL

## output the beginning and the ending date in our sample
lg<-length(DATE)
DATE[1]
DATE[lg]

## The number of firms that are kept and dropped
length(vec_DELETE)
length(TICKER_Clean)

## Calculate Firm excess returns
RF<-market$V3
Firm_RET<-Port-RF

## Step 2.1
library(e1071)
## descriptive statistics, such as mean,
##standard deviation, and Sharpe-ratio
Firm_Mean<-apply(Firm_RET, 2, mean, na.rm=TRUE)
Firm_Std<-apply(Firm_RET, 2, sd, na.rm=TRUE)
Firm_Skew<-apply(Firm_RET, 2, skewness, na.rm=TRUE)
Firm_Kurt<-apply(Firm_RET, 2, kurtosis, na.rm=TRUE)
Firm_Min<-apply(Firm_RET, 2, min, na.rm=TRUE)
Firm_Max<-apply(Firm_RET, 2, max, na.rm=TRUE)
Firm_Sharpe<-(Firm_Mean/Firm_Std)*sqrt(252)

## Calculate descriptive statistics, such as mean,
##standard deviation, and Sharpe-ratio for IBM
IBM<-Firm_RET[, which(SECID_Clean==106276)]
IBM_Mean<-mean(IBM)
IBM_Std<-sd(IBM)
IBM_Skew<-skewness(IBM)
IBM_Kurt<-kurtosis(IBM)
IBM_Min<-min(IBM)
IBM_Max<-max(IBM)
IBM_Sharpe<-(IBM_Mean/IBM_Std)*sqrt(252)

## Calculate Quantile for each descriptive statistics
Quantile_Percent<-c(0.05, 0.25, 0.5, 0.75, 0.95)
Mean_Quantile<-quantile(Firm_Mean, Quantile_Percent, na.rm=TRUE)
Std_Quantile<-quantile(Firm_Std, Quantile_Percent, na.rm=TRUE)
Skew_Quantile<-quantile(Firm_Skew, Quantile_Percent, na.rm=TRUE)
Kurt_Quantile<-quantile(Firm_Kurt, Quantile_Percent, na.rm=TRUE)
Min_Quantile<-quantile(Firm_Min, Quantile_Percent, na.rm=TRUE)
Max_Quantile<-quantile(Firm_Max, Quantile_Percent, na.rm=TRUE)
Sharpe_Quantile<-quantile(Firm_Sharpe, Quantile_Percent, na.rm=TRUE)

## Construct a table to present the results
Table_2_1<-matrix(data=NA,nrow = 7, ncol = 6)
Table_2_1[1,]<-c(IBM_Mean, Mean_Quantile)
Table_2_1[2,]<-c(IBM_Std,Std_Quantile)
Table_2_1[3,]<-c(IBM_Skew,Skew_Quantile)
Table_2_1[4,]<-c(IBM_Kurt,Kurt_Quantile)
Table_2_1[5,]<-c(IBM_Min,Min_Quantile)
Table_2_1[6,]<-c(IBM_Max,Max_Quantile)
Table_2_1[7,]<-c(IBM_Sharpe,Sharpe_Quantile)
rownames(Table_2_1)<-c("Mean", "Std", "Skew", "Kurt", "Min", "Max", "Sharpe-Ratio")
colnames(Table_2_1)<-c("IBM","Q5","Q25","Q50","Q75","Q95")
as.table(round(Table_2_1,2))

## Step 2.2
## Firm identification
TICK_vec<-rep(NaN,4)
TICK_vec[1]<-which(SECID_Clean==106276)
TICK_vec[2]<-which(Firm_Sharpe==min(Firm_Sharpe, na.rm = TRUE))
TICK_vec[3]<-which((Firm_Sharpe==quantile(Firm_Sharpe, c(0.50), na.rm = TRUE)))
TICK_vec[4]<-which(Firm_Sharpe==max(Firm_Sharpe, na.rm = TRUE))

## Study these firms
Qmissing_Index<-list()
QLength<-integer()
QStarting<-character()
QEnding<-character()
for (i in 1:length(TICK_vec)){
  Qmissing_Index[[i]]<-which(is.nan(Firm_RET[,TICK_vec[i]])== "FALSE")
  QLength[i]<-length(Qmissing_Index[[i]])
  QStarting[i]<-market$V1[Qmissing_Index[[i]][1]]
  QEnding[i]<-market$V1[Qmissing_Index[[i]][QLength[i]]]
}

## Construct table to present previous results
Table_2_2<-matrix(data=NA,nrow = 4, ncol = 4)
Table_2_2[1,]<-round(Firm_Sharpe[TICK_vec],4)
Table_2_2[2,]<-QLength
Table_2_2[3,]<-QStarting
Table_2_2[4,]<-QEnding
rownames(Table_2_2)<-c("Sharpe-Ratio","Min","Q50", "Max")
colnames(Table_2_2)<-c("IBM","Min","Q50","Max")
as.table(Table_2_2)

## Step 2.3
jpeg(filename = "Case3_Rock_Hist.jpeg")
Q50<-Firm_RET[,TICK_vec[3]]
hist(Q50,main = "Daily Rock Excess returns (percentage)",breaks=40,probability = T)
curve(dnorm(x,mean(Q50),sd(Q50)),add = T)
dev.off()

jpeg(filename = "Case3_Rock_QQ.jpeg")
qqnorm(Q50,main = "Q-Q plot of Rock Excess Returns")
qqline(Q50)
dev.off()

library(tseries)
library(nortest)
jarque.bera.test(Q50)
lillie.test(Q50)
## Step 3 Beta Estimation
## Step 3.0 Drop Firms
## Create a variable to store the number of Nan for firm
Firm_num_Nan<-rep(0, ncol(Firm_RET))
## calculate the number of Nan for each firm
for(i in 1:ncol(Firm_RET)){
  Firm_num_Nan[i]<-sum (ifelse(is.nan(Firm_RET[, i]), 1, 0))
}
## Find firms that have incomplete sample length
Firm_DELETE<-which(Firm_num_Nan>0)
## Drop the firms with incomplete information during the sample period
Firm_RET[,Firm_DELETE]<-NULL
## count the number of firms that are kept and dropped
Num_Firms<-length(Firm_RET)
Num_Delete<-length(Firm_DELETE)
TICKER_Clean2<-TICKER_Clean[-c(Firm_DELETE)]
SECID_Clean2<-SECID_Clean[-c(Firm_DELETE)]
## Step 3.1 Unique Month
## Find the the specific dates for the end of each month in the sample period.
End_Month<-tapply(as.character(DATE), substr(DATE, 1, 7), max)

## Create a variable to store the index for the end of each month
End_Month_Index<-rep(NA, length(End_Month))

## Find the row number for the end of each month
for( i in 1:length(End_Month)){
  End_Month_Index[i]<-which(DATE==End_Month[i])
}

## 3.2 OLS regression firm by firm
Market_EXERT<-market$V2
Num_Month<-length(End_Month_Index)
Win<-60
Starting_Month_Index<-3
Beta_Estimation_3_2<-matrix(NA, nrow = 240, ncol=1111)
## Firm Beta Estimation
for (i in 1:Num_Firms){
  for(j in Starting_Month_Index:Num_Month){
    y<-Firm_RET[(End_Month_Index[j]-Win+1):End_Month_Index[j], i]
    x<-Market_EXERT[(End_Month_Index[j]-Win+1):End_Month_Index[j]]
    Model<-lm(y~x)
    Beta_Estimation_3_2[j,i]<-Model$coefficients[2]
  }
}
i =  1 
j= Starting_Month_Index
y<-Firm_RET[(End_Month_Index[j]-Win+1):End_Month_Index[j], i]
x<-Market_EXERT[(End_Month_Index[j]-Win+1):End_Month_Index[j]]
Model<-lm(y~x)

  

## Step 4.1 Construct Portfolios at the end of each month
Month_RET_4<-matrix(NA, nrow =Num_Month, ncol =ncol(Firm_RET))
Port_RET_Q<-matrix(NA, nrow =Num_Month, ncol =6)
for (j in Starting_Month_Index:(Num_Month-1)){
  for (i in 1:Num_Firms){
    Month_RET_4[j,i]<-sum(Firm_RET[(End_Month_Index[j]+1):End_Month_Index[(j+1)], i], na.rm=TRUE)
  }
  ## use beta quantiles as cutoff points
  cutoff<-quantile(Beta_Estimation_3_2[j,], c(0, 0.2, 0.4, 0.6,0.8,1), na.rm=TRUE)
  ## form portfolios at the end of each month
  Port_RET_Q[j,1]<-mean(Month_RET_4[j, which((Beta_Estimation_3_2[j,]>=cutoff[1])
                                             & (Beta_Estimation_3_2[j,]<=cutoff[2]))])
  Port_RET_Q[j,2]<-mean(Month_RET_4[j, which((Beta_Estimation_3_2[j,]>cutoff[2])
                                             & (Beta_Estimation_3_2[j,]<=cutoff[3]))])
  Port_RET_Q[j,3]<-mean(Month_RET_4[j, which((Beta_Estimation_3_2[j,]>cutoff[3])
                                             & (Beta_Estimation_3_2[j,]<=cutoff[4]))])
  Port_RET_Q[j,4]<-mean(Month_RET_4[j, which((Beta_Estimation_3_2[j,]>cutoff[4])
                                             & (Beta_Estimation_3_2[j,]<=cutoff[5]))])
  Port_RET_Q[j,5]<-mean(Month_RET_4[j, which((Beta_Estimation_3_2[j,]>cutoff[5])
                                             & (Beta_Estimation_3_2[j,]<=cutoff[6]))])
  ## Return difference between highest quintile and lowest quintile
  Port_RET_Q[j,6]<-Port_RET_Q[j,5]-Port_RET_Q[j,1]
}

## Step 4.2 Portfolio Returns
## Calculate the time series average for each portfolio in each quantile                                             
Port_RET_Q_mean<-rep(NA,6)
Port_RET_Q_std<-rep(NA,6)
Port_RET_Q_Sharpe<-rep(NA,6)
for (i in 1:6) {
  Port_RET_Q_mean[i]<-mean(Port_RET_Q[,i],na.rm = T)
  Port_RET_Q_std[i]<-sd(Port_RET_Q[,i],na.rm = T)
  Port_RET_Q_Sharpe[i]<-(Port_RET_Q_mean[i]/Port_RET_Q_std[i])*sqrt(12)
    }
x_4<-rep(1,Num_Month)
lmSUM<-summary(lm(Port_RET_Q~0+x_4))
Port_RET_Q_pvalue<-rep(NA,6)
Port_RET_Q_pvalue[1]<-lmSUM[["Response Y1"]]$coefficients[1,4]
Port_RET_Q_pvalue[2]<-lmSUM[["Response Y2"]]$coefficients[1,4]
Port_RET_Q_pvalue[3]<-lmSUM[["Response Y3"]]$coefficients[1,4]
Port_RET_Q_pvalue[4]<-lmSUM[["Response Y4"]]$coefficients[1,4]
Port_RET_Q_pvalue[5]<-lmSUM[["Response Y5"]]$coefficients[1,4]
Port_RET_Q_pvalue[6]<-lmSUM[["Response Y6"]]$coefficients[1,4]

Table_4_2<-matrix(data=NA,nrow = 4, ncol = 6)
Table_4_2[1,]<-round(Port_RET_Q_mean,3)
Table_4_2[2,]<-round(Port_RET_Q_std,3)
Table_4_2[3,]<-round(Port_RET_Q_pvalue,3)
Table_4_2[4,]<-round(Port_RET_Q_Sharpe,3)
rownames(Table_4_2)<-c("Mean","Std","P-value","Sharpe-Ratio")
colnames(Table_4_2)<-c("Q1","Q2","Q3","Q4","Q5","Q5-Q1")
as.table(Table_4_2)

jpeg(filename = "Case3_Port_mean_bar.jpg")
barplot(Port_RET_Q_mean,ylab = 'Average Returns',xlab = 'Quantile')
dev.off()

## Step 4.3
Test_Value_4_3<-lmSUM[["Response Y6"]]$coefficients[1,3]
Test_Result_4_3<-ifelse(Test_Value_4_3>qt(0.95,(Num_Month-Starting_Month_Index-1)),
                        "reject", "can't reject")


## Step 5.1 Estimate the Market Risk Permium month by month
Market_Risk_Slope_5<-rep(NA,(Num_Month))
Market_Risk_Intercept_5<-rep(NA,(Num_Month))
for (j in Starting_Month_Index:(Num_Month-1)){
  Model<-lm(Month_RET_4[j,]~Beta_Estimation_3_2[j,])
  Market_Risk_Slope_5[j]<-Model$coefficients[2]
  Market_Risk_Intercept_5[j]<-Model$coefficients[1]
}

nmonth<-as.Date(End_Month)
jpeg(filename = "Case3_maeket_lambda.jpg")
plot(nmonth[3:239],Market_Risk_Slope_5[3:239],type = 'l')
dev.off()

## Step 5.2 Is the market risk postively priced
Test_Value_5_2<-summary(lm(Market_Risk_Slope_5~0+x_4))$coefficients[,3]
Test_Result_5_2<-ifelse(Test_Value_5_2>
                          qt(0.95, (Num_Month-Starting_Month_Index-1)),
                        "reject", "can't reject")

## Step 5.3 Are there other factors
Test_p_value_5_3<-summary(lm(Market_Risk_Intercept_5~0+x_4))$coefficients[,4]
Test_Result_5_3<-ifelse(Test_p_value_5_3<0.05, "reject", "can't reject")

##Step 7.1 OLS regression firm by firm
VIX2<-market$V4
VIX_lag<-c(NA,VIX2[2:lg]-VIX2[1:lg-1])
Beta_Estimation_Market<-matrix(NA, nrow = 240, ncol=1111)
Beta_Estimation_VIX<-matrix(NA, nrow = 240, ncol=1111)
for (i in 1:Num_Firms){
  for(j in Starting_Month_Index:Num_Month){
    y<-Firm_RET[(End_Month_Index[j]-Win+1):End_Month_Index[j], i]
    x<-Market_EXERT[(End_Month_Index[j]-Win+1):End_Month_Index[j]]
    xv<-VIX_lag[(End_Month_Index[j]-Win+1):End_Month_Index[j]]
    ModelV<-lm(y~x+xv)
    Beta_Estimation_Market[j,i]<-ModelV$coefficients[2]
    Beta_Estimation_VIX[j,i]<-ModelV$coefficients[3] 
  }
}
i =  1 
j= Starting_Month_Index
y<-Firm_RET[(End_Month_Index[j]-Win+1):End_Month_Index[j], i]
x<-Market_EXERT[(End_Month_Index[j]-Win+1):End_Month_Index[j]]
ModelV<-lm(y~x)
##Step 8.1 Construct Portfolios at the end of each month
## use beta quantiles as cutoff points
Port_RET_V<-matrix(NA, nrow =Num_Month, ncol =6)
for (j in Starting_Month_Index:(Num_Month-1)){
  for (i in 1:Num_Firms){
    Month_RET_4[j,i]<-sum(Firm_RET[(End_Month_Index[j]+1):End_Month_Index[(j+1)], i], na.rm=TRUE)
  }
  ## use beta quantiles as cutoff points
  cutoff2<-quantile(Beta_Estimation_VIX[j,], c(0, 0.2, 0.4, 0.6,0.8,1), na.rm=TRUE)
  
  ## form portfolios at the end of each month
  Port_RET_V[j,1]<-mean(Month_RET_4[j, which((Beta_Estimation_VIX[j,]>=cutoff2[1])
                                             & (Beta_Estimation_VIX[j,]<=cutoff2[2]))])
  Port_RET_V[j,2]<-mean(Month_RET_4[j, which((Beta_Estimation_VIX[j,]>cutoff2[2])
                                             & (Beta_Estimation_VIX[j,]<=cutoff2[3]))])
  Port_RET_V[j,3]<-mean(Month_RET_4[j, which((Beta_Estimation_VIX[j,]>cutoff2[3])
                                             & (Beta_Estimation_VIX[j,]<=cutoff2[4]))])
  Port_RET_V[j,4]<-mean(Month_RET_4[j, which((Beta_Estimation_VIX[j,]>cutoff2[4])
                                             & (Beta_Estimation_VIX[j,]<=cutoff2[5]))])
  Port_RET_V[j,5]<-mean(Month_RET_4[j, which((Beta_Estimation_VIX[j,]>cutoff2[5])
                                             & (Beta_Estimation_VIX[j,]<=cutoff2[6]))])
  ## Return difference between highest quintile and lowest quintile
  Port_RET_V[j,6]<-Port_RET_V[j,5]-Port_RET_V[j,1]
}

##8.2 Portfolio Returns
Port_RET_V_mean<-rep(NA,6)
Port_RET_V_std<-rep(NA,6)
Port_RET_V_Sharpe<-rep(NA,6)
for (i in 1:6) {
  Port_RET_V_mean[i]<-mean(Port_RET_V[,i],na.rm = T)
  Port_RET_V_std[i]<-sd(Port_RET_V[,i],na.rm = T)
  Port_RET_V_Sharpe[i]<-(Port_RET_V_mean[i]/Port_RET_V_std[i])*sqrt(12)
}
x_8<-rep(1,Num_Month)
lmSUMv<-summary(lm(Port_RET_V~0+x_8))
Port_RET_V_pvalue<-rep(NA,6)
Port_RET_V_pvalue[1]<-lmSUM[["Response Y1"]]$coefficients[1,4]
Port_RET_V_pvalue[2]<-lmSUM[["Response Y2"]]$coefficients[1,4]
Port_RET_V_pvalue[3]<-lmSUM[["Response Y3"]]$coefficients[1,4]
Port_RET_V_pvalue[4]<-lmSUM[["Response Y4"]]$coefficients[1,4]
Port_RET_V_pvalue[5]<-lmSUM[["Response Y5"]]$coefficients[1,4]
Port_RET_V_pvalue[6]<-lmSUM[["Response Y6"]]$coefficients[1,4]

Table_8_2<-matrix(data=NA,nrow = 4, ncol = 6)
Table_8_2[1,]<-round(Port_RET_V_mean,3)
Table_8_2[2,]<-round(Port_RET_V_std,3)
Table_8_2[3,]<-round(Port_RET_V_pvalue,3)
Table_8_2[4,]<-round(Port_RET_V_Sharpe,3)
rownames(Table_8_2)<-c("Mean","Std","P-value","Sharpe-Ratio")
colnames(Table_8_2)<-c("Q1","Q2","Q3","Q4","Q5","Q5-Q1")
as.table(Table_8_2)

jpeg(filename = "Case3_Port_mean_V.jpg")
barplot(Port_RET_V_mean,ylab = 'Average Returns',xlab = 'Quantile')
dev.off()

## Step 8.3 Return Differential
## Find the t-statistic for the coefficients
Test_Value_8_3<-lmSUMv[["Response Y6"]]$coefficients[1,3]
Test_Result_8_3<-ifelse(Test_Value_8_3<-
                          qt(0.05,(Num_Month-Starting_Month_Index-1)),
                        "reject", "can't reject")

##Step 9.1 Estimate the Risk Premium month by month
Market_Risk_Slope_Markret<-rep(NA,(Num_Month))
Market_Risk_Slope_VIX<-rep(NA,(Num_Month))
Market_Risk_Intercept_9<-rep(NA,(Num_Month))
for (j in Starting_Month_Index:(Num_Month-1)){
  Model9<-lm(Month_RET_4[j,]~Beta_Estimation_Market[j,]+Beta_Estimation_VIX[j,])
  Market_Risk_Slope_VIX[j]<-Model9$coefficients[3]
  Market_Risk_Slope_Markret[j]<-Model9$coefficients[2]
  Market_Risk_Intercept_9[j]<-Model9$coefficients[1]
}

jpeg(filename = "Case3_VIX_lambda.jpg")
plot(nmonth[3:239],Market_Risk_Slope_VIX[3:239],type = 'l',main = "Lambda Volitivity")
dev.off()

##Step 9.2 Is the volatility risk negatively priced?
Test_Value_9_2<-summary(lm(Market_Risk_Slope_VIX~0+x_4))$coefficients[,3]
Test_Result_9_2<-ifelse(Test_Value_9_2>
                          qt(0.95, (Num_Month-Starting_Month_Index-1)),
                        "reject", "can't reject")

##Step 9.2 HT Is the volatility risk negatively priced
Test_p_value_9_3<-summary(lm(Market_Risk_Intercept_9~0+x_4))$coefficients[,4]
Test_Result_9_3<-ifelse(Test_p_value_9_3<0.05, "reject", "can't reject")
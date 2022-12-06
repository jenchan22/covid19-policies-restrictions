library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(forecast)
library(readxl)
library(ggfortify)
library(xts)
library(dynlm)
library(scales)
library(caret)
library(flexmix)

US <- read_excel("~/covid19-policies-restrictions/US_by_date.xlsx")
data <- read_csv("~/covid19-policies-restrictions/US_all_merged.csv")
CAN_policies <-read_csv("~/covid19-policies-restrictions/CAN_policies.csv")
CAN_sent <- read_csv("~/covid19-policies-restrictions/date-sentiment-CAN.csv")


US$Date <- as.Date(US$Date)
Canada$Date <- as.Date(Canada$`Row Labels`)
US[3:17] <- lapply(US[3:17],factor)
US <- US[,-c(6,13:14)] #remove unchanged policies
US_1 <- ts(US[,-1], start = decimal_date(as.Date("2020-03-19")), frequency = 365) #???
summary(US_1)

CAN_1 <- ts(CAN_sent[,-1], start = decimal_date(as.Date("2020-03-19")), frequency = 365) 

#Decomposition Plot - US
options(repr.plot.width = 18, repr.plot.height = 10)
decomp <- stl(US_1[,1], s.window = 'periodic')
decomp$time.series <- as.Date(decomp$time.series)
autoplot(decomp) + theme_bw()  + 
   ggtitle("Sentiment Score (US) Decomposition Plot")


predictors_ts <- US_1[,c(2:13)]
sent_ts <- US_1[,1]
face_ts <- predictors_ts[,1]

## 1) Full model with all predictors 
#autoregression plot - using arima
fit_arima <- 
  auto.arima(US[2], xreg=data.matrix(US[3:14]),stepwise=FALSE, seasonal=FALSE, approximation =FALSE, biasadj=FALSE)

#stepwise=false - larger set of models will be seaerched
#approximation=false - don't use the default approximations that speeds up the search process and potentially not find a min AIC model
#auto arima runs the model with different p and qs, then returns the best model with the smallest AIC value 
#this model gives ARIMA(0,0,1)

#summary(fit_arima)
#checkresiduals(fit_arima)
#yt = 0.1237 + 0.1377error_t-1 + 0.0061facial_t-1 - 0.0062stay_at_home_t-1 - 0.0222intl_travel_t-1 +..... + error_t
#error_t = sqrt(sigma^2) = sqrt(0.00256)
#ma1? (error_t-1)

#using dynlm, choosing period when face_ts changed
model <- dynlm(sent_ts ~  L(predictors_ts))
summary(model)

## 2) Using VIF threshold < 2 to remove predictors with multicollinearity
#'Facial Coverings','Travel Restriction', 'Close Public Transport', 'Stay at Home')
predictors_ts2 <-predictors_ts[,c(1,2,3,8)]
model_dynlm2 <- dynlm(sent_ts ~ L(predictors_ts2))
summary(model_dynlm2)

#predictor3 <- data.matrix(US[,c(3,4,5,10)])
#fit_arima3 <- auto.arima(US[2], xreg=predictor3,stepwise=FALSE, seasonal=FALSE, approximation =FALSE, biasadj=FALSE)
#summary(fit_arima3)


## 3) Full model with 7 chosen predictors
#just by looking at the plot and choosing predictors that have changed frequently over time
#facial_coverings, international_travel_controls,  workplace_closures, cancel_public_events, restrictions_internal_movements, vaccination_policy, school_closures

predictors_ts3 <-predictors_ts[,c(1,3,5,6,7,10,12)]
model_dynlm3 <- dynlm(sent_ts ~ L(predictors_ts3))
summary(model_dynlm3)
#predictor2 <- data.matrix(US[,c(3,5,7,9,12,14)])
#fit_arima2 <- auto.arima(US[2], xreg=predictor2,stepwise=FALSE, seasonal=FALSE, approximation =FALSE, biasadj=FALSE)
#summary(fit_arima2)


## 4) Choosing 4 predictors from Model 2
predictors_ts4 <-predictors_ts[,c(1,5,6,12)]
model_dynlm4 <- dynlm(sent_ts ~ L(predictors_ts4))
summary(model_dynlm4)

## Find model with lowest BIC value - best fits the dataset
BIC(model)
BIC(model_dynlm2)
BIC(model_dynlm3)
BIC(model_dynlm4)

AIC(model)
AIC(model_dynlm2)
AIC(model_dynlm3)
AIC(model_dynlm4)

#compare models using ANOVA and check p-values
anova(model_dynlm2, model)
anova(model_dynlm3, model)
anova(model_dynlm4, model)

anova(model_dynlm4,model_dynlm3)

#Check Canada - as Control Variable
CAN_ts <- CAN_1[,2]
#assume we choose model4
control_fit <- model_dynlm4 <- dynlm(CAN_ts ~ L(predictors_ts4))
summary(control_fit)

#Use Canada policies with US sentiment score
CAN.policies_ts <- ts(data.matrix(CAN_policies[,3:17]), start = decimal_date(as.Date("2020-03-19")), frequency = 365) #???
CAN.predictors_ts4 <-CAN.policies_ts[,c(2,12,14,15)]
control_fit2 <- model_dynlm4 <- dynlm(sent_ts ~ L(CAN.predictors_ts4))
summary(control_fit2)


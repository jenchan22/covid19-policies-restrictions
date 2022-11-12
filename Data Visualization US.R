library(dplyr)
library(ggplot2)
library(tidyverse)
library(zoo)

US <- read.csv("Desktop/Data Science Capstone/date-sentiment-US.csv")
summary(US)
US = subset(US, select = -c(X))
dim(US) #926*2

colnames(US)[1] = "Date"
colnames(US)[2] = "Sentiment_Score"

#convert character object to date object 
US$created_at <- as.Date(US$created_at, format = "%Y-%m-%d")
#US$sentiment.score <- as.integer(US$sentiment.score)

US %>% 
  ggplot(aes(x=Date, y=Sentiment_Score), color="blue") + 
  geom_line() + 
  geom_line(aes(y=rollmean(Sentiment_Score, 7, na.pad=TRUE)))


US %>%
  ggplot(aes(x=Date, y=Sentiment_Score)) +
  ylab("Sentiment Score") +
  xlab("Date") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)


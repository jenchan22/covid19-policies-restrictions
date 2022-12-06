library(car)
library(caTools)
library(dplyr)
library(tidyverse)
library(ggplot2)
library("gridExtra")

US <- read.csv("Desktop/Data Science Capstone/date-sentiment-US.csv")

testing_policy <- read.csv("Desktop/Data Science Capstone/raw_data/covid-19-testing-policy.csv")
contact_tracing <- read.csv("Desktop/Data Science Capstone/raw_data/covid-contact-tracing.csv")
contaiment_health_index <- read.csv("Desktop/Data Science Capstone/raw_data/covid-containment-and-health-index.csv")
vacc_policy <- read.csv("Desktop/Data Science Capstone/raw_data/covid-vaccination-policy.csv")
debt_relief <- read.csv("Desktop/Data Science Capstone/raw_data/debt-relief-covid.csv")
face_covering <- read.csv("Desktop/Data Science Capstone/raw_data/face-covering-policies-covid.csv")
income_support <- read.csv("Desktop/Data Science Capstone/raw_data/income-support-covid.csv")
internal_movement <- read.csv("Desktop/Data Science Capstone/raw_data/internal-movement-covid.csv")
international_travel <- read.csv("Desktop/Data Science Capstone/raw_data/international-travel-covid.csv")
public_campaigns <- read.csv("Desktop/Data Science Capstone/raw_data/public-campaigns-covid.csv")
public_events <- read.csv("Desktop/Data Science Capstone/raw_data/public-events-covid.csv")
public_gathering <- read.csv("Desktop/Data Science Capstone/raw_data/public-gathering-rules-covid.csv")
public_transport <- read.csv("Desktop/Data Science Capstone/raw_data/public-transport-covid.csv")
school_closures <- read.csv("Desktop/Data Science Capstone/raw_data/school-closures-covid.csv")
stay_at_home <- read.csv("Desktop/Data Science Capstone/raw_data/stay-at-home-covid.csv")
workplace_closures <- read.csv("Desktop/Data Science Capstone/raw_data/workplace-closures-covid.csv")
covid_data <- read.csv("Desktop/Data Science Capstone/raw_data/owid-covid-data.csv")

#change data to Date-time
US[['created_at']] <- as.POSIXct(US[['created_at']],format = "%Y-%m-%d")
US <- US %>%
  filter(created_at >= '2020-03-19' & created_at <= '2022-09-30' )

#change class to Date-time
df.list <- list(testing_policy, contact_tracing, contaiment_health_index, vacc_policy, debt_relief,
                face_covering, income_support, internal_movement, international_travel, public_campaigns,
                public_events, public_gathering, public_transport, school_closures, stay_at_home, 
                workplace_closures)

#filter only policies of the US
US_workplace <- workplace_closures %>%
  filter(Code=="USA")
US_workplace[['Day']] <- as.POSIXct(US_workplace[['Day']],format = "%Y-%m-%d")
US_workplace <- US_workplace %>%
  filter(Day >= '2020-03-19' & Day <= '2022-09-30' )

US_sah <- stay_at_home %>%
  filter(Code=="USA")
US_sah[['Day']] <- as.POSIXct(US_sah[['Day']],format = "%Y-%m-%d")
US_sah <- US_sah %>%
  filter(Day >= '2020-03-19' & Day <= '2022-09-30' )

US_school <- school_closures %>%
  filter(Code=="USA")
US_school[['Day']] <- as.POSIXct(US_school[['Day']],format = "%Y-%m-%d")
US_school <- US_school %>%
  filter(Day >= '2020-03-19' & Day <= '2022-09-30' )

US_transport <- public_transport %>%
  filter(Code=="USA")
US_transport[['Day']] <- as.POSIXct(US_transport[['Day']],format = "%Y-%m-%d")
US_transport <- US_transport %>%
  filter(Day >= '2020-03-19' & Day <= '2022-09-30' )

US_gathering <- public_gathering %>%
  filter(Code=="USA")
US_gathering[['Day']] <- as.POSIXct(US_gathering[['Day']],format = "%Y-%m-%d")
US_gathering <- US_gathering %>%
  filter(Day >= '2020-03-19' & Day <= '2022-09-30' )

US_events <- public_events %>%
  filter(Code=="USA")
US_events[['Day']] <- as.POSIXct(US_events[['Day']],format = "%Y-%m-%d")
US_events <- US_events %>%
  filter(Day >= '2020-03-19' & Day <= '2022-09-30' )


US_campaigns <- public_campaigns %>%
  filter(Code=="USA")
US_campaigns[['Day']] <- as.POSIXct(US_campaigns[['Day']],format = "%Y-%m-%d")
US_campaigns <- US_campaigns %>%
  filter(Day >= '2020-03-19' & Day <= '2022-09-30' )

US_testing <- testing_policy %>%
  filter(Code=="USA")
US_testing[['Day']] <- as.POSIXct(US_testing[['Day']],format = "%Y-%m-%d")
US_testing <- US_testing %>%
  filter(Day >= '2020-03-19' & Day <= '2022-09-30' )

US_contact <- contact_tracing %>%
  filter(Code=="USA")
US_contact[['Day']] <- as.POSIXct(US_contact[['Day']],format = "%Y-%m-%d")
US_contact <- US_contact %>%
  filter(Day >= '2020-03-19' & Day <= '2022-09-30' )

US_containment <- contaiment_health_index %>%
  filter(Code=="USA")
US_containment[['Day']] <- as.POSIXct(US_containment[['Day']],format = "%Y-%m-%d")
US_containment <- US_containment %>%
  filter(Day >= '2020-03-19' & Day <= '2022-09-30' )

US_vacc <- vacc_policy %>%
  filter(Code=="USA")
US_vacc[['Day']] <- as.POSIXct(US_vacc[['Day']],format = "%Y-%m-%d")
US_vacc <- US_vacc %>%
  filter(Day >= '2020-03-19' & Day <= '2022-09-30' )

US_debt <- debt_relief %>%
  filter(Code=="USA")
US_debt[['Day']] <- as.POSIXct(US_debt[['Day']],format = "%Y-%m-%d")
US_debt <- US_debt %>%
  filter(Day >= '2020-03-19' & Day <= '2022-09-30' )

US_face <- face_covering %>%
  filter(Code=="USA")
US_face[['Day']] <- as.POSIXct(US_face[['Day']],format = "%Y-%m-%d")
US_face <- US_face %>%
  filter(Day >= '2020-03-19' & Day <= '2022-09-30' )

US_income <- income_support %>%
  filter(Code=="USA")
US_income[['Day']] <- as.POSIXct(US_income[['Day']],format = "%Y-%m-%d")
US_income <- US_income %>%
  filter(Day >= '2020-03-19' & Day <= '2022-09-30' )

US_internal <- internal_movement %>%
  filter(Code=="USA")
US_internal[['Day']] <- as.POSIXct(US_internal[['Day']],format = "%Y-%m-%d")
US_internal <- US_internal %>%
  filter(Day >= '2020-03-19' & Day <= '2022-09-30' )

US_travel <- international_travel %>%
  filter(Code=="USA")
US_travel[['Day']] <- as.POSIXct(US_travel[['Day']],format = "%Y-%m-%d")
US_travel <- US_travel %>%
  filter(Day >= '2020-03-19' & Day <= '2022-09-30' )

df.list <- list(testing_policy, contact_tracing, contaiment_health_index, vacc_policy, debt_relief,
                face_covering, income_support, internal_movement, international_travel, public_campaigns,
                public_events, public_gathering, public_transport, school_closures, stay_at_home, 
                workplace_closures)

#Step 1: remove testing policy, contact tracing, and public campaigns as they never changed
predictors <- data.frame(US_containment$containment_index, 
                         US_vacc$vaccination_policy, US_debt$debt_relief, US_face$facial_coverings,
                         US_income$income_support, US_internal$restrictions_internal_movements, 
                         US_travel$international_travel_controls,
                         US_events$cancel_public_events, US_gathering$restriction_gatherings, 
                         US_transport$close_public_transport, US_school$school_closures, 
                         US_sah$stay_home_requirements,
                         US_workplace$workplace_closures)
all <- data.frame(US$Sentiment_Score, predictors)
model <- lm(all$US.Sentiment_Score~.,data=all)
vif_values <- vif(model)

#Step 2: remove VIF values > 15
# remove contaiment index, income support, gathering restriction, vaccination policy, 
predictors <- data.frame(US_debt$debt_relief, US_face$facial_coverings,
                         US_internal$restrictions_internal_movements, 
                         US_travel$international_travel_controls,
                         US_events$cancel_public_events, 
                         US_transport$close_public_transport, US_school$school_closures, 
                         US_sah$stay_home_requirements,
                         US_workplace$workplace_closures)
all <- data.frame(US$Sentiment_Score, predictors)
model <- lm(all$US.Sentiment_Score~.,data=all)
vif_values <- vif(model)

#Step 3: remove predictors with VIF > 5 (debt_relief, internal movments, school closures)
predictors <- data.frame( US_face$facial_coverings,
                         US_events$cancel_public_events, 
                         US_workplace$workplace_closures, US_school$school_closures)
all <- data.frame(US$Sentiment_Score, predictors)
model <- lm(all$US.Sentiment_Score~.,data=all)
vif_values <- vif(model)

#check for model4
predictors <- data.frame( US_face$facial_coverings,
                          US_events$cancel_public_events, 
                          US_transport$close_public_transport, 
                          US_sah$stay_home_requirements,
                          US_workplace$workplace_closures)
all <- data.frame(US$Sentiment_Score, predictors)
model <- lm(all$US.Sentiment_Score~.,data=all)
vif_values <- vif(model)


#Step 4: remove predictors with highest VIF till there no variables VIF > 2 
#remove (cancel public events &  workplace_closures )
predictors <- data.frame( US_face$facial_coverings,
                          US_travel$international_travel_controls, 
                          US_transport$close_public_transport, 
                          US_sah$stay_home_requirements)
all <- data.frame(US$sentiment.score, predictors)
colnames(all) <- c('Sentiment Score','Facial Coverings','Travel Restriction', 
                   'Close Public Transport', 'Stay at Home')
model <- lm(all$`Sentiment Score`~.,data=all)
vif_values <- vif(model)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") 

#Histogram for sentiment score
hist <- hist(US$sentiment.score)

x_values <- seq(min(US$sentiment.score), max(US$sentiment.score), length = 100)
y_values <- dnorm(x_values, mean = mean(US$sentiment.score), sd = sd(US$sentiment.score)) 
y_values <- y_values * diff(hist$mids[1:2]) * length(US$sentiment.score) 
#overlay normal curve on histogram
lines(x_values, y_values, lwd = 2)

US_face_factor = US_face
US_face_factor$facial_coverings = as.factor(US_face_factor$facial_coverings)

#plot change of facial covering policies over the last 2+ years
ggplot(US_face_factor, aes(x=Day, y=facial_coverings, color=facial_coverings)) + 
  geom_point() + 
  ggtitle("Change of Facial Covering Policies between March 2020 to September 2022") +
  scale_colour_discrete(name = "Facial Covering", 
                        labels=c("Recommended","Required in some specified shared spaces",
                                 "Required in all shared spaces when social distancing not possible", 
                                 "Required outside the home at all times regardless of location")) 


US_travel_factor = US_travel
US_travel_factor$international_travel_controls = as.factor(US_travel_factor$international_travel_controls)

#plot change of international travel controls over the last 2+ years
ggplot(US_travel_factor, aes(x=Day, y=international_travel_controls, color=international_travel_controls)) + 
  geom_point() + 
  ggtitle("Change of International Travel Controls between March 2020 to September 2022") +
  scale_colour_discrete(name = "International Travel Controls", 
                        labels=c("Screening", 
                                 "Ban on high-risk regions")) 

US_sah_factor = US_sah
US_sah_factor$stay_home_requirements = as.factor(US_sah$stay_home_requirements)
ggplot(US_sah_factor, aes(x=Day, y=stay_home_requirements, color=stay_home_requirements)) + 
  geom_point() + 
  ggtitle("Change of Stay-at-home Restrictions between March 2020 to September 2022") +
  scale_colour_discrete(name = "Stay-at-home Restrictions", 
                        labels=c("No measures", 
                                 "Recommended",
                                 "Required(except essentials)")) 

#Rgression 1: 
#Simple LR model Sentiment_score =B0 + B1*Travel_Restriction + Error
simple_model <- lm(`Sentiment Score`~ `Travel Restriction`, data=all)
summary(simple_model)
#slope = 0.007177, intercept = 0.167482
simple_factor_model <- lm(all$`Sentiment Score`~ US_face_factor$facial_coverings)

#plot for simple regression - scatter plot and abline
travel_plot <- ggplot(all, aes(`Travel Restriction`, `Sentiment Score`, 
                color=`Travel Restriction`)) +
  geom_point(aes(color = `Travel Restriction`)) + 
  ggtitle("Sentiment Score vs Travel Restriction") +
  geom_abline(intercept = 0.167482, slope = 0.007177, color='red') +
  theme(legend.position = "none",axis.title=element_text(size=8),plot.title=element_text(size=10)) +
  scale_x_continuous(limits = c(0, 4))

ggplot(all, aes(`Travel Restriction`, `Sentiment Score`, 
                color=`Travel Restriction`)) +
  geom_jitter(aes(color = `Travel Restriction`)) + 
  ggtitle("Sentiment Score vs International Travel Restriction") +
  geom_abline(intercept = 0.167482, slope = 0.007177, color='red') + 
  theme(legend.position = "none")


#Regression 1b)
#Simple LR model Sentiment_score =B0 + B1*Facial_Covering + Error
simple_facial_model <- lm(`Sentiment Score`~ `Facial Coverings`, data=all)
summary(simple_facial_model)


facial_plot <- ggplot(all, aes(`Facial Coverings`, `Sentiment Score`, 
                color=`Facial Coverings`)) +
  geom_point(aes(color = `Facial Coverings`)) + 
  ggtitle("Sentiment Score vs Facial Coverings Policy") +
  geom_abline(intercept = 0.148532, slope = 0.013035, color='red') + 
  theme(legend.position = "none",axis.title=element_text(size=8),plot.title=element_text(size=10))+
  scale_x_continuous(limits = c(0, 4))

#Regression 1c) 
#with Stay at Home variable
simple_sah_model <- lm(all$`Sentiment Score`~US_sah$stay_home_requirements)
summary(simple_sah_model)
sah_plot <- ggplot(all, aes(`Stay at Home`, `Sentiment Score`, 
                color=`Stay at Home`)) +
  geom_point(aes(color = `Stay at Home`)) + 
  ggtitle("Sentiment Score vs Stay at Home policy") +
  geom_abline(intercept = 0.173109, slope = 0.010069, color='red') + 
  theme(legend.position = "none",axis.title=element_text(size=8),plot.title=element_text(size=10))+
  scale_x_continuous(limits = c(0, 4))

#Regression 1d) 
#with Close of Transport Variable
simple_transport_model <- lm(all$`Sentiment Score`~US_transport$close_public_transport)
summary(simple_transport_model)
transport_plot <- ggplot(all, aes(`Close Public Transport`, `Sentiment Score`, 
                color=`Close Public Transport`)) +
  geom_point(aes(color = `Close Public Transport`)) + 
  ggtitle("Sentiment Score vs Public Transport policy") +
  geom_abline(intercept = 0.186896, slope = -0.002354, color='red') + 
  theme(legend.position = "none",axis.title=element_text(size=8),plot.title=element_text(size=10))+
  scale_x_continuous(limits = c(0, 4))

grid.arrange(travel_plot, facial_plot, sah_plot, transport_plot, nrow = 4)

#Regression 2: Control other variables (facial coverings and travel restriction)
#Sentiment_score = B0 + B1*Travel_Restriction + B2*Facial_coverings + B3*Stay_at_home + Error
reg_model2 <- lm(`Sentiment Score` ~ 
                   `Travel Restriction` + `Facial Coverings` , data=all)

summary(reg_model2)

#Plot two simple LR together without interacting
equation1=function(x){coef(reg_model2)[2]*x+coef(reg_model2)[1]}
equation2=function(x){coef(reg_model2)[2]*x+coef(reg_model2)[1]+coef(reg_model2)[3]}

ggplot(all,aes(y=`Sentiment Score`,x=`Travel Restriction`))+geom_point()+
  stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])

"""
sentiment_score 
y = 1.486e-01  + -6.344e-05 *travel_restriction + 1.308e-02*facial_covering

"""

#Regression 3: Interacting Travel Restriction and Facial Covering
inter_model <- lm(`Sentiment Score` ~ `Travel Restriction`*`Facial Coverings`, data=all)
summary(inter_model)

##Try standardizing Sentiment Score
df2 <- all %>% mutate_at(c('Sentiment Score'), ~(scale(.) %>% as.vector))
df2
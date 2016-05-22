######################################### Libraries ##############################################
install.packages("wesanderson")
install.packages("hydroGOF")
install.packages("ISOweek")
library(wesanderson)
library(rpart)
library(rpart.plot)
library(rattle)
library(data.table)
library(MASS)
library(e1071)
library(dplyr)
library(randomForest)
library(stringr)
library(GPArotation)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(zoo)
library(psych)
library(corrplot)
library(RColorBrewer)
library(hydroGOF)
library(ISOweek)
set.seed(1501)

########################################## Data Loading ###########################################

train <- fread("train.csv", stringsAsFactors = T)
store <- fread("store.csv", stringsAsFactors = T)

str(train)
str(store)
summary(train)
summary(store)

########################################## Data Cleaning #########################################

train$Open <- as.factor(train$Open)
train$DayOfWeek <- as.factor(train$DayOfWeek)
train$Promo <- as.factor(train$Promo)
train$Date <- as.Date(train$Date)
train$DayOfWeek <- NULL
train$Date <- NULL

store$CompetitionOpenSinceMonth <- str_pad(store$CompetitionOpenSinceMonth, width = 2, side = "left", pad = "0")
store <- unite(store, Comp_Since, CompetitionOpenSinceMonth, CompetitionOpenSinceYear, sep= "-")
store$Comp_Since <- as.yearmon(store$Comp_Since, format = "%m-%Y")
store$Comp_Since <- as.Date.yearmon(store$Comp_Since)
store$Comp_Since <- today() - store$Comp_Since
store$Comp_Since <- as.integer(store$Comp_Since)
store$Promo2SinceWeek <- NULL
store$PromoInterval <- NULL
store$Promo2SinceYear <- NULL
store$Promo2 <- as.factor(store$Promo2)

#Create different train sets.

# 1st Train set for Sales ~ Competition

train_comp <- train %>% select(Store, Customers, Sales)
train_comp <- train_comp %>% group_by(Store) %>% summarise_each(funs(sum))

store_comp <- train_comp %>% inner_join(store, by = "Store")

summary(store_comp)
store_comp[is.na(store_comp),] <- 0

index <- sample(nrow(store_comp), nrow(store_comp)*0.75)
st_comp <- store_comp[index,]
st_compv <- store_comp[-index,]

# Sales and Competition Models
lm_Sales <- lm(Sales ~ CompetitionDistance + Comp_Since + StoreType + Assortment + factor(Promo2) , data = st_comp)
summary(lm_Sales)
rf_Sales <- randomForest(Sales ~ CompetitionDistance + Comp_Since + StoreType + Assortment + Promo2 , data = st_comp, ntree = 500)
rf_Sales$importance
varImpPlot(rf_Sales)

pred1 <- predict(lm_Sales, st_compv)
pred2 <- predict(rf_Sales, st_compv)
rmse(pred2, st_compv$Sales)
rmse(pred1, st_compv$Sales)

lm_Cust <- lm(Customers ~ CompetitionDistance + Comp_Since + StoreType + Assortment + factor(Promo2) , data = st_comp)
summary(lm_Cust)
rf_Customers <- randomForest(Customers ~ CompetitionDistance + Comp_Since + StoreType + Assortment + Promo2 , data = st_comp)
rf_Customers$importance

pred3 <- predict(lm_Cust, st_compv)
pred4 <- predict(rf_Customers, st_compv)
rmse(pred3, st_compv$Customers)
rmse(pred4, st_compv$Customers)


# 2nd Train set for Holiday Analysis

train_hol <- train %>% group_by(Store, StateHoliday, SchoolHoliday, Promo, Open) %>% summarise_each(funs(sum))

index <- sample(nrow(train_hol), nrow(train_hol)*0.75)
tr_hol <- train_hol[index,]
tr_holv <- train_hol[-index,]

lm_sales_hol <- lm(Sales ~ StateHoliday + SchoolHoliday + Promo + Open, data = tr_hol)
summary(lm_sales_hol)
pred5 <- predict(lm_sales_hol, tr_holv)
rmse(pred5,tr_holv$Sales )

lm_cust_hol <- lm(Customers ~ StateHoliday + SchoolHoliday + Promo + Open, data = tr_hol)
summary(m6)
pred6 <- predict(lm_cust_hol, tr_holv)
rmse(pred6,tr_holv$Customers)

rf_sales_hol <- randomForest(Sales ~ StateHoliday + SchoolHoliday + Promo, data = tr_hol, ntree = 500)
pred7 <- predict(rf_sales_hol, tr_holv)
rmse(pred7,tr_holv$Sales)

rf_cust_hol <- randomForest(Sales ~ StateHoliday + SchoolHoliday + Promo, data = tr_hol)
varImpPlot(rf_cust_hol)
pred8 <- predict(rf_cust_hol, tr_holv)
rmse(pred8,tr_holv$Customers)


pred5 <- predict(m5, tr_holv)
conf5 <- table(pred5, tr_holv$Sales)
acc5 <- sum(diag(conf5))/sum(conf5)
acc5

'''
ggplot(train_hol, aes(x = StateHoliday, y = Sales, col = Promo, size = SchoolHoliday )) + geom_point(position = "jitter") + theme_few()

train_fact <- train %>% select(Store, Customers, Sales, SchoolHoliday, StateHoliday)
View(train_fact)
train_fact <- train_fact %>% group_by(Store, SchoolHoliday, StateHoliday) %>% summarise_each(funs(sum))
store_train <- train_fact %>% inner_join(store, by = "Store")
View(store_train)
str(store_train)
store_train$Promo2 <- as.factor(store_train$Promo2)

index <- sample(nrow(store_train), nrow(store_train)*0.75)
st_train <- store_train[index,]
st_valid <- store_train[-index,]


m6 <- lm(Sales ~ SchoolHoliday + StateHoliday + StoreType + Assortment + CompetitionDistance + Comp_Since + Promo2, data = st_train)
summary(m6)
'''
#########################################################################################



train1 <- train %>% group_by(Store, Promo, Open, StateHoliday) %>% summarise_each(funs(sum))
View(train1)


index <- sample(nrow(store_train), nrow(store_train)*0.75)
st_train <- store_train[index,]
st_valid <- store_train[-index,]

######################################## models ##########################################

store_train <- store_train %>% filter(Sales <= mean(Sales) + 3*sd(Sales))
store_train <- store_train %>% filter(Sales >= mean(Sales) - 3*sd(Sales))

#Remove Outliers

store_train$Sales <- (store_train$Sales)^2

#Square root transformation

store_train <- store_train[ Sales >= mean(Sales) - (3*sd(Sales)), .SD ]
store_train <- store_train[ , Year_Month := NULL]
store_train$Sales <- as.numeric(store_train$Sales)

cor(st_train$Sales, aggregate(st_train$CompetitionDistance), use = "complete")
cor(st_train$Sales, st_train$Comp_Since, use = "complete")

m1 <- lm(Sales ~  CompetitionDistance + Comp_Since + StoreType + Assortment + Promo2, data = st_train)
summary(m1)

str(st_train)


m2 <- lm(Sales ~ Promo + StateHoliday  + StoreType + Assortment + CompetitionDistance + Promo2 + Comp_Since, data = st_train)
summary(m2)


######################################### Prediction ######################################

pred <- predict(m2, st_valid)



rmse(st_valid$Sales,pred)

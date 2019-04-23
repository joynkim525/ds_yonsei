library(tidyverse)
library(MASS)
library(caret)
library(e1071)
library(pROC)

cust <- read.csv("file:///C:/Users/Lucy Jooyeon Kim/Desktop/ESC/2019-1/[Week 3] Assignment/customer.csv")
glimpse(cust)
summary(cust)
#Customer_ID ; unique -> not neccessary (r.1)
#PhoneService ; same info is included in MultipleLines (r.7)
#'No' from InternetService is related to other variables
#like Online Security, OnlineBackup, DEviceProtection, TechSupport, StreamingTV, StreamingMovies
#1st try ; 'No internet service' -> 'NO'
#2nd try ; interaction term ?

cust <- cust[,-c(1,7)]
cust1 <- cust
cust1$OS.yes <- ifelse(cust1$OnlineSecurity=='Yes', 1, 0)
cust1$OB.yes <- ifelse(cust1$OnlineBackup=='Yes', 1, 0)
cust1$DP.yes <- ifelse(cust1$DeviceProtection=='Yes', 1, 0)
cust1$TS.yes <- ifelse(cust1$TechSupport=='Yes', 1, 0)
cust1$ST.yes <- ifelse(cust1$StreamingTV=='Yes', 1, 0)
cust1$SM.yes <- ifelse(cust1$StreamingMovies=='Yes', 1, 0)
cust1 <- cust1[,-c(8:13)]
summary(cust1)

summary(cust1$Churn)
cust1$Churn <- ifelse(cust1$Churn == 'No', 0, 1)
colSums(is.na(cust))


set.seed(20190403)
train = sample(1:dim(cust1)[1], 0.7*dim(cust1)[1])
test=(!train)

y1 = cust1$Churn
x1 = model.matrix(Churn~., cust1)[,-1]

train.md <- glm(Churn ~ . , data=cust1[train,], family=binomial)
summary(train.md)
coef(train.md)

step.md <- stepAIC(train.md, direction = "both", trace = FALSE)
summary(step.md)

fin.md <- glm(formula = Churn ~ gender + SeniorCitizen + tenure + MultipleLines + 
                   InternetService + Contract + PaperlessBilling + PaymentMethod + 
                   MonthlyCharges + TotalCharges + OS.yes + TS.yes + ST.yes + 
                   SM.yes, family = binomial, data = cust1[train,])
summary(fin.md)
plot(fin.md)
y.ft <- fitted.values(fin.md)

test.dt <- cust1[-train,]
y.true <- y1[test]
y.pred <- predict(fin.md, type='response', newdata=test.dt[,-13])
y.pdt <- ifelse(y.pred >= 0.5, 1, 0)

y.tr <- as.factor(y.true)
y.pdt <- as.factor(y.pdt)
confusionMatrix(y.pdt, y.tr)

precisionscore=precision(y.pdt, y.tr)
recall=sensitivity(y.pdt, y.tr)
F1 <- (2 * precisionscore * recall) / (precisionscore + recall)
recall
F1

options(repr.plot.width =10, repr.plot.height = 8)
validation.roc <- roc(response = test.dt$Churn, predictor = as.numeric(y.pred))
train.roc <- roc(response = cust1[train,]$Churn, predictor = as.numeric(y.ft))
plot(validation.roc, legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)
plot(train.roc,  col='blue',    add = TRUE, print.auc.y = 1.0, print.auc = TRUE)
legend("right",c('train','validation'),lty=c(1,1),lwd=c(2,2),col=c('blue','black'))


#OR

try.model <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure +
                   MultipleLines + InternetService + OnlineSecurity + OnlineBackup +
                   DeviceProtection + TechSupport + StreamingTV + StreamingMovies +
                   Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges +
                   OnlineSecurity:InternetService + OnlineBackup:InternetService +
                   DeviceProtection:InternetService+TechSupport:InternetService+
                   StreamingTV:InternetService+StreamingMovies:InternetService,
                 data=cust[train,], family=binomial)
summary(try.model)

step.model <- stepAIC(train.model, direction = "both", trace = FALSE)
summary(step.model)
# 14 variables
fin.model <- glm(formula = Churn ~ gender + SeniorCitizen + tenure + MultipleLines + 
                   InternetService + Contract + PaperlessBilling + PaymentMethod + 
                   MonthlyCharges + TotalCharges + OS.yes + TS.yes + ST.yes + 
                   SM.yes, family = binomial, data = customer.train)
summary(fin.model)

head(customer.test)
length(test)
length(train)
dim(x1)[1]
x1.test <- x1[-train,]
dim(x1.test)

y.true <- y1[test]
y.pred <- predict(fin.model, newx = x1.test)
mean((y.pred - y.true)^2)
?prediction

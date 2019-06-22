library(MASS)
library(dplyr)
library(ggplot2)
library(rtf)
library(scales)
library(caret)
library(class)
library(pROC)


#1) Reading data and doing the subset
rm(list=ls())
df <- read.csv("Attrition.csv", stringsAsFactors = F)

set.seed(2019)

dfNew <- subset(df, select = c(Attrition,Age,DistanceFromHome,HourlyRate,
                               JobSatisfaction,PerformanceRating,
                               YearsAtCompany,PercentSalaryHike,
                               TotalWorkingYears,TrainingTimesLastYear,
                               YearsInCurrentRole,YearsSinceLastPromotion,
                               YearsWithCurrManager))

#2) Standardize selected variables and dividing the data 


dfNewScaled <- scale(dfNew[,2:13])
dfPartition <- createDataPartition(dfNew$Attrition, p = .7,
                                   list = FALSE,
                                   times = 1)

Train <- dfNew[dfPartition,-1]
Test <- dfNew[-dfPartition,-1]

#3)

test_atr<-dfNew[-dfPartition,"Attrition"]



myKnn <- knn(Train, Test, dfNew[dfPartition, "Attrition"], k= floor(sqrt(nrow(Train))), prob = TRUE)

test_atr<-dfNew[-dfPartition,"Attrition"]
test_atr <- as.factor(test_atr)
# Testing Accuracy 

table(myKnn, test_atr )
confusionMatrix(myKnn,test_atr)

predict(myKnn,Test, type="prob")

mean(myKnn == test_atr)

auc(as.vector(test_atr),attributes(myKnn)$prob)



#5)

new.emp <-c(Age = 40, DistanceFromHome = 3, HourlyRate = 20,JobSatisfaction = 2, 
            PerformanceRating = 3,  YearsAtCompany = 8,PercentSalaryHike = 2, TotalWorkingYears = 10, 
            TrainingTimesLastYear = 1,YearsInCurrentRole = 5, YearsSinceLastPromotion = 4, 
            YearsWithCurrManager = 2)

myKnn2<-knn(Train,new.emp,dfNew[dfPartition, "Attrition"],k=floor(sqrt(nrow(Train))),prob=T)

attr(myKnn2,"prob") #The probapility is 0.75

#6)
library(caret)

trn<-dfNew[dfPartition,]

train_control <- trainControl(method="repeatedcv", number=10)

grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))

model <- train(Attrition~., data=trn, trControl=train_control, method="knn")

# summarize results
print(model)
plot(model)

#7) Confusion Matrix


test_atr_factor <- as.factor(test_atr) # I need to transfer it first to factor 

predClass <- predict(model, Test) # Predict the model

confusionMatrix(predClass,test_atr_factor) # draw the confusion matrix 




library(mice)
library(data.table)

train_data <- fread("train.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))


#creating factors
train_data$Gender <- as.factor(train_data$Gender)
train_data$Married <- as.factor(train_data$Married)
train_data$Dependents <- as.factor(train_data$Dependents)
train_data$Education <- as.factor(train_data$Education)
train_data$Self_Employed <- as.factor(train_data$Self_Employed)
train_data$Credit_History <- as.factor(train_data$Credit_History)
train_data$Property_Area <- as.factor(train_data$Property_Area)
train_data$Loan_Status <- as.factor(train_data$Loan_Status)
train_data$ApplicantIncome <- as.numeric(train_data$ApplicantIncome)
train_data$CoapplicantIncome <- as.numeric(train_data$CoapplicantIncome)
train_data$LoanAmount <- as.numeric(train_data$LoanAmount)
train_data$Loan_Amount_Term <- as.numeric(train_data$Loan_Amount_Term)


mice <- mice(train_data[,-c(1,12)],m=1,maxit=10,meth='pmm')

completedData <- complete(mice,1)

t_data <- cbind(completedData, train_data$Loan_Status)
colnames(t_data)

t_data <- cbind(completedData, train_data$Loan_Status)
colnames(t_data) <- c("Gender",
                      "Married",
                      "Dependents",
                      "Education",
                      "Self_Employed",
                      "ApplicantIncome",
                      "CoapplicantIncome",
                      "LoanAmount",
                      "Loan_Amount_Term",
                      "Credit_History",
                      "Property_Area",
                      "Loan_Status")

n = nrow(t_data)

# We create an index for 70% of obs. by random
set.seed(20)

trainIndex = sample(1:n, 
                    size = round(0.7*n), 
                    replace=FALSE) 

# We use the index to create training data

train_data = t_data[trainIndex,] 

# We take the remaining 30% as the testing data

test_data = t_data[-trainIndex,] 

summary(train_data)
summary(test_data)

# Load packages required for random forest:

library(randomForest)

#RF Classifier with ntree 10

rf_10 <-randomForest(train_data$Loan_Status ~., data=train_data, ntree=10, na.action=na.exclude, 
                     importance=TRUE, proximity=TRUE)

print(rf_10)

#RF Classifier with ntree 20

rf_20 <-randomForest(train_data$Loan_Status ~., data=train_data, ntree=20, na.action=na.exclude, 
                     importance=TRUE, proximity=TRUE)

print(rf_20)

#RF Classifier with ntree 30

rf_30 <-randomForest(train_data$Loan_Status ~., data=train_data, ntree=30, na.action=na.exclude, 
                     importance=TRUE, proximity=TRUE)

print(rf_30)

#RF Classifier with ntree 40

rf_40 <-randomForest(train_data$Loan_Status ~., data=train_data, ntree=40, na.action=na.exclude, 
                     importance=TRUE, proximity=TRUE)

print(rf_40)

#RF Classifier with ntree 50

rf_50 <-randomForest(train_data$Loan_Status ~., data=train_data, ntree=50, na.action=na.exclude, 
                     importance=TRUE, proximity=TRUE)

print(rf_50)

#RF Classifier with ntree 60

rf_60 <-randomForest(train_data$Loan_Status ~., data=train_data, ntree=60, na.action=na.exclude, 
                     importance=TRUE, proximity=TRUE)

print(rf_60)

#RF Classifier with ntree 70

rf_70 <-randomForest(train_data$Loan_Status ~., data=train_data, ntree=70, na.action=na.exclude, 
                     importance=TRUE, proximity=TRUE)

print(rf_70)

#RF Classifier with ntree 80

rf_80 <-randomForest(train_data$Loan_Status ~., data=train_data, ntree=80, na.action=na.exclude, 
                     importance=TRUE, proximity=TRUE)

print(rf_80)

#RF Classifier with ntree 90

rf_90 <-randomForest(train_data$Loan_Status ~., data=train_data, ntree=90, na.action=na.exclude, 
                     importance=TRUE, proximity=TRUE)

print(rf_90)

#RF Classifier with ntree 100

rf_100 <-randomForest(train_data$Loan_Status ~., data=train_data, ntree=100, na.action=na.exclude, 
                      importance=TRUE, proximity=TRUE)

print(rf_100)

#RF Classifier with ntree 100

rf_110 <-randomForest(train_data$Loan_Status ~., data=train_data, ntree=110, na.action=na.exclude, 
                      importance=TRUE, proximity=TRUE)

print(rf_110)


#RF Classifier with ntree 120

rf_120 <-randomForest(train_data$Loan_Status ~., data=train_data, ntree=120, na.action=na.exclude, 
                      importance=TRUE, proximity=TRUE)

print(rf_120)

varImpPlot(rf)

#Tuning RF

mtry <- tuneRF(train_data[-11], train_data$Loan_Status, ntreeTry=100,  
               stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE,
               na.action=na.exclude)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

#RF with the best mtry

set.seed(32)

rf <-randomForest(Loan_Status~., data=train_data, 
                  mtry=best.m, importance=TRUE, ntree=80) 

print(rf)


# RF on test data

predicted_values <- predict(rf, test_data, type= "prob")

# Confusion matrix

library(caret)

threshold <- 0.4

pred <- factor( ifelse(predicted_values[,2] > threshold, "Y", "N") )

head(pred)

levels(test_data$Loan_Status)[2]

confusionMatrix(pred, test_data$Loan_Status, 
                positive = levels(test_data$Loan_Status)[2]) 


#Test Data Approach

mydata2=read.csv("test.csv", sep=",", 
                 header=T, strip.white=T,
                 na.strings=c("NA", "NaN", "", "?"))

mydata2$Gender<-NULL

#creating factors
#mydata2$Gender <- as.factor(mydata2$Gender)
mydata2$Married <- as.factor(mydata2$Married)
mydata2$Dependents <- as.factor(mydata2$Dependents)
mydata2$Education <- as.factor(mydata2$Education)
mydata2$Self_Employed <- as.factor(mydata2$Self_Employed)
mydata2$Credit_History <- as.factor(mydata2$Credit_History)
mydata2$Property_Area <- as.factor(mydata2$Property_Area)
mydata2$Loan_Status <- as.factor(mydata2$Loan_Status)
mydata2$ApplicantIncome <- as.numeric(mydata2$ApplicantIncome)
mydata2$CoapplicantIncome <- as.numeric(mydata2$CoapplicantIncome)
mydata2$LoanAmount <- as.numeric(mydata2$LoanAmount)
mydata2$Loan_Amount_Term <- as.numeric(mydata2$Loan_Amount_Term)


mice <- mice(mydata2[,-c(1)],m=1,maxit=10,meth='pmm')

completedData <- complete(mice,1)

test_data <- cbind(completedData)
colnames(test_data)

predicted_values <- predict(rf, test_data, type= "prob")

# Confusion matrix

library(caret)

threshold <- 0.4

pred <- factor( ifelse(predicted_values[,2] > threshold, "Y","N") )

head(pred)

write.csv(pred,file="testdata")




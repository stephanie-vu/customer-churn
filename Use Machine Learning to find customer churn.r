#customer churn analysis
#Stephanie Vu

library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(MASS)
library(randomForest)
library(party)
library(caret)

churn <- read.csv("C:\\STEPHY\\BACKUP\\CSUMB\\Fall 2022\\bus 421\\Machine Learning\\Churn.csv")
str(churn)

#use sapply to check the number if missing values in each columns

sapply(churn, function(x) sum(is.na(x)))

# delete missing values
churn <- churn[complete.cases(churn), ]

# change “No internet service” and  to “No” for the following 6 features

# OnlineSecurity
# OnlineBackup
# DeviceProtection
# TechSupport
# streamingTV
# streamingMovies

cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}


# change “No phone service” to “No” for column “MultipleLines”

churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

# Since the minimum tenure is 1 month and maximum tenure is 72 months, we can group them into five tenure groups:
#   
#   “0–12 Month”
# “12–24 Month”
# “24–48 Months”
# “48–60 Month”
# “> 60 Month”

min(churn$tenure); max(churn$tenure)

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

churn$tenure_group <- sapply(churn$tenure,group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)              #convert to factor


#Change the values in column “SeniorCitizen” from 0 or 1 to “No” or “Yes”.
churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

#change character features to factors
churn$Churn <- as.factor(churn$Churn)
churn$gender <- as.factor(churn$gender)
churn$Partner <- as.factor(churn$Partner)
churn$Dependents <- as.factor(churn$Dependents)
churn$PhoneService <- as.factor(churn$PhoneService)
churn$InternetService <- as.factor(churn$InternetService)

churn$Contract <- as.factor(churn$Contract)
churn$PaperlessBilling <- as.factor(churn$PaperlessBilling)
churn$PaymentMethod <- as.factor(churn$PaymentMethod)


### split train test set 70% train 30% test
set.seed(2017)
intrain<- createDataPartition(churn$Churn,p=0.7,list=FALSE)
training<- churn[intrain,]
testing<- churn[-intrain,]
str(training)

#remove customer ID from the data
training2 <- subset(training, select=-c(customerID))

#run setseed again
set.seed(2017)


## random forest with 200 trees
rfModel <- randomForest(Churn ~., data = training2, ntree=200)
print(rfModel)

#tuning RF by adjusting number of trees to 1000
rfModel2 <- randomForest(Churn ~., data = training2, ntree=1000)
print(rfModel2)

varImpPlot(rfModel, sort=T, n.var = 5, main = 'Top 5 Feature Importance')

##-Build Decision Tree with 5 Most Important Variables----##


#build the decision tree
tree <- ctree(Churn~MonthlyCharges+tenure_group+TotalCharges+Contract+PaymentMethod, training2)
plot(tree)

pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree with 5 Feature Importance"); 
table(Predicted = pred_tree, Actual = testing$Churn)

# calculate the decision tree accuracy

p <- predict(tree, training2)
tab2 <- table(Predicted = p, Actual = training2$Churn)
tab3 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab3))/sum(tab3)))








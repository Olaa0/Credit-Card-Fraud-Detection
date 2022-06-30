#credit card fraud detection on a sample data set

#importation of library 
install.packages("pie3d")
install.packages("ranger")
install.packages("caret")
install.packages("data.table")

library(ranger)
library(caret)
library(data.table)
library(ggplot2)
library(pie3d)

getwd()
setwd("C:\\Users\\oladi\\OneDrive\\Desktop\\Financial Analyst")
credit <- read.csv("creditcard.csv")

#The structure of the data set
str(credit)
#conversion of the class to a factor variable
credit$Class <-factor(credit$Class, levels = c(0,1))
#summary of the data
summary(credit)
#identify the missing value
sum(is.na(credit))
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#separate genuine=0 and fraudulent=1 transactions into new data frame
credit.genuine <- credit[credit$Class==0,]
credit.fraudulent <- credit[credit$Class==1,]


#Data visualization on the basis of physically imp features 
ggplot()+geom_density(data=credit.genuine,aes(x=Time),color="blue",
                      fill="blue",alpha=0.12)+
  geom_density(data = credit.fraudulent,aes(x=Time),color="red",fill="red",
               alpha=0.12)
ggplot()+geom_density(data=credit.genuine,aes(x=Amount),color="blue",
                      fill="blue",alpha=0.12)+
  geom_density(data = credit.fraudulent,aes(x=Amount),color="red",fill="red",
               alpha=0.12)

#ascertain the distribution of genuine and fraud transactions in the data set 
table(credit$Class)

#ascertain the percentage of fraud and legit transactions in the data set
prop.table(table(credit$Class))

#the use of pie chart of credit card transactions 
labels <- c("genuine","fraudulent") # create a vector first of genuine and fraudulent 
labels <- paste(labels,round(100 * prop.table(table(credit$Class)),2 ))
labels <-paste0(labels, "%")

#the use of pie chart for data set
pie(table(credit$Class),labels,col = c("orange","red"),
    main = "Pie chart of credit card transactions",radius =1 )


#exploration 
data.table(credit)

#performing random statistics on the data set 


summary(credit)
#imbalance of the data set because of the huge difference between fraudulent and genuine  
table(credit$Class)
#headings on each variable
names(credit)

#summary of amount 
summary(credit$Amount)
sd(credit$Amount)
IQR(credit$Amount)

var(credit$Amount)

#manipulation
credit$amount <-scale(credit$Amount)

#remove the time variable 
credit2 <-credit[,-c(1)]
head(credit2)

set.seed(12)
library(caTools)

sample.credit <- sample.split(credit2$Class,SplitRatio = 0.80)

train_data <-subset(credit2,sample.credit==TRUE)
test_data <-subset(credit2,sample.credit==FALSE)

#dimension of both train_data & test_data)
dim(train_data)
dim(test_data)

#using logistic regression data 
logistic_Model <- glm(Class~.,test_data,family = binomial())
summary(logistic_Model)

plot(logistic_Model)
#issues drawing up the plot 

logistic_Model1 <- glm(Class~.,train_data,family = binomial())
summary(logistic_Model1)

plot(logistic_Model1)

#use of confusion matrix 
library(caret)
confusionMatrix(data = logistic_Model1, reference = credit$Class)

#the use of ROC curve for evaluation 
install.packages("pROC")
library(pROC)
lr.predict <- predict(logistic_Model1,test_data,probability =TRUE)
auc.gb <-roc(test_data$Class, lr.predict,plot =TRUE, col ="red")


#We have about 90% accuracy because the data is highly imbalance 

#use of decision tree 
library(rpart)
library(rpart.plot)

decision_model <-rpart(Class ~.,credit,method ="class")
predicted_val  <-predict(decision_model,credit,type = "class")
probability <-predict(decision_model, credit,type ='prob')
rpart.plot(decision_model)
summary(decision_model)

#the use of neural network 
install.packages("neuralnet")
library(neuralnet)
NN_model <- neuralnet::neuralnet(Class ~.,train_data,linear.output = FALSE)
plot(NN_model)

predNN <-compute(NN_model,test_data)
resultNN <-predNN$net.result
resultNN=ifelse(resultNN>0.6,1,0)
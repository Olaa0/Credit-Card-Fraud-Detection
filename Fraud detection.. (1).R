getwd()
setwd("C:\\Users\\oladi\\OneDrive\\Desktop\\Financial Analyst")
credit2 <- read.csv("creditcard.csv")

#Glance of the structure of the data set
str(credit2)

#conversion of the class variable to a factor, 0 = Genuine transactions, 1 = fraudulent transactions 
credit2 $Class <- factor(credit2$Class,levels = c(0,1) )

#summary of the data set
summary(credit2)

#identify the number of missing value, it was identified "0" missing value 
sum(is.na(credit2))

#ascertain the distribution of genuine and fraudulent transactions through the use of table function in the data set 
table(credit2$Class)

#ascertain the percentage of genuine and fraudulent transactions in the data set 
prop.table(table(credit2$Class))

#Pie chat of credit card transactions in between genuine and fraudulent transactions 
labels <-c("Genuine","fraudulent")
labels <-paste(labels,round(100*prop.table(table(credit2$Class)),2))
labels <-paste0(labels,"%") 

pie(table(credit2$Class),labels,col = c("orange","red"),
        main = "Pie chart of credit card transactions")
#----------------------------------------------------------------------------------------------------------------------------------------
#build a model using sample set, a machine learning technique can not be adopted because of an imbalance data set. 
#there is clear difference between genuine and fraudulent transactions 

library(dplyr)#used to ascertain a small fraction of the data set 

set.seed(1) 
credit2 <- credit2 %>% sample_frac(0.1) #extraction of 10% of the data set.(credit2) 

table(credit2$Class)
install.packages("ggplot2")
library(ggplot2)

ggplot(data = credit2,aes(x = V1, y = V2,col = Class)) + 
  geom_point() +
  theme_bw() + 
  scale_color_manual(values = c('dodgerblue2','red'))
#-------------------------------------------------------------------------------------------------------------------------------------------------
#Creating training and test sets for fraud detection model

#it is important to find a balance on training model rather than the test data to use a model  
library(caTools)

set.seed(123)

#split the data set on the percentage of 80%  
data.sample = sample.split(credit2$Class,SplitRatio = 0.80)
train_data = subset(credit2, data.sample==TRUE)

test_data = subset(credit2,data.sample==FALSE)

dim(train_data)
dim(test_data)

#Before creating model for this data set, there is need to balance the data set whereby both fraudulent and genuine transactions are equal
# the use of random over sampling(ROS)
table(train_data$Class)
#number of genuine transactions in the train data set.
no_genuine  <-22750
#fraction that the train data set should be divided into 50%
new_fraction_legit <-0.50 
new_number_total <- no_genuine/new_fraction_legit #22750/0.50

#install package ('ROSE')
install.packages("ROSE")
#Random Over Sampling Examples, Random over sample technique selects examples from the minority class. 
library(ROSE) 
oversampling_result <- ovun.sample(Class ~ .,
                                   data = train_data,
                                   method = "over",
                                   N = new_n_total,
                                   seed = 2019)

oversampled_credit2 <-oversampling_result$data

table(oversampled_credit2$Class)

ggplot(data = oversampled_credit2,aes(x = V1, y = V2,col = Class)) + 
  geom_point(position = position_jitter(width = 0.2)) +
  theme_bw() + 
  scale_color_manual(values = c('dodgerblue2','red'))
#--------------------------------------------------------------------------------------

table(train_data$Class)

number_fraud <-35
new_frac_fraud <- 0.50 
new_n_total <- number_fraud/new_frac_fraud # = 35/0.50

#library(rose)
#Random under sampling involves selecting examples from the majority class 
undersampling_result <-ovun.sample(Class ~.,
                                   data = train_data,
                                   method = "under",
                                   N = new_n_total,
                                   seed = 2019)

undersample_credit2 <- undersampling_result$data
table(undersample_credit2$Class)

ggplot(data = undersample_credit2,aes(x = V1, y = V2,col = Class)) + 
  geom_point() +
  theme_bw() + 
  scale_color_manual(values = c('dodgerblue2','red'))
#------------------------------------------------------------------------
n_new <-nrow(train_data) #22785
fraction_fraud_new <- 0.50 

sampling_result <- ovun.sample(Class ~.,
                               data = train_data,
                               method ="both",
                               N = n_new,
                               p = fraction_fraud_new,
                               seed = 2019)
sampled_credit <- sampling_result$data
table(sampled_credit$Class)

prop.table(table(sampled_credit$Class))

ggplot(data = sampled_credit,aes(x = V1, y = V2,col = Class)) + 
  geom_point() +
  theme_bw() + 
  scale_color_manual(values = c('dodgerblue2','red'))

##using SMOTE to balance the data set 
install.packages("smotefamily")
library(smotefamily)

table(train_data$Class)

#set the number of fraud and genuine transaction and the desired percentage 
n0 <- 22750 # number of genuine transactions 
n1 <- 35    # number of fraudulent transactions 
r0 <- 0.6   #desired percentage to balance the data set 

#calculate the value for the duplicate _size parameter of SMOTE 
ntimes <- ((1- r0) /r0) * (n0 / n1) -1

smote_output = SMOTE(X = train_data[, -c(1,31)],
                     target = train_data$Class,
                     K = 5, dup_size = ntimes)

credit_smote <- smote_output$data 

colnames(credit_smote)[30] <-"Class" #removing the  last column of data set.  

#converted the percentage of 60% of genuine and 40% of fraudulent. 
prop.table(table(credit_smote$class))

#class distribution for the over-sampled data set using SMOTE 

ggplot(data = credit2,aes(x = V1, y = V2,col = Class)) + 
  geom_point() +
  theme_bw() + 
  scale_color_manual(values = c('dodgerblue2','red'))

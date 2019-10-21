#Load data
setwd("C:/Users/Aditi/Desktop/Aditi/Fall 2018/Data Preparation/DPProject")
bc.df <- read.csv("breastcancer.csv", header = TRUE)  

dim(bc.df) # find the dimension of data frame 
head(bc.df) # show the first six rows 
View(bc.df) # show all the data in a new tab

#Sampling of dataset
s <- sample(row.names(bc.df), 286) 
bc.df <- bc.df[s,]
View(bc.df)

bc.df$Recurrence <- as.integer(bc.df$Recurrence)
str(bc.df)

#Plotting the Histogram
bc.df$Recurrence <-as.factor(bc.df$Recurrence)
ggplot(bc.df)+ geom_bar(aes(x=Tumor.size,fill=Recurrence))
plot(Age,Recurrence)

#creating dummy variables
install.packages("dummies")
library(dummies)
library(caret)
library(PreProcess)
library(neuralnet)
dummiestrain <- dummyVars( ~ ., data = bc.df)
dmyfinal <- data.frame(predict(dummiestrain, newdata = bc.df))
print(dmyfinal)
View(dmyfinal)

#replacing missing values with median
summary(dmyfinal)

for(i in 1:ncol(dmyfinal))
{dmyfinal[is.na(dmyfinal[,i]), i] <- median(dmyfinal[,i], na.rm = TRUE)}


# Random sampling
samplesize = 0.60 * nrow(dmyfinal)
set.seed(80)
index = sample( seq_len ( nrow ( dmyfinal ) ), size = samplesize )
dmyfinal

# Create training and test set
datatrain = dmyfinal[ index, ]
datatest = dmyfinal[ -index, ]

## Scale data for neural network

max = apply(dmyfinal , 2 , max)
min = apply(dmyfinal, 2 , min)
scaled = as.data.frame(scale(dmyfinal, center = min, scale = max - min))


# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]


#NEURAL NETWORK

#Fitting the neural network
allvars <- colnames(dmyfinal)
predictorvars <- allvars[!allvars%in%"Recurrence"]
predictorvars <- paste(predictorvars, collapse ="+")
form=as.formula(paste("Recurrence~",predictorvars, collapse ="+" ))

#train data
NN_train= neuralnet(formula=form, hidden =5, linear.output= T, data=trainNN, stepmax=5000)
predict_trainNN = compute(NN_train, trainNN[,c(2:35)])
str(predict_trainNN)
plot(NN_train)
predict_trainNN = (predict_trainNN$net.result*(max(trainNN$Recurrence) - min(trainNN$Recurrence))) + min(trainNN$Recurrence)
actual_values_trainNN= (trainNN$Recurrence)*(max(trainNN$Recurrence)-min(trainNN$Recurrence))+min(trainNN$Recurrence)
output<-compute(NN_train,trainNN[,-1])
p1<-output$net.result
pred1<-ifelse(p1>0.5,1,0)
tab1<-table(pred1,trainNN$Recurrence)
tab1
sum(diag(tab1))/sum(tab1)
plot(trainNN$Recurrence, predict_trainNN)

#test data
NN_test= neuralnet(formula=form, hidden =5, linear.output= T, data=testNN, stepmax=5000)
predict_testNN = compute(NN_test, testNN[,c(2:35)])
str(predict_testNN)
predict_testNN = (predict_testNN$net.result*(max(testNN$Recurrence) - min(testNN$Recurrence))) + min(testNN$Recurrence)
actual_values_testNN= (testNN$Recurrence)*(max(testNN$Recurrence)-min(testNN$Recurrence))+min(testNN$Recurrence)
output_test<-compute(NN_test,testNN[,-1])
p2<-output_test$net.result
pred2<-ifelse(p2>0.5,1,0)
tab2<-table(pred2,testNN$Recurrence)
tab2
sum(diag(tab2))/sum(tab2)
plot(testNN$Recurrence, predict_testNN)






#LOGISTIC REGRESSION

#run logistic regression model
mymodel<-glm(Recurrence~., data= trainNN, family ='binomial' )
summary(mymodel)
#predict with training data
predict1<-predict(mymodel, trainNN, type = 'response')
head(predict1)
#check for accuracy and error percentage
pr1<-ifelse(predict1>0.5,1,0)
table_1<-table(predicted=pr1, Actual=trainNN$Recurrence)
table_1
error1 <- 1-sum(diag(table_1))/sum(table_1)
acc1 <- sum(diag(table_1))/sum(table_1)
plot(trainNN$Recurrence, predict1)

#predict with test data
predict2<-predict(mymodel, testNN, type='response')
head(predict2) 
#check for accuracy and error
pr2<-ifelse(predict2>0.5,1,0)
table_2<-table(prediction=pr2, Actual=testNN$Recurrence)
table_2
error2 <- 1-sum(diag(table_2))/sum(table_2)
acc2 <- sum(diag(table_2))/sum(table_2)



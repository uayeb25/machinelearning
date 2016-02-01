library(caret)
library(kernlab)
library(ISLR)
library(ggplot2)
library(Hmisc)
library(gridExtra)
library(RANN)
library(ROCR)
library(rpart)
library(rpart.plot)
library(e1071)
library(randomForest)
library(ada)
library(FactoMineR)






################### Cleaning



dataTraining <- read.csv("../pml-training.csv",header = T)
dataTesting <- read.csv("../pml-testing.csv",header = T)



#To Training 
myframe <- data.frame(fields = c(),naValues = c(),len = c(),percent = c())
for(name in colnames(dataTraining)){
    myfield <- dataTraining[,name]
    navals <- sum(length(which(is.na(myfield))))
    tmpframe <- data.frame(fields = c(name),naValues = c(navals),len = c(length(myfield)),percent = c((navals/length(myfield))*100))
    myframe <- rbind(myframe,tmpframe)
}
fieldswithoutNas <- subset(myframe,percent == 0)
training <- dataTraining[,fieldswithoutNas$fields]
testing <- dataTesting[,fieldswithoutNas$fields]


#To testing
myframe <- data.frame(fields = c(),naValues = c(),len = c(),percent = c())
for(name in colnames(testing)){
    myfield <- testing[,name]
    navals <- sum(length(which(is.na(myfield))))
    tmpframe <- data.frame(fields = c(name),naValues = c(navals),len = c(length(myfield)),percent = c((navals/length(myfield))*100))
    myframe <- rbind(myframe,tmpframe)
}
fieldswithoutNas <- subset(myframe,percent == 0)
training <- training[,fieldswithoutNas$fields]
testing <- testing[,fieldswithoutNas$fields]


#Factor with many levels
newfields <- c()
for(name in colnames(training)){
    if(name != "X"){
        myfield <- training[,name]
        tmpclass <- class(myfield)
        if( tmpclass == "factor" ){
            u <- unique(myfield)
            l <- length(u)
            if(l<10){
                newfields <- c(newfields,name)
            }
        }else{
            newfields <- c(newfields,name)
        }
    }
}

training <- training[,newfields]
testing <- testing[,newfields[1:57]]


inTrain <- createDataPartition(y=training$classe,p=0.7,list = FALSE)

mytraining <- training[inTrain,]
validation <- training[-inTrain,]



##RPART



model <- rpart(classe~.,data = mytraining)
prp(model,extra = 104, branch.type = 2,box.col = c("pink","palegreen3")[model$frame$yval])


pred <- predict(model,validation, type="class")
mc <- table(validation$classe,pred)
mc
acierto <- (sum(diag(mc)))/sum(mc)
acierto
error <- 1 - acierto
error


###Forest

modelo <- randomForest(classe~.,data = mytraining,importance=TRUE)
pred <- predict(modelo,validation[,-58])
mc <- table(validation$classe,pred)
mc
acierto <- (sum(diag(mc)))/sum(mc)
acierto
error <- 1 - acierto
error



##### The 20 rows for test 
testing2 <- testing
testing2$classe <- NA
testing2$classe <- as.factor(testing2$classe)
validation2 <- rbind(validation,testing2)

finaltest <- subset(validation2,is.na(classe))
finaltest


pred <- predict(modelo,newdata=finaltest)
pred







---
title: "Final Project"
author: "Uayeb Caballero"
output: html_document
---


#Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: <a href='http://groupware.les.inf.puc-rio.br/har'>http://groupware.les.inf.puc-rio.br/har</a> (see the section on the Weight Lifting Exercise Dataset).


# Load Libraries

```{r}
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
```

# Read Data

```{r}
dataTraining <- read.csv("../pml-training.csv",header = T)
dataTesting <- read.csv("../pml-testing.csv",header = T)
```


# Clean Data

Our first step to clean our datasets is identify which columns have NAs values


```{r}

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

```

Now our second step is identify which columns are factors and have many levels to remove them

```{r}
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
```

# Create our dataset to training and validation

```{r}
training <- training[,newfields]
testing <- testing[,newfields[1:57]]


inTrain <- createDataPartition(y=training$classe,p=0.7,list = FALSE)

mytraining <- training[inTrain,]
validation <- training[-inTrain,]
```

Now to validate our validation-set we gonna use two distinct ML one of them is RPART and the next will be RandomForest.

We have taken this desicion to validate which method is better to work with the dataset got


#RPART

```{r}
model <- rpart(classe~.,data = mytraining)
prp(model,extra = 104, branch.type = 2,box.col = c("pink","palegreen3")[model$frame$yval])


pred <- predict(model,validation, type="class")
mc <- table(validation$classe,pred)
mc
acierto <- (sum(diag(mc)))/sum(mc)
acierto
error <- 1 - acierto
error
```

#Random Forest
```{r}
modelo <- randomForest(classe~.,data = mytraining,importance=TRUE)
pred <- predict(modelo,validation[,-58])
mc <- table(validation$classe,pred)
mc
acierto <- (sum(diag(mc)))/sum(mc)
acierto
error <- 1 - acierto
error
```

Now as we can see for this dataset randomforest works well to predict which classe has been used


# Final Prediction with our test-set

```{r}
testing2 <- testing
testing2$classe <- NA
testing2$classe <- as.factor(testing2$classe)
validation2 <- rbind(validation,testing2)

finaltest <- subset(validation2,is.na(classe))
finaltest


pred <- predict(modelo,newdata=finaltest)
pred
```

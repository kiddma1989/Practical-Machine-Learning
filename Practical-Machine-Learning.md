---
title: "Practical Machine Learning_Course Project"
author: "Luoning"
date: "6/13/2020"
output:
  html_document: default
  pdf_document: default
---

## Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

## Data loading and cleaning
### Dataset
The training data for this project are available here:

[https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv)

The test data are available here:

[https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv)

The data for this project come from [http://groupware.les.inf.puc-rio.br/har](http://groupware.les.inf.puc-rio.br/har). 

### Environment prepration

```{r library, warning=FALSE, cache=TRUE}
library(caret)
library(knitr)
library(rpart)
library(rpart.plot)
library(rattle)
library(reshape2)
```

### Data loading and cleaning
The next step is loading the dataset from the URL provided above. The training dataset is then partinioned in 2 to create a Training set (70% of the data) for the modeling process and a Test set (with the remaining 30%) for the validations. The testing dataset is not changed and will only be used for testing the trained model.

```{r load, warning=FALSE, cache=TRUE}
urltrain<-'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
urlval<-'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

data<-read.csv(url(urltrain),header = T, na.strings = c("", "NA"))
validation<-read.csv(url(urlval),header = T, na.strings = c("", "NA"))

dim(data)
dim(validation)
```

The datasets have 160 variables. But many of them have lots of NAs or near zero variables.

```{r clean, warning=FALSE, cache=TRUE}
#remove variables with nearly zero variance
NZVvar<-nearZeroVar(data)
fulldata<-data[,-NZVvar]
validation<-validation[,-NZVvar]
dim(fulldata)

#remove variables that are mostly NA
NAratio<-sapply(fulldata, function(x) mean(is.na(x)))>0.95
fulldata<-fulldata[,NAratio==FALSE]
validation<-validation[,NAratio==FALSE]
intrain<-createDataPartition(fulldata$classe,p=0.7,list = FALSE)
training<-fulldata[intrain,]
training<-training[,-(1:5)]
testing<-fulldata[-intrain,]
testing<-testing[,-(1:5)]
dim(training)
dim(testing)
```
With the cleaning process above, the number of variables for the analysis has been reduced to 54 only.

### Correlation analysis
To analyze the correlation between variables, a correlation map is plotted.

```{r correlation, warning=FALSE, cache=FALSE}
cor.matrix<-cor(training[sapply(training, is.numeric)])
temp<-melt(cor.matrix)
qplot(x=Var1, y=Var2, data=temp, fill=value, geom="tile") +
   scale_fill_gradient2(limits=c(-1, 1)) +
    theme(axis.text.x = element_text(angle=-90, vjust=0.5, hjust=0))
```
Some of the variables appear to be highly correlated, and the correlated variables were removed using threshold (> 0.9).

```{r remove, warning=FALSE, cache=FALSE}
temp<-findCorrelation(cor.matrix,cutoff = 0.9)
training<-training[,-temp]
testing<-testing[,-temp]
dim(training)
dim(testing)
```

## Prediction model
### Random forest
```{r class}
set.seed(21218)
modFitRF<-train(classe~.,data = training,method='rf',trControl=trainControl(method="cv", number=3, verboseIter=FALSE))
modFitRF$finalModel
predRF<-predict(modFitRF,newdata=testing)
confRF<-confusionMatrix(predRF,testing$classe)
confRF$overall[1]
```

### Decision trees
```{r dt}
set.seed(21218)
modFitDT<-train(classe~.,data = training,method='rpart')
fancyRpartPlot(modFitDT$finalModel)
predDT<-predict(modFitDT,testing)
confDT<-confusionMatrix(predDT,testing$classe)
confDT$overall[1]
```

### Generalized Boosting
```{r gbm}
set.seed(21218)
ControlGBM<-trainControl(method = "repeatedcv", number = 5, repeats = 1)
modFitGBM<-train(classe~.,data = training,method='gbm',trControl=ControlGBM,verbose=FALSE)
modFitGBM$finalModel
predGBM<-predict(modFitGBM,newdata=testing)
confGBM<-confusionMatrix(predGBM,testing$classe)
confGBM$overall[1]
```

## Predictions
The accuracy for the models is Random forest > Generalized boosting > Decision tree. Apply the Random forest model to predict the 20 testing data.
```{r predict, warning=FALSE, cache=FALSE}
predTest<-predict(modFitRF,validation)
predTest
```
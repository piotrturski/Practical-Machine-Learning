library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

## Q2
summary(mixtures$Superplasticizer)

## Q3
# Load the Alzheimer's disease data using the commands: ...
# Find all the predictor variables in the training set that begin with IL. Perform principal components 
# on these variables with the preProcess() function from the caret package. 
# Calculate the number of principal components needed to capture 90% of the variance. How many are there?

# names(training)[grep('^IL',names(training))]
# summary(training[grep('^IL',names(training))])

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

preProcess((training[grep('^IL',names(training))]), method='pca', thresh = 0.9)$numComp

## Q4
# Load the Alzheimer's disease data using the commands:..
# Create a training data set consisting of only the predictors with variable names beginning with IL 
# and the diagnosis. Build two predictive models, one using the predictors as they are and one using PCA 
# with principal components explaining 80% of the variance in the predictors. Use method="glm" in the 
# train function. What is the accuracy of each method in the test set? Which is more accurate?

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

trdata <- training[grep('^IL',names(training))]

train(training$diagnosis ~ ., method='glm', data = trdata)$results$Accuracy

train(training$diagnosis ~ .,
      method='glm', 
      preProcess='pca',
      trControl = trainControl(preProcOptions = list(thresh = 0.8)),
      data = trdata)$results$Accuracy



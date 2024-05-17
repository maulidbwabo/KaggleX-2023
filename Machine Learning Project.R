##checking the data sets
data("longley")
head(longley)
summary(longley)
tail(longley)
#
tempdir()
# [1] "C:\Users\XYZ~1\AppData\Local\Temp\Rtmp86bEoJ\Rtxt32dcef24de2"
dir.create(tempdir())
library()
describe(longley)
describe.by(longley)
describe.by(longley$GNP)
describeBy(longley$GNP)
describeBy(longley$Year)
##UCI data sets
install.packages("mlbench")
library(help="mlbench")
library(mlbench)
##Boston data sets
data("BostonHousing")
head(BostonHousing)
tail(BostonHousing)
summary(BostonHousing)
describe(BostonHousing)
##breast cancer 
data("BreastCancer")
head(BreastCancer)
summary(BreastCancer)
describe(BreastCancer)
#glass data 
data("Glass")
head(Glass)
tail(Glass)
summary(Glass)
describe(Glass)
is.na(Glass)
##data isones
data(Ionosphere)
summary(Ionosphere)
summary(Ionosphere)
describe(Ionosphere)
dim(Ionosphere)
##data diabetes 
data("PimaIndiansDiabetes")
summary(PimaIndiansDiabetes)
head(PimaIndiansDiabetes)
describe(PimaIndiansDiabetes)
dim(PimaIndiansDiabetes)
tail(PimaIndiansDiabetes)
plot(PimaIndiansDiabetes)
##data sonar
data("Sonar")
summary(Sonar)
head(Sonar)
describe(Sonar)
dim(Sonar)
##visualization
plot(Sonar$V1)
##data soyabean
data("Soybean")
head(Soybean)
tail(Soybean)
is.na(Soybean)
na.omit(Soybean)
summary(Soybean)
head(Soybean)
plot(Soybean)
plot(Soybean$plant.stand)
plot(Soybean$temp)
plot(Soybean$crop.hist,Soybean$Class)
##package applied to predictive modelling 
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
library(help="AppliedPredictiveModeling")
##abalone data
data("abalone")
summary(abalone)
dim(abalone)
describe(abalone)
head(abalone)
##Load the data from the website 
library(RCurl)
##
urlfile ='https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data'
downloaded = getURL(urlfile, ssl.verifypeer=FALSE)
connection = textConnection(downloaded)
dataset <- read.csv(connection, header=FALSE)
head(dataset)
summary(dataset)
##Descriptive statistics 
library(mlbsench)
##Pima 
summary(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes,n=20)
##
dim(PimaIndiansDiabetes)
head(PimaIndiansDiabetes)
data("PimaIndiansDiabetes",n=20)
head(PimaIndiansDiabetes,n=20)
head(PimaIndiansDiabetes,n=100)
##data types
sapply(BostonHousing,class)
sapply(PimaIndiansDiabetes,class)
##Class distributions
y = PimaIndiansDiabetes$diabetes
cbind(freq=table(y), percentage=prop.table(table(y))*100)
##data summary 
#standard deviation
sapply(PimaIndiansDiabetes[,1:8], sd)
describe(PimaIndiansDiabetes)
##Histograms
data(iris)
# create histograms for each attribute
par(mfrow=c(1,4))
for(i in 1:4) {
  hist(iris[,i], main=names(iris)[i])
}
##
head(PimaIndiansDiabetes)
## Descriptive statistics 
par(mfrow=c(1,8))
for(i in 1:8) {
  hist(PimaIndiansDiabetes[,i], main=names(PimaIndiansDiabetes)[i])
}
##
par(mfrow=c(1,4))
for(i in 1:4) {
  plot(density(iris[,i]), main=names(iris)[i])
}
##
par(mfrow=c(1,6))
for(i in 1:6) {
  plot(density(PimaIndiansDiabetes[,i]), main=names(PimaIndiansDiabetes)[i])
}
##
par(mfrow=c(1,6))
for(i in 1:6) {
  boxplot(PimaIndiansDiabetes[,i], main=names(PimaIndiansDiabetes)[i])
}
##
head(BreastCancer)
##
par(mfrow=c(2,4))
for(i in 2:9) {
  counts <- table(BreastCancer[,i])
  name <- names(BreastCancer)[i]
  barplot(counts, main=name)
}
#pima
head(PimaIndiansDiabetes)
##
par(mfrow=c(2,4))
for(i in 2:7) {
  counts = table(PimaIndiansDiabetes[,i])
  name = names(PimaIndiansDiabetes)[i]
  barplot(counts, main=name)
}
##Mapping out the missing data
##
install.packages("Amelia")
library(Amelia)
library(Rcpp)
##
missmap(Soybean, col=c("green", "grey"), legend=FALSE)
##Multivariate visualiation
library(corrplot)
##
# calculate correlations
correlations = cor(iris[,1:4])
# create correlation plot
corrplot(correlations, method="circle")
##
pairs(iris)
pairs(PimaIndiansDiabetes)
str(PimaIndiansDiabetes)
head(PimaIndiansDiabetes)
View(PimaIndiansDiabetes)
library(ggplot2)
library(caret)
library(lattice)
##
pairs(diabetes~., data=PimaIndiansDiabetes, col=PimaIndiansDiabetes$diabetes)
##Density plots
x = PimaIndiansDiabetes[,1:8]
y = PimaIndiansDiabetes[,9]
scales = list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)
featurePlot(x=x, y=y, plot="box")
##Data pre-processing through PCA
#Data scaling 
# summarize data
summary(PimaIndiansDiabetes[,1:8])
describe(PimaIndiansDiabetes[,1:8])
# calculate the pre-process parameters from the dataset
preprocessDiabetes = preProcess(PimaIndiansDiabetes[,1:8], method=c("scale"))
# summarize transform parameters
print(preprocessDiabetes)
summary(preprocessDiabetes)
# transform the dataset using the parameters
transformedDiabetes= predict(preprocessDiabetes, PimaIndiansDiabetes[,1:8])
# summarize the transformed dataset
transformedDiabetes
summary(transformedDiabetes)
##Centering 
# calculate the pre-process parameters from the data set (centering)
preprocessDiabetes1 = preProcess(PimaIndiansDiabetes[,1:8], method=c("center"))
# summarize transform parameters
print(preprocessDiabetes1)
summary(preprocessDiabetes1)
# transform the dataset using the parameters
transformedDiabetes1= predict(preprocessDiabetes1, PimaIndiansDiabetes[,1:8])
# summarize the transformed dataset
transformedDiabetes1
summary(transformedDiabetes1)
##Standardized the data (center and scale)
# calculate the pre-process parameters from the dataset
preprocessDiabetes2 = preProcess(PimaIndiansDiabetes[,1:8], method=c("center", "scale"))
print(preprocessDiabetes2)
summary(preprocessDiabetes2)
# transform the dataset using the parameters
transformedDiabetes2= predict(preprocessDiabetes2, PimaIndiansDiabetes[,1:8])
# summarize the transformed dataset
summary(transformedDiabetes2)
##Data Normalization
preprocessDiabetes3 = preProcess(PimaIndiansDiabetes[,1:8], method=c("range"))
summary(preprocessDiabetes3)
##data transformation
transformedDiabetes3=predict(preprocessDiabetes3,PimaIndiansDiabetes[,1:8])
summary(transformedDiabetes3)
## BoXCOX transformation
preprocessDiabetes4=preProcess(PimaIndiansDiabetes[,1:8],method = c("BoxCox"))
print(preprocessDiabetes4)
transformedDiabetes4=predict(preprocessDiabetes4,PimaIndiansDiabetes[,1:8])
summary(transformedDiabetes4)
##Yeo-Johnson transformation
preprocessDiabetes5=preProcess(PimaIndiansDiabetes[,1:8],method = c("YeoJohnson"))
print(preprocessDiabetes5)
##Transformation
transformedDiabetes5=predict(preprocessDiabetes5,PimaIndiansDiabetes[,1:8])
summary(transformedDiabetes5)
##PCA and ICA
preprocessDiabetes6=preProcess(PimaIndiansDiabetes,method = c("center","scale","pca"))
print(preprocessDiabetes6)
##transformation
transformedDiabetes6=predict(preprocessDiabetes6,PimaIndiansDiabetes[,1:8])
summary(transformedDiabetes6)
#ICA
install.packages("fastICA")
library(fastICA)
preprocessDiabetes7=preProcess(PimaIndiansDiabetes,method = c("center","scale","ica"),n.comp=5)
print(preprocessDiabetes7)
##transformation
transformedDiabetes7=predict(preprocessDiabetes7,PimaIndiansDiabetes[,1:8])
summary(transformedDiabetes7)
##Re-sampling methods to estimates the machine learning models
##Data splitting 
#Diabetes data set will deployed into this example of model evaluation
install.packages("klaR")
library(klaR)
library(MASS)
# define an 80%/20% train/test split of the data set
trainIndex = createDataPartition(PimaIndiansDiabetes$diabetes, p=0.80, list=FALSE)
dataTrain= PimaIndiansDiabetes[ trainIndex,]
dataTest = PimaIndiansDiabetes[-trainIndex,]
head(dataTest)
##
# train a naive Bayes model
fit = NaiveBayes(diabetes~., data=dataTrain)
fit
# make predictions
predictions= predict(fit, dataTest[,1:8])
# summarize results
confusionMatrix(predictions$class, dataTest$diabetes)
##Bootstrap
# define training control
trainControl = trainControl(method="boot", number=100)
# evaluate the model
fit1= train(diabetes~., data=dataTrain, trControl=trainControl, method="nb")
# display the results
print(fit1)
##Cross Validation
# define training control
trainControl1= trainControl(method="cv", number=10)
#evaluate the model
fit2= train(diabetes~., data=dataTrain, trControl=trainControl1, method="nb")
# display the results
print(fit2)
##Repeated cross validation
# define training control
trainControl2 = trainControl(method="repeatedcv", number=10, repeats=5)
#evaluate the model
fit3= train(diabetes~., data=dataTrain, trControl=trainControl2, method="nb")
# display the results
print(fit3)
##LOOCV
# define training control
trainControl3 = trainControl(method="LOOCV")
#evaluate the model
fit4= train(diabetes~., data=dataTrain, trControl=trainControl3, method="nb")
# display the results
print(fit4)
##EVALUATION METRICS
#Kappa and accuracy 
# prepare resampling method
trainControl4= trainControl(method="cv", number=5)
set.seed(7)
fit5 = train(diabetes~., data=PimaIndiansDiabetes, method="glm", metric="Accuracy",
             trControl=trainControl4)
fit5
names(fit5)
fit5$control
fit5$finalModel
#RME
# load data
data(longley)
# prepare resampling method
trainControl5= trainControl(method="cv", number=5)
set.seed(7)
fiT6= train(Employed~., data=longley, method="lm", metric="RMSE", trControl=trainControl5)
# display results
print(fiT6)
##ROC
# prepare re-sampling method
trainControl6 = trainControl(method="cv", number=5, classProbs=TRUE,
                             summaryFunction=twoClassSummary)
set.seed(7)
fit7= train(diabetes~., data=PimaIndiansDiabetes, method="glm", metric="ROC",
            trControl=trainControl6)
# display results
print(fit7)
##logarithmic loss
# prepare resampling method
trainControl7 = trainControl(method="cv", number=5, classProbs=TRUE,
                             summaryFunction=mnLogLoss)
set.seed(7)
fit8 = train(Species~., data=iris, method="rpart", metric="logLoss", trControl=trainControl7)
# display results
print(fit7)
##Spot-checking algorithms
summary(BostonHousing)
head(BostonHousing)
describe(BostonHousing)
pairs(BostonHousing)
## Linear regression
# fit model
LG1 = lm(medv~., BostonHousing)
# summarize the fit
print(LG1)
# make predictions
predictions = predict(LG1, BostonHousing)
# summarize accuracy
mse = mean((BostonHousing$medv - predictions)^2)
print(mse)
##linear regression in a caret package
# train
set.seed(7)
trainControl = trainControl(method="cv", number=5)
fit.lg1 = train(medv~., data=BostonHousing, method="lm", metric="RMSE", preProc=c("center",
                                                                                  "scale"), trControl=trainControl)
# summarize fit
print(fit.lg1)
##Logistic regressions
# fit model
fit.log = glm(diabetes~., data=PimaIndiansDiabetes, family=binomial(link='logit'))
# summarize the fit
print(fit.log)
# make predictions
probabilities = predict(fit.log, PimaIndiansDiabetes[,1:8], type='response')
predictions = ifelse(probabilities > 0.5,'pos','neg')
# summarize accuracy
table(predictions, PimaIndiansDiabetes$diabetes)
#GLM in the caret package 
# Load the dataset
data(PimaIndiansDiabetes)
# train
library(readxl)
apply(PimaIndiansDiabetes,2, class)   # shows all columns are numeric
install.packages(pak)
library(pak)
pak::pak('topepo/caret/pkg/caret')
str(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes)
PimaIndiansDiabetes=data.frame(PimaIndiansDiabetes)
set.seed(7)
control = trainControl(method="repeatedcv", number=10, repeats=5)
fit.glm = train(PimaIndiansDiabetes$diabetes~., data=PimaIndiansDiabetes[,1:8], 
                method="logit", metric="Accuracy",
                preProc=c("center", "scale"),
                trControl=control)
# summarize fit
print(fit.glm)
##LDA
fit.LDA = lda(diabetes~., data=PimaIndiansDiabetes)
# summarize the fit
print(fit.LDA)
names(fit.LDA)
# make predictions
predictions = predict(fit.LDA, PimaIndiansDiabetes[,1:8])$class
# summarize accuracy
table(predictions, PimaIndiansDiabetes$diabetes)
##Caret package
# train
set.seed(7)
trainControl = trainControl(method="cv", number=5)
fit.lda = train(diabetes~., data=PimaIndiansDiabetes, method="lda", metric="Accuracy",
                preProc=c("center", "scale"), trControl=trainControl)
# summarize fit
print(fit.lda)
##Regularized regressions
install.packages("glmnet")
library(glmnet)
library(Matrix)
#convert the data into matrix
x = as.matrix(PimaIndiansDiabetes[,1:8])
y = as.matrix(PimaIndiansDiabetes[,9])
# fit model
fit.R= glmnet(x, y, family="binomial", alpha=0.5, lambda=0.001)
# summarize the fit
print(fit.R)
names(fit.R)
fit.R$lambda
# make predictions
predictions <- predict(fit, x, type="class")
# summarize accuracy
table(predictions, PimaIndiansDiabetes$diabetes)
##Boston house data set
data(BostonHousing)
head(BostonHousing)
str(BostonHousing)
BostonHousing$chas = as.numeric(as.character(BostonHousing$chas))
x1 = as.matrix(BostonHousing[,1:13])
y1 = as.matrix(BostonHousing[,14])
# fit model
fit.B = glmnet(x1, y1, family="gaussian", alpha=0.5, lambda=0.001)
# summarize the fit
print(fit.B)
# make predictions
predictions <- predict(fit, x, type="link")
# summarize accuracy
mse <- mean((y - predictions)^2)
##Regurization with the caret package
# train
set.seed(7)
trainControl = trainControl(method="cv", number=5)
fit.glmnet = train(diabetes~., data=PimaIndiansDiabetes, method="glmnet",
                   metric="Accuracy", preProc=c("center", "scale"), trControl=trainControl)
# summarize fit
print(fit.glmnet)
##boston house data sets
# train
set.seed(7)
trainControl = trainControl(method="cv", number=5)
fit.glmnet1 = train(medv~., data=BostonHousing, method="glmnet", metric="RMSE",
                    preProc=c("center", "scale"), trControl=trainControl)
# summarize fit
print(fit.glmnet1)
##Non-linear algorithms(KNN,Naive,SVM)
# fit model(KNN)
fit.N = knn3(diabetes~., data=PimaIndiansDiabetes, k=3)
# summarize the fit
print(fit.N)
# make predictions
predictions = predict(fit.N, PimaIndiansDiabetes[,1:8], type="class")
# summarize accuracy
table(predictions, PimaIndiansDiabetes$diabetes)
##Boston data set
BostonHousing$chas = as.numeric(as.character(BostonHousing$chas))
x2 = as.matrix(BostonHousing[,1:13])
y2 = as.matrix(BostonHousing[,14])
# fit model
fit.BO = knnreg(x2, y2, k=3)
# summarize the fit
print(fit.BO)
summary(fit.BO)
# make predictions
predictions = predict(fit.BO, x2)
# summarize accuracy
mse = mean((BostonHousing$medv - predictions)^2)
print(mse)
##PIMAINDIA
set.seed(7)
trainControl = trainControl(method="cv", number=5)
fit.knn.p = train(diabetes~., data=PimaIndiansDiabetes, method="knn", metric="Accuracy",
                  preProc=c("center", "scale"), trControl=trainControl)
# summarize fit
print(fit.knn.p)
names(fit.knn.p)
##BOSTON
# train
set.seed(7)
trainControl = trainControl(method="cv", number=5)
fit.knn.BOST = train(medv~., data=BostonHousing, method="knn", metric="RMSE",
                     preProc=c("center", "scale"), trControl=trainControl)
# summarize fit
print(fit.knn.BOST)
##NAIVE 
library(e1071)
# fit model
fit.NAIVE = naiveBayes(diabetes~., data=PimaIndiansDiabetes)
# summarize the fit
print(fit.NAIVE)
# make predictions
predictions = predict(fit.NAIVE, PimaIndiansDiabetes[,1:8])
# summarize accuracy
table(predictions, PimaIndiansDiabetes$diabetes)
##PIMA INDIA
# train
set.seed(7)
trainControl = trainControl(method="cv", number=5)
fit.nb = train(diabetes~., data=PimaIndiansDiabetes, method="nb", metric="Accuracy",
               trControl=trainControl)
# summarize fit
print(fit.nb)
summary(fit.nb)
##SUPPORT VECTOR MACHINE
library(kernlab)
# fit model
fit.SVM = ksvm(diabetes~., data=PimaIndiansDiabetes, kernel="rbfdot")
# summarize the fit
print(fit.SVM)
# make predictions
predictions = predict(fit.SVM, PimaIndiansDiabetes[,1:8], type="response")
# summarize accuracy
table(predictions, PimaIndiansDiabetes$diabetes)
##REGRESSION EXAMPLE
library(mlbench)
data("BostonHousing")
str(BostonHousing)
# load data
# fit model
fit.MED = ksvm(medv~., BostonHousing, kernel="rbfdot")
# summarize the fit
print(fit.MED)
# make predictions
predictions = predict(fit.MED, BostonHousing)
# summarize accuracy
mse = mean((BostonHousing$medv - predictions)^2)
print(mse)
##SVM with radial basis
set.seed(7)
trainControl = trainControl(method="cv", number=5)
fit.svmRadial = train(diabetes~., data=PimaIndiansDiabetes, method="svmRadial",
                      metric="Accuracy", trControl=trainControl)
# summarize fit
print(fit.svmRadial)
##Regression
# train
set.seed(7)
trainControl = trainControl(method="cv", number=5)
fit.svmradial = train(medv~., data=BostonHousing, method="svmRadial", metric="RMSE",
                      trControl=trainControl)
# summarize fit
print(fit.svmradial)
##CLASSIFICATION AND REGRESSION TREEE
library(rpart)
library(rpart.plot)
##
# fit model
fit.r= rpart(diabetes~., data=PimaIndiansDiabetes)
# summarize the fit
print(fit.r)
# make predictions
predictions = predict(fit.r, PimaIndiansDiabetes[,1:8], type="class")
# summarize accuracy
table(predictions, PimaIndiansDiabetes$diabetes)
## Regressions example
# fit model
fit.part= rpart(medv~., data=BostonHousing, control=rpart.control(minsplit=5))
# summarize the fit
print(fit.part)
# make predictions
predictions = predict(fit.part, BostonHousing[,1:13])
# summarize accuracy
mse = mean((BostonHousing$medv - predictions)^2)
print(mse)
##CARET PACKAGE
# train
set.seed(7)
trainControl = trainControl(method="cv", number=5)
fit.rpart = train(diabetes~., data=PimaIndiansDiabetes, method="rpart", metric="Accuracy",
                  trControl=trainControl)
# summarize fit
print(fit.rpart)
summary(fit.rpart)
##regression example
# train
set.seed(7)
trainControlx = trainControl(method="cv", number=2)
fit.rpart1 = train(medv~., data=BostonHousing, method="rpart", metric="RMSE",
                   trControl=trainControlx)
# summarize fit
print(fit.rpart1)
##COMPARE THE MACHINE LEARNING MODELS
#TRAIN MODELS
# prepare training scheme
trainControly = trainControl(method="repeatedcv", number=10, repeats=3)
# CART
set.seed(7)
fit.cart = train(diabetes~., data=PimaIndiansDiabetes, method="rpart",
                 trControl=trainControly)
# LDA
set.seed(7)
fit.lda = train(diabetes~., data=PimaIndiansDiabetes, method="lda", trControl=trainControly)
# SVM
set.seed(7)
fit.svm = train(diabetes~., data=PimaIndiansDiabetes, method="svmRadial",
                trControl=trainControly)
# KNN
set.seed(7)
fit.knn = train(diabetes~., data=PimaIndiansDiabetes, method="knn", trControl=trainControly)
# Random Forest
set.seed(7)
fit.rf = train(diabetes~., data=PimaIndiansDiabetes, method="rf", trControl=trainControly)
# collect resamples
results = resamples(list(CART=fit.cart, LDA=fit.lda, SVM=fit.svm, KNN=fit.knn, RF=fit.rf))
##
summary(results)
##box and whisker plots to compare models
scales= list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)
#DENSITY PLOT
# density plots of accuracy
scales = list(x=list(relation="free"), y=list(relation="free"))
densityplot(results, scales=scales, pch = "|")
##parallel plot
parallelplot(results)
splom(results)
#statistical significance
diffs=diff(results)
diffs
summary(diffs)
##Navigating the caret package
help(package="caret")
vignette(package="caret")
##Tuning machine learning model
library(randomForest)
data("Sonar")
head(Sonar)
str(Sonar)
describe(Sonar)
dataset=Sonar
y=dataset[,1:60]
x=dataset[,61]
str(x)
str(y)
##Create model with default paramters
trainControl = trainControl(method="repeatedcv", number=10, repeats=3)
seed = 7
metric = "Accuracy"
set.seed(seed)
mtry = sqrt(x)
mtry = sqrt(ncol(y))
tunegrid = expand.grid(.mtry=mtry)
rfDefault = train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid,
                  trControl=trainControl)
print(rfDefault)
#Tuning using the caret package
#Random search tuning parameters 
# Random Search
trainControla = trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry =sqrt(ncol(y))
rfRandom = train(Class~., data=dataset, method="rf", metric=metric, tuneLength=15,
                 trControl=trainControla)
print(rfRandom)
plot(rfRandom)
##Grid Search 
trainControlz = trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid = expand.grid(.mtry=c(1:15))
rfGrid = train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid,
               trControl=trainControlz)
print(rfGrid)
plot(rfGrid)
##
#Tuning manually 
# Manual Search
trainControlh = trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegridz = expand.grid(.mtry=c(sqrt(ncol(y))))
modellist = list()
for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(seed)
  fit = train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegridz,
              trControl=trainControlh, ntree=ntree)
  key = toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results = resamples(modellist)
summary(results)
dotplot(results)
##Tuning the RF model with an MTRY and NTREE
customRF = list(type="Classification", library="randomForest", loop=NULL)
customRF$parameters = data.frame(parameter=c("mtry", "ntree"), class=rep("numeric", 2),
                                 label=c("mtry", "ntree"))
customRF$grid = function(x, y, len=NULL, search="grid") {}
customRF$fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry=param$mtry, ntree=param$ntree, ...)
}
customRF$predict = function(modelFit, newdata, preProc=NULL, submodels=NULL)
  predict(modelFit, newdata)
customRF$prob = function(modelFit, newdata, preProc=NULL, submodels=NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort = function(x) x[order(x[,1]),]
customRF$levels = function(x) x$classes
##Tuning using the caret package
# train model
trainControlf <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrida = expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
set.seed(seed)
custom = train(Class~., data=dataset, method=customRF, metric=metric, tuneGrid=tunegrida,
               trControl=trainControlf)
summary(custom)
names(custom)
plot(custom)
##Combine predictions
##Bagging, Boosting, and Stacking
#Install some packages
library(caretEnsemble)
library(mlbench)
data(Ionosphere)
str(Ionosphere)
head(Ionosphere)
##Removing some constant and change the character into numeric 
dataset = Ionosphere
dataset = dataset[,-2]
dataset$V1 = as.numeric(as.character(dataset$V1))
##
head(dataset)
str(dataset)
##
# Example of Boosting Algorithms
trainControl = trainControl(method="repeatedcv", number=10, repeats=3)
seed = 7
metric = "Accuracy"
# C5.0
set.seed(seed)
fit.c50 = train(Class~., data=dataset, method="C5.0", metric=metric,
                trControl=trainControl)
fit.c50
names(fit.c50)
##pimaindians
data("PimaIndiansDiabetes")
str(PimaIndiansDiabetes)
fit.c501 = train(diabetes~., data=PimaIndiansDiabetes, method="C5.0", metric=metric,
                 trControl=trainControl)
fit.c501
# Stochastic Gradient Boosting
set.seed(seed)
fit.gbm = train(Class~., data=dataset, method="gbm", metric=metric,
                trControl=trainControl, verbose=FALSE)
fit.gbm
##
fit.gbm1 = train(diabetes~., data=PimaIndiansDiabetes, method="gbm", metric=metric,
                 trControl=trainControl, verbose=FALSE)
fit.gbm1
# summarize results
boostingResults1 = resamples(list(c5.0=fit.c501, gbm=fit.gbm1))
summary(boostingResults1)
dotplot(boostingResults1)
boostingResults = resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boostingResults)
dotplot(boostingResults)
##
# Example of Bagging algorithms
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# Bagged CART
set.seed(seed)
fit.treebag = train(Class~., data=dataset, method="treebag", metric=metric,
                    trControl=trainControl)
fit.treebag
# Random Forest
set.seed(seed)
fit.rf = train(Class~., data=dataset, method="rf", metric=metric, trControl=trainControl)
fit.rf
# summarize results
baggingResults = resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(baggingResults)
dotplot(baggingResults)
##Stacking models
# Example of Stacking algorithms
# create submodels
trainControl = trainControl(method="repeatedcv", number=10, repeats=3,
                            savePredictions=TRUE, classProbs=TRUE)
algorithmList = c('lda', 'rpart', 'glm', 'knn', 'svmRadial')
set.seed(seed)
models = caretList(Class~., data=dataset, trControl=trainControl, methodList=algorithmList)
results = resamples(models)
summary(results)
dotplot(results)
##
# correlation between results
modelCor(results)
splom(results)
##
# stack using glm
stackControl = trainControl(method="repeatedcv", number=10, repeats=3,
                            savePredictions=TRUE, classProbs=TRUE)
set.seed(seed)
stack.glm = caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
##Using the random forest for stacking 
# stack using random forest
set.seed(seed)
stack.rf = caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)
##Save and finalize the machine learning model
# load dataset
data(PimaIndiansDiabetes)
# create 80%/20% for training and validation datasets
set.seed(9)
validationIndex = createDataPartition(PimaIndiansDiabetes$diabetes, p=0.80, list=FALSE)
validation = PimaIndiansDiabetes[-validationIndex,]
training = PimaIndiansDiabetes[validationIndex,]
# train a model and summarize model
set.seed(9)
trainControl = trainControl(method="cv", number=10)
fit.lda = train(diabetes~., data=training, method="lda", metric="Accuracy",
                trControl=trainControl)
print(fit.lda)
print(fit.lda$finalModel)
# estimate skill on validation dataset
set.seed(9)
predictions = predict(fit.lda, newdata=validation)
confusionMatrix(predictions, validation$diabetes)
##
library(mlbench)
data("Sonar")
# create 80%/20% for training and validation datasets
validationIndex = createDataPartition(Sonar$Class, p=0.80, list=FALSE)
validation = Sonar[-validationIndex,]
training = Sonar[validationIndex,]
# train a model and summarize model
set.seed(7)
trainControl = trainControl(method="repeatedcv", number=10, repeats=3)
fit.rf = train(Class~., data=training, method="rf", metric="Accuracy",
               trControl=trainControl, ntree=2000)
print(fit.rf)
print(fit.rf$finalModel)
# create standalone model using all training data
set.seed(7)
str(validation)
head(validation)
dim(validation)
finalModel = randomForest(Class~., training, mtry=2, ntree=2000)
# make a predictions on "new data" using the final model
finalPredictions = predict(finalModel, validation[,1:60])
confusionMatrix(finalPredictions, validation$Class)
##
# load packages
library(caret)
library(mlbench)
library(randomForest)
install.packages("doMC")
##
install.packages("doMC", repos="http://R-Forge.R-project.org")
library(doMC)
install.packages("foreach")
installed.packages("iterators")
installed.packages("parallel")
##
library(caret)
library(ggplot2)
library(lattice)
library(randomForest)
# create 80%/20% for training and validation datasets
validationIndex = createDataPartition(Sonar$Class, p=0.80, list=FALSE)
validation = Sonar[-validationIndex,]
training = Sonar[validationIndex,]
# create final standalone model using all training data
set.seed(7)
finalModel = randomForest(Class~., training, mtry=2, ntree=2000)
# save the model to disk
saveRDS(finalModel, "./finalModel.rds")
# later...
# load the model
superModel = readRDS("./finalModel.rds")
print(superModel)
# make a predictions on "new data" using the final model
finalPredictions = predict(superModel, validation[,1:60])
confusionMatrix(finalPredictions, validation$Class)
##First machine learning project
library(readr)
Machine_Learning = read.csv("C:/Users/bwabo/OneDrive/Desktop/DATASETS/Machine Learning.csv")
View(Machine_Learning)
project=Machine_Learning
View(project)
summary(project)
mdata=subset(project,select  = -c(2:5))
str(mdata)
mdata$Gender=as.factor(mdata$Gender)
str(mdata)
dim(mdata)
is.na(mdata)
na.omit(mdata)
dim(mdata)
machinedata=mdata
##Splitting the data set
# create a list of 80% of the rows in the original dataset we can use for training
validationIndex = createDataPartition(machinedata$Gender, p=0.80, list=FALSE)
# select 20% of the data for validation
validation = machinedata[-validationIndex,]
# use the remaining 80% of data to training and testing the models
mdataset = machinedata[validationIndex,]
## descriptive analysis 
dim(mdataset)
str(mdataset)
summary(mdataset)
na.omit(mdataset)
summary(mdataset)
is.na(mdataset)
##Check NA
library(psych)
describe(mdataset)
head(mdataset)
tail(mdataset)
summary(mdataset)
sapply(mdataset,class)
View(mdataset)
mdataset=as.numeric(mdataset)
str(mdataset)
##Lapply functions 
mdataset[2:22] = lapply(mdataset[2:22], as.numeric)
head(mdataset)
str(mdataset)
##Class Distribution
# summarize the class distribution
percentage = prop.table(table(mdataset$Gender)) * 100
cbind(freq=table(mdataset$Gender), percentage=percentage)
##statistical summary
summary(mdataset)
describe(mdataset)
##data visualization
x = mdataset[,2:22]
y = mdataset[,1]
##Box plot
# boxplot for each attribute on one image
par(mfrow=c(2,8))
for(i in 2:8) {
  boxplot(x[,i], main=names(mdataset)[i])
}
##plot
plot(y)
##multivariate plot 
featurePlot(x=x, y=y, plot="ellipse")
featurePlot(x=x, y=y, plot="box")
##
# density plots for each attribute by class value
scales = list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)
#Testing Harness 
# Run algorithms using 10-fold cross validation
trainControl = trainControl(method="cv", number=10)
metric = "Accuracy"
##Model building 
# LDA
set.seed(7)
fit.lda = train(Gender~., data=mdataset, method="lda", metric=metric,
                trControl=trainControl)
fit.lda
# CART
set.seed(7)
fit.cart = train(Gender~., data=mdataset, method="rpart", metric=metric,
                 trControl=trainControl)
fit.cart
# KNN
set.seed(7)
fit.knn = train(Gender~., data=mdataset, method="knn", metric=metric,
                trControl=trainControl)
fit.knn
# SVM
set.seed(7)
fit.svm = train(Gender~., data=mdataset, method="svmRadial", metric=metric,
                trControl=trainControl)
fit.svm
set.seed(7)
fit.rf = train(Gender~., data=mdataset, method="rf", metric=metric, trControl=trainControl)
fit.rf
##Select the best model
results = resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
str(PimaIndiansDiabetes)
dotPlot(results)
##
Bigdata1 = read.csv("C:/Users/bwabo/OneDrive/Desktop/DATASETS/Bigdata.csv1")
str(Bigdata1)
newmachine=Bigdata1
head(newmachine)
summary(newmachine)
View(newmachine)
newmachine=subset(newmachine,select = -c(6:8))
newmachine
dim(newmachine)
str(newmachine)
na.omit(newmachine)
na.omit(newmachine$Inflation)
summary(newmachine)
str(Bigdata)
describe(newmachine)
#
Bigdata$ISO3=as.factor(Bigdata$ISO3)
str(Bigdata)
Bigdata$country=as.factor(Bigdata$country)
str(Bigdata)
##sample 
##data visualization
x1 = newmachine[,1:4]
y1 = newmachine[,5]
##Box plot
# boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x1[,i], main=names(newmachine)[i])
}
##plot
plot(y)
##multivariate plot 
featurePlot(x=x1, y=y1, plot="ellipse")

featurePlot(x=x1, y=y1, plot="box")
##
# density plots for each attribute by class value
scales = list(x1=list(relation="free"), y=list(relation="free"))
featurePlot(x=x1, y=y1, plot="density", scales=scales)
##
Bigdata_csv1 = read.csv("C:/Users/bwabo/OneDrive/Desktop/DATASETS/Bigdata.csv1.csv")
machine=Bigdata_csv1
str(machine)
summary(machine)
# create a list of 80% of the rows in the original dataset we can use for training
validationIndex = createDataPartition(machine$Inflation, p=0.80, list=FALSE)
# select 20% of the data for validation
validation1 = machine[-validationIndex,]
str(validation1)
dim(validation1)
# use the remaining 80% of data to training and testing the models
machiner = machine[validationIndex,]
dim(machiner)
str(machiner)
summary(machiner)
##
head(machine)
machiner$
  ##data visualization
  x2 = machine[,1:5]
y2 = machine[,6]
##Box plot
# boxplot for each attribute on one image
par(mfrow=c(1,5))
for(i in 1:5) {
  boxplot(x1[,i], main=names(newmachine)[i])
}
##plot
##multivariate plot 
featurePlot(x=x2, y=y2, plot="ellipse")

featurePlot(x=x1, y=y1, plot="box")
##
# density plots for each attribute by class value
scales = list(x1=list(relation="free"), y=list(relation="free"))
featurePlot(x=x1, y=y1, plot="density", scales=scales)
##Des
na.omit(machine)
dim(machine)
str(machine)
cor(machine[,1:5])
##Numeric 
head(machine)
View(machine)
machine=subset(machine,select = -c(6:8))
machine[,6:8] = as.numeric(as.character(machine[,6:8]))
str(machine)
describe(machine)
##visualization
par(mfrow=c(2,3))
for(i in 1:5) {
  hist(machine[,i], main=names(machine)[i])
}
##
# density plot for each attribute
par(mfrow=c(2,3))
for(i in 1:5) {
  plot(density(machine[,i]), main=names(machine)[i])
}
##
# scatterplot matrix
pairs(machine[,1:5])
##
# correlation plot
library(corrplot)
correlations = cor(machine[,1:5])
corrplot(correlations, method="circle")
##
trainControl = trainControl(method="repeatedcv", number=10, repeats=3)
metric = "RMSE"
##
set.seed(7)
fit.lm = train(Inflation~., data=machine, method="lm", metric=metric, preProc=c("center","scale"), trControl=trainControl)
fit.lm
# GLM
set.seed(7)
fit.glm = train(Inflation~., data=machine, method="glm", metric=metric, preProc=c("center",
                                                                                  "scale"), trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet = train(Inflation~., data=machine, method="glmnet", metric=metric,
                   preProc=c("center", "scale"), trControl=trainControl)
# SVM
set.seed(7)
fit.svm = train(Inflation~., data=machine, method="svmRadial", metric=metric,
                preProc=c("center", "scale"), trControl=trainControl)
# CART
set.seed(7)
grid = expand.grid(.cp=c(0, 0.05, 0.1))
fit.cart = train(Inflation~., data=machine, method="rpart", metric=metric, tuneGrid=grid,
                 preProc=c("center", "scale"), trControl=trainControl)
# KNN
set.seed(7)
fit.knn = train(Inflation~., data=machine, method="knn", metric=metric, preProc=c("center",
                                                                                  "scale"), trControl=trainControl)
##
# Compare algorithms
results = resamples(list(LM=fit.lm, GLM=fit.glm, GLMNET=fit.glmnet, SVM=fit.svm,
                         CART=fit.cart, KNN=fit.knn))
summary(results)
dotplot(results)
##Highly correlated 
set.seed(7)
cutoff = 0.70
correlations = cor(machine[,1:5])
highlyCorrelated = findCorrelation(correlations, cutoff=cutoff)
for (value in highlyCorrelated) {
  print(names(machine)[value])
}
##
datasetFeatures = machine[,-highlyCorrelated]
dim(datasetFeatures)
##
set.seed(7)
fit.lm = train(Inflation~., data=datasetFeatures, method="lm", metric=metric,
               preProc=c("center", "scale"), trControl=trainControl)
fit.lm
##
# lm
set.seed(7)
fit.lm <- train(Inflation~., data=machine, method="lm", metric=metric, preProc=c("center",
                                                                                 "scale", "BoxCox"), trControl=trainControl)
# GLM
set.seed(7)
fit.glm <- train(Inflation~., data=machine, method="glm", metric=metric, preProc=c("center",
                                                                                   "scale", "BoxCox"), trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(Inflation~., data=machine, method="glmnet", metric=metric,
                    preProc=c("center", "scale", "BoxCox"), trControl=trainControl)
# SVM
set.seed(7)
fit.svm <- train(Inflation~., data=machine, method="svmRadial", metric=metric,
                 preProc=c("center", "scale", "BoxCox"), trControl=trainControl)
# CART
set.seed(7)
grid <- expand.grid(.cp=c(0, 0.05, 0.1))
fit.cart <- train(Inflation~., data=machine,method="rpart", metric=metric, tuneGrid=grid,
                  preProc=c("center", "scale", "BoxCox"), trControl=trainControl)
# KNN
set.seed(7)
fit.knn <- train(Inflation~., data=machine, method="knn", metric=metric, preProc=c("center",
                                                                                   "scale", "BoxCox"), trControl=trainControl)
# Compare algorithms
transformResults <- resamples(list(LM=fit.lm, GLM=fit.glm, GLMNET=fit.glmnet, SVM=fit.svm,
                                   CART=fit.cart, KNN=fit.knn))
summary(transformResults)
dotplot(transformResults)
##Tuning 
fit.svm
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
set.seed(7)
grid <- expand.grid(.sigma=c(0.025, 0.05, 0.1, 0.15), .C=seq(1, 10, by=1))
fit.svm <- train(Inflation~., data=machine, method="svmRadial", metric=metric, tuneGrid=grid,
                 preProc=c("BoxCox"), trControl=trainControl)
print(fit.svm)
plot(fit.svm)
##Ensemble Method
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
# Random Forest
set.seed(seed)
fit.rf = train(Inflation~., data=machine, method="rf", metric=metric, preProc=c("BoxCox"),
               trControl=trainControl)
fit.rf
# Stochastic Gradient Boosting
set.seed(seed)
fit.gbm = train(Inflation~., data=machine, method="gbm", metric=metric, preProc=c("BoxCox"),
                trControl=trainControl, verbose=FALSE)
fit.gbm
# Cubist
set.seed(seed)
fit.cubist = train(Inflation~., data=machine, method="cubist", metric=metric,
                   preProc=c("BoxCox"), trControl=trainControl)
# Compare algorithms
ensembleResults = resamples(list(RF=fit.rf, GBM=fit.gbm, CUBIST=fit.cubist))
summary(ensembleResults)
dotplot(ensembleResults)
#
fit.cubist
# Tune the Cubist algorithm
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
set.seed(7)
grid = expand.grid(.committees=seq(15, 25, by=1), .neighbors=c(3, 5, 7))
tune.cubist = train(Inflation~., data=machine, method="cubist", metric=metric,
                    preProc=c("BoxCox"), tuneGrid=grid, trControl=trainControl)

print(tune.cubist)
plot(tune.cubist)
##FINALIZING MODEL
# prepare the data transform using training data
set.seed(7)
xy = machine[,1:4]
yz = machine[,5]
preprocessParams = preProcess(xy, method=c("BoxCox"))
transX = predict(preprocessParams, xy)
# train the final model
library(Cubist)
finalModel = cubist(x=transX, y=yz, committees=18)
summary(finalModel)
finalModel
##VALIDATION
set.seed(7)
valX = validation1[,1:4]
valX
str(valX)
trans_valX = predict(preprocessParams, valX)
trans_valX
valY = validation1[,5]
valY
na.omit(valY)
str(valY)
# use final model to make predictions on the validation dataset
predictions = predict(finalModel, newdata=trans_valX, neighbors=3)
# calculate RMSE
rmse = RMSE(predictions, valY)
rmse
r2 = R2(predictions, valY)
print(rmse)
##Robustness check
library(glmnet)
library(Matrix)
#Ridge regressions and lasso
xr = as.matrix(machine[,1:4])
yr = as.matrix(machine[,5])
# fit model
fit = glmnet(xr, yr, family="gaussian", alpha=0.5, lambda=0.001)
fit2=cv.glmnet(xr, yr, family="gaussian",nfolds = 5)
fit2
plot(fit2)
coef(fit2, s="lambda.1se")
##
plot(fit2$glmnet.fit, xvar="lambda")
#add in vertical lines for the optimal values of lambda
abline(v=log(c(fit2$lambda.min, fit2$lambda.1se)), lty=2)

# summarize the fit
print(fit)
# make predictions
predictions = predict(fit, xr, type="link")
# summarize accuracy
mse = mean((yr - predictions)^2)
print(mse)
##DATA PREPARATIONS
data_csv2 = read.csv("C:/Users/bwabo/OneDrive/Desktop/DATASETS/Bigdata.csv1.csv")
##
str(data_csv1)
summary(data_csv1)
##Convert the above data into data table 
data_csv1=as.data.frame.table(data_csv1)
data_csv1
str(data_csv1)
##
library(data.table)
d2=copy(data_csv1)
sort(unique(d2$Freq.Country))
head(d2)
str(d2)
datedata=d2
##Install package
tempdir()
dir.create(tempdir())
install.packages("lubridate")
library(lubridate)
Reduce(c, lapply(datedata$Freq.Year,fun))
##
format(dmy(d2$Freq.Year) %m+% months(-1),"%d/%m/%Y")
str(d2)
newdate=d2
newdate$Freq.Year
##
str(d2)
hotcoding=d2
##
ddum = dummyVars("~.", data = hotcoding)
Hotdata = data.table(predict(ddum, newdata = hotcoding))
rm(ddum) #remove ddum as unneeded
str(Hotdata)
head(Hotdata)
summary(Hotdata)
describe(Hotdata)
##
sort(unique(d$Freq.Country))
str(d)
head(d)
##Change year into numeric 
d[,Freq.Year:=as.numeric(Freq.Year)]
d$Freq.Year=as.numeric(d$Freq.Year)
str(d)
##Extra mile machine learning analysis 
data_csv2 = read.csv("C:/Users/bwabo/OneDrive/Desktop/DATASETS/Bigdata.csv1.csv")
##
datadate=data_csv2
str(datadate)
d3=datadate
##
d3=as.data.frame.table(d3)
str(d3)
summary(d3)
describe(d3)
View(d3)
#
d4=datadate
d5=datadate
##
d5=as.data.frame.table(d5)
str(d5)
summary(d5)
d4$Year=as.Date.numeric(d4$Year)
str(d4)
d3$Freq.Year = as.numeric(as.character(d3$Freq.Year))
str(d3)

str(d3)
na.omit(d3)
str(d3)
## Final analysis
##Lapply functions 
mdataset[2:22] = lapply(mdataset[2:22], as.numeric)
head(mdataset)
str(mdataset)
##Class Distribution
# summarize the class distribution
percentage = prop.table(table(d5$Freq.Country)) * 100
cbind(freq=table(d5$Freq.Country), percentage=percentage)
##statistical summary

##data visualization
View(d5)
xn = d5[,1:8]
yn = d5[,9]
##Box plot
# boxplot for each attribute on one image
par(mfrow=c(1,9))
for(i in 1:9) {
  boxplot(x[,i], main=names(d5)[i])
}
##plot
plot(yn)
##multivariate plot 
featurePlot(x=xn, y=yn, plot="ellipse")
featurePlot(x=xn, y=yn, plot="box")
##
# density plots for each attribute by class value
scales = list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=xn, y=yn, plot="density", scales=scales)
##lappy function
d5[7:8] = lapply(d5[7:8], as.numeric)
##
na.omit(d5)
str(d5)
is.na(d5)
##Class
d6=datadate
d7=datadate
str(d7)
summary(d7)
dim(d7)
head(d7)
describe(d7$Year)
describe(d7$Open)
describe(d7)
##
class(d6$Year)
#Change the character into the date
dnew=datadate
manidata1=d7
##
dnew$Year=as.Date(dnew$Year)
str(dnew)
str(dnew)
##Newformat
d7$Year=as.Date(d7$Year)
##
summary(d7)
manidata1=d7
library(data.table)
manidata1=as.data.table(manidata1)
str(manidata1)
library(psych)
describe(manidata1)
manidata1$Year=as.Date(manidata1$Year)
str(manidata1)
manidata1$Year=as.numeric(manidata1$Year)
str(manidata1)
#Year has been transformed into numeric values
describe(manidata1)
sapply(manidata1,mode)
d=copy(manidata1)
##Dummy coding 
sort(unique(d$Country))
str(d)
d3$Freq.Year = as.numeric(as.character(d3$Freq.Year))
Achilles=d
Achilles2=d
##Passing Achilles from d
Achilles3=d
Achilles3$Year=as.Date(Achilles3$Year)
str(Achilles3)
Achilles3$Year=as.numeric(Achilles3$Year)
str(Achilles3)
dataf=Achilles3
str(dataf)
#Hot Coding 
library(fastDummies)
install.packages("fastDummies")
## Fast-dummies
dataf = dummy_cols(dataf, select_columns = 'Country')
str(dataf)
summary(dataf)
View(dataf)
##Change the country data into categorical variables 
machinec=dataf
machinec$Country=as.factor(machinec$Country)
##Structure 
str(machinec)
head(machinec)
library(lattice)
library(caret)
validationIndex = createDataPartition(machinec$Inflation, p=0.80, list=FALSE)
# select 20% of the data for validation
validation = machinec[-validationIndex,]
Testingset = machinec[validationIndex,]
##Deep dive
dim(validation)
dim(Testingset)
##visualization
qplot(Testingset$Inflation,High, data = Testingset, colour = factor(Testingset$Country))+geom_smooth()
##Grouping 
p = ggplot(Testingset, aes(Testingset$Inflation, Close, group = Subject)) +
  geom_line()
p
mlplot = ggplot(Testingset, aes(Testingset$Country,Testingset$Inflation )) + geom_line(aes(group = Subject), colour = "#3366FF")
mlplot
mlPlot + geom_line(aes(group = Subject), colour = "#3366FF")
##
require(nlme, quiet = TRUE, warn.conflicts = FALSE)
##
depth_dist = ggplot(Testingset, aes(Inflation)) + xlim(58, 68)

depth_dist +
  geom_histogram(aes(y = ..density..), binwidth = 0.1) +
  facet_grid(Inflation ~ .)

depth_dist + geom_histogram(aes(fill =Inflation ), binwidth = 0.1,
                            position = "fill")
##
depth_dist + geom_freqpoly(aes(y = ..density.., colour = cut),
                           binwidth = 0.1)

oplot <- ggplot(Oxboys, aes(age, height, group = Subject)) +
  + geom_line()
##Logistic regressions 
achillesx= glm(Country~ Open + High + Low + Close +  Year +Inflation+
                 Country_AFG+Country_BDI+Country_BFA+Country_CAF+Country_CMR+Country_COD+Country_COG+
                 Country_GMB+Country_GNB+Country_HTI+Country_IRQ+Country_LAO+
                 Country_LBN+Country_LBR+Country_MLI+Country_MMR+Country_MOZ+
                 Country_NER+Country_NGA+Country_SDN+Country_SOM+Country_SSD+
                 Country_SYR+Country_TCD+Country_YEM,
               data=machinec, family=binomial(link="logit"))
##
achillesx
coefplot(achillesx)
##
library(caret)
library(lattice)
library(caretEnsemble)
# prepare training scheme
trainControl = trainControl(method="repeatedcv", number=10, repeats=3)
# CART
set.seed(7)
fit.cartm = train(Country~., data=machinec, method="rpart",
                  trControl=trainControl)
fit.cartm
# LDA
set.seed(7)
fit.ldam = train(Country~., data=machinec, method="lda", trControl=trainControl)
# SVM
set.seed(7)
fit.svmm = train(Country~., data=machinec, method="svmRadial",
                 trControl=trainControl)
fit.svmm
# KNN
set.seed(7)
fit.knnm = train(Country~., data=machinec, method="knn", trControl=trainControl)
fit.knnm
# Random Forest
set.seed(7)
fit.rfm = train(Country~., data=machinec, method="rf", trControl=trainControl)
fit.rfm
##
resultsm = resamples(list(CART=fit.cartm, SVM=fit.svmm,fit.knnm, RF=fit.rfm))
summary(resultsm)
##
# 10-fold cross validation with 3 repeats
trainControlm <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
# LG
set.seed(7)
fit.glmm = train(Country~., data=machinec, method="glm", metric=metric, preProc=c("BoxCox"),
                 trControl=trainControl)


##Residual Plotting 
# Calculate residuals (SVM)
y_pred = predict(fit.lmf, Testingset)
residuals = Testingset$Inflation - y_pred
##
plot(y_pred, residuals, main = "support vector machine", 
     xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at 0 for reference
## Model Residual for KNN
y_pred1 = predict(fit.knnf, Testingset)
plot(y_pred1, residuals, main = "KNN", 
     xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at 0 for reference

##Model Residual for GLMNET
y_pred2 = predict(fit.glmnetf, Testingset)
plot(y_pred2, residuals, main = "GLMNET", 
     xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at 0 for reference
##
##Model Residual for GLM
y_pred3 = predict(fit.glmnetf, Testingset)
plot(y_pred3, residuals, main = "GLM", 
     xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at 0 for reference
##
##Model Residual for GLM
y_pred4 = predict(fit.cartf, Testingset)
plot(y_pred4, residuals, main = "CART", 
     xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at 0 for reference
# Random Forest
y_pred5 = predict(fit.rfr, Testingset)
plot(y_pred5, residuals, main = "Random Forest", 
     xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at 0 for reference
# Create a scatter plot of residuals
plot(predict(fit.lmf), residuals_rf, xlab = "Predicted Values", ylab = "Residuals",
     main = "Random Forest Residual Plot")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0

##Feature selections algorithms 
# Run algorithms using 10-fold cross validation
trainControl = trainControl(method="repeatedcv", number=10, repeats=3)
metric = "RMSE"
# lm
set.seed(7)
fit.lmf = train(Inflation~., data=Testingset, method="lm", metric=metric,
                preProc=c("center", "scale"), trControl=trainControl)
# GLM
set.seed(7)
fit.glmf = train(Inflation~., data=Testingset, method="glm", metric=metric,
                 preProc=c("center", "scale"), trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnetf = train(Inflation~., data=Testingset, method="glmnet", metric=metric,
                    preProc=c("center", "scale"), trControl=trainControl)
# SVM
set.seed(7)
fit.svmf = train(Inflation~., data=Testingset, method="svmRadial", metric=metric,
                 preProc=c("center", "scale"), trControl=trainControl)
# CART
set.seed(7)
grid = expand.grid(.cp=c(0, 0.05, 0.1))
fit.cartf = train(Inflation~., data=Testingset, method="rpart", metric=metric,
                  tuneGrid=grid, preProc=c("center", "scale"), trControl=trainControl)
# KNN
set.seed(7)
fit.knnf = train(Inflation~., data=Testingset, method="knn", metric=metric,
                 preProc=c("center", "scale"), trControl=trainControl)
# Compare algorithms
feature_resultsf = resamples(list(LM=fit.lmf, GLM=fit.glmf, GLMNET=fit.glmnetf, SVM=fit.svmf,
                                  CART=fit.cartf, KNN=fit.knnf))
summary(feature_resultsf)
dotplot(feature_resultsf)
##Significance level
diffs = diff(feature_resultsf)
summary(diffs)
##Improve the KNN models
print(fit.knnf)
print(fit.svmf)
##Tuning Ridge regressions
ridgeGrid = data.frame(.lambda = seq(0, .1, length = 20))
set.seed(100)
set.seed(7)
ridgeRegFit = train(Inflation~., data=Testingset, method="ridge", metric=metric,
                    preProc=c("center", "scale"),tunegrid=ridgeGrid, trControl=trainControl)
ridgeRegFit
##Tuning
enetGrid = expand.grid(.lambda = c(0, 0.01, .1),
                       .fraction = seq(.05, 1, length = 10))
metric = "RMSE"
set.seed(100)
enetTune = train(Inflation~., data=Testingset, method="enet", metric=metric,
                 preProc=c("center", "scale"),tunegrid=enetGrid, trControl=trainControl)

enetTune
##KNN Tune
# KNN
# Define a range of k values
k_values = seq(1, 20, by = 2)
set.seed(7)
knn.tune = train(Inflation~., data=Testingset, method="knn", metric=metric,
                 preProc=c("center", "scale"), tunegrid=data.frame(k_values), trControl=trainControl)

knn.tune
##CART TUNE
set.seed(7)
grid = expand.grid(.cp=c(0, 0.05, 0.1))
cart.tune = train(Inflation~., data=Testingset, method="rpart", metric=metric,
                  tuneGrid=grid, preProc=c("center", "scale"), trControl=trainControl)

cart.tune
plot(cart.tune)
##GLMNET Tuning
View(Testingset)
data=Testingset
data=subset(data,select = -c(7))
View(data)
## Convert data into matrix
# Example data (replace with your own)
x = as.matrix(data[, 1:31])
y = as.matrix(Testingset[, 7])
##
# Fit glmnet model with cross-validation
cv_model = cv.glmnet(x, y, alpha = 0.5, lambda = seq(0.0001, 1, length = 100))
cv_model$glmnet.fit
best_model = cv_model$glmnet.fit

coef(best_model)
##
install.packages("BuenaVista")
library(BuenaVista)
library(glmnet)
install.packages("glmnetUtils")
library(glmnetUtils)
# Define a range of lambda values and alpha values
lambda_values = 10^seq(10, -2, length = 100)
alpha_values = seq(0, 1, by = 0.05)
metric = "RMSE"
set.seed(7)
glm.tune = train(Inflation~., data=Testingset, method="glmnet", metric=metric,
                 preProc=c("center", "scale"),alpha=alpha_values,lambda=lambda_values , trControl=trainControl)


glmnet_tune = cv.glmnet(x = as.matrix(Testingset[, -c("Inflation")]), y = Testingset$Inflation, alpha = alpha_values, lambda = lambda_values, nfolds = 5)
##
trainControlx = trainControl(method="repeatedcv", number=10, repeats=3,
                             savePredictions=TRUE)
algorithmlist = c('fit.lmf', 'fit.glmf', 'fit.glmnetf', 'fit.svmf', 'fit.cartf','fit.knnf')
set.seed(seed)
models = caretList(Inflation~., data=Testingset, trControl=trainControlx, methodList=algorithmList)
results = resamples(models)
summary(results)
dotplot(results)
##
# tune SVM sigma and C parametres
trainControl = trainControl(method="repeatedcv", number=10, repeats=3)
metric = "RMSE"
set.seed(7)
grid = expand.grid(.sigma=c(0.025, 0.05, 0.1, 0.15), .C=seq(1, 10, by=1))
fit.svmc = train(Inflation~., data=Testingset, method="svmRadial", metric=metric, tuneGrid=grid,
                 preProc=c("BoxCox"), trControl=trainControl)
##Results
print(fit.svmc)
plot(fit.svmc)
# estimate variable importance
importance = varImp(fit.knnf, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
## Ensemble Machine learning models
#Regressions/Ensemble
trainControl = trainControl(method="repeatedcv", number=10, repeats=3)
metric = "RMSE"
# Random Forest
set.seed(seed)
fit.rfr = train(Inflation~., data=Testingset, method="rf", metric=metric, preProc=c("BoxCox"),
                trControl=trainControl)
fit.rfr
fit.rfr
plot(fit.rfr)
##
#Estimate variable importance
importance2 = varImp(fit.rfr, scale=FALSE)
# summarize importance
print(importance2)
# plot importance
plot(importance2)
##
# Stochastic Gradient Boosting
set.seed(seed)
fit.gbmG = train(Inflation~., data=Testingset, method="gbm", metric=metric, preProc=c("BoxCox"),
                 trControl=trainControl, verbose=FALSE)

fit.gbmG
#Estimate variable importance
importance3 = varImp(fit.gbmG, scale=FALSE)
# summarize importance
print(importance3)
# plot importance
plot(importance3)
##
# Random Forest
y_pred6 = predict(fit.gbmG, Testingset)
plot(y_pred6, residuals, main = "gbm", 
     xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at 0 for reference
# Cubist
set.seed(seed)
fit.cubistb = train(Inflation~., data=Testingset, method="cubist", metric=metric,
                    preProc=c("BoxCox"), trControl=trainControl)

fit.cubistb
##
y_pred7 = predict(fit.cubistb, Testingset)
y_pred7
plot(y_pred7, residuals, main = "Cubist", 
     xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at 0 for reference
# estimate variable importance
importance4 = varImp(fit.cubistb, scale=FALSE)
# summarize importance
print(importance4)
# plot importance
plot(importance4)
##Tune Manually
library(randomForest)
library(caret)
library(caretEnsemble)
##
tuning=Testingset
str(Testingset)
View(Testingset)
tuning=subset(tuning, select = c(7))
y=Testingset[,7]

##
# Create model with default parameters
trainControl = trainControl(method="repeatedcv", number=10, repeats=3)
seed = 7
metric = "RMSE"
set.seed(seed)
mtry = sqrt(ncol(tuning))
tunegrid = expand.grid(.mtry=mtry)
metric = "RMSE"
# Random Forest
set.seed(seed)
rfDefault = train(Inflation~., data=Testingset, method="rf", metric=metric, tuneGrid=tunegrid,
                  trControl=trainControl)
print(rfDefault)
##RANDOM SEARCH 
# Random Search
trainControlA = trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry = sqrt(ncol(tuning))
rfRandom = train(Inflation~., data=Testingset, method="rf", metric=metric, tuneLength=15,
                 trControl=trainControlA)
print(rfRandom)
plot(rfRandom)

##
# Manual Search
metric = "RMSE"
trainControlz = trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegridh = expand.grid(.mtry=c(sqrt(ncol(tuning))))
modellist = list()
for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(seed)
  fit <- train(Inflation~., data=Testingset, method="rf", metric=metric, tuneGrid=tunegridh,
               trControl=trainControlz, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)
##Cstomization
customRF <- list(type="Regession", library="randomForest", loop=NULL)
customRF$parameters = data.frame(parameter=c("mtry", "ntree"), class=rep("numeric", 1),
                                 label=c("mtry", "ntree"))
customRF$grid = function(tuning, y, len=NULL, search="grid") {}
customRF$fit = function(tuning, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(tuning, y, mtry=param$mtry, ntree=param$ntree, ...)
}
customRF$predict = function(modelFit, newdata, preProc=NULL, submodels=NULL)
  predict(modelFit, newdata)
customRF$prob = function(modelFit, newdata, preProc=NULL, submodels=NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort = function(tuning) tuning[order(tuning[,1]),]
customRF$levels = function(tuning) x$classes

##Density Plot 
scales =list(x=list(relation="free"), y=list(relation="free"))
densityplot(feature_resultsf, scales=scales, pch = "|")
densityplot(feature_resultsf)
##Regresssions methods
# Run algorithms using 10-fold cross validation
trainControl = trainControl(method="repeatedcv", number=10, repeats=3)
metric = "RMSE"
# lm
set.seed(7)
fit.lmx = train(Inflation~., data=machinec, method="lm", metric=metric, preProc=c("center",
                                                                                  "scale", "BoxCox"), trControl=trainControl)
fit.lmx

# GLM
set.seed(7)
fit.glm = train(Inflation~., data=machinec, method="glm", metric=metric, preProc=c("center",
                                                                                   "scale", "BoxCox"), trControl=trainControl)
fit.glm
# GLMNET
set.seed(7)
fit.glmnetx = train(Inflation~., data=machinec, method="glmnet", metric=metric,
                    preProc=c("center", "scale", "BoxCox"), trControl=trainControl)
fit.glmnetx
# SVM
set.seed(7)
fit.svmx = train(Inflation~., data=machinec, method="svmRadial", metric=metric,
                 preProc=c("center", "scale", "BoxCox"), trControl=trainControl)
fit.svmx
# CART
set.seed(7)
grid = expand.grid(.cp=c(0, 0.05, 0.1))
fit.cart = train(Inflation~., data=machinec, method="rpart", metric=metric, tuneGrid=grid,
                 preProc=c("center", "scale", "BoxCox"), trControl=trainControl)

fit.cart
# KNN
set.seed(7)
fit.knnx = train(Inflation~., data=machinec, method="knn", metric=metric, preProc=c("center",
                                                                                    "scale", "BoxCox"), trControl=trainControl)
fit.knnx
# Compare algorithms
transformResults = resamples(list(LM=fit.lmx, GLM=fit.glm, GLMNET=fit.glmnetx, SVM=fit.svmx,
                                  CART=fit.cart, KNN=fit.knnx))

summary(transformResults)
dotplot(transformResults)
##Density Plot 
scales =list(x=list(relation="free"), y=list(relation="free"))
densityplot(transformResults, scales=scales, pch = "|")
densityplot(transformResults)
##
diffs = diff(transformResults)
summary(diffs)

##
describe(machinec)
library(psych)
describe(machinec)
pairs(machinec)
pairs(machinec[,1:7])
#Regressions/Ensemble 
metric <- "RMSE"
# Random Forest
set.seed(seed)
fit.rfr = train(Inflation~., data=machinec, method="rf", metric=metric, preProc=c("BoxCox"),
                trControl=trainControlm)
fit.rfr


fit.rfr
plot(fit.rfr)
##
#Estimate variable importance
importance2 = varImp(fit.rfr, scale=FALSE)
# summarize importance
print(importance2)
# plot importance
plot(importance2)
##
library(caret)
# Stochastic Gradient Boosting
set.seed(seed)
fit.gbmG = train(Inflation~., data=machinec, method="gbm", metric=metric, preProc=c("BoxCox"),
                 trControl=trainControl, verbose=FALSE)

fit.gbmG
# Cubist
set.seed(seed)
fit.cubistb = train(Inflation~., data=machinec, method="cubist", metric=metric,
                    preProc=c("BoxCox"), trControl=trainControl)

fit.cubistb
#Estimate variable importance
importance3 = varImp(fit.cubistb, scale=FALSE)
# summarize importance
print(importance3)
# plot importance
plot(importance3)
##
# Compare algorithms
ensembleResults = resamples(list(RF=fit.rfr, GBM=fit.gbmG, CUBIST=fit.cubistb))
summary(ensembleResults)
dotplot(ensembleResults)
##
# Tune the Cubist algorithm
trainControl = trainControl(method="repeatedcv", number=10, repeats=3)
metric = "RMSE"
set.seed(7)
grid = expand.grid(.committees=seq(15, 25, by=1), .neighbors=c(3, 5, 7))
tune.cubist = train(Inflation~., data=machinec, method="cubist", metric=metric,
                    preProc=c("BoxCox"), tuneGrid=grid, trControl=trainControl)
print(tune.cubist)
plot(tune.cubist)
## Finalize the model
# prepare the data transform using training data
set.seed(7)
dataset1=machinec
##
library(Cubist)
dataset1=subset(dataset1, select = -c(7))
View(dataset1)
x = dataset1[,1:31]
y = machinec[,7]
preprocessParams = preProcess(x, method=c("BoxCox"))
transX = predict(preprocessParams, x)
# train the final model
finalModel = cubist(x=transX, y=y, committees=18)
summary(finalModel)
finalModel$splits
finalModel$coefficients
##Evaluate the Hold-out data sets
# transform the validation dataset
set.seed(7)
View(validation)
validation1=validation
validation1=subset(validation1,select = -(7))
##
valX = validation1[,1:31]
trans_valX = predict(preprocessParams, valX)
valY = validation[,7]
# use final model to make predictions on the validation dataset
predictions = predict(finalModel, newdata=trans_valX, neighbors=3)
# calculate RMSE
rmse = RMSE(predictions, valY)
r2 = R2(predictions, valY)
print(rmse)
# LDA
set.seed(7)
fit.ldm = train(Country~., data=machinec, method="lda", metric=metric, preProc=c("BoxCox"),
                trControl=trainControlm)
# box and whisker plots to compare models
scales = list(x=list(relation="free"), y=list(relation="free"))
bwplot(resultsm, scales=scales)
##
# density plots of accuracy
scales = list(x=list(relation="free"), y=list(relation="free"))
densityplot(resultsm, scales=scales, pch = "|")

##
library(fastDummies)
ddum = dummyVars("~.", data = Achilles3)
Achilles3 = data.table(predict(ddum, newdata = Achilles3))
rm(ddum) #remove ddum as unneeded
str(Achilles3)
Achilles2$Year=as.Date.numeric(Achilles2$Year)
str(Achilles2)
Achilles$Year=as.numeric(character(Achilles$Year))
Achilles$Year=as.numeric(Achilles$Year)
str(Achilles)
##
manidata1$Year=as.numeric(manidata1$Year)
str(manidata1)
manidata$Country=as.numeric(manidata$Country)
manidata$Country=as.factor(manidata$Country)
## manidata1
str(manidata)
describe(manidata)
ddum = dummyVars("~.", data = manidata)
str(ddum)
str(ddum)
d7$Country=data.table(predict(ddum,newdata = d7$Country))
str(d7)
##
d3$Freq.Year = as.numeric(as.character(d3$Freq.Year))
##
ggplot(manidata1, aes(x=Inflation)) +
  geom_histogram(binwidth=10) + labs(x="Inflation")
##
View(manidata1)
str(manidata1)
manidata1lm = lm(Year ~ Open + High + Low + Close + Inflation, data=manidata1)
manidata1lm
##library



library(caret)
library(lattice)
library(ggplot2)
library(corrplot)
library(gplots)

product<-read.csv("file:///C:/Users/User/Desktop/Ubiqum/Task3/Task3-1/existingproductattributes2017.2.csv")
NewDataa<-read.csv("file:///C:/Users/User/Desktop/Ubiqum/Task3/Task3-1/newproductattributes2017.2.csv")

summary(product)
str(product)
View(product)

#Pre_Process The data
NewData<- dummyVars("~.", data = product)
readyData<- data.frame(predict(NewData, newdata = product))
View(readyData)

summary(readyData)
#correlation
readyData$BestSellersRank<-NULL
corrData<-cor(readyData)
corrplot(corrData)

heatmap.2(cor(readyData), Rowv = FALSE, Colv = FALSE, 
          dendrogram = "none", cellnote = round(cor(readyData),2), notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

ggplot(readyData, aes(Volume, Price)) +geom_jitter()
which(readyData$volume>6000)
readyData<- readyData[-c(which(readyData>6000)),]
#readyData<- filter(readyData, readyData$Volume>=6000)

readyData$x2StarReviews<-NULL
readyData$x3StarReviews<-NULL
readyData$x4StarReviews<-NULL
readyData$ShippingWeight<-NULL
readyData$x1StarReviews<-NULL

#readyData<- filter(readyData, Volume<=6000)

plot(readyData$Volume)
#Let's develop our multiple Regression Models
intrain<-createDataPartition(readyData$Volume, p=0.80, list = FALSE)
Training<-readyData[intrain,]
Testing<-readyData[-intrain,]

#10fold cross validations
set.seed(2017)
xctrl<- trainControl(method="repeatedcv", number=10, repeats =3)

svmModel<- train(Volume~., method="svmLinear", data=Training, trControl=xctrl, preProcess=c("center","scale") )
print(svmModel)
svmPredict<-predict(svmModel, Testing)
svmPredict

rdModel<- train(Volume~., method="rf", data=Training, trControl=xctrl, preProcess=c("center","scale"))
print(rdModel)
rfPredict<-predict(rdModel, Testing)
rfPredict

gbModel<- train(Volume~., method="gbm", data=Training, trControl=xctrl, preProcess=c("center", "scale"))
print(gbModel)
gbPredict<-Predict(gbModel, Testing)
gbPredict

result<-resamples(list(rf=rdModel, gbm=gbModel, svm=svmModel))
summary(result)

dotplot(result)

#prediction of the new product

#New Product

#NewData1<- dummyVars("~.", data = NewDataa)
#NewDataa<- data.frame(predict(NewData1, newdata = NewDataa))

NewDataa$BestSellersRank<-NULL
NewDataa$x2StarReviews<-NULL
NewDataa$x3StarReviews<-NULL
NewDataa$x4StarReviews<-NULL
NewDataa$ShippingWeight<-NULL
NewDataa$x1StarReviews<-NULL

predictNew<-predict(rdModel, NewDataa)

NewDataa$Newpredict<-predictNew
View(NewDataa)

result<- NewDataa
write.csv(result, file = "c5.T5result.csv", row.names = TRUE)


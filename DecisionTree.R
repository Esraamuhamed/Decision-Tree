library("rpart")
library("rpart.plot")
library("caTools")

 

carData<-read.table("C:/Users/E.MOHAMED/Desktop/car-dataset.csv",header = TRUE ,sep=",")



set.seed(45443)
spliting <- sample.split(carData$Label,SplitRatio=0.80)
Training_Set <- subset(carData,spliting==TRUE)
Testing_Set <- subset(carData,spliting==FALSE)

fit <- rpart(Label ~ Feature1 + Feature2 + Feature3 + Feature4 +Feature5 + Feature6,method="class",
             data=Training_Set,
             control=rpart.control(minsplit=1),
             parms=list(split='information'))

rpart.plot(fit, type=4, extra=1,clip.right.labs = FALSE,varlen = 0,faclen = 3)
pred_car <- predict(fit,newdata=Testing_Set,type="class")

table_mat <- table(Testing_Set$Label, pred_car)
#table_mat

accuracy <- sum(diag(table_mat)) / sum(table_mat)
accuracy
#accuracy is 0.9586466

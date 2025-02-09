library(ggplot2)
library(caret)
library(dplyr)
library(rattle)
library(corrplot)
library(dplyr)

download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
              destfile = "train.csv")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
              destfile = "test.csv")

tr<-read.csv("train.csv")
val<-read.csv("test.csv")

set.seed(69)

table(tr$classe)/nrow(tr)
table(tr$new_window)
table(is.na(tr$max_picth_belt))
table(tr$new_window)[1]/nrow(tr)
## ONLY FEW YES

x<-tr[tr$new_window=="no",]
nzv<-nearZeroVar(x,saveMetrics = TRUE)
tr<-tr[,!nzv$nzv]

val<-val[,!nzv$nzv]


intrain<-createDataPartition(tr$classe,p=0.7,list=FALSE)
ts<-tr[-intrain,]
tr<-tr[intrain,]

dim(tr)
dim(ts)
dim(val)




table(is.na(tr))

## We remove the observations' ID's and timestamps 
## because of the irrelevance and the infulence they have 
## on the outcome (correlation)
tr<-tr[,-c(1:6)]
ts<-ts[,-c(1:6)]
val<-val[,-c(1:6)]



corrplot(cor(tr[, -53]), order = "FPC", method = "color", type = "upper", 
         tl.cex = 0.5, tl.col = rgb(0, 0, 0))

prep<-preProcess(tr,method = c("center","scale"))
tr<-predict(prep,tr)
ts<-predict(prep,ts)
val<-predict(prep,val)


ctr<-trainControl(method="repeatedcv",number=5,repeats=5)
tune<-data.frame(cp=c(0.1,0.01,0.001,0.0001))
mf1<-train(classe~.,data=tr,method="rpart",trControl=ctr,tuneGrid=tune)

fancyRpartPlot(mf1$finalModel)

pre1<-predict(mf1,ts)
confusionMatrix(pre1,ts$classe)

plot(mf1)

pre2<-predict(mf1,val)
pre2
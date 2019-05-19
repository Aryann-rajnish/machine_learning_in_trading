library("quantmod")
library("rpart")
library("rpart.plot")

df = read.csv("decision tree charting.csv")
df = df[,-1]
colnames(df) = c("Class","RSI","SMA","LMA","ADX")

trainingSet<-df[1:500,]
testSet<-df[501:nrow(df),]

DecisionTree<-rpart(Class~RSI+SMA+LMA+ADX,data=trainingSet, cp=.001)
 
prp(DecisionTree,type=2,extra=8)
table(predict(DecisionTree,testSet,type="class"),testSet[,5],dnn=list('predicted','actual'))
cat("accuracy : 75 ")


PG_DN<-read.csv("PG_DN.csv", header=T, stringsAsFactors = F)
str(PG_DN)

library(dplyr)
DN <- dplyr::select(PG_DN, c(심사결정요양급여비용총액,기준년도,주상병코드,입내원일수,요양기관종별,청구.DRG.번호))

nrows <- NROW(DN)
set.seed(218)
index <- sample(1:nrows, 0.7 * nrows)

train <- DN[index,]
test <- DN[-index,]

library(randomForest)

learn_rf <- randomForest(심사결정요양급여비용총액~., 
                         data=train, ntree=500, proximity=T, importance=T)
pre_rf <- predict(learn_rf, test[,1])
cm_rf <- confusionMatrix(pre_rf, test$심사결정요양급여비용총액)
cm_rf

plot(learn_rf, main="Random Forest (Error Rate vs. Number of Trees)")

plot(margin(learn_rf,test$diagnosis))

varImpPlot(learn_rf)
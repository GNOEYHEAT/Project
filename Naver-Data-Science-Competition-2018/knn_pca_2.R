wbcd<-read.csv("data.csv", header=T, stringsAsFactors = F)
wbcd$X<-NULL
wbcd<-wbcd[,-1]
wbcd$diagnosis<-factor(ifelse(wbcd$diagnosis=="B","Benign","Malignant"))


nrows <- NROW(wbcd)
set.seed(218)                           ## fix random value
index <- sample(1:nrows, 0.7 * nrows)   ## shuffle and divide

#train <- wbcd                          ## 569 test data (100%)
train <- wbcd[index,]                   ## 398 test data (70%)
test <- wbcd[-index,]                   ## 171 test data (30%)

prop.table(table(train$diagnosis))
prop.table(table(test$diagnosis))

library(class)

acc_test <- numeric() 

for(i in 1:10){
  predict <- knn(train=train[,-1], test=test[,-1], cl=train[,1], k=i, prob=T)
  acc_test <- c(acc_test,mean(predict==test[,1]))
}

acc <- data.frame(k= seq(1,10), cnt = acc_test)

opt_k <- subset(acc, cnt==max(cnt))[1,]
sub <- paste("Optimal number of k is", opt_k$k, "(accuracy :", opt_k$cnt,") in KNN")

install.packages("highcharter")

library(highcharter)
hchart(acc, 'line', hcaes(k, cnt)) %>%
  hc_title(text = "Accuracy With Varying K (KNN)") %>%
  hc_subtitle(text = sub) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_xAxis(title = list(text = "Number of Neighbors(k)")) %>%
  hc_yAxis(title = list(text = "Accuracy"))


# install.packages("caret")
# install.packages("e1071")

library(caret)

pre_knn <- knn(train = train[,-1], test = test[,-1], cl = train[,1], k=opt_k$k, prob=T)
cm_knn  <- confusionMatrix(pre_knn, test$diagnosis)
cm_knn

##########################################
# PCA
##########################################

library(factoextra)
wbcd_pca <- transform(wbcd) 

mean_pca <- prcomp(wbcd_pca[,c(2:11)], scale = TRUE)
summary(mean_pca)

########################################

nrows <- NROW(wbcd_pca)
set.seed(218)                           ## fix random value
index <- sample(1:nrows, 0.7 * nrows)   ## shuffle and divide

#train <- wbcd                          ## 569 test data (100%)
train <- wbcd_pca[index,]                   ## 398 test data (70%)
test <- wbcd_pca[-index,]                   ## 171 test data (30%)

prop.table(table(train$diagnosis))
prop.table(table(test$diagnosis))

library(class)

acc_test <- numeric() 

for(i in 1:10){
  predict <- knn(train=train[,-1], test=test[,-1], cl=train[,1], k=i, prob=T)
  acc_test <- c(acc_test,mean(predict==test[,1]))
}

acc <- data.frame(k= seq(1,10), cnt = acc_test)

opt_k <- subset(acc, cnt==max(cnt))[1,]
sub <- paste("Optimal number of k is", opt_k$k, "(accuracy :", opt_k$cnt,") in KNN")

install.packages("highcharter")

library(highcharter)
hchart(acc, 'line', hcaes(k, cnt)) %>%
  hc_title(text = "Accuracy With Varying K (KNN)") %>%
  hc_subtitle(text = sub) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_Axis(title = list(text = "Number of Neighbors(k)")) %>%
  hc_yAxis(title = list(text = "Accuracy"))


# install.packages("caret")
#install.packages("e1071")

library(caret)

pre_knn <- knn(train = train[,-1], test = test[,-1], cl = train[,1], k=opt_k$k, prob=T)
cm_knn  <- confusionMatrix(pre_knn, test$diagnosis)
cm_knn


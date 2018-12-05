# Naver

# CSV file import # stringsAsFactors
wbcd<-read.csv("data.csv",stringsAsFactors = FALSE)

head(is.na(wbcd))

# wbcd 데이터 프레임의 구조
View(wbcd)
str(wbcd)

# id 속성 제거
wbcd<-wbcd[-1]

# 진단 테이블
table(wbcd$diagnosis)
##B는 양성 M는 음성

# 팩터로서 진단 변수 변환
wbcd$diagnosis<-factor(wbcd$diagnosis, levels=c("B","M"),
                       labels=c("Benign", "Malignant"))

# 세 속성에 대한 요약
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])
  # 속성에 대한 관찰  - 편차가 크기때문에 정규화 필요

# 정규화 함수
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

# wbcd 데이터 정규화
wbcd_n<-as.data.frame(lapply(wbcd[2:31],normalize))
  ##lapply는 리스트를 입력받아서 리스트의 각원소에 함수를 적용함
  ## 즉 wbcd에 있는 모든 numeric data 정규화 한것 0 - 1

# 훈련 데이터와 테스트 데이터 생
wbcd_train<-wbcd_n[1:469, ]
wbcd_test<-wbcd_n[470:569, ]

# 훈련 데이터와 테스트 데이터에 대한 라벨 생
wbcd
wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]
wbcd_train_labels
wbcd_test_labels

## Step.2 Traing a model on the data ###

# "class" 라이브러리 로드
#install.packages("class")
library(class)
wbcd_test_pred<-knn(train=wbcd_train,
                    test=wbcd_test,
                    cl=wbcd_train_labels,
                    k=21)

# "gmodels" 라이브러리 로드
#install.packages("gmodels")
library(gmodels)

# 예측값과 실제값의 교차표 생성
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.c=FALSE)
 #2개가 오분류

# 데이터 프레임를 z-score 표준화하기 위해 scale() 함수 사용
wbcd_z<-as.data.frame(scale(wbcd[-1]))

# 변환이 정확하게 적용되었는지 확인
summary(wbcd_z$area_mean)
  ## z는 평균이 0

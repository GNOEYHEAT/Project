wbcd<-read.csv("data.csv", header=T, stringsAsFactors = F)
wbcd$X<-NULL
wbcd<-wbcd[,-1]
wbcd$diagnosis<-factor(ifelse(wbcd$diagnosis=="B","Benign","Malignant"))

str(wbcd)
summary(wbcd)
knitr::kable(head(wbcd))

library(dplyr)

wbcd_b=wbcd %>% filter(diagnosis == "Benign")
wbcd_b

wbcd_m=wbcd %>% filter(diagnosis == "Malignant")
wbcd_m

head(wbcd_b[2:11])
summary(wbcd_b)
View(wbcd_b)
head(wbcd_m[2:11])
summary(wbcd_m)
View(wbcd_m)

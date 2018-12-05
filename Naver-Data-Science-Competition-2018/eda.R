# 2. Data Importing & Cleaning & Inspecting

wbcd<-read.csv("data.csv", header=T, stringsAsFactors = F)
wbcd$X<-NULL
wbcd<-wbcd[,-1]
wbcd$diagnosis<-factor(ifelse(wbcd$diagnosis=="B","Benign","Malignant"))

str(wbcd)
summary(wbcd)
knitr::kable(head(wbcd))

# 3. Analyze the Correlaion between variables

library(PerformanceAnalytics)
chart.Correlation(wbcd[,c(2:11)], histogram=TRUE, col="grey10", pch=1, main="Cancer Mean")

library(GGally)

ggpairs(wbcd[,c(2:11,1)], aes(color=diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Cancer Mean")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

ggcorr(wbcd[,c(2:11)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="Cancer Mean")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

# install.packages("factoextra")
library(factoextra)
wbcd_pca <- transform(wbcd) 

all_pca <- prcomp(wbcd_pca[,-1], cor=TRUE, scale = TRUE)
summary(all_pca)

fviz_eig(all_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "Cancer All Variances - PCA",
       x = "Principal Components", y = "% of variances")

all_var <- get_pca_var(all_pca)
all_var

library("corrplot")
corrplot(all_var$cos2, is.corr=FALSE)


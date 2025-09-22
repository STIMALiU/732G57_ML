#-------------------------------------------------------------------------------
# Data 1: faithful
#-------------------------------------------------------------------------------
library(ggplot2)
?faithful
ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))

#-------------------------------------------------------------------------------
# Data 2: geyser
#-------------------------------------------------------------------------------
library(MASS)
?geyser
ggplot(data = geyser) + 
  geom_point(mapping = aes(x = duration, y = waiting))



#-------------------------------------------------------------------------------
# Bearbetning 
#-------------------------------------------------------------------------------


D1<-as.data.frame(scale(faithful)[,])
head(D1)
# standarisera och plotta
ggplot(data = D1,aes(x=eruptions,y=waiting))+geom_point(alpha=0.5,col="red",size=2)+theme_bw()

apply(D1,2,sd)
apply(D1,2,mean)


D2<-as.data.frame(scale(geyser)[,])
head(D2)
# standarisera och plotta
ggplot(data = D2,aes(x=duration,y=waiting))+geom_point(alpha=0.5,col="red",size=2)+theme_bw()

apply(D2,2,sd)
apply(D2,2,mean)

#-------------------------------------------------------------------------------
# 1) k-means: faithful
#-------------------------------------------------------------------------------


set.seed(100)
K<-4
clust_kmeans<-kmeans(D1, centers=K, nstart = 20,iter.max = 25)

tab1<-table(clust_kmeans$cluster)
tab1<-as.data.frame(tab1)
colnames(tab1)<-c("Kluster ID","Antal")
tab1$Andel<-round(tab1$Antal/nrow(D1),2)
knitr::kable(tab1)


cluster_id<-as.factor(clust_kmeans$cluster)
ggplot(data = D1,aes(x=eruptions,y=waiting))+geom_point(alpha=1,size=3,aes(col=cluster_id))+
  theme_bw()+ggtitle("k-means klustering")+ 
  geom_point(data=data.frame(clust_kmeans$centers),aes(x=eruptions,y=waiting),shape=7,size=3)



#-------------------------------------------------------------------------------
# 1) k-means: geyser
#-------------------------------------------------------------------------------

set.seed(1001)
K<-4
clust_kmeans<-kmeans(D2, centers=K, nstart = 20,iter.max = 25)

tab1<-table(clust_kmeans$cluster)
tab1<-as.data.frame(tab1)
colnames(tab1)<-c("Kluster ID","Antal")
tab1$Andel<-round(tab1$Antal/nrow(D1),2)
knitr::kable(tab1)


cluster_id<-as.factor(clust_kmeans$cluster)
ggplot(data = D2,aes(x=duration,y=waiting))+geom_point(alpha=1,size=3,aes(col=cluster_id))+
  theme_bw()+ggtitle("k-means klustering")+ 
  geom_point(data=data.frame(clust_kmeans$centers),aes(x=duration,y=waiting),shape=7,size=3)









#-------------------------------------------------------------------------------
# 2) Hierarkisk klustring
#-------------------------------------------------------------------------------


hc.complete <- hclust(dist(D2), method = "complete")

plot(hc.complete, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9,labels = FALSE)
abline(h=0.27,lty="dashed",col="blue")


hclust_complete<-cutree(hc.complete, 4)
#hclust_complete[]
tab2<-table(hclust_complete)
tab2<-as.data.frame(tab2)
colnames(tab2)<-c("Kluster ID","Antal")
tab2$Andel<-round(tab2$Antal/nrow(D2),2)

knitr::kable(tab2)

cluster_id<-as.factor(hclust_complete)
ggplot(data = D2,aes(x=x,y=y))+geom_point(alpha=1,size=3,aes(col=cluster_id))+
  theme_bw()+ggtitle("Hierarkisk klustring")




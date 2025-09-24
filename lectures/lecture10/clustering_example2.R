#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Testar att klustring på två olika dataset: faithful och geyser
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# rensar i den globala miljön:
rm(list=ls())

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
# PAM: faithful
#-------------------------------------------------------------------------------

# vi ser två naturliga kluster i data

library(cluster)

set.seed(100)
K<-2 
# vi testar K=2 först, testa sen andra värden på K och se hur resultatet ändrar sig

# centers = ett heltal = K = antal kluster
# nstart = antal omstarter
clust_pam<-p <- pam(D1, k = K)

tab1<-table(clust_pam$cluster)
tab1<-as.data.frame(tab1)
colnames(tab1)<-c("Kluster ID","Antal")
tab1$Andel<-round(tab1$Antal/nrow(D1),2)
knitr::kable(tab1)

clust_pam$medoids

cluster_id<-as.factor(clust_pam$cluster)
ggplot(data = D1,aes(x=eruptions,y=waiting))+geom_point(alpha=1,size=3,aes(col=cluster_id))+
  theme_bw()+ggtitle("k-means klustering")+ 
  geom_point(data=data.frame(clust_pam$medoids),aes(x=eruptions,y=waiting),shape=7,size=3)
# skattade medoids:
clust_pam$medoids



#-------------------------------------------------------------------------------
# PAM: geyser
#-------------------------------------------------------------------------------

set.seed(1001)
K<-3
clust_pam<-p <- pam(D2, k = K)

tab1<-table(clust_pam$cluster)
tab1<-as.data.frame(tab1)
colnames(tab1)<-c("Kluster ID","Antal")
tab1$Andel<-round(tab1$Antal/nrow(D2),2)
knitr::kable(tab1)

clust_pam$medoids

cluster_id<-as.factor(clust_pam$cluster)
ggplot(data = D2,aes(x=duration,y=waiting))+geom_point(alpha=1,size=3,aes(col=cluster_id))+
  theme_bw()+ggtitle("k-means klustering")+ 
  geom_point(data=data.frame(clust_pam$medoids),aes(x=duration,y=waiting),shape=7,size=3)
# skattade medoids:
clust_pam$medoids





#-------------------------------------------------------------------------------
# DBscan: faithful
#-------------------------------------------------------------------------------
library(dbscan)
k<-4
dbscan::kNNdistplot(x = D1,k = k)
h<-0.22
#h<-0.25
abline(h = h, col = "red")


dbs1<-dbscan(x = D1,eps = h,minPts = k,borderPoints = TRUE)

cluster_id<-as.factor(dbs1$cluster)
tab4<-table(cluster_id)
tab4<-as.data.frame(tab4)
colnames(tab4)<-c("Kluster ID","Antal")
tab4$Andel<-round(tab4$Antal/nrow(D1),2)

knitr::kable(tab4)

#D4<-data.frame(D2,cluster=as.factor(dbs1$cluster))
ggplot(data = D1,aes(x=eruptions,y=waiting))+geom_point(alpha=1,size=3,aes(col=cluster_id))+
  theme_bw()+ggtitle("Täthetsbaserad klustering")



#-------------------------------------------------------------------------------
# DBscan: geyser
#-------------------------------------------------------------------------------
library(dbscan)
k<-4
dbscan::kNNdistplot(x = D2,k = k)
h<-0.35
h<-0.3
abline(h = h, col = "red")


dbs1<-dbscan(x = D2,eps = h,minPts = k,borderPoints = TRUE)

cluster_id<-as.factor(dbs1$cluster)
tab4<-table(cluster_id)
tab4<-as.data.frame(tab4)
colnames(tab4)<-c("Kluster ID","Antal")
tab4$Andel<-round(tab4$Antal/nrow(D2),2)

knitr::kable(tab4)

#D4<-data.frame(D2,cluster=as.factor(dbs1$cluster))
ggplot(data = D2,aes(x=duration,y=waiting))+geom_point(alpha=1,size=3,aes(col=cluster_id))+
  theme_bw()+ggtitle("Täthetsbaserad klustering")





#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Välja antal kluster: Average silhouette coefficient
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#  k-means: faithful 
#-------------------------------------------------------------------------------

k_vect <- 2:10
asc <- sapply(k_vect, FUN=function(k) {
  set.seed(10+k)
  fpc::cluster.stats(dist(D2), kmeans(D2, centers=k, nstart = 20,iter.max = 25)$cluster)$avg.silwidth
})

best_k <- k_vect[which.max(asc)]
best_k
df_avg_silwidth<-data.frame(k=k_vect,avg.silwidth=asc)
ggplot(data = df_avg_silwidth,aes(x=k,y=avg.silwidth))+geom_point()+
  geom_line(linetype="dashed")+ylab("Average silhouette coefficient")+xlab("Number of klusters")+theme_bw()

set.seed(10+best_k)
clust_kmeans<-kmeans(D2, centers=best_k, nstart = 20,iter.max = 25)


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
#  PAM: faithful
#-------------------------------------------------------------------------------

k_vect <- 2:10
asc_pam <- sapply(k_vect, FUN=function(k) {
  set.seed(10+k)
  fpc::cluster.stats(dist(D2), pam(D2, k = k)$cluster)$avg.silwidth
})

best_k <- k_vect[which.max(asc_pam)]
best_k
df_avg_silwidth<-data.frame(k=k_vect,avg.silwidth=asc_pam)
ggplot(data = df_avg_silwidth,aes(x=k,y=avg.silwidth))+geom_point()+
  geom_line(linetype="dashed")+ylab("Average silhouette coefficient")+xlab("Number of klusters")+theme_bw()


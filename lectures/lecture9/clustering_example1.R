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
# k-means: faithful
#-------------------------------------------------------------------------------

# vi ser två naturliga kluster i data

set.seed(100)
K<-2 
# vi testar K=2 först, testa sen andra värden på K och se hur resultatet ändrar sig

# centers = ett heltal = K = antal kluster
# nstart = antal omstarter
clust_kmeans<-kmeans(D1, centers=K, nstart = 1,iter.max = 25)

tab1<-table(clust_kmeans$cluster)
tab1<-as.data.frame(tab1)
colnames(tab1)<-c("Kluster ID","Antal")
tab1$Andel<-round(tab1$Antal/nrow(D1),2)
knitr::kable(tab1)


cluster_id<-as.factor(clust_kmeans$cluster)
ggplot(data = D1,aes(x=eruptions,y=waiting))+geom_point(alpha=1,size=3,aes(col=cluster_id))+
  theme_bw()+ggtitle("k-means klustering")+ 
  geom_point(data=data.frame(clust_kmeans$centers),aes(x=eruptions,y=waiting),shape=7,size=3)
# skattade centroider:
clust_kmeans$centers


# testa
# sätt ingen fix seed
clust_kmeans<-kmeans(D1, centers=10, nstart = 1,iter.max = 25)
# plotta klustren likt ovan och kör om algortimen flera gånger och plotta varje gång
# är resultaten stabila? 



#-------------------------------------------------------------------------------
# k-means: geyser
#-------------------------------------------------------------------------------

set.seed(1001)
K<-3
clust_kmeans<-kmeans(D2, centers=K, nstart = 1,iter.max = 25)

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
# Hierarkisk klustring: faithful
#-------------------------------------------------------------------------------

# vi måste välja länkningsmetod

#-------------------------------------------------------------------------------
# single linkage

hc.single <- hclust(dist(D1), method = "single")

plot(hc.single, main = "Single Linkage",
     xlab = "", sub = "", cex = .9,labels = FALSE)
cut_val_single <- 0.4
abline(h=cut_val_single,lty="dashed",col="blue") 
# vi drar en linje där vi tycker att det är bra att dela upp dendrogrammet 
# i en klusteruppdelning


hclust_single<-cutree(hc.single, k = 2)
tab2<-table(hclust_single)
tab2<-as.data.frame(tab2)
colnames(tab2)<-c("Kluster ID","Antal")
tab2$Andel<-round(tab2$Antal/nrow(D1),2)

knitr::kable(tab2)
head(faithful)
cluster_id<-as.factor(hclust_single)

ggplot(data = D1,aes(x=eruptions,y=waiting))+geom_point(alpha=1,size=3,aes(col=cluster_id))+
  theme_bw()+ggtitle("Hierarkisk klustring: Single Linkage")



#-------------------------------------------------------------------------------
# complete linkage

hc.complete <- hclust(dist(D1), method = "complete")

plot(hc.complete, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9,labels = FALSE)
cut_val_complete <- 4
abline(h=cut_val_complete,lty="dashed",col="blue") 
# vi drar en linje där vi tycker att det är bra att dela upp dendrogrammet 
# i en klusteruppdelning


# k = ger antal kluster i en uppdelning, testa att ange olika värden 
hclust_complete<-cutree(hc.complete, k = 2)
tab2<-table(hclust_complete)
tab2<-as.data.frame(tab2)
colnames(tab2)<-c("Kluster ID","Antal")
tab2$Andel<-round(tab2$Antal/nrow(D1),2)

knitr::kable(tab2)
#head(faithful)
cluster_id<-as.factor(hclust_complete)

ggplot(data = D1,aes(x=eruptions,y=waiting))+geom_point(alpha=1,size=3,aes(col=cluster_id))+
  theme_bw()+ggtitle("Hierarkisk klustring: Complete Linkage")



#-------------------------------------------------------------------------------
# wards linkage

hc.ward <- hclust(dist(D1), method = "ward")

plot(hc.ward, main = "Wards Linkage",
     xlab = "", sub = "", cex = .9,labels = FALSE)
cut_val_ward <- 150
abline(h=cut_val_ward,lty="dashed",col="blue") 
# vi drar en linje där vi tycker att det är bra att dela upp dendrogrammet 
# i en klusteruppdelning


hclust_ward<-cutree(hc.ward, 2)
tab2<-table(hclust_ward)
tab2<-as.data.frame(tab2)
colnames(tab2)<-c("Kluster ID","Antal")
tab2$Andel<-round(tab2$Antal/nrow(D1),2)

knitr::kable(tab2)
#head(faithful)
cluster_id<-as.factor(hclust_ward)

ggplot(data = D1,aes(x=eruptions,y=waiting))+geom_point(alpha=1,size=3,aes(col=cluster_id))+
  theme_bw()+ggtitle("Hierarkisk klustring: Wards Linkage")




#-------------------------------------------------------------------------------
# Hierarkisk klustring: geyser
#-------------------------------------------------------------------------------

# vi måste välja länkningsmetod

#-------------------------------------------------------------------------------
# single linkage

hc.single <- hclust(dist(D2), method = "single")

plot(hc.single, main = "Single Linkage",
     xlab = "", sub = "", cex = .9,labels = FALSE)
# här är det inte lika uppenbart vad som är ett bra antal kluster
cut_val_single <- 0.5
abline(h=cut_val_single,lty="dashed",col="blue") 
# vi drar en linje där vi tycker att det är bra att dela upp dendrogrammet 
# i en klusteruppdelning

# notera: cut_val_single <- 0.5 ger inte direkt meningsfulla kluster
# testa med några olika antal kluster nedan när ni plottar


hclust_single<-cutree(hc.single, 7) # testa med 3 och 7 kluster
tab2<-table(hclust_single)
tab2<-as.data.frame(tab2)
colnames(tab2)<-c("Kluster ID","Antal")
tab2$Andel<-round(tab2$Antal/nrow(D2),2)

knitr::kable(tab2)
head(geyser)
cluster_id<-as.factor(hclust_single)

ggplot(data = D2,aes(x=duration,y=waiting))+geom_point(alpha=1,size=3,aes(col=cluster_id))+
  theme_bw()+ggtitle("Hierarkisk klustring: Single Linkage")



#-------------------------------------------------------------------------------
# complete linkage

hc.complete <- hclust(dist(D2), method = "complete")

plot(hc.complete, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9,labels = FALSE)
cut_val_complete <- 4.3
abline(h=cut_val_complete,lty="dashed",col="blue") 
# vi drar en linje där vi tycker att det är bra att dela upp dendrogrammet 
# i en klusteruppdelning


# k = ger antal kluster i en uppdelning, testa att ange olika värden 
hclust_complete<-cutree(hc.complete, k = 2) # testa med k = 2,3,4
tab2<-table(hclust_complete)
tab2<-as.data.frame(tab2)
colnames(tab2)<-c("Kluster ID","Antal")
tab2$Andel<-round(tab2$Antal/nrow(D2),2)

knitr::kable(tab2)
head(geyser)
cluster_id<-as.factor(hclust_complete)

ggplot(data = D2,aes(x=duration,y=waiting))+geom_point(alpha=1,size=3,aes(col=cluster_id))+
  theme_bw()+ggtitle("Hierarkisk klustring: Complete Linkage")



#-------------------------------------------------------------------------------
# wards linkage

hc.ward <- hclust(dist(D2), method = "ward")

plot(hc.ward, main = "Wards Linkage",
     xlab = "", sub = "", cex = .9,labels = FALSE)
cut_val_ward <- 150
abline(h=cut_val_ward,lty="dashed",col="blue") 
# vi drar en linje där vi tycker att det är bra att dela upp dendrogrammet 
# i en klusteruppdelning


hclust_ward<-cutree(hc.ward,k = 2) # testa med k = 2,3
tab2<-table(hclust_ward)
tab2<-as.data.frame(tab2)
colnames(tab2)<-c("Kluster ID","Antal")
tab2$Andel<-round(tab2$Antal/nrow(D2),2)

knitr::kable(tab2)
#head(faithful)
cluster_id<-as.factor(hclust_ward)

ggplot(data = D2,aes(x=duration,y=waiting))+geom_point(alpha=1,size=3,aes(col=cluster_id))+
  theme_bw()+ggtitle("Hierarkisk klustring: Wards Linkage")
  




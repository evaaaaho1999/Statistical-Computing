install.packages("ramify")
library(tidyverse)
library(ramify)
library(h2o)
library(ggpubr)
library(GGally)
library(factoextra)
library(RColorBrewer)
library(ggplotify)
library(hrbrthemes)
library(dendextend)
library(plyr)
library(dplyr)
library(corrplot)
library(ClusterR)
library(knitr)
library(gridExtra)
library(magrittr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(gridExtra)
library(grid)
library(lattice)
library(GGally)
library(factoextra)
library(ClustOfVar)
library(cluster)
library(KSD)
library(mclust)
library(gridExtra)

fatel<-read.csv("C:/Users/eva/Desktop/作業 上課資料(清大)/大四下/統算/midterm/data/fetal_health.csv")
head(fatel)
fatel$fetal_health
ncol(fatel)
fatel2<-fatel[,-22]
summary(fatel2)


#Histogram
graph<-gather(fatel2, Attributes, happen_times, 1:21 )
ggplot(graph, aes(x=happen_times, fill=Attributes))+
  geom_histogram(colour="black", show.legend=FALSE)+
  facet_wrap(~Attributes, scales="free_x") + #以attribute變量進行數據分類, free_x代表自由調整x軸刻度範圍
  labs(x="happen_times", y="Frequency",
       title="Wines Attributes - Histograms") +
  theme_bw()


#density plot
ggplot(graph, aes(x=happen_times, fill=Attributes))+
  geom_density(colour="black", show.legend=FALSE, alpha=0.8)+
  facet_wrap(~Attributes, scales="free") + #以attribute變量進行數據分類, free_x代表自由調整x軸刻度範圍
  labs(x="happen_times", y="Frequency",
       title="Wines Attributes - density") +
  theme_bw()


box<-gather(fatel2, Attributes, values)
ggplot(box, aes(x=reorder(Attributes, values, FUN=median), y=values, fill=Attributes))+ #reorder(x,y,使用的函式為median),結果依照y的中位數大小排序
  geom_boxplot(show.legend=FALSE)+
  labs(title="Wines Attributes - Boxplots") +
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank())+
  coord_flip()

# Correlation matrix 
options(repr.plot.width = 14, repr.plot.height = 8) # => bigger plots for the following
cor_features <- cor(fatel2[,1:21], method='spearman')
corrplot::corrplot(cor_features, tl.cex=0.6, type='lower', method="ellipse")

#scatter plot
ggplot(fatel, aes(x = histogram_mode, y = histogram_median)) +
  geom_point(size = 2, alpha = 0.6, show.legend = FALSE, shape=18,  color="blue") +
  geom_smooth(method=lm, linetype="dashed",color="darkred", fill="blue")+
  labs(title = 'Scatterplots', subtitle = 'histogram_mode with histogram_median')

ggplot(fatel,aes(x = histogram_mode, y = histogram_median))+
  geom_point(aes(color = factor(fetal_health)))
 
# Normalization

fatelNorm <- as.data.frame(scale(fatel2))
distancef <- get_dist(fatelNorm)
head(distancef)
fviz_dist(distancef, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"),
          show_labels = F)

#K-means

fatel_km2 <- kmeans(fatelNorm, centers=2)
fatel_km2$cluster #A vector of integers indicating the cluster to which each point is allocated.
fatel_km2$centers #A matrix of cluster centers.
fatel_km2$size # The number of points in each cluster.
fatel_km2$betweenss
fatel_km2$withinss
fatel_km2$tot.withinss
fatel_km2$totss
fviz_cluster(fatel_km2, data = fatelNorm)

between <- numeric()
within <- numeric()



# Run the algorithm for different values of k (choose best k)
#method 1
set.seed(234)
for(i in 1:10){
  
  # For each k, calculate betweenss and tot.withinss
  between[i] <- kmeans(fatelNorm, centers=i)$betweenss
  within[i] <- kmeans(fatelNorm, centers=i)$tot.withinss
  
}

# Between-cluster sum of squares vs Choice of k
bet <- qplot(1:10, between, geom=c("point", "line"),
             xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  geom_line(color="#CC0033", size=1.5)+
  geom_point(size=2)+
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()+
  labs(title = 'betweenss')

# Total within-cluster sum of squares vs Choice of k
with <- qplot(1:10, within, geom=c("point", "line"),
              xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  geom_line(color="#663399", size=1.5)+
  geom_point(size=2)+
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()+
  labs(title = 'withiness')

# Subplot
grid.arrange(bet, with, ncol=2)


#method 2
fviz_nbclust(fatelNorm, kmeans, method = "wss")
#method 3
fviz_nbclust(fatelNorm, kmeans, method = "silhouette")



#Doing K-means with K=3
set.seed(1234)
# num of cluster=3
fatel_km3 <- kmeans(fatelNorm, centers=3)
# Mean values of each cluster
aggregate(fatel2, by=list(fatel_km3$cluster), mean)

#clustering
ggpairs(cbind(fatel2, Cluster=as.factor(fatel_km3$cluster)),
        columns=1:21, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous = gglegend("points")),
        axisLabels="none", switch="both")+ 
  theme_bw()


#comparison

table(fatel_km3$cluster, fatel$fetal_health)
table(fatel_km2$cluster)
table(fatel$fetal_health)

fatel_km4 <- kmeans(fatelNorm, centers=4)
p2<-fviz_cluster(fatel_km2, data = fatelNorm)+
  ggtitle("k = 2")
p3<-fviz_cluster(fatel_km3, data = fatelNorm)+
  ggtitle("k = 3")
p4<-fviz_cluster(fatel_km4, data = fatelNorm)+
  ggtitle("k = 4")

grid.arrange(p2,p3,p4,p3, nrow = 2)


#AGNES, bottom-up
E.dist <- dist(x = fatel2, method = "euclidean")
M.dist <- dist(x = fatel2, method = "manhattan")

par(mfrow=c(1,2))
h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="euclidean", main='euclidean distance', hang = -1, cex = 0.6)
h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="manhattan", main='manhattan distance', hang = -1, cex = 0.6)



hc<-par(mfrow= c(2,3))
plot(hclust(E.dist, method="single"),xlab = "single-linkage", main='single-linkage method', hang = -1, cex = 0.6, labels = F)   # 最近法
plot(hclust(E.dist, method="complete"), xlab = "complete-linkage", main='complete-linkage', hang = -1, cex = 0.6, labels = F)  # 最遠法
plot(hclust(E.dist, method="average"), xlab = "average-linkage", main='average-linkage', hang = -1, cex = 0.6, labels = F)  # 平均法
plot(hclust(E.dist, method="centroid"), xlab = "centroid-linkage", main='centroid-linkage', hang = -1, cex = 0.6, labels = F) # 中心法
plot(hclust(E.dist, method="ward.D2"), xlab = "Ward's Method", main="Ward's Method", hang = -1, cex = 0.6, labels = F)  # 華德法
par(hc)
#採用歐式距離搭配不同聚合演算法，並算出聚合係數(agglomerative coefficient)
#衡量群聚結構被辨識的程度，聚合係數越接近1代表有堅固的群聚結構(strong clustering structure)。
#在這個使用歐式距離搭配華德連結演算法的群聚係數有高達99%的表現。
m <- c( "average", "single", "complete", "ward")
m_ac=NULL
for(i in 1:length(m)){
  x<-agnes(E.dist, method=m[i])$ac
  m_ac<-append(m_ac,x)
}
names(m_ac) <- c( "average", "single", "complete", "ward")
m_ac

par(mfrow=c(1,1))
hc3 <- hclust(E.dist, method="ward.D2")
plot(hc3, hang = -1, cex = 0.6)
# rect.hclust(tree =hc3, k = 3, border = "purple")
# rect.hclust(tree =hc3, k = 2, border = "pink")
cut.h.cluster <- cutree(tree = hc3, k = 3)




#決定K
#method 1
fviz_nbclust(fatel2, FUN = hcut, method = "wss")
#method2
fviz_nbclust(x = fatel2, FUNcluster = hcut, method = "silhouette")

p2<-fviz_cluster(fatel_km2, data = fatelNorm)+
  ggtitle("k = 2")
p3<-fviz_cluster(fatel_km3, data = fatelNorm)+
  ggtitle("k = 3")
p4<-fviz_cluster(fatel_km4, data = fatelNorm)+
  ggtitle("k = 4")
grid.arrange(p2,p3,p4,p3, nrow = 2)


#GMM
dat2 = center_scale(fatel2, mean_center = T, sd_scale = T)  # centering and scaling the data
gmm2 = GMM(dat2, 3, dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10,
           em_iter = 10, verbose = F)          
pr = predict_GMM(dat2, gmm2$centroids, gmm2$covariance_matrices, gmm2$weights)    
table(pr$cluster_labels)
table(fatel$fetal_health)
table(fatel$fetal_health)
#accuracy comparison
hier_com<-table(cut.h.cluster, fatel$fetal_health)
acc1<-sum(diag(hier_com))/sum(hier_com)
km_com<-table(fatel_km3$cluster,fatel$fetal_health)
acc2<-sum(diag(km_com))/sum(km_com)
gmm_com<-table(fatel$fetal_health, pr$cluster_labels)
acc3<-sum(diag(gmm_com))/sum(gmm_com)

acc<-c(acc1,acc2,acc3)
names(acc)<-c('hierarchical','K-means','GMM')
acc


#Simulation
#Mixing Normal distribution
Mixnorm=NULL
U<-runif(1000,0,1)
for(i in 1:1000){
  if(U[i]<.3){
    x1<-rnorm(1,0,1)
    Mixnorm[i]<-x1
    }else if(.3<U[i]&&U[i]<.5){
    x2<-rnorm(1,-5,1)
    Mixnorm[i]<-x2
    }else{
    x3<-rnorm(1,5,1)
    Mixnorm[i]<-x3
  }
}
hist(Mixnorm,prob=T,breaks = 200)
#K-means
bet<-numeric()
wit<-numeric()
# mixnorm<-scale(Mix)
Mixnorm1<-as.data.frame(Mixnorm)
set.seed(178782)
for(i in 1:10){
  
  # For each k, calculate betweenss and tot.withinss
  bet[i] <- kmeans(Mixnorm1, centers=i)$betweenss
  wit[i] <- kmeans(Mixnorm1, centers=i)$tot.withinss
  
}
betw <- qplot(1:10, bet, geom=c("point", "line"),
              xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  geom_line(color="#CC0033", size=1.5)+
  geom_point(size=2)+
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()+
  labs(title = 'betweenss')

with1 <- qplot(1:10, wit, geom=c("point", "line"),
               xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  geom_line(color="#663399", size=1.5)+
  geom_point(size=2)+
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()+
  labs(title = 'withiness')

grid.arrange(betw, with1, ncol=2)

fviz_nbclust(Mixnorm1, kmeans, method = "wss")
fviz_nbclust(Mixnorm1, kmeans, method = "silhouette")
#choose k=3
n_km3<-kmeans(Mixnorm1,3)
table(n_km3$cluster)

#Hierachial
Eu.dist1 <- dist(x = Mixnorm1, method = "euclidean")
Ma.dist1 <- dist(x = Mixnorm1, method = "manhattan")
par(mfrow=c(1,2))
h.Eu.cluster1 <- hclust(Eu.dist1)
plot(h.Eu.cluster1, xlab="euclidean", main='euclidean distance of Mixed normal Distribution', hang = -1, cex = 0.6)
h.Ma.cluster1 <- hclust(Ma.dist1) 
plot(h.Ma.cluster1, xlab="manhattan", main='manhattan distance of Mixed normal Distribution', hang = -1, cex = 0.6)
par(mfrow= c(1,1))
plot(hclust(Eu.dist1, method="ward.D2"), xlab = "Ward's Method of Mixed normal Distribution", main="Ward's Method", hang = -1, cex = 0.6, labels = F)  # 華德法

#GMM
Mixndat = center_scale(Mixnorm1, mean_center = T, sd_scale = T)  # centering and scaling the data
gmmn = GMM(Mixndat, 3, dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10,
           em_iter = 10, verbose = F)          
prn = predict_GMM(Mixndat, gmmn$centroids, gmmn$covariance_matrices, gmmn$weights)    



#Mixing distribution
set.seed(12325)
Mixdist<-NULL
U<-runif(1000,0,1)
for(i in 1:1000){
  if(U[i]<.3){
    x1<-rnorm(1,-10,1)
    Mixdist[i]<-x1
  }else if(.3<U[i]&&U[i]<.65){
    x2<-rgamma(1,12,rate = 1/3)
    Mixdist[i]<-x2
  }else{
    x3<-rexp(1,1)
    Mixdist[i]<-x3
  }
}
hist(Mixdist,prob=T,breaks = 200)
betm<-numeric()
witm<-numeric()
Mixdist1<-as.data.frame(Mixdist)
set.seed(178882)
for(i in 1:10){
  
  # For each k, calculate betweenss and tot.withinss
  betm[i] <- kmeans(Mixdist1, centers=i)$betweenss
  witm[i] <- kmeans(Mixdist1, centers=i)$tot.withinss
  
}
betw2 <- qplot(1:10, betm, geom=c("point", "line"),
              xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  geom_line(color="#CC0033", size=1.5)+
  geom_point(size=2)+
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()+
  labs(title = 'betweenss')

with2 <- qplot(1:10, witm, geom=c("point", "line"),
               xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  geom_line(color="#663399", size=1.5)+
  geom_point(size=2)+
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()+
  labs(title = 'withiness')

grid.arrange(betw2, with2, ncol=2)

fviz_nbclust(Mixdist1, kmeans, method = "wss")
fviz_nbclust(Mixdist1, kmeans, method = "silhouette")
#choose k=3
m_km3<-kmeans(Mixdist1,3)
table(m_km3$cluster)
#Hierachial
Eu.dist2 <- dist(x = Mixdist1, method = "euclidean")
Ma.dist2 <- dist(x = Mixdist1, method = "manhattan")
par(mfrow=c(1,2))
h.Eu.cluster2 <- hclust(Eu.dist2)
plot(h.Eu.cluster2, xlab="euclidean", main='euclidean distance of Mixed Distribution', hang = -1, cex = 0.6)
h.Ma.cluster2 <- hclust(Ma.dist2) 
plot(h.Ma.cluster2, xlab="manhattan", main='manhattan distance of Mixed Distribution', hang = -1, cex = 0.6)
par(mfrow=c(1,1))
plot(hclust(Eu.dist2, method="ward.D2"), xlab = "Ward's Method", main="Ward's Method of Mixed Distribution", hang = -1, cex = 0.6, labels = F)  # 華德法
#GMM
Mixmdat = center_scale(Mixdist1, mean_center = T, sd_scale = T)  # centering and scaling the data
gmmm = GMM(Mixmdat, 3, dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10,
           em_iter = 10, verbose = F)          
prm = predict_GMM(Mixmdat, gmmm$centroids, gmmm$covariance_matrices, gmmm$weights)    
table(prm$cluster_labels)

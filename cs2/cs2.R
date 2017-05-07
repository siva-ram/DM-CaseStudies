data("iris")

set.seed(10669365)
subset <- sample(nrow(iris), nrow(iris) * 0.9)
iris_sample = iris[subset, ]
seed<-iris_sample[,1:4]


seed <- scale(seed) 
library(fpc)

par(mfrow=c(3,2))
set.seed(10669365)
#2 clusters
fit <- kmeans(seed, 2) #5 cluster solution
p1=plotcluster(seed, fit$cluster)
aggregate(seed,by=list(fit$cluster),FUN=mean)
table(fit$cluster)
#3 clusters
fit <- kmeans(seed, 3) #5 cluster solution
p2=plotcluster(seed, fit$cluster)
aggregate(seed,by=list(fit$cluster),FUN=mean)
table(fit$cluster)
#4 clusters
fit <- kmeans(seed, 4) #5 cluster solution
p3=plotcluster(seed, fit$cluster)
aggregate(seed,by=list(fit$cluster),FUN=mean)
table(fit$cluster)
#5 clusters
fit <- kmeans(seed, 5) #5 cluster solution
p4=plotcluster(seed, fit$cluster)
aggregate(seed,by=list(fit$cluster),FUN=mean)
table(fit$cluster)
fit <- kmeans(seed, 6) #6 cluster solution
p4=plotcluster(seed, fit$cluster)
aggregate(seed,by=list(fit$cluster),FUN=mean)
table(fit$cluster)
fit <- kmeans(seed, 7) #7 cluster solution
p4=plotcluster(seed, fit$cluster)
aggregate(seed,by=list(fit$cluster),FUN=mean)
table(fit$cluster)

par(mfrow=c(1,1))
set.seed(10669365)
# Determine number of clusters
wss <- (nrow(seed)-1)*sum(apply(seed,2,var))
for (i in 2:12) wss[i] <- sum(kmeans(seed,
                                     centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

set.seed(10669365)
prediction.strength(seed, Gmin=2, Gmax=15, M=10,cutoff=0.8)

set.seed(10669365)
d = dist(seed, method = "euclidean")
result = matrix(nrow = 14, ncol = 3)
for (i in 2:15){
  cluster_result = kmeans(seed, i)
  clusterstat=cluster.stats(d, cluster_result$cluster)
  result[i-1,1]=i
  result[i-1,2]=clusterstat$avg.silwidth
  result[i-1,3]=clusterstat$dunn   
}
plot(result[,c(1,2)], type="l", ylab = 'silhouette width', xlab = 'number of clusters')

plot(result[,c(1,3)], type="l", ylab = 'dunn index', xlab = 'number of clusters')

#Wards Method or Hierarchical clustering
#Calculate the distance matrix
seed.dist=dist(seed)
#Obtain clusters using the Wards method
seed.hclust=hclust(seed.dist, method="ward.D")

par(mfrow=c(3,2))

i=2
for (i in 2:7){
 
seed.3clust = cutree(seed.hclust,k=i)
#plotcluster(ZooFood, fit$cluster)
plotcluster(seed, seed.3clust)
}
par(mfrow=c(1,1))
seed.3clust = cutree(seed.hclust,k=2)
#plotcluster(ZooFood, fit$cluster)
plotcluster(seed, seed.3clust)

aggregate(seed,by=list(seed.3clust),FUN=mean)


?cutree

hc_result = hclust(dist(seed))
plot(hc_result)
#Cut Dendrogram into 3 Clusters
clus = rect.hclust(hc_result, k=2)

#association rules

TransFood <- read.csv('http://homepages.uc.edu/~maifg/DataMining/data/food_4_association.csv')
TransFood <- TransFood[, -1]
TransFood <- as(as.matrix(TransFood), "transactions")

library(arules)
View(TransFood)
itemFrequencyPlot(TransFood, support = 0.1, cex.names=0.8)
basket_rules <- apriori(TransFood,parameter = list(sup = 0.004, conf = 0.8,target="rules"))
summary(basket_rules)
inspect(sort(basket_rules, by="lift"))

library('arulesViz')
plot(basket_rules)
plot(head(sort(basket_rules, by="lift"), 10), method = "graph")
plot(basket_rules, method="grouped")




#clustering

Food_by_month <- read.csv('http://homepages.uc.edu/~maifg/DataMining/data/qry_Food_by_Month.csv')

str(Food_by_month)
View(Food_by_month)
seed <- Food_by_month[,2:7]

library(ggplot2)
barplot(seed)

library(fpc)


par(mfrow=c(3,2))
set.seed(10669365)
#2 clusters
fit <- kmeans(seed, 2) #5 cluster solution
p1=plotcluster(seed, fit$cluster)
aggregate(seed,by=list(fit$cluster),FUN=mean)
table(fit$cluster)
#3 clusters
fit <- kmeans(seed, 3) #5 cluster solution
p2=plotcluster(seed, fit$cluster)
aggregate(seed,by=list(fit$cluster),FUN=mean)
table(fit$cluster)
#4 clusters
fit <- kmeans(seed, 4) #5 cluster solution
p3=plotcluster(seed, fit$cluster)
aggregate(seed,by=list(fit$cluster),FUN=mean)
table(fit$cluster)
#5 clusters
fit <- kmeans(seed, 5) #5 cluster solution
p4=plotcluster(seed, fit$cluster)
aggregate(seed,by=list(fit$cluster),FUN=mean)
table(fit$cluster)
set.seed(10669365)
fit <- kmeans(seed, 6) #6 cluster solution
p4=plotcluster(seed, fit$cluster)
aggregate(seed,by=list(fit$cluster),FUN=mean)
table(fit$cluster)
fit <- kmeans(seed, 7) #7 cluster solution
p4=plotcluster(seed, fit$cluster)
aggregate(seed,by=list(fit$cluster),FUN=mean)
table(fit$cluster)

par(mfrow=c(1,1))
set.seed(10669365)
# Determine number of clusters
wss <- (nrow(seed)-1)*sum(apply(seed,2,var))
for (i in 2:12) wss[i] <- sum(kmeans(seed,
                                     centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

set.seed(10669365)
prediction.strength(seed, Gmin=2, Gmax=15, M=10,cutoff=0.8)

set.seed(10669365)
d = dist(seed, method = "euclidean")
result = matrix(nrow = 14, ncol = 3)
for (i in 2:15){
  cluster_result = kmeans(seed, i)
  clusterstat=cluster.stats(d, cluster_result$cluster)
  result[i-1,1]=i
  result[i-1,2]=clusterstat$avg.silwidth
  result[i-1,3]=clusterstat$dunn   
}
plot(result[,c(1,2)], type="l", ylab = 'silhouette width', xlab = 'number of clusters')

plot(result[,c(1,3)], type="l", ylab = 'dunn index', xlab = 'number of clusters')

set.seed(10669365)
fit <- kmeans(seed, 6) #6 cluster solution
p4=plotcluster(seed, fit$cluster)
aggregate(seed,by=list(fit$cluster),FUN=mean)
table(fit$cluster)

final_1 = data.frame(Food_by_month[,1],fit$cluster)

final_1[which(final_1[,2]==1),1]
final_1[which(final_1[,2]==2),1]
final_1[which(final_1[,2]==3),1]
final_1[which(final_1[,2]==4),1]
final_1[which(final_1[,2]==5),1]
final_1[which(final_1[,2]==6),1]


plot(final)

#Wards Method or Hierarchical clustering
#Calculate the distance matrix
seed.dist=dist(seed)
#Obtain clusters using the Wards method
seed.hclust=hclust(seed.dist, method="ward.D")

par(mfrow=c(3,2))

i=2
for (i in 2:7){
  
  seed.3clust = cutree(seed.hclust,k=i)
  #plotcluster(ZooFood, fit$cluster)
  plotcluster(seed, seed.3clust)
}
par(mfrow=c(1,1))
seed.3clust = cutree(seed.hclust,k=5)
#plotcluster(ZooFood, fit$cluster)
plotcluster(seed, seed.3clust)

aggregate(seed,by=list(seed.3clust),FUN=mean)


?cutree

hc_result = hclust(dist(seed))
plot(hc_result)
#Cut Dendrogram into 3 Clusters
clus = rect.hclust(hc_result, k=5)


final_1 = data.frame(Food_by_month[,1],clus$)

final_1[which(final_1[,2]==1),1]
final_1[which(final_1[,2]==2),1]
final_1[which(final_1[,2]==3),1]
final_1[which(final_1[,2]==4),1]
final_1[which(final_1[,2]==5),1]
final_1[which(final_1[,2]==6),1]
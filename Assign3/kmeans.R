source('~/MachineLearning/Assign3/unsup.R')
source('~/MachineLearning/Assign3/pca.R')
library(stats)
library(useful)

wine.train = load.wine_data()
cars.train = load.cars_data()

cars.withinss = c()
cars.pcak.withinss = c()
cars.clusters = c(1:20)
for(a in cars.clusters){
  cars.kmeans = kmeans(cars.train[,-7], centers=a, iter.max = 50, nstart=20)
  cars.pcak = kmeans(cars.pca$scores[,1:3], centers=a, iter.max = 50, nstart=20)
  cars.pcak.withinss = c(cars.pcak.withinss, mean(cars.pcak$withinss))
  cars.withinss = c(cars.withinss, mean(cars.kmeans$withinss))
}

cars.withinss2 <- c()
cars.starts = c(1, 2, 4, 6, 8, 12, 14, 16)
for(a in cars.starts){
  cars.kmeans = kmeans(cars.train[,-7], centers=4, iter.max = 50, nstart=a)
  cars.withinss2 = c(cars.withinss2, mean(cars.kmeans$withinss)) 
}
 
wine.withinss= c()
wine.pcak.withinss = c()
wine.clusters = c(1:20)
for(a in wine.clusters){
  wine.kmeans = kmeans(wine.train[,-12], centers=a, iter.max = 50, nstart=20)
  wine.pcak = kmeans(wine.pca$scores[,1:3], centers=a, iter.max = 50, nstart=20)
  wine.pcak.withinss = c(wine.pcak.withinss, mean(wine.pcak$withinss))
  wine.withinss = c(wine.withinss, mean(wine.kmeans$withinss))
}

wine.withinss2 <- c()
wine.starts = c(1, 2, 4, 6, 8, 12, 14, 16)
for(a in wine.starts){
  wine.kmeans = kmeans(wine.train[,-12], centers=8, iter.max = 50, nstart=a)
  wine.withinss2 <- c(wine.withinss2,  mean(wine.kmeans$withinss))
}

starts = wine.starts
starts = append(starts, cars.starts)

clusters = wine.clusters
clusters=append(clusters, cars.clusters)

within2 = wine.withinss2
within2 = append(within2, cars.withinss2)

within = cars.withinss
within = append(within, wine.withinss)

pca.within = cars.pcak.withinss
pca.within = append(pca.within, wine.pcak.withinss)

names = rep("wine",20)
car.names = rep("car",20)
names = append(names, car.names)

names2 = rep("wine",8)
car.names2 = rep("car",8)
names2 = append(names2, car.names2)

results = data.frame(clusters, names, within)
results2 = data.frame(starts, names2, within2)

pca.results = data.frame(clusters, names, pca.within)

wine.kmeans = kmeans(wine.train[,-12], centers=8, iter.max = 50, nstart=20)
wine.pcak = kmeans(wine.pca$scores[,1:3], centers=8, iter.max = 50, nstart=20)
cars.kmeans = kmeans(cars.train[,-7], centers=4, iter.max = 50, nstart=20)
cars.pcak = kmeans(cars.pca$scores[,1:3], centers=4, iter.max = 50, nstart=20)

### cars and wine pca sets
within.pca = cars.pcak.withinss
within.pca = append(within.pca, wine.pcak.withinss)

results.pcak = data.frame(clusters, names, within.pca)

#plot of withinss after PCA
pl11 = ggplot(pca.results, aes(x=clusters, y=within.pca, group=names)) +
  geom_line(aes(colour=names)) +
  ggtitle("Avg Cluster SS Vs. Restarts after PCA") +
  theme(axis.title=element_text(face="bold.italic", color="brown"), title=element_text(face="bold", color="brown")) +
  xlab("# Clusters") +
  ylab("Avg Cluster SS") +
  theme(legend.position="bottom")

pl2 = ggplot(results2, aes(x=starts, y=within2, group=names2)) +
  geom_line(aes(colour=names2)) +
  ggtitle("Avg Cluster SS Vs. Restarts") +
  theme(axis.title=element_text(face="bold.italic", color="brown"), title=element_text(face="bold", color="brown")) +
  xlab("# Restarts") +
  ylab("Avg Cluster SS") +
  theme(legend.position="bottom")

pl3 = ggplot(results, aes(x=clusters, y=within, group=names)) +
  geom_line(aes(colour=names)) +
  ggtitle("Avg Cluster SS Vs. #Clusters") +
  theme(axis.title=element_text(face="bold.italic", color="brown"), title=element_text(face="bold", color="brown")) +
  xlab("# Clusters") +
  ylab("Avg Cluster SS") +
  theme(legend.position="bottom")

wine.x = c(1,2,3,4,5,6,7,8)
wine.counts = c()
wine.pcak.counts = c()
for(i in wine.x){
  wine.counts = c(wine.counts, sum(wine.kmeans$cluster == i))
  wine.pcak.counts = c(wine.pcak.counts, sum(wine.pcak$cluster == i))
}

cars.x = c(1,2,3,4)
cars.pcak.counts = c()
cars.counts = c()
for(i in cars.x){
  cars.counts = c(cars.counts, sum(cars.kmeans$cluster == i))
  cars.pcak.counts = c(cars.pcak.counts, sum(cars.pcak$cluster == i))
}

pcak.counts = sort(wine.pcak.counts)
pcak.counts = append(pcak.counts, sort(cars.pcak.counts))

k.counts = sort(wine.counts)
k.counts = append(k.counts, sort(cars.counts))

names3 = rep("wine",8)
car.names3 = rep("car",4)
names3 = append(names3, car.names3)

x= c(wine.x, cars.x)
k.results3 = data.frame(k.counts, names3, x)
pcak.results = data.frame(pcak.counts, names3, x)


pl1 = ggplot(k.results3, aes(x=k.results3$x, fill=k.results3$names3)) + geom_density(alpha=.3)

####plot for pca k means
pl4 = ggplot(pcak.results, aes(x=pcak.results$x, fill=pcak.results$names)) + geom_density(alpha=.3)


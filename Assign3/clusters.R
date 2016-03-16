source('~/MachineLearning/Assign3/kmeans.R')
source('~/MachineLearning/Assign3/em.R')
library(plyr)

em.results3 = rename(em.results3, c("em.counts"="counts", "em.names3"="names", "emx"="x"))
k.results3 = rename(k.results3, c("k.counts"="counts", "names3"="names"))

clust.em.results = rename(results.pcaem, c("pcaem.counts"="counts", "em.names3"="names", "emx"="x"))
clust.k.results = rename(pcak.results, c("pcak.counts"="counts", "names3"="names"))

kw.tmp = k.results3[k.results3[,2]=="wine",]
kc.tmp = k.results3[k.results3[,2]=="car",]
kw.tmp[,2]="k.wine"
kc.tmp[,2] = "k.car"
emw.tmp = em.results3[em.results3[,2]=="wine",]
emc.tmp = em.results3[em.results3[,2]=="car",]
emw.tmp[,2]="em.wine"
emc.tmp[,2] = "em.car"

clust.kw.tmp = clust.k.results[clust.k.results[,2]=="wine",]
clust.kc.tmp = clust.k.results[clust.k.results[,2]=="car",]
clust.kw.tmp[,2]="k.wine"
clust.kc.tmp[,2] = "k.car"
clust.emw.tmp = clust.em.results[clust.em.results[,2]=="wine",]
clust.emc.tmp = clust.em.results[clust.em.results[,2]=="car",]
clust.emw.tmp[,2]="em.wine"
clust.emc.tmp[,2] = "em.car"

car.res = rbind(emc.tmp, kc.tmp)
wine.res = rbind(emw.tmp, kw.tmp)

clust.car.res = rbind(clust.emc.tmp, clust.kc.tmp)
clust.wine.res = rbind(clust.emw.tmp, clust.kw.tmp)

wine.x = c(1,2,3,4,5,6)
wine.counts = c()
for(i in wine.x){
  wine.counts = c(wine.counts, sum(wine.train$quality == i))
}

cars.x = c(1,2,3,4)
cars.counts = c()
for(i in cars.x){
  cars.counts = c(cars.counts, sum(cars.train$class2 == i))
}

wine.counts = sort(wine.counts)
cars.counts = sort(cars.counts)

wine.names = rep("label.wine",6)
cars.names = rep("label.car",4)
wine.tmp = data.frame(wine.counts, wine.names, wine.x)
cars.tmp = data.frame(cars.counts, cars.names, cars.x)
wine.tmp = rename(wine.tmp, c("wine.counts"="counts", "wine.names"="names", "wine.x"="x"))
cars.tmp = rename(cars.tmp, c("cars.counts"="counts", "cars.names"="names", "cars.x"="x"))

car.res = rbind(car.res, cars.tmp)
wine.res = rbind(wine.res, wine.tmp)

clust.car.res = rbind(clust.car.res, cars.tmp)
clust.wine.res = rbind(clust.wine.res, wine.tmp)

pl3 = ggplot(results, aes(x=clusters, y=within, group=names)) +
  geom_line(aes(colour=names)) +
  ggtitle("Avg Cluster SS Vs. #Clusters") +
  theme(axis.title=element_text(face="bold.italic", color="brown"), title=element_text(face="bold", color="brown")) +
  xlab("# Clusters") +
  ylab("Avg Cluster SS") +
  theme(legend.position="bottom")


pl5 = ggplot(wine.res, aes(x=wine.res$x, fill=wine.res$names)) +geom_density(alpha=.3) + ggtitle("Wine Quality Densities")+
  theme(plot.title = element_text(lineheight=.8, face="bold.italic", color="brown"))+
  labs(x="Clusters/Labels", y="Density") + theme(axis.title=element_text(face="bold.italic", color="brown")) +
  theme(legend.position="bottom",  legend.title=element_blank())

pl6 = ggplot(car.res, aes(x=car.res$x, fill=car.res$names)) +geom_density(alpha=.3) + ggtitle("Cars Eval. Densities")+
  theme(plot.title = element_text(lineheight=.8, face="bold", color="brown"))+
  labs(x="Clusters/Labels", y="Density") + theme(axis.title=element_text(face="bold.italic", color="brown")) +
  theme(legend.position="bottom",  legend.title=element_blank())

####plot for pca k means
pl8 = ggplot(clust.wine.res, aes(x=clust.wine.res$x, fill=clust.wine.res$names)) +geom_density(alpha=.3)+ ggtitle("Cars Eval. after PCA")+
  theme(plot.title = element_text(lineheight=.8, face="bold", color="brown"))+
  labs(x="Clusters/Labels", y="Density") + theme(axis.title=element_text(face="bold.italic", color="brown")) +
  theme(legend.position="bottom",  legend.title=element_blank())

pl9 = ggplot(clust.car.res, aes(x=clust.car.res$x, fill=clust.car.res$names)) +geom_density(alpha=.3)  + ggtitle("Wine Quality after PCA")+
  theme(plot.title = element_text(lineheight=.8, face="bold", color="brown"))+
  labs(x="Clusters/Labels", y="Density") + theme(axis.title=element_text(face="bold.italic", color="brown")) +
  theme(legend.position="bottom", legend.title=element_blank())

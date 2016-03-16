source('~/MachineLearning/Assign3/unsup.R')
source('~/MachineLearning/Assign3/pca.R')
library(mclust)

wine.train = load.wine_data()
cars.train = load.cars_data()

#cars.mclust = Mclust(cars.train[,-7], 1:20)
#k = cars.mclust$G
#Mclust' model object:
#best model: ellipsoidal, equal volume and orientation (EVE) with 9 components
#but EII model looks less noisy - 4 components

modelName = "EII"
data = cars.train[,-7]
z = unmap(cars.train[,7])
msEst <- mstep(modelName, data, z)
modelName = msEst$modelName
parameters = msEst$parameters
cars.em = em(modelName, data, parameters)
cars.pcaem = em(modelName, cars.pca$scores[,1:4], parameters)

pcaem.cars.class = cars.pcaem$z
tmp <- c()
for(i in 1:nrow(pcaem.cars.class)){
  c = 0
  m = 0
  for( j in 1:ncol(pcaem.cars.class)){
    if(pcaem.cars.class[i,j]>m){
      c = j
      m = pcaem.cars.class[i,j]
    }
  }
  tmp <- append(tmp, c)
}
pcaem.cars.class = cbind(pcaem.cars.class, tmp)

em.cars.class = cars.em$z
border = c()
tmp <- c()
for(i in 1:nrow(em.cars.class)){
  c = 0
  m = 0
  b = 0
  for( j in 1:ncol(em.cars.class)){
    if(em.cars.class[i,j]>m){
      c = j
      m = em.cars.class[i,j]
    }
    if(em.cars.class[i,j] >= 0.45 && em.cars.class[i,j] <=0.55 ){
      b=1
    }
  }
  border = append(border, b)
  tmp <- append(tmp, c)
}
cars.borderline = sum(border)/nrow(em.cars.class)
em.cars.class = cbind(em.cars.class, tmp)

#wine.mclust = Mclust(wine.train[,-12], 1:20)
#'Mclust' model object:
#best model: ellipsoidal, equal orientation (VVE) with 9 components

modelName = "VVI"
data = wine.train[,-12]
z = unmap(wine.train[,12])
msEst <- mstep(modelName, data, z)
modelName = msEst$modelName
parameters = msEst$parameters
wine.em = em(modelName, data, parameters)
em.wine.class = wine.em$z
pcaem.wine = em(modelName, wine.pca$scores[,1:7], parameters)

pcaem.wine.class = pcaem.wine$z
tmp <- c()
for(i in 1:nrow(pcaem.wine.class)){
  c = 0
  m = 0
  for( j in 1:ncol(pcaem.wine.class)){
    if(pcaem.wine.class[i,j]>m){
      c = j
      m = pcaem.wine.class[i,j]
    }
  }
  tmp <- append(tmp, c)
}
pcaem.wine.class = cbind(pcaem.wine.class, tmp)


tmp <- c()
border = c()
for(i in 1:nrow(em.wine.class)){
  c = 0
  m = 0
  b=0
  for( j in 1:ncol(em.wine.class)){
    if(em.wine.class[i,j]>m){
      c = j
      m = em.wine.class[i,j]
    }
    if(em.wine.class[i,j] >= 0.45 && em.wine.class[i,j] <=0.55 ){
      b=1
    }
  }
  border = append(border, b)
  tmp <- append(tmp, c)
}
em.wine.class = cbind(em.wine.class, tmp)
wine.borderline = sum(border)/nrow(em.wine.class)

em.wine.x = c(1,2,3,4,5,6)
em.wine.counts = c()
pcaem.wine.counts = c()
for(i in em.wine.x){
  em.wine.counts = c(em.wine.counts, sum(em.wine.class[,7] == i))
  pcaem.wine.counts = c(pcaem.wine.counts, sum(pcaem.wine.class[,7] == i))
}

em.cars.x = c(1,2,3,4)
em.cars.counts = c()
pcaem.cars.counts = c()
for(i in cars.x){
  em.cars.counts = c(em.cars.counts, sum(em.cars.class[,5] == i))
  pcaem.cars.counts = c(pcaem.cars.counts, sum(pcaem.cars.class[,5] == i))
}

em.counts = sort(em.wine.counts)
em.counts = append(em.counts, sort(em.cars.counts))

em.names3 = rep("wine",6)
em.car.names3 = rep("car",4)
em.names3 = append(em.names3, em.car.names3)

emx= c(em.wine.x, em.cars.x)
em.results3 = data.frame(em.counts, em.names3, emx)

pcaem.counts = sort(pcaem.wine.counts)
pcaem.counts = append(pcaem.counts, sort(pcaem.cars.counts))

pcaem.results3 = data.frame(pcaem.counts, em.names3, emx)

### cars and wine pca sets
within.pcaem = cars.pcak.withinss
within.pcaem = append(within, wine.pcak.withinss)

results.pcaem = data.frame(pcaem.counts, em.names3, emx)

pl6 = ggplot(em.results3, aes(x=em.results3$emx, fill=em.results3$em.names3)) +geom_density(alpha=.3)
pl7 = ggplot(results.pcaem, aes(x=results.pcaem$emx, fill=results.pcaem$em.names3)) +geom_density(alpha=.3)
source('~/MachineLearning/Assign3/unsup.R')
library(svd)

wine.train = load.wine_data()
cars.train = load.cars_data()

cars.svd <- svd(cars.train[,-7])
#cars.svd.plot = plot(cars.svd$d)

wine.svd <- svd(wine.train[,-12])
#wine.svd.plot = plot(wine.svd$d)


wine.svd.d <- diag(wine.svd$d)
wine.recon <- wine.svd$u[,1:6] %*% wine.svd.d[1:6,1:6] %*% t(wine.svd$v[,1:6])
source('~/MachineLearning/Assign3/unsup.R')
library(stats)

wine.train = load.wine_data()
cars.train = load.cars_data()

cars.pca = princomp(cars.train[,-7])
cars.pca$scores
#cars.plot = plot(cars.pca)
#cars.biplot = biplot(cars.pca)

wine.pca = princomp(wine.train[,-12])
wine.pca$scores
#wine.plot = plot(wine.pca)
#wine.biplot = biplot(wine.pca)
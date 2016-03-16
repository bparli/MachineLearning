source('~/MachineLearning/Assign3/unsup.R')
library(ica)

wine.train = load.wine_data()
cars.train = load.cars_data()


cars.ica = icaimax(cars.train[,-7], ncol(cars.train)-1)
cars.new = cbind(cars.ica$S, cars.train[,7])
#cars.plot = plot(cars.ica$vafs)

wine.ica = icaimax(wine.train[,-12], ncol(wine.train)-1)
wine.new = cbind(wine.ica$S, wine.train[,12])
#wine.plot = plot(wine.ica$vafs)
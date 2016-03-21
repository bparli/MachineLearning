source('~/MachineLearning/Assign3/unsup.R')
source('~/MachineLearning/Assign3/random_proj.R')

wine.train = load.wine_data()
cars.train = load.cars_data()

cars.rand = gaussian_random_projection(cars.train[,-7], n_features=3)
cars.E = cars.rand$R*cars.train[,-7]

wine.rand = gaussian_random_projection(wine.train[,-12], n_features=6)
wine.E = wine.rand$R*wine.train[,-12]

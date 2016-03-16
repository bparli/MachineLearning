library(stats)

load.wine_data = function(){
  
  neuralwine = read.csv2(file="C:/Users/bparli/Downloads/winequality-red.csv", header=TRUE)
  neuralwine$citric.acid=as.numeric(as.character(neuralwine$citric.acid))
  neuralwine$fixed.acidity=as.numeric(as.character(neuralwine$fixed.acidity))
  neuralwine$volatile.acidity=as.numeric(as.character(neuralwine$volatile.acidity))
  neuralwine$residual.sugar=as.numeric(as.character(neuralwine$residual.sugar))
  neuralwine$chlorides=as.numeric(as.character(neuralwine$chlorides))
  neuralwine$free.sulfur.dioxide=as.numeric(as.character(neuralwine$free.sulfur.dioxide))
  neuralwine$total.sulfur.dioxide=as.numeric(as.character(neuralwine$total.sulfur.dioxide))
  neuralwine$density=as.numeric(as.character(neuralwine$density))
  neuralwine$pH=as.numeric(as.character(neuralwine$pH))
  neuralwine$sulphates=as.numeric(as.character(neuralwine$sulphates))
  neuralwine$alcohol=as.numeric(as.character(neuralwine$alcohol))
  neuralwine$quality=as.factor(neuralwine$quality)
  
  mins = apply(neuralwine[,-12], 2, min)
  maxs = apply(neuralwine[,-12], 2, max)
  neuralscaled = as.data.frame(scale(neuralwine[,-12], center = mins, scale = maxs - mins))
  neuralscaled = cbind(neuralscaled,quality=neuralwine$quality)
  
  set.seed(1)
  samp.wine=floor(nrow(neuralscaled)*0.75)
  wine.train_ind <- sample(seq_len(nrow(neuralscaled)), size = samp.wine)
  wine.train=neuralscaled[wine.train_ind,]
  wine.test=neuralscaled[-wine.train_ind,]
  
  wine.train$quality = as.numeric(wine.train$quality)
  sub = function(num){
    return(num-2)
  }
  
  sapply(wine.train$quality, sub)
  return(wine.train)
}

load.cars_data = function(){
  cars = read.csv2(file="C:/Users/bparli/Downloads/cars.txt", header=TRUE, sep=",")
  
  cars$buying2 = 1
  cars$buying2[cars$buying=="vhigh"] = 4
  cars$buying2[cars$buying=="high"] = 3
  cars$buying2[cars$buying=="med"] = 2
  
  cars$maint2 = 1
  cars$maint2[cars$maint=="vhigh"] = 4
  cars$maint2[cars$maint=="high"] = 3
  cars$maint2[cars$maint=="med"] = 2
  
  cars$doors2 = 1
  cars$doors2[cars$doors=="5more"] = 4
  cars$doors2[cars$doors=="4"] = 3
  cars$doors2[cars$doors=="3"] = 2
  
  cars$persons = as.numeric(cars$persons)
  cars$safety = as.numeric(cars$safety)
  cars$lug_boot = as.numeric(cars$lug_boot)
  
  cars$class2 = 1
  cars$class2[cars$class=="vgood"] = 4
  cars$class2[cars$class=="good"] = 3
  cars$class2[cars$class=="acc"] = 2
  cars$class2[cars$class=="unacc"] = 1
  
  cars$buying=NULL
  cars$maint=NULL
  cars$class = NULL
  cars$doors = NULL
  
  set.seed(1)
  samp.cars=floor(nrow(cars)*0.75)
  cars.train_ind <- sample(seq_len(nrow(cars)), size = samp.cars)
  cars.train=cars[cars.train_ind,]
  cars.test=cars[-cars.train_ind,]
  return(cars.train)
}


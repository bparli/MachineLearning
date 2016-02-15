install.packages("class")
library(class)

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
cars$doors2[cars$maint=="5more"] = 4
cars$doors2[cars$maint=="4"] = 3
cars$doors2[cars$maint=="3"] = 2

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
samp.cars=floor(nrow(nncars)*0.75)
cars.train_ind <- sample(seq_len(nrow(cars)), size = samp.cars)
cars.train=cars[cars.train_ind,]
cars.test=cars[-cars.train_ind,]

cars.knn=knn(train=cars.train[,-7], test=cars.train[,-7], cl=cars.train[,7],k=4)
table(cars.knn, cars.train[,7])

cars.knntest=knn(train=cars.train[,-7], test=cars.test[,-7], cl=cars.train[,7],k=4)
table(cars.knntest, cars.test[,7])

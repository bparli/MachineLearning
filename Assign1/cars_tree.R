library(C50)

cars = read.csv2(file="C:/Users/bparli/Downloads/cars.txt", header=TRUE, sep=",")

set.seed(1)
samp.cars=floor(nrow(cars)*0.75)
cars.train_ind <- sample(seq_len(nrow(cars)), size = samp.cars)
cars.train=cars[cars.train_ind,]
cars.test=cars[-cars.train_ind,]

cars.tree=C5.0(cars.train$class~.,data=cars.train)
cars.pred=predict(cars.tree, cars.train, type="class")
summary(cars.pred)
table(cars.pred, cars.train$class)
cars.pred=predict(cars.tree, cars.test, type="class")
summary(cars.pred)

table(cars.pred, cars.test$class)
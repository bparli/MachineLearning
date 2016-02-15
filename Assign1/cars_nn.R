install.packages("nnet")
library(nnet)

nncars = read.csv2(file="C:/Users/bparli/Downloads/cars.txt", header=TRUE, sep=",")


samp.cars=floor(nrow(nncars)*0.75)
cars.train_ind <- sample(seq_len(nrow(nncars)), size = samp.cars)
cars.train=nncars[cars.train_ind,]
cars.test=nncars[-cars.train_ind,]
cars.nn = nnet(class~buying+maint+doors+persons+lug_boot+safety, data=cars.train, size=5, type="class", maxit=200)

cars.pred = predict(cars.nn, cars.train[,-7],type="class")
table(cars.pred, cars.train$class)

cars.pred_test = predict(cars.nn, cars.test[,-7],type="class")
table(cars.pred_test, cars.test$class)
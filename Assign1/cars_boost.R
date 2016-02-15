install.packages("adabag")
library(adabag)

cars = read.csv2(file="C:/Users/bparli/Downloads/cars.txt", header=TRUE, sep=",")

set.seed(1)
samp.cars=floor(nrow(nncars)*0.75)
cars.train_ind <- sample(seq_len(nrow(nncars)), size = samp.cars)
cars.train=nncars[cars.train_ind,]
cars.test=nncars[-cars.train_ind,]

iters = c(5,10,25,50,75,100)
times=c()

for(iter in 1:6) {
  tmp = system.time(boosting(quality~.,data=wine.train, mfinal=iters[iter], boos=TRUE))
  times[iter] = tmp[1]
}

cars.boost = boosting(class~.,data=cars.train, mfinal=10, boos=TRUE)
cars.pred = predict(cars.boost, newdata=cars.train)
cars.pred$confusion
cars.pred$error

cars.pred_test = predict(cars.boost, newdata=cars.test)
cars.pred_test$confusion
cars.pred_test$error

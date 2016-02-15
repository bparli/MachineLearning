install.packages("e1071")
library(e1071)

cars = read.csv2(file="C:/Users/bparli/Downloads/cars.txt", header=TRUE, sep=",")


set.seed(1)
samp.cars=floor(nrow(nncars)*0.75)
cars.train_ind <- sample(seq_len(nrow(nncars)), size = samp.cars)
cars.train=nncars[cars.train_ind,]
cars.test=nncars[-cars.train_ind,]


cars.tune = tune(svm, class~.,data=cars.train, kernel="polynomial", scale=TRUE, ranges=list(cost=c(450, 500, 550, 600, 650, 700, 750)))
summary(cars.tune)
cars.svmpoly = svm(class~.,data=cars.train, kernel="polynomial", scale=TRUE, cost=450)
cars.predpoly = predict(cars.svmpoly, cars.test)
table(cars.predpoly, cars.test$class)

cars.tune_lin = tune(svm, class~.,data=cars.train, kernel="linear", scale=TRUE, ranges=list(cost=c(0.01, 0.1, 1, 10, 20, 50)))
cars.svm_lin = svm(class~.,data=cars.train, kernel="linear", scale=TRUE, cost=10)
summary(cars.tune_lin)
cars.pred_lin = predict(cars.svm_lin, cars.test)
table(cars.pred_lin, cars.test$class)


cars.tune_sig = tune(svm, class~.,data=cars.train, kernel="sigmoid", scale=TRUE, ranges=list(cost=c(0.01, 0.1, 1, 10, 20, 50)))
cars.svm_sig = svm(class~.,data=cars.train, kernel="sigmoid", scale=TRUE, cost=50)
summary(cars.tune_sig)
cars.pred_sig = predict(cars.svm_sig, cars.test)
table(cars.pred_sig, cars.test$class)
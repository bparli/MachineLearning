install.packages("e1071")
library(e1071)

wine = read.csv2(file="C:/Users/bparli/Downloads/winequality-red.csv", header=TRUE)
wine$citric.acid=as.numeric(as.character(wine$citric.acid))
wine$fixed.acidity=as.numeric(as.character(wine$fixed.acidity))
wine$volatile.acidity=as.numeric(as.character(wine$volatile.acidity))
wine$residual.sugar=as.numeric(as.character(wine$residual.sugar))
wine$chlorides=as.numeric(as.character(wine$chlorides))
wine$free.sulfur.dioxide=as.numeric(as.character(wine$free.sulfur.dioxide))
wine$total.sulfur.dioxide=as.numeric(as.character(wine$total.sulfur.dioxide))
wine$density=as.numeric(as.character(wine$density))
wine$pH=as.numeric(as.character(wine$pH))
wine$sulphates=as.numeric(as.character(wine$sulphates))
wine$alcohol=as.numeric(as.character(wine$alcohol))
wine$quality=as.factor(wine$quality)

svm_test_errs = c()
svm_train_errs = c()
samplings = c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.9)
for(samp in 1:8){
  set.seed(1)
  samp.wine=floor(nrow(wine)*samplings[samp])
  wine.train_ind <- sample(seq_len(nrow(wine)), size = samp.wine)
  wine.train=wine[wine.train_ind,]
  wine.test=wine[-wine.train_ind,]

  wine.polysvm = svm(quality~.,data=wine.train, kernel="polynomial", scale=TRUE, cost=5)
  
  wine.pred = predict(wine.polysvm, newdata=wine.train)
  tr_matrix = table(wine.pred, wine.train$quality)
  svm_train_errs[samp] = 1-((tr_matrix[1,1]+tr_matrix[2,2]+tr_matrix[3,3]+tr_matrix[4,4]+tr_matrix[5,5]+ tr_matrix[6,6])/nrow(wine.train))
  
  wine.pred = predict(wine.polysvm, newdata=wine.test)
  test_matrix = table(wine.pred, wine.test$quality) 
  svm_test_errs[samp] = 1-((test_matrix[1,1]+test_matrix[2,2]+test_matrix[3,3]+test_matrix[4,4]+test_matrix[5,5]+ test_matrix[6,6])/nrow(wine.test))
}

set.seed(1)
samp.wine=floor(nrow(wine)*0.75)
wine.train_ind <- sample(seq_len(nrow(wine)), size = samp.wine)
wine.train=wine[wine.train_ind,]
wine.test=wine[-wine.train_ind,]

wine.poly = tune(svm, quality~.,data=wine.train, kernel="polynomial", scale=TRUE, ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10)))
wine.polysvm = svm(quality~.,data=wine.train, kernel="polynomial", scale=TRUE, cost=5)

wine.lin = tune(svm, quality~.,data=wine.train, kernel="linear", scale=TRUE, ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10)))
wine.svmlin = svm(quality~.,data=wine.train, kernel="linear", scale=TRUE, cost=0.1)

wine.sig = tune(svm, quality~.,data=wine.train, kernel="sigmoid", scale=TRUE, ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10)))
wine.svmsig = svm(quality~.,data=wine.train, kernel="sigmoid", scale=TRUE, cost=0.1)

(wine.predpoly = predict(wine.polysvm, newdata=wine.test)
table(wine.predpoly, wine.test$quality)

wine.predlin = predict(wine.svmlin, newdata=wine.test)
table(wine.predlin, wine.test$quality)

wine.predsig = predict(wine.svmsig, newdata=wine.test)
table(wine.predsig, wine.test$quality)


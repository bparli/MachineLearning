install.packages("adabag")
library(adabag)

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
wine$quality = as.factor(wine$quality)

set.seed(1)
samp.wine=floor(nrow(wine)*0.75)
wine.train_ind <- sample(seq_len(nrow(wine)), size = samp.wine)
wine.train=wine[wine.train_ind,]
wine.test=wine[-wine.train_ind,]

training = c(0.50,0.55,0.60,0.65,0.70,0.75,0.80)


for(iter in 1:7) {
  tmp = system.time(boosting(quality~.,data=wine.train, mfinal=iters[iter], boos=TRUE))
  times[iter] = tmp[1]
}

wine.boost = boosting(quality~.,data=wine.train, mfinal=100, boos=TRUE)
wine.pred_train = predict(wine.boost, newdata=wine.train)
wine.pred_train$confusion
wine.pred_train$error

wine.pred = predict(wine.boost, newdata=wine.test)
wine.pred$confusion
wine.pred$error


boost_test_errs = c()
boost_train_errs = c()
samplings = c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.9)
for(samp in 1:8){
  set.seed(1)
  samp.wine=floor(nrow(wine)*samplings[samp])
  wine.train_ind <- sample(seq_len(nrow(wine)), size = samp.wine)
  wine.train=wine[wine.train_ind,]
  wine.test=wine[-wine.train_ind,]
  
  wine.boost = boosting(quality~.,data=wine.train, mfinal=100, boos=TRUE)
  wine.pred_train = predict(wine.boost, newdata=wine.train)
  boost_train_errs[samp] = wine.pred_train$error
  
  wine.pred_test = predict(wine.boost, newdata=wine.test)
  boost_test_errs[samp] = wine.pred_test$error
}

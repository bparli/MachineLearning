install.packages("class")
library(class)

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

mins = apply(wine[,-12], 2, min)
maxs = apply(wine[,-12], 2, max)
scaled = as.data.frame(scale(wine[,-12], center = mins, scale = maxs - mins))
scaled = cbind(scaled,quality=wine$quality)

knn_test_errs = c()
knn_train_errs = c()
samplings = c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.9)
for(samp in 1:8){
  set.seed(1)
  samp.wine=floor(nrow(scaled)*samplings[samp])
  wine.train_ind <- sample(seq_len(nrow(scaled)), size = samp.wine)
  wine.train=scaled[wine.train_ind,]
  wine.test=scaled[-wine.train_ind,]
  
  wine.knn = knn(train=wine.train[,-12],test=wine.train[,-12], cl=wine.train[,12], k=3)
  tr_matrix = table(wine.knn, wine.train[,12])
  knn_train_errs[samp] = 1-((tr_matrix[1,1]+tr_matrix[2,2]+tr_matrix[3,3]+tr_matrix[4,4]+tr_matrix[5,5]+ tr_matrix[6,6])/nrow(wine.train))
  
  wine.knn_test = knn(train=wine.train[,-12],test=wine.test[,-12], cl=wine.train[,12], k=3)
  test_matrix = table(wine.knn_test, wine.test[,12]) 
  knn_test_errs[samp] = 1-((test_matrix[1,1]+test_matrix[2,2]+test_matrix[3,3]+test_matrix[4,4]+test_matrix[5,5]+ test_matrix[6,6])/nrow(wine.test))
}

set.seed(1)
samp.wine=floor(nrow(scaled)*0.75)
wine.train_ind <- sample(seq_len(nrow(scaled)), size = samp.wine)
wine.train=scaled[wine.train_ind,]
wine.test=scaled[-wine.train_ind,]


wine.knn = knn(train=wine.train[,-12],test=wine.train[,-12], cl=wine.train[,12], k=3)
tr_matrix = table(wine.knn, wine.train[,12])

wine.knn_test = knn(train=wine.train[,-12],test=wine.test[,-12], cl=wine.train[,12], k=3)
test_matrix = table(wine.knn_test, wine.test[,12])
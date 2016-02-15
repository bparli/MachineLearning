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

library(nnet)

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


nn_test_errs = c()
nn_train_errs = c()
samplings = c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.9)
for(samp in 1:8){
  set.seed(1)
  samp.wine=floor(nrow(scaled)*samplings[samp])
  wine.train_ind <- sample(seq_len(nrow(neuralscaled)), size = samp.wine)
  wine.train=neuralscaled[wine.train_ind,]
  wine.test=neuralscaled[-wine.train_ind,]
  
  wine.nn = nnet(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=wine.train, size=12, decay=5e-4, maxit=1000)
  wine.pred = predict(wine.nn, wine.train,type="class")
  tr_matrix = table(wine.pred, wine.train$quality)
  nn_train_errs[samp] = 1-((tr_matrix[1,1]+tr_matrix[2,2]+tr_matrix[3,3]+tr_matrix[4,4]+tr_matrix[5,5]+ tr_matrix[6,6])/nrow(wine.train))
  
  wine.pred = predict(wine.nn,wine.test,type="class")  
  test_matrix = table(wine.pred, wine.test$quality) 
  nn_test_errs[samp] = 1-((test_matrix[1,1]+test_matrix[2,2]+test_matrix[3,3]+test_matrix[4,4]+test_matrix[5,5]+ test_matrix[6,6])/nrow(wine.test))
}

library(C50)

wine = read.csv2(file="C:/Users/bparli/Downloads/winequality-red.csv", header=TRUE)
wine$quality=as.factor(wine$quality)

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

tree_test_errs = c()
tree_train_errs = c()
samplings = c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.9)
for(samp in 1:8){
  set.seed(1)
  samp.wine=floor(nrow(wine)*samplings[samp])
  wine.train_ind <- sample(seq_len(nrow(wine)), size = samp.wine)
  wine.train=wine[wine.train_ind,]
  wine.test=wine[-wine.train_ind,]
  
  wine.tree=C5.0(wine.train$quality~.,data=wine.train, nglobalpruning=TRUE)
  wine.pred=predict(wine.tree, wine.train, type="class")
  
  tr_matrix = table(wine.pred, wine.train$quality)
  tree_train_errs[samp] = 1-((tr_matrix[1,1]+tr_matrix[2,2]+tr_matrix[3,3]+tr_matrix[4,4]+tr_matrix[5,5]+ tr_matrix[6,6])/nrow(wine.train))
  
  wine.pred = predict(wine.tree, wine.test, type="class")
  test_matrix = table(wine.pred, wine.test$quality) 
  tree_test_errs[samp] = 1-((test_matrix[1,1]+test_matrix[2,2]+test_matrix[3,3]+test_matrix[4,4]+test_matrix[5,5]+ test_matrix[6,6])/nrow(wine.test))
}

df_train = data.frame(samplings, svm_train_errs, knn_train_errs, nn_train_errs, tree_train_errs)
ggplot(df_train, aes(samplings, y = value, color = variable)) + 
  geom_line(aes(y = svm_train_errs, col = "SVM Train Errors")) + 
  geom_line(aes(y = knn_train_errs, col = "KNN Train Errors")) +
  geom_line(aes(y = nn_train_errs, col = "NN Train Errors")) +
  geom_line(aes(y = tree_train_errs, col = "Tree Train Errors"))


df_test = data.frame(samplings, svm_test_errs, knn_test_errs, nn_test_errs, tree_test_errs)
ggplot(df_test, aes(samplings, y = value, color = variable)) + 
  geom_line(aes(y = svm_test_errs, col = "SVM Test Errors")) + 
  geom_line(aes(y = knn_test_errs, col = "KNN Test Errors")) +
  geom_line(aes(y = nn_test_errs, col = "NN Test Errors")) +
  geom_line(aes(y = tree_test_errs, col = "Tree Test Errors")) 


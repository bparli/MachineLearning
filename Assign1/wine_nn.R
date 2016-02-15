install.packages("nnet")
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



